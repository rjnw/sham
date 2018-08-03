#include "llvm-c/Types.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm-c/Core.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CodeGenCWrappers.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/SubtargetFeature.h"

#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLAndersAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLAndersAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"

#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

// #include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/ForceFunctionAttrs.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/Transforms/IPO/InferFunctionAttrs.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
// #include "llvm/Transforms/Scalar/SimpleLoopUnswitch.h"
#include "llvm/Transforms/Vectorize.h"

#include "llvm/IR/LegacyPassManager.h"



#include "stdlib.h"
#include "dlfcn.h"

#ifdef __cplusplus
extern "C" {
#endif

  using namespace llvm;

  TargetMachine* LLVMCreateCurrentTargetMachine () {
    TargetOptions targetOptions;
    auto targetTriple = sys::getProcessTriple();
    std::string error;
    auto target = TargetRegistry::lookupTarget (targetTriple, error);
    auto relocModel = Optional<Reloc::Model>(Reloc::Static);

    SubtargetFeatures features;
    StringMap<bool> hostFeatures;
    if (sys::getHostCPUFeatures(hostFeatures)) {
      for (auto &feature : hostFeatures) {
	features.AddFeature(feature.first(), feature.second);
      }
    }
    auto targetMachine = target->createTargetMachine(targetTriple,
						     sys::getHostCPUName(),
						     features.getString(),
						     targetOptions, relocModel);
    return targetMachine;
  }

  LLVMTargetMachineRef LLVMCreateCurrentTargetMachineRef () {
    return
      reinterpret_cast<LLVMTargetMachineRef>(const_cast<TargetMachine *>
					     (LLVMCreateCurrentTargetMachine ()));
  }

  LLVMBool LLVMCreateMCJITCompilerForModuleWithTarget(LLVMExecutionEngineRef *outJIT, LLVMModuleRef moduleRef,
						      LLVMMCJITCompilerOptions *options, char **outError) {
    TargetOptions targetOptions;
    targetOptions.EnableFastISel = options->EnableFastISel;

    std::unique_ptr<Module> module(unwrap(moduleRef));
    std::string error;
    EngineBuilder builder(std::move(module));
    builder.setEngineKind(EngineKind::JIT)
      .setErrorStr(&error)
      .setOptLevel((CodeGenOpt::Level)options->OptLevel)
      .setMCPU (sys::getHostCPUName())
      .setCodeModel(unwrap(options->CodeModel))
      .setTargetOptions(targetOptions);

    if (options->MCJMM) {
      builder.setMCJITMemoryManager(
				    std::unique_ptr<RTDyldMemoryManager>(unwrap(options->MCJMM)));
    }
    if (ExecutionEngine *jit = builder.create(LLVMCreateCurrentTargetMachine())) {
      *outJIT = wrap(jit);
      return 0;
    }
    *outError = strdup(error.c_str());
    return 1;
  }


  // copied from llc
  static bool addPass(PassManagerBase &PM, const char *argv0,
                      StringRef PassName, TargetPassConfig &TPC) {
    if (PassName == "none")
      return false;

    const PassRegistry *PR = PassRegistry::getPassRegistry();
    const PassInfo *PI = PR->getPassInfo(PassName);
    Pass *P;
    if (PI->getNormalCtor())
      P = PI->getNormalCtor()();
    else {
      errs() << argv0 << ": cannot create pass: " << PI->getPassName() << "\n";
      return true;
    }
    std::string Banner = std::string("After ") + std::string(P->getPassName());
    PM.add(P);
    TPC.printAndVerify(Banner);

    return false;
  }


  static AnalysisID getPassID(const char *argv0, const char *OptionName,
                              StringRef PassName) {
    if (PassName.empty())
      return nullptr;

    const PassRegistry &PR = *PassRegistry::getPassRegistry();
    const PassInfo *PI = PR.getPassInfo(PassName);
    if (!PI) {
      errs() << argv0 << ": " << OptionName << " pass is not registered.\n";
      exit(1);
    }
    return PI->getTypeInfo();
  }

  static int optimizeModule(CodeGenOpt::Level OptLevel, Module* M) {
    std::string CPUStr = getCPUStr(), FeaturesStr = getFeaturesStr();
    // Build up all of the passes that we want to do to the module.
    legacy::PassManager PM;

    // Add an appropriate TargetLibraryInfo pass for the module's triple.
    TargetLibraryInfoImpl TLII(Triple(M->getTargetTriple()));

    // The -disable-simplify-libcalls flag actually disables all builtin optzns.
    if (DisableSimplifyLibCalls)
      TLII.disableAllFunctions();
    PM.add(new TargetLibraryInfoWrapperPass(TLII));

    // Add the target data from the target machine, if it exists, or the module.
    M->setDataLayout(Target->createDataLayout());


      // Override function attributes based on CPUStr, FeaturesStr, and command line
    // flags.
    setFunctionAttributes(CPUStr, FeaturesStr, *M);
  }


  void LLVMPassManagerBuilderSetLoopVectorize(LLVMPassManagerBuilderRef PMB, LLVMBool Value) {
    PassManagerBuilder *Builder = unwrap(PMB);
    Builder->LoopVectorize=Value;
  }

  void LLVMPassManagerBuilderSetSLPVectorize(LLVMPassManagerBuilderRef PMB, LLVMBool Value) {
    PassManagerBuilder *Builder = unwrap(PMB);
    Builder->SLPVectorize=Value;
  }

  void LLVMPassManagerBuilderSetInliner(LLVMPassManagerBuilderRef PMB, unsigned OptLevel, unsigned SizeLevel) {
    PassManagerBuilder *Builder = unwrap(PMB);
    Builder->Inliner = createFunctionInliningPass(OptLevel, SizeLevel, false);
  }

  void LLVMTargetMachineAdjustPassManagerBuilder(LLVMPassManagerBuilderRef PMB, LLVMTargetMachineRef T) {
    TargetMachine *TM = unwrap(T);
    PassManagerBuilder *Builder = unwrap(PMB);
    TM->adjustPassManager(Builder);
  }



  static void AddOptimizationPasses(legacy::PassManagerBase &MPM,
                                    legacy::FunctionPassManager &FPM,
                                    TargetMachine *TM, unsigned OptLevel,
                                    unsigned SizeLevel) {
    if (!NoVerify || VerifyEach)
      FPM.add(createVerifierPass()); // Verify that input is correct

    PassManagerBuilder Builder;
    Builder.OptLevel = OptLevel;
    Builder.SizeLevel = SizeLevel;

    if (DisableInline) {
      // No inlining pass
    } else if (OptLevel > 1) {
      Builder.Inliner = createFunctionInliningPass(OptLevel, SizeLevel, false);
    } else {
      Builder.Inliner = createAlwaysInlinerLegacyPass();
    }
    Builder.DisableUnitAtATime = !UnitAtATime;
    Builder.DisableUnrollLoops = (DisableLoopUnrolling.getNumOccurrences() > 0) ?
      DisableLoopUnrolling : OptLevel == 0;

    // This is final, unless there is a #pragma vectorize enable
    if (DisableLoopVectorization)
      Builder.LoopVectorize = false;
    // If option wasn't forced via cmd line (-vectorize-loops, -loop-vectorize)
    else if (!Builder.LoopVectorize)
      Builder.LoopVectorize = OptLevel > 1 && SizeLevel < 2;

    // When #pragma vectorize is on for SLP, do the same as above
    Builder.SLPVectorize = DisableSLPVectorization ? false : OptLevel > 1 && SizeLevel < 2;

    if (TM)
      TM->adjustPassManager(Builder);

    if (Coroutines)
      addCoroutinePassesToExtensionPoints(Builder);

    Builder.populateFunctionPassManager(FPM);
    Builder.populateModulePassManager(MPM);
  }


  int LLVMCustomInitializeCL(int argc, char **argv) {
    sys::PrintStackTraceOnErrorSignal(argv[0]);
    llvm::PrettyStackTraceProgram X(argc, argv);

    // Enable debug stream buffering.
    EnableDebugBuffering = true;

    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmPrinters();
    InitializeAllAsmParsers();

    // Initialize passes
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    initializeCore(Registry);
    initializeCoroutines(Registry);
    initializeScalarOpts(Registry);
    initializeObjCARCOpts(Registry);
    initializeVectorization(Registry);
    initializeIPO(Registry);
    initializeAnalysis(Registry);
    initializeTransformUtils(Registry);
    initializeInstCombine(Registry);
    initializeInstrumentation(Registry);
    initializeTarget(Registry);
    // For codegen passes, only passes that do IR to IR transformation are
    // supported.
    initializeScalarizeMaskedMemIntrinPass(Registry);
    initializeCodeGenPreparePass(Registry);
    initializeAtomicExpandPass(Registry);
    initializeRewriteSymbolsLegacyPassPass(Registry);
    initializeWinEHPreparePass(Registry);
    initializeDwarfEHPreparePass(Registry);
    initializeSafeStackLegacyPassPass(Registry);
    initializeSjLjEHPreparePass(Registry);
    initializePreISelIntrinsicLoweringLegacyPassPass(Registry);
    initializeGlobalMergePass(Registry);
    initializeInterleavedAccessPass(Registry);
    initializeCountingFunctionInserterPass(Registry);
    initializeUnreachableBlockElimLegacyPassPass(Registry);
    initializeExpandReductionsPass(Registry);
    polly::initializePollyPasses(Registry);

    cl::ParseCommandLineOptions(argc, argv, "llvm cl used internaly for use with jit.");
  }

  legacy::PassManagerBase &LLVMCreatePassManager() {
    // Create a PassManager to hold and optimize the collection of passes we are
    // about to build.
    //
    legacy::PassManager Passes;
    // Add an appropriate TargetLibraryInfo pass for the module's triple.
    TargetLibraryInfoImpl TLII(ModuleTriple);

    // The -disable-simplify-libcalls flag actually disables all builtin optzns.
    if (DisableSimplifyLibCalls)
      TLII.disableAllFunctions();
    Passes.add(new TargetLibraryInfoWrapperPass(TLII));

    // Add internal analysis passes from the target machine.
    Passes.add(createTargetTransformInfoWrapperPass(TM ? TM->getTargetIRAnalysis()
                                                    : TargetIRAnalysis()));
  }

  legacy::FunctionPassManager &LLVMCreateFunctionPassManager(TargetMachine* TM) {
    std::unique_ptr<legacy::FunctionPassManager> FPasses;
    FPasses.reset(new legacy::FunctionPassManager(TargetMachine *TM));
    FPasses->add(createTargetTransformInfoWrapperPass(TM ? TM->getTargetIRAnalysis() : TargetIRAnalysis()));
  }

  int LLVMRunFunctionPassManagerOnModule(LLVMPassManagerRef FPMC, LLVMModuleRef MC) {
    legacy::FunctionPassManager *FPM = unwrap<legacy::FunctionPassManager>(FPMC);
    Module* M = unwrap(M)
    FPM->doInitialization();
    for (Function &F : *M) {
      FPM->run(F);
    }
    FPM->doFinalization();
    delete FPM;
  }

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */
