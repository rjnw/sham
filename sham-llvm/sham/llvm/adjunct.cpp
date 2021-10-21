
#include "llvm-c/Types.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm-c/Core.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
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

#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/CallGraphSCCPass.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/RegionPass.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LegacyPassNameParser.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/LinkAllIR.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Coroutines.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <algorithm>
#include <memory>



#include "stdlib.h"
#include "dlfcn.h"

#ifdef __cplusplus
extern "C" {
#endif

  using namespace llvm;

  void LLVMCustomInitializeCL(int argc, char **argv) {
    // sys::PrintStackTraceOnErrorSignal(argv[0]);

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
    // initializeScalarizeMaskedMemIntrinPass(Registry);
    initializeCodeGenPreparePass(Registry);
    initializeAtomicExpandPass(Registry);
    initializeRewriteSymbolsLegacyPassPass(Registry);
    initializeWinEHPreparePass(Registry);
    // initializeDwarfEHPreparePass(Registry);
    initializeSafeStackLegacyPassPass(Registry);
    initializeSjLjEHPreparePass(Registry);
    initializePreISelIntrinsicLoweringLegacyPassPass(Registry);
    initializeGlobalMergePass(Registry);
    initializeInterleavedAccessPass(Registry);
    // initializeCountingFunctionInserterPass(Registry);
    initializeUnreachableBlockElimLegacyPassPass(Registry);
    initializeExpandReductionsPass(Registry);
    // polly::initializePollyPasses(Registry);

    cl::ParseCommandLineOptions(argc, argv, "llvm cl used internaly for use with jit.");
  }

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
      // .setCodeModel(unwrap(options->CodeModel, jit))
      .setTargetOptions(targetOptions);

    if (options->MCJMM) {
      builder.setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>(unwrap(options->MCJMM)));
    }
    if (ExecutionEngine *jit = builder.create(LLVMCreateCurrentTargetMachine())) {
      *outJIT = wrap(jit);
      return 0;
    }
    *outError = strdup(error.c_str());
    return 1;
  }

  void LLVMPassManagerBuilderSetLoopVectorize(LLVMPassManagerBuilderRef pmb, LLVMBool value) {
    PassManagerBuilder *builder = reinterpret_cast<PassManagerBuilder*>(pmb);
    builder->LoopVectorize=value;
  }

  void LLVMPassManagerBuilderSetSLPVectorize(LLVMPassManagerBuilderRef PMB, LLVMBool Value) {
    PassManagerBuilder *Builder = reinterpret_cast<PassManagerBuilder*>(PMB);
    Builder->SLPVectorize=Value;
  }

  void LLVMPassManagerBuilderSetInliner(LLVMPassManagerBuilderRef PMB, unsigned OptLevel, unsigned SizeLevel) {
    PassManagerBuilder *Builder = reinterpret_cast<PassManagerBuilder*>(PMB);
    Builder->Inliner = createFunctionInliningPass(OptLevel, SizeLevel, false);
  }

  void LLVMTargetMachineAdjustPassManagerBuilder(LLVMPassManagerBuilderRef PMB, LLVMTargetMachineRef T) {
    TargetMachine *TM = reinterpret_cast<TargetMachine*>(T);
    PassManagerBuilder *Builder = reinterpret_cast<PassManagerBuilder*>(PMB);
    TM->adjustPassManager(*Builder);
  }

  void LLVMPassManagerAddTargetLibraryInfoPass(LLVMPassManagerRef passManagerRef, LLVMModuleRef moduleRef) {
    legacy::PassManager *pm = unwrap<legacy::PassManager>(passManagerRef);
    Module* module = unwrap(moduleRef);
    TargetLibraryInfoImpl tlii(Triple(module->getTargetTriple()));
    pm->add(new TargetLibraryInfoWrapperPass(tlii));
  }

  void LLVMPassManagerAddTargetIRAnalysis(LLVMPassManagerRef passManagerRef, LLVMTargetMachineRef targetMachineRef) {
    legacy::PassManager *pm = unwrap<legacy::PassManager>(passManagerRef);
    TargetMachine* tm = reinterpret_cast<TargetMachine*>(targetMachineRef);
    pm->add(createTargetTransformInfoWrapperPass(tm->getTargetIRAnalysis()));
  }


#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */
