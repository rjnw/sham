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

  LLVMTargetMachineRef LLVMCreateCurrentTargetMachineRef (){
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

  LLVMBool LLVMRunOurModulePasses(LLVMModuleRef M) {
    auto MPM = legacy::PassManager();
    auto TLII = TargetLibraryInfoImpl(Triple(sys::getProcessTriple()));

    MPM.add(new TargetLibraryInfoWrapperPass(TLII));
    MPM.add(createPromoteMemoryToRegisterPass());
    MPM.add(createInstructionCombiningPass());
    MPM.add(createReassociatePass());
    MPM.add(createGVNPass());
    MPM.add(createCFGSimplificationPass());

    // MPM->add(createForceFunctionAttrsLegacyPass());

    // MPM->add(createCFLSteensAAWrapperPass());
    // MPM->add(createCFLAndersAAWrapperPass());

    // MPM->add(createTypeBasedAAWrapperPass());
    // MPM->add(createScopedNoAliasAAWrapperPass());


    // MPM->add(createInferFunctionAttrsLegacyPass());


    // MPM->add(createIPSCCPPass());          // IP SCCP
    // MPM->add(createGlobalOptimizerPass()); // Optimize out global vars

    // MPM->add(createDeadArgEliminationPass()); // Dead argument elimination
    // MPM->add(createPruneEHPass()); // Remove dead EH info

    // MPM->add(createGlobalsAAWrapperPass());
    // //;;function simplifications
    // MPM->add(createSROAPass());
    // MPM->add(createEarlyCSEPass(1));
    // MPM->add(createGVNHoistPass());

    // // MPM->add(createGVNSinkPass());
    // MPM->add(createCFGSimplificationPass());

    // MPM->add(createSpeculativeExecutionIfHasBranchDivergencePass());
    // MPM->add(createJumpThreadingPass());         // Thread jumps.
    // MPM->add(createCorrelatedValuePropagationPass()); // Propagate conditionals
    // MPM->add(createCFGSimplificationPass());     // Merge & remove BBs

    // MPM->add(createInstructionCombiningPass(1)); //


    // MPM->add(createTailCallEliminationPass()); // Eliminate tail calls
    // MPM->add(createCFGSimplificationPass());     // Merge & remove BBs
    // MPM->add(createReassociatePass());           // Reassociate expressions

    // MPM->add(createLoopRotatePass(1));

    // MPM->add(createLICMPass());                  // Hoist loop invariants

    // // MPM->add(createSimpleLoopUnswitchLegacyPass());
    // // MPM->add(createLoopUnswitchPass(1, 0));

    // MPM->add(createCFGSimplificationPass());

    // MPM->add(createInstructionCombiningPass(1));

    // MPM->add(createIndVarSimplifyPass());        // Canonicalize indvars
    // MPM->add(createLoopIdiomPass());             // Recognize idioms like memset.

    // MPM->add(createLoopDeletionPass());          // Delete dead loops

    // MPM->add(createLoopInterchangePass()); // Interchange loops
    // MPM->add(createCFGSimplificationPass());
    // MPM->add(createSimpleLoopUnrollPass());    // Unroll small loops

    // MPM->add(createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds
    // MPM->add(createNewGVNPass());

    // MPM->add(createMemCpyOptPass());             // Remove memcpy / form memset
    // MPM->add(createSCCPPass());                  // Constant prop with SCCP

    // MPM->add(createBitTrackingDCEPass());        // Delete dead bit computations

    // // Run instcombine after redundancy elimination to exploit opportunities
    // // opened up by them.
    // MPM->add(createInstructionCombiningPass(1));

    // MPM->add(createJumpThreadingPass());         // Thread jumps
    // MPM->add(createCorrelatedValuePropagationPass());
    // MPM->add(createDeadStoreEliminationPass());  // Delete dead stores
    // MPM->add(createLICMPass());

    // MPM->add(createLoopRerollPass());

    // MPM->add(createSLPVectorizerPass()); // Vectorize parallel scalar chains.
    // MPM->add(createAggressiveDCEPass());         // Delete dead instructions
    // MPM->add(createCFGSimplificationPass()); // Merge & remove BBs
    // // Clean up after everything.

    // //functino simplificationsend
    // InlineParams IP;
    // IP.DefaultThreshold = 75;
    // // FIXME: The hint threshold has the same value used by the regular inliner.
    // // This should probably be lowered after performance testing.
    // IP.HintThreshold = 325;

    // MPM->add(createFunctionInliningPass(IP));
    // MPM->add(createSROAPass());
    // MPM->add(createEarlyCSEPass());             // Catch trivial redundancies
    // MPM->add(createCFGSimplificationPass());    // Merge & remove BBs




    // MPM->add(createEliminateAvailableExternallyPass());
    // MPM->add(createGlobalOptimizerPass());
    // MPM->add(createLoopVersioningLICMPass());    // Do LoopVersioningLICM
    // MPM->add(createLICMPass());                  // Hoist loop invariants

    // MPM->add(createGlobalsAAWrapperPass());

    // MPM->add(createFloat2IntPass());
    // MPM->add(createLoopRotatePass(-1)); //default 16

    // MPM->add(createLoopDistributePass());

    // MPM->add(createLoopVectorizePass(0, 1));
    // MPM->add(createLoopLoadEliminationPass());

    // MPM->add(createInstructionCombiningPass(1));

    // MPM->add(createEarlyCSEPass());
    // MPM->add(createCorrelatedValuePropagationPass());
    // MPM->add(createInstructionCombiningPass(1));

    // MPM->add(createLICMPass());
    // MPM->add(createLoopUnswitchPass(1));//tune this
    // MPM->add(createCFGSimplificationPass());
    // MPM->add(createInstructionCombiningPass(1));


    // MPM->add(createSLPVectorizerPass()); // Vectorize parallel scalar chains.
    // MPM->add(createEarlyCSEPass());

    // // MPM->add(createLateCFGSimplificationPass()); // Switches to lookup tables
    // MPM->add(createInstructionCombiningPass(1));

    // MPM->add(createLoopUnrollPass(4, 400, 0, 1, 1));    // Unroll small loops
    // // Pass *llvm::createLoopUnrollPass(int OptLevel, int Threshold, int Count,
    // //                                  int AllowPartial, int Runtime,
    // //                                  int UpperBound) {
    // //   // TODO: It would make more sense for this function to take the optionals
    // //   // directly, but that's dangerous since it would silently break out of tree
    // //   // callers.
    // // Pass *llvm::createSimpleLoopUnrollPass(int OptLevel) {
    // //   return llvm::createLoopUnrollPass(OptLevel, -1, -1, 0, 0, 0);
    // // }

    // // LoopUnroll may generate some redundency to cleanup.
    // MPM->add(createInstructionCombiningPass(1));

    // // Runtime unrolling will introduce runtime check in loop prologue. If the
    // // unrolled loop is a inner loop, then the prologue will be inside the
    // // outer loop. LICM pass can help to promote the runtime check out if the
    // // checked value is loop invariant.
    // MPM->add(createLICMPass());

    // // After vectorization and unrolling, assume intrinsics may tell us more
    // // about pointer alignments.
    // MPM->add(createAlignmentFromAssumptionsPass());

    // MPM->add(createGlobalDCEPass());         // Remove dead fns and globals.
    // MPM->add(createConstantMergePass());     // Merge dup global constants


    // // LoopSink pass sinks instructions hoisted by LICM, which serves as a
    // // canonicalization pass that enables other optimizations. As a result,
    // // LoopSink pass needs to be a very late IR pass to avoid undoing LICM
    // // result too early.
    // MPM->add(createLoopSinkPass());
    // // Get rid of LCSSA nodes.
    // MPM->add(createInstructionSimplifierPass());

    // // LoopSink (and other loop passes since the last simplifyCFG) might have
    // // resulted in single-entry-single-exit or empty blocks. Clean up the CFG.
    // MPM->add(createCFGSimplificationPass());



    MPM.run(*unwrap(M));
    return 1;
  }

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */
