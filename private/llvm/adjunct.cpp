#include "llvm-c/Types.h"
#include "llvm-c/ExecutionEngine.h"
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

LLVMBool LLVMCreateMCJITCompilerForModuleWithTarget(
    LLVMExecutionEngineRef *outJIT, LLVMModuleRef moduleRef,
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

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */
