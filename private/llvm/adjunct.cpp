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

LLVMBool LLVMCreateMCJITCompilerForModuleWithTarget(
    LLVMExecutionEngineRef *OutJIT, LLVMModuleRef M,
    LLVMMCJITCompilerOptions *options, char **OutError) {
  TargetOptions targetOptions;
  targetOptions.EnableFastISel = options->EnableFastISel;

  std::unique_ptr<Module> Mod(unwrap(M));
  std::string Error;
  // TargetMachine* TM(reinterpret_cast<TargetMachine* >(TMR));

  auto TargetTriple = sys::getDefaultTargetTriple();
  auto Target = TargetRegistry::lookupTarget (TargetTriple, Error);
  auto RM = Optional<Reloc::Model>();

  printf ("creating mcjit for target with triple: %s\n", TargetTriple.c_str());
  printf ("host cpu: %s\n", sys::getHostCPUName ().data ());

  SubtargetFeatures features;
  StringMap<bool> HostFeatures;
  if (sys::getHostCPUFeatures(HostFeatures)) {
    for (auto &F : HostFeatures) {
      features.AddFeature(F.first(), F.second);
    }
  }
  printf ("feature string: %s\n", features.getString().c_str());
  auto TargetMachine = Target->createTargetMachine (TargetTriple,
						    sys::getHostCPUName(),
						    features.getString(),
						    targetOptions, RM);
  
  EngineBuilder builder(std::move(Mod));
  builder.setEngineKind(EngineKind::JIT)
         .setErrorStr(&Error)
         .setOptLevel((CodeGenOpt::Level)options->OptLevel)
         .setCodeModel(unwrap(options->CodeModel))
         .setTargetOptions(targetOptions);

  if (options->MCJMM) {
    builder.setMCJITMemoryManager(
      std::unique_ptr<RTDyldMemoryManager>(unwrap(options->MCJMM)));
  }
  if (ExecutionEngine *JIT = builder.create(TargetMachine)) {
    *OutJIT = wrap(JIT);
    return 0;
  }
  *OutError = strdup(Error.c_str());
  return 1;
}

void LLVMAdjunctAddGlobalMapping(LLVMExecutionEngineRef engine,
				 LLVMValueRef fvalue,
				 char* libname, char* fname) {
  void* flib = dlopen (libname, RTLD_LAZY);
  void* fnameptr = dlsym (flib, fname);
  LLVMAddGlobalMapping (engine, fvalue, fnameptr);
  dlclose (flib);
}

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */
