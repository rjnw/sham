#include "llvm-c/Types.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CodeGenCWrappers.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetOptions.h"

#include "stdlib.h"
#include "dlfcn.h"

#ifdef __cplusplus
extern "C" {
#endif

using namespace llvm;

LLVMBool LLVMCreateMCJITCompilerForModuleWithTarget(
    LLVMExecutionEngineRef *OutJIT, LLVMModuleRef M, LLVMTargetMachineRef TMR,
    LLVMMCJITCompilerOptions *options, char **OutError) {
  TargetOptions targetOptions;
  targetOptions.EnableFastISel = options->EnableFastISel;

  std::unique_ptr<Module> Mod(unwrap(M));
  std::string Error;
  TargetMachine* TM(reinterpret_cast<TargetMachine* >(TMR));
  
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
  if (ExecutionEngine *JIT = builder.create(TM)) {
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
