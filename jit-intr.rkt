#lang racket
(require ffi/unsafe)
(require "llvm/ffi/all.rkt")
(require "jit-env.rkt")
(require "jit-type.rkt")
(provide register-jit-internals)

(define (register-jit-internals env context)
  (register-internal-instructions env))

(define (register-internal-instructions env)
  (define (get-unary-compiler llvm-builder)
    (lambda (jit-builder args [name "v"])
      (llvm-builder jit-builder (first args) name)))
  (define (get-binary-compiler llvm-builder)
    (lambda (jit-builder args [name "v"])
      (llvm-builder jit-builder (first args) (second args) name)))
  (define (register-internal intr reg env)
    (env-extend (car intr) (env-jit-intr-function (reg (cadr intr))) env))
  (define (register-int-predicate env)
    (for/fold [(env env)]
              [(predicate '(LLVMIntEQ
                            LLVMIntNE
                            LLVMIntUGT
                            LLVMIntUGE
                            LLVMIntULT
                            LLVMIntULE
                            LLVMIntSGT
                            LLVMIntSGE
                            LLVMIntSLT
                            LLVMIntSLE))
               (pr '(jit-icmp-eq
                     jit-icmp-ne
                     jit-icmp-ugt
                     jit-icmp-uge
                     jit-icmp-ult
                     jit-icmp-ule
                     jit-icmp-sgt
                     jit-icmp-sge
                     jit-icmp-slt
                     jit-icmp-sle))]
      (env-extend pr
                  (env-jit-intr-function
                   (lambda (jit-builder args [name "ipred"])
                     (LLVMBuildICmp jit-builder
                                    predicate
                                    (first args)
                                    (second args)
                                    name)))
                  env)))
  (define (register-real-predicate env)
    (for/fold [(env env)]
              [(predicate '(LLVMRealOEQ
                            LLVMRealOGT
                            LLVMRealOGE
                            LLVMRealOLT
                            LLVMRealOLE
                            LLVMRealONE
                            LLVMRealORD
                            LLVMRealUNO
                            LLVMRealUEQ
                            LLVMRealUGT
                            LLVMRealUGE
                            LLVMRealULT
                            LLVMRealULE
                            LLVMRealUNE))
               (pr '(jit-fcmp-oeq
                     jit-fcmp-ogt
                     jit-fcmp-oge
                     jit-fcmp-olt
                     jit-fcmp-ole
                     jit-fcmp-one
                     jit-fcmp-ord
                     jit-fcmp-uno
                     jit-fcmp-ueq
                     jit-fcmp-ugt
                     jit-fcmp-uge
                     jit-fcmp-ult
                     jit-fcmp-ule
                     jit-fcmp-une))]
      (env-extend pr
                  (env-jit-intr-function
                   (lambda (jit-builder args [name "fpred"])
                     (LLVMBuildICmp jit-builder
                                    predicate
                                    (first args)
                                    (second args)
                                    name)))
                  env)))
  (define (register-specials env)
    (env-extend 'jit-store!
                (env-jit-intr-function
                 (lambda (jit-builder args [name "v"])
                   (LLVMBuildStore jit-builder (first args) (second args))))
                env))
  (define (register-internals intrs reg env)
    (for/fold ([env env])
              ([intr intrs])
      (register-internal intr reg env)))
  (register-real-predicate
   (register-int-predicate
    (register-specials
     (register-internals
      unary-internals get-unary-compiler
      (register-internals
       binary-internals get-binary-compiler
       env))))))

(define binary-internals
  `((jit-add ,LLVMBuildAdd)
    (jit-add-nsw ,LLVMBuildNSWAdd)
    (jit-add-nuw ,LLVMBuildNUWAdd)
    (jit-fadd ,LLVMBuildFAdd)

    (jit-sub ,LLVMBuildSub)
    (jit-sub-nsw ,LLVMBuildNSWSub)
    (jit-sub-nuw ,LLVMBuildNUWSub)
    (jit-fsub ,LLVMBuildFSub)

    (jit-mul ,LLVMBuildMul)
    (jit-mul-nsw ,LLVMBuildNSWMul)
    (jit-mul-nuw ,LLVMBuildNUWMul)
    (jit-fmul ,LLVMBuildFMul)

    (jit-udiv ,LLVMBuildUDiv)
    (jit-sdiv ,LLVMBuildSDiv)
    (jit-exact-sdiv ,LLVMBuildExactSDiv)
    (jit-fdiv ,LLVMBuildFDiv)

    (jit-urem ,LLVMBuildURem)
    (jit-srem ,LLVMBuildSRem)
    (jit-frem ,LLVMBuildFRem)

    (jit-shl ,LLVMBuildShl)
    (jit-lshr ,LLVMBuildLShr)
    (jit-ashr ,LLVMBuildAShr)

    (jit-or ,LLVMBuildOr)
    (jit-xor ,LLVMBuildXor)
    (jit-and ,LLVMBuildAnd)

    (jit-arr-malloc ,LLVMBuildArrayMalloc)
    (jit-arr-alloca ,LLVMBuildArrayAlloca)

    ;;casts
    (jit-fp->ui ,LLVMBuildFPToUI)
    (jit-fp->si ,LLVMBuildFPToSI)
    (jit-ui->fp ,LLVMBuildUIToFP)
    (jit-si->fp ,LLVMBuildSIToFP)
    ))

(define unary-internals
  `((jit-neg ,LLVMBuildNeg)
    (jit-neg-nsw ,LLVMBuildNSWNeg)
    (jit-neg-nuw ,LLVMBuildNUWNeg)
    (jit-fneg ,LLVMBuildFNeg)

    (jit-not ,LLVMBuildNot)
    (jit-load ,LLVMBuildLoad)
    (jit-malloc ,LLVMBuildMalloc)

    (jit-alloca ,LLVMBuildAlloca)

    (jit-free ,LLVMBuildFree)))

(module+ test
  (display (register-internal-instructions (empty-env))))
