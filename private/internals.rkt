#lang racket
(require ffi/unsafe)
(require "llvm/ffi/all.rkt")
(require "env.rkt")
(require "types.rkt")
(provide register-jit-internals)

;;TODO figure out how we can add constant internals

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
               (pr '(icmp-eq
                     icmp-ne
                     icmp-ugt
                     icmp-uge
                     icmp-ult
                     icmp-ule
                     icmp-sgt
                     icmp-sge
                     icmp-slt
                     icmp-sle))]
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
               (pr '(fcmp-oeq
                     fcmp-ogt
                     fcmp-oge
                     fcmp-olt
                     fcmp-ole
                     fcmp-one
                     fcmp-ord
                     fcmp-uno
                     fcmp-ueq
                     fcmp-ugt
                     fcmp-uge
                     fcmp-ult
                     fcmp-ule
                     fcmp-une))]
      (env-extend pr
                  (env-jit-intr-function
                   (lambda (jit-builder args [name "fpred"])
                     (LLVMBuildFCmp jit-builder
                                    predicate
                                    (first args)
                                    (second args)
                                    name)))
                  env)))
  (define (register-specials env)
    (env-extend 'store!
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
  `((add ,LLVMBuildAdd)
    (add-nsw ,LLVMBuildNSWAdd)
    (add-nuw ,LLVMBuildNUWAdd)
    (fadd ,LLVMBuildFAdd)

    (sub ,LLVMBuildSub)
    (sub-nsw ,LLVMBuildNSWSub)
    (sub-nuw ,LLVMBuildNUWSub)
    (fsub ,LLVMBuildFSub)

    (mul ,LLVMBuildMul)
    (mul-nsw ,LLVMBuildNSWMul)
    (mul-nuw ,LLVMBuildNUWMul)
    (fmul ,LLVMBuildFMul)

    (udiv ,LLVMBuildUDiv)
    (sdiv ,LLVMBuildSDiv)
    (exact-sdiv ,LLVMBuildExactSDiv)
    (fdiv ,LLVMBuildFDiv)

    (urem ,LLVMBuildURem)
    (srem ,LLVMBuildSRem)
    (frem ,LLVMBuildFRem)

    (shl ,LLVMBuildShl)
    (lshr ,LLVMBuildLShr)
    (ashr ,LLVMBuildAShr)

    (or ,LLVMBuildOr)
    (xor ,LLVMBuildXor)
    (and ,LLVMBuildAnd)

    (arr-malloc ,LLVMBuildArrayMalloc)
    (arr-alloca ,LLVMBuildArrayAlloca)

    ;;casts
    (trunc  ,LLVMBuildTrunc)
    (zext   ,LLVMBuildZExt)
    (sext   ,LLVMBuildSExt)
    (fp->ui ,LLVMBuildFPToUI)
    (fp->si ,LLVMBuildFPToSI)
    (ui->fp ,LLVMBuildUIToFP)
    (si->fp ,LLVMBuildSIToFP)
    (fp-trunc ,LLVMBuildFPTrunc)
    (fp-ext   ,LLVMBuildFPExt)
    (ptr->int ,LLVMBuildPtrToInt)
    (int->ptr ,LLVMBuildIntToPtr)
    (bitcast  ,LLVMBuildBitCast)
    (addrspacecast ,LLVMBuildAddrSpaceCast)
    (zextorbitcast ,LLVMBuildZExtOrBitCast)
    (sextorbitcast ,LLVMBuildSExtOrBitCast)
    (ptrcast  ,LLVMBuildPointerCast)
    (intcast  ,LLVMBuildIntCast)
    (fpcast   ,LLVMBuildFPCast)
    ))

(define unary-internals
  `((neg ,LLVMBuildNeg)
    (neg-nsw ,LLVMBuildNSWNeg)
    (neg-nuw ,LLVMBuildNUWNeg)
    (fneg ,LLVMBuildFNeg)

    (not ,LLVMBuildNot)
    (load ,LLVMBuildLoad)
    (malloc ,LLVMBuildMalloc)

    (alloca ,LLVMBuildAlloca)

    (free ,LLVMBuildFree)))

(module+ test
  (display (register-internal-instructions (empty-env))))
