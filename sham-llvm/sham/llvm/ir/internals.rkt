#lang racket

(require ffi/unsafe
         sham/llvm/ffi
         sham/llvm/ir/env)

(provide create-internal-environment)

(define (create-internal-environment context)
  (register-llvm-internal-instructions (empty-assoc-env) context))

(define (register-llvm-internal-instructions env)
  (define (unary-builder llvm-op)
    (lambda (llvm-builder flags args compile-type compile-value result-name)
      (llvm-op llvm-builder (compile-value (first args)) result-name)))
  (define (binary-builder llvm-op)
    (lambda (llvm-builder flags args compile-type compile-value result-name)
      (llvm-op llvm-builder (compile-value (first args)) (compile-value (second args)) result-name)))
  (define (ternary-builder llvm-op)
    (lambda (llvm-builder flags args compile-type compile-value result-name)
      (define args^ (map compile-value args))
      (llvm-op llvm-builder (first args^) (second args^) (third args^) result-name)))
  (define (register-basic-internals intr reg env)
    (assoc-env-extend env (car intr) (reg (cdr intr))))
  (define (register-int-predicates env)
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
               (pr-name '(icmp-eq
                          icmp-ne
                          icmp-ugt
                          icmp-uge
                          icmp-ult
                          icmp-ule
                          icmp-sgt
                          icmp-sge
                          icmp-slt
                          icmp-sle))]
      (assoc-env-extend env pr-name
                        (lambda (llvm-builder flags args compile-type compile-value result-name)
                          (LLVMBuildICmp llvm-builder
                                         predicate
                                         (compile-value (first args))
                                         (compile-value (second args))
                                         result-name)))))
  (define (register-real-predicates env)
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
               (pr-name '(fcmp-oeq
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
      (assoc-env-extend env pr-name
                        (lambda (llvm-builder flags args compile-type compile-value result-name)
                          (LLVMBuildFCmp llvm-builder
                                         predicate
                                         (compile-value (first args))
                                         (compile-value (second args))
                                         result-name)))))
  (define specials
    (list (cons 'store!
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildStore llvm-builder
                                  (compile-value (first args))
                                  (compile-value (second args)))))
          (cons 'free
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildFree llvm-builder (compile-value (first args)))))
          (cons 'malloc
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildMalloc llvm-builder (compile-type (first args)) result-name)))
          (cons 'array-malloc
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildArrayMalloc llvm-builder
                                        (compile-type (first args))
                                        (compile-value (second args))
                                        result-name)))
          (cons 'alloca (λ (llvm-builder flags args compile-type compile-value result-name)
                          (LLVMBuildAlloca llvm-builder (compile-type (first args)) result-name)))
          (cons 'array-alloca
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildArrayAlloca llvm-builder
                                        (compile-type (first args))
                                        (compile-value (second args))
                                        result-name)))
          (cons 'gep
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildGEP llvm-builder
                                (compile-type (first args))
                                (map compile-value (cdr args))
                                result-name)))))
  (define (register-specials env)
    (for/fold ([env env])
              ([special specials])
      (assoc-env-extend env (car special) (cdr special))))

  (define (register-internals intrs reg env)
    (for/fold ([env env])
              ([intr intrs])
      (register-basic-internals intr reg env)))
  (register-real-predicates
   (register-int-predicates
    (register-specials
     (register-internals
      unary-internals unary-builder
      (register-internals
       binary-internals binary-builder
       (register-internals
        ternary-internals ternary-builder
        env)))))))

(define ternary-internals
  `((insertelement ,LLVMBuildInsertElement)))

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

    ;; vector
    (extract-element ,LLVMBuildExtractElement)

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
    (addr-space-cast ,LLVMBuildAddrSpaceCast)
    (zext-or-bit-cast ,LLVMBuildZExtOrBitCast)
    (sext-or-bit-cast ,LLVMBuildSExtOrBitCast)
    (ptr-cast  ,LLVMBuildPointerCast)
    (int-cast  ,LLVMBuildIntCast)
    (fp-cast   ,LLVMBuildFPCast)))


(define unary-internals
  `((neg ,LLVMBuildNeg)
    (neg-nsw ,LLVMBuildNSWNeg)
    (neg-nuw ,LLVMBuildNUWNeg)
    (fneg ,LLVMBuildFNeg)

    (not ,LLVMBuildNot)
    (load ,LLVMBuildLoad)))


(module+ test
  (display (register-llvm-internal-instructions (empty-assoc-env))))
