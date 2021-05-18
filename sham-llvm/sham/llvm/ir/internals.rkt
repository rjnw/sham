#lang racket

(require ffi/unsafe
         sham/private/env
         sham/llvm/ffi
         sham/llvm/ir/env)

(provide (all-defined-out))
(define (create-internal-environment context)
  (register-llvm-internal-types
   (register-llvm-internal-instructions (empty-assoc-env))
   context))

(define (register-llvm-internal-types env context)
  (define (register-types ts)
    (for/fold [(env env)]
              [(t ts)]
      (assoc-env-extend env (car t) (cadr t))))
  (register-types
   `((i1 ,(LLVMInt1TypeInContext context))
     (i8 ,(LLVMInt8TypeInContext context))
     (i16 ,(LLVMInt16TypeInContext context))
     (i32 ,(LLVMInt32TypeInContext context))
     (i64 ,(LLVMInt64TypeInContext context))
     (i128 ,(LLVMInt128TypeInContext context))
     (f32 ,(LLVMFloatTypeInContext context))
     (f64 ,(LLVMDoubleTypeInContext context))
     (void ,(LLVMVoidTypeInContext context)))))

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
    (assoc-env-extend env (car intr) (reg (cadr intr))))
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
                                (compile-value (first args))
                                (map compile-value (cdr args))
                                result-name)))
          (cons 'app
                (λ (llvm-builder flags args compile-type compile-value result-name)
                  (LLVMBuildCall llvm-builder (compile-value (car args))
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
  `((insert-element ,LLVMBuildInsertElement)))

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

(define-syntax basic-ops
  `(
    icmp-eq icmp-ne icmp-ugt icmp-uge icmp-ult icmp-ule icmp-sgt icmp-sge icmp-slt icmp-sle
    fcmp-oeq fcmp-ogt fcmp-oge fcmp-olt fcmp-ole fcmp-one fcmp-ord fcmp-uno fcmp-ueq
    fcmp-ugt fcmp-uge fcmp-ult fcmp-ule fcmp-une
    add add-nsw add-nuw fadd
    sub sub-nsw sub-nuw fsub
    mul mul-nsw mul-nuw fmul
    udiv sdiv exact-sdiv fdiv
    urem srem frem
    shl lshr ashr
    or xor and
    extract-element insert-element
    trunc zext sext fp->ui fp->si ui->fp si->fp
    fp-trunc fp-ext ptr->int int->ptr
    bitcast addr-space-cast zext-or-bit-cast sext-or-bit-cast
    ptr-cast int-cast fp-cast
    neg neg-nsw neg-nuw fneg
    not load malloc alloca free store! array-malloc array-alloca
    gep phi app))

(define-for-syntax iwidths `(1 8 16 32 64))
(define-for-syntax fwidths `(32 64))

(define-syntax int-widths iwidths)
(define-syntax float-widths fwidths)

(define-for-syntax ints (map (λ (w) (format "i~a" w)) iwidths))
(define-for-syntax floats (map (λ (w) (format "f~a" w)) fwidths))

(define-syntax numeric-types (append ints floats))
(define-syntax basic-types (append `(void) ints floats))
(define-syntax intrinsic-ops
  `(
    (memcpy ("p0" . (* i8)) ("p0" . (* i8)) ("" . ,ints) i1 void)
    (memcpy.inline ("p0" . (* i8)) ("p0" . (* i8)) ("" . ,ints) i1 void)
    (memmove ("p0" . (* i8)) ("p0" . (* i8)) ("" . ,ints) i1 void)
    (memset ("p0" . (* i8)) i8 ("" . ,ints) i1 void)
    (pow ("" . ,floats) 0 0)
    (powi ("" . ,floats) i32 0)
    ((sqrt sin cons exp exp2 log log10 log2) ("" . ,floats) 0)
    (fma ("" . ,floats) 0 0 0)
    (fabs ("" . ,floats) 0)
    ((minnum maxnum minimum maximum copysign) ("" . ,floats) 0 0)
    ((floor ceil trunc rint nearbyint round) ("" . ,floats) 0)
    ((lround llround lrint llrint) ((1 . "") . ,floats) ((0 . "") . ,ints))
    ((bitreverse bswap ctpop) ("" . ,ints) 0)
    ((ctlz cttz) ("" . ,ints) i1 0)
    ((fshl fshr) ("" . ,ints) 0 0 0)
    ((sadd.with.overflow uadd.with.overflow ssub.with.overflow usub.with.overflow smul.with.overflow umul.with.overflow)
     ("" . ,ints) 0 (:struct 0 i1))
    ((sadd.sat uadd.sat ssub.sat usub.sat)
     ("" . ,ints) 0 0)
    ((smul.fix umul.fix smul.fix.sat umul.fix.sat sdiv.fix udiv.fix sdiv.fix.sat udiv.fix.sat)
     ("" . ,ints) 0 i32 0)
    (canonicalize ("" . ,floats) 0)
    (fmuladd ("" . ,floats) 0 0 0)
    (convert.to.fp16 ("" . ,floats) i16)
    (convert.from.fp16 i16 ("" . ,floats))))

;; (define v4i32 (llvm-type-vector i32 4))
;; (define v4i64 (llvm-type-vector i64 4))
;; (define v4f32 (llvm-type-vector f32 4))
;; (define v4f64 (llvm-type-vector f64 4))

(module+ test
  (display (register-llvm-internal-instructions (empty-assoc-env))))
