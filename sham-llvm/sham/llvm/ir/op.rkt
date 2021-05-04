#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define)

(require sham/llvm/ir/simple)

(provide (except-out (all-defined-out)
                     definer-for-ops
                     define-ops))

(define-syntax (definer-for-ops stx)
  (syntax-parse stx
    [(_ op-names:id)
     (define names (map (λ (n) (datum->syntax stx n)) (syntax-local-value #'op-names)))
     #`(define-ops #,@names)]))
(define-syntax (define-ops stx)
  (syntax-parse stx
    [(_ op-name:id ...)
     #:with (orig-op ...) (syntax->list #`(op-name ...))
     #'(begin (define-syntax (op-name stx)
                (syntax-parse stx
                  [(_ (~optional (~seq #:flags flags) #:defaults ([flags #'#f])) result args (... ...))
                   #`(inst-op result (quote op-name) flags args (... ...))]))
              ...)]))

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

(definer-for-ops basic-ops)

(define-for-syntax ints `(i8 i32 i64))
(define-for-syntax floats `(f32 f64))
(define-for-syntax ptrs (append `(void) ints floats))
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
