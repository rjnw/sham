#lang racket

(require syntax/parse/define
         syntax/parse
         (for-syntax racket/syntax
                     racket/match))

(require sham/llvm/ir/ast)
(provide (all-defined-out))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (to-prefix:id from-prefix:id)
        [names:id ...])
     #:with (to-names ...) (map (λ (n) (format-id n "~a~a" #'to-prefix n)) (syntax->list #`(names ...)))
     #:with (from-names ...) (map (λ (n) (format-id n "~a~a" #'from-prefix n)) (syntax->list #`(names ...)))
     #`(begin (define to-names from-names) ...)]))

(define-alias (def- llvm:def:)
  [module function type global global-string external intrinsic])
(define-alias (ast- llvm:ast:)
  [block])
(define-alias (ast- llvm:ast:instruction:)
  [op])
(define-alias (ast- llvm:ast:instruction:terminator:)
  [retv ret br bru switch])
(define-alias (val- llvm:ast:value:)
  [fl si ui string llvm basic-struct named-struct array vector])
(define-alias (type- llvm:ast:type:)
  [internal ref struct function pointer array vector])

(define-syntax (define-ref-types stx)
  (syntax-parse stx
    [(_ names:id ...)
     (define name-list (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
                                        (build-list 4 (λ (j) (make-string j #\*)))))
                            (syntax->list #`(names ...))))
     (define (rec prev l)
       (match* (prev l)
         [(#f (cons curr next))
          (cons #`(define #,curr (type-ref (quote #,curr))) (rec curr next))]
         [(p (cons curr next))
          (cons #`(define #,curr (type-pointer #,p)) (rec curr next))]
         [(p empty) empty]))
     #`(begin #,@(apply append (map (λ (n) (rec #f n)) name-list)))]))

(define-ref-types i1 i8 i16 i32 i64 f32 f64 void)

(define-syntax (definer-for-ops stx)
  (syntax-parse stx
    [(_ op-names:id)
     (define names (map (λ (n) (datum->syntax stx n)) (syntax-local-value #'op-names)))
     #`(define-ops #,@names)]))
(define-syntax (define-ops stx)
  (syntax-parse stx
    [(_ op-name:id ...)
     #`(begin (define (op-name result args #:flags (flags #f)) (ast-op result (quote op-name) flags args)) ...)]))

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
