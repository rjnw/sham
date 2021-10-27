#lang racket

(require sham/md
         sham/ir
         (prefix-in ll- sham/llvm/ir/simple))

(provide (all-defined-out))

(define pow-f
  (function (pow [x : i64] [n : i64] : i64)
            (stmt-if (op-icmp-ule n (ui64 0))
                     (stmt-return (ui64 1))
                     (stmt-return (op-mul x (expr-app 'pow x (op-sub-nuw n (ui64 1))))))))

(define (gen-pow-f n)
  (make-def-function
     (format "pow-~a" n)
     (ll-type-function i64 #f i64)
     (stmt-return (for/fold ([result (ui64 1)])
                            ([i n])
                    (op-mul (expr-ref 0) result)))))