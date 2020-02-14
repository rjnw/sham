#lang racket

(require sham)


(module+ test
  (require rackunit)

  (define fastcc-module
    (create-empty-sham-module "fastcc-module"))

  (current-sham-module fastcc-module)


  (define-sham-function
    #:info (function-info-set-fastcc (empty-function-info))
    (a1 (i : i64) (v : i64) (res : i64) : i64)
    (if^ (icmp-eq i (ui64 0))
         (ret res)
         (ret (fastcc! (b1 (sub i (ui64 1))
                          v
                          (add-nuw res v))))))
  (define-sham-function
    #:info (function-info-set-fastcc (empty-function-info))
    (b1 (i : i64) (v : i64) (res : i64) : i64)
    (if^ (icmp-eq i (ui64 0))
         (ret res)
         (ret (fastcc! (a1 i (sub v (ui64 1)) res)))))
  (define-sham-function
    (wrap1 (inp : i64) : i64)
    (slet^ ([r (fastcc! (b1 inp inp (ui64 1))) : i64])
           (ret r)))

  (parameterize ([build-options '(pretty verify dump)]
                 [compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     fastcc-module
     #:opt-level 3))


  (define normal-module
    (create-empty-sham-module "normal-module"))

  (current-sham-module normal-module)


  (define-sham-function
    #:info (empty-function-info)
    (a2 (i : i64) (v : i64) (res : i64) : i64)
    (if^ (icmp-eq i (ui64 0))
         (ret res)
         (ret  (b2 (sub i (ui64 1))
                   v
                   (add-nuw res v)))))
  (define-sham-function
    #:info  (empty-function-info)
    (b2 (i : i64) (v : i64) (res : i64) : i64)
    (if^ (icmp-eq i (ui64 0))
         (ret res)
         (ret  (a2 i (sub v (ui64 1)) res))))
  (define-sham-function
    (wrap2 (inp : i64) : i64)
    (slet^ ([r  (b2 inp inp (ui64 1)) : i64])
           (ret r)))

  (parameterize ([build-options '(pretty verify dump)]
                 [compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     normal-module
     #:opt-level 3))

  (time (sham-app wrap1 10000000000))
  (time (sham-app wrap2 10000000000))
  )

;; fastcc: cpu time: 2392 real time: 2393 gc time: 0
;; nofastcc:
