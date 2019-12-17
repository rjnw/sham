#lang racket

;; (require "private/llvm/ffi/all.rkt")

;; (define (sham-mod->exe-path mod [path "a.out"])
;;   (define llvm-mod (compile-module mod))
;;   ;;(optimize-module llvm-mod #:opt-level 3)
;;   (define tmp-llvm-mod-file
;;     (make-temporary-file "sham-tmp-~a.ll"))
;;   (define tmp-asm-file
;;     (make-temporary-file "sham-tmp-~a.s"))
;;   (try
;;    (jit-write-module llvm-mod tmp-llvm-mod-file)
;;    (unless (system (format "llc-5.0 -o ~a ~a"
;;                            tmp-asm-file
;;                            tmp-llvm-mod-file))
;;      (error 'sham "couldn't compile llvm module"))
;;    (unless (system (format "clang-5.0 -o ~a ~a"
;;                            path
;;                            tmp-asm-file))
;;      (error 'sham "couldn't assemble"))
;;    path
;;    (finally (delete-file tmp-llvm-mod-file)
;;             (delete-file tmp-asm-file))))

;; (define (sham-module-execute mod . args)
;;   (define tmp-exe-file (make-temporary-file "sham-tmp-~a"))
;;   (try
;;    (define exe-path (sham-mod->exe-path mod tmp-exe-file))
;;    (apply system*/exit-code exe-path args)
;;    (finally (delete-file tmp-exe-file))))

;; (require rackunit)
;; (define (test-sham-module description mod
;;                           #:args [args '()]
;;                           #:returns [returns 0]
;;                           #:stdin-str [stdin ""]
;;                           #:stdout-rx [stdout #".*"]
;;                           #:stderr-rx [stderr #"^$"])
;;   (define outp (open-output-string))
;;   (define errp (open-output-string))
;;   (test-case description
;;     (parameterize ([current-output-port outp]
;;                    [current-error-port errp]
;;                    [current-input-port (open-input-string stdin)])
;;       (test-begin
;;         (check-equal? (apply sham-module-execute mod args) returns)
;;         (check-pred (curry regexp-match? stdout) (get-output-string outp))
;;         (check-pred (curry regexp-match? stderr) (get-output-string errp))))))


;; (module+ test
;;   (require rackunit)
;;   (define i32 (sham:type:ref 'i32))
;;   (define empty-mod (sham:module (empty-mod-env-info) '()))
;;   (test-exn "empty module" exn?
;;             ;; Discards error output of the compiler
;;             ;; FIXME This should be internalized and raised with the racket error
;;             (thunk (parameterize ([current-output-port (open-output-nowhere)]
;;                                   [current-error-port (open-output-nowhere)])
;;                      (sham-mod->exe-path empty-mod))))
;;   (define unit-mod
;;     (sham:module
;;      (empty-mod-env-info)
;;      (list
;;       (sham:def:function (make-hash) 'main '() '() i32
;;                          (sham:stmt:return (sham:expr:ui-value 0 i32))))))

;;   (test-not-exn "compile unit module" (thunk (sham-mod->exe-path unit-mod)))
;;   (test-equal? "execute unit module" (sham-module-execute unit-mod) 0)
;;   (test-sham-module "test unit module" unit-mod #:returns 1 #:stdout-rx #rx"foo")

;;   )
