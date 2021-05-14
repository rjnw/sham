#lang racket

(require sham/sam/reader)

;; (build-reader $llvm llvm
;;   (def
;;     #:default-context 'top-level
;;     #:bind [id #:in rkt-scope #:with define #:to def]
;;     [module #:context '(module-begin module)])
;;   (type
;;    #:default-context 'expression)
;;   (instruction ))
