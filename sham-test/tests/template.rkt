#lang racket

(require sham)


#;(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (<id> (<arg> : <arg-type>) ...) : <return-type>
    <body>)

  (parameterize ([compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     test-module
     #:opt-level 3))
  (check-eq? ... ...))
