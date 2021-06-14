#lang racket

(require "private/utils.rkt")
(provide (all-defined-out))
(define (default-metadata . specs)
  (ormap (curry info-1value 'default-metadata) specs))
