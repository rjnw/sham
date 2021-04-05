#lang racket

(provide (all-defined-out))

(define (from-maybe v (default #f))
  (cond
    [v]
    [(procedure? default) (default)]
    [else default]))
