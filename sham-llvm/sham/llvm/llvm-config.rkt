#lang racket

(provide llvm-config)

(define llvm-config-name
  (let ([cmd (or (getenv "LLVM_CONFIG") "llvm-config")])
    (unless (find-executable-path cmd)
      (error 'sham/llvm-config-name
             (string-join
              '("Couldn't find llvm-config."
                "Consider seting LLVM_CONFIG enviroment variable.")
              "\n")))
    cmd))


(define (get-out-string process-str)
  (let* ([out (open-output-string)]
         [l (process/ports out #f #f process-str)])
    ((last l) 'wait)
    (string-trim (get-output-string out))))

(define (llvm-config . a)
  (get-out-string (string-join (cons llvm-config-name a) " ")))
