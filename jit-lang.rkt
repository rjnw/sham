#lang racket

(define-compiler ll-jit
  (terminals
    (variable  (x))
    (float     (f))
    (number    (n))
    (basetype  (tb))

    (llvm-contextref (lcr))
    (llvm-moduleref  (lmr))
    (llvm-typeref    (ltr))
    (llvm-valueref   (lvr)))
  #;(environment
     (type
      [,x -> ,t])
     (scope
      [,x -> ,t]))
  (expressions
   (type (t)
         [name         : ,x
                       #;(@ x type)]
         [base         : ,tb]
         [struct       : (struct (,x : ,t) ...)]
         [pointer      : (pointer ,t)]
         [function     : (,x ... -> ,x)])
   (expr (e)
         [var          : ,x
                       #;(@ x scope)]
         [float        : (fl-value ,d ,t)]
         [signed-int   : (si-value ,n ,t)]
         [unsigned-int : (ui-value ,n ,t)]
         [type         : (type ,t)
                       #;(@ t type)]
         [sizeof       : (sizeof ,expr-type)]
         [gep          : (gep ,e (,e ...))]
         [app          : (,e ,e ...)])
   (phi  (p)
         [phis         : ([,x : ,t] ...)
                       #;(@ t type)])
   (stmt (s)
         [expr         : (expr ,e)]
         [if-phi       : (if ,phis ,e ,s ,s)]
         [if           : (if ,e ,s ,s)]
         [let          : (let ([,x : ,t ,e] ...) ,s)
           #;(s:scope (+ (x -> t)))]
         [while-phi    : (while ,phis ,e ,s)]
         [while        : (while ,e ,s)]
         [return       : (return ,e)]
         [return-void  : (return-void)]
         [set          : (set! ,x ,e)
                       #;(@ x scope)]
         [store        : (store! ,x ,e)
                       #;(@ x scope)])
   #;(se  (se) #:reuse (stmt-while stmt-store expr-*))
   (def (d)
         [type         : (define-type ,x ,t)
                       #;(type! (x t))]
         [function     : (define-function ,x-f ((,x-a : ,t-a) ...) : ,t-r ,s)
                       #;(s:scope (+ (x-a -> t-a)
                                     (x-f -> (type-function (t-a ... -> t-r)))))
                       #;(scope! (x-f -> (type-function (t-a ... -> t-r))))])
   (mod (m)
        [module        : (module ,d ...)
                       #;(pass-environment * d)]))

  (languages
   (LLC1 mod
         (- (stmt if-phi while-phi)))
   (LLC0 #:extends LLC1
         mod
         (- (stmt (if while))))))
#|
  Compiler architecture
   for the whole compiler we define terminals, expressions, languages

 terminals are the terminal part of languages specify racket stuff with a predicate with same name and ?
 expressions are the different types of ast's which can be the part of a language,
   they are subdivided into groups with each group having its variables which can be used specify
   in the ast definitions what the sub parts of ast can be.
   For each ast we also specify a name, and a group of variables which
   can be used in expression definitions for sub specification or in language definitions for
   adding and removing them from expression groups.

 languages are the parts of compiler which can contain the ast structures specified in expressions.
  we can remove and add some ast definitions from group or remove and add whole groups.
  we can also extend the languages from each other and then add or remove from the extended language.

 passes: ability to easily define pass from one language of a compiler to another with the
  automatic generation of transforms over ast structures from a subset of transforms.


 * figure out if we can do something for the binding structure.
 ability to use the racket contract and type systems, automatic generation of types and contracts
  which an option to choose.



 future 
  add environments for reusing ast forms and storing scope and other information
  connect compilers with the input and output of passes of different compiler languages.
|#

;; (parse-language C-LL0 '(...))
;; (parse-expression C-mod '(module (define-type ...) ...))

;; (define (variable? x)
;;   ...)
;; (define (float? f)
;;   ...)
;; (define (number? n)
;;   ...)
;; (define (basetype? tb)
;;   ...)
