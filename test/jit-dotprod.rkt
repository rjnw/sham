
       ;; (define-function (dot-product (arr1 : uint*) (arr2 : uint*)
       ;;                               (size : uint) : uint)
       ;;   (let ((sum : uint)
       ;;         (i : uint))
       ;;     (block
       ;;      (set! i (#%value 0 uint))
       ;;      (set! sum (#%value 0 uint))
       ;;      (while (#%app jit-lt? i size)
       ;;        (let ((ptr-pos : int))
       ;;          (block
       ;;           (set! ptr-pos (#%app jit-mul i (#%sizeof uint)))
       ;;           (set! sum (#%app jit-add
       ;;                            sum
       ;;                            (#%app jit-mul
       ;;                                   (* arr1 ptr-pos : uint)
       ;;                                   (* arr2 ptr-pos : uint))))
       ;;           (set! i (#%app jit-add i (#%value 1 uint))))))
       ;;      (return sum))))
       ;; (define-type float32-p (pointer float32))
       ;; (define-type array-real (struct (size : int) (data : float32-p)))
       ;; (define-type array-real-p (pointer array-real))
       ;; (define-function (get-size (arr : array-real-p) : int)
       ;;   (return (* arr (#%offset array-real size) : int)))
       


  ;; (define dot-product (jit-get-function (env-lookup 'dot-product module-env)))
  ;; (define bigarray (list->cblock biglist _ulong))
  ;; (time (begin
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))))
  ;; (time
  ;;  (begin(for/sum [(a1 (in-vector bigvec))
  ;;             (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))))
  ;; (define test (jit-get-function (env-lookup 'test module-env)))
  ;; (pretty-print (test))

