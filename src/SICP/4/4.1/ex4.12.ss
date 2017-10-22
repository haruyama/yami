(load "./4.1_2.ss")
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
         (car vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
         (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
        ((eq? var (car vars))
         (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(driver-loop)
(define i 0)
(while (< i 10)
       (display i)
       (newline)
       (set! i (+ i 1)))
end

(define (scan var val frame null-func eq-func)
  (define (iter vars vals)
    (cond ((null? vars)
           (null-func var val))
      ((eq? var (car vars))
       (eq-func vals val))
      (else (iter
              (cdr vars) (cdr vals)))))
  (iter  (frame-variables frame)
         (frame-values frame)))



(define (lookup-variable-value var env)
  (define (null-func var val)
    (env-loop (enclosing-environment env)))
  (define (eq-func vals val)
    (car vals))
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan
          var
          0
          frame
          null-func
          eq-func
          ))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (null-func var val)
    (env-loop (enclosing-environment env)))
  (define (eq-func vals val)
    (set-car! vals val))
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan
          var
          val
          frame
          null-func
          eq-func
          ))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (null-func var val)
      (add-binding-to-frame! var val frame))
    (define (eq-func vals val)
      (set-car! vals val))
    (scan var val
          frame
          null-func
          eq-func
          )))


(define (scan var vars vals)
  (cond ((null? vars) '())
    ((eq? var (car vars)) vals)
    (else
      (scan var (cdr vars) (cdr vals)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
          (if (null? result-of-scan)
            (env-loop (enclosing-environment env))
            (car result-of-scan))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
          (if (null? result-of-scan)
            (env-loop (enclosing-environment env))
            (set-car! result-of-scan val))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
      (if (null? result-of-scan)
        (add-binding-to-frame! var val frame)
        (set-car! result-of-scan val)))))


(driver-loop)
(define i 0)
(while (< i 10)
       (display i)
       (newline)
       (set! i (+ i 1)))
end

