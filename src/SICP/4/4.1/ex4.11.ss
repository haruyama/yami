(load "./4.1_2.ss")

(define (make-frame variables values)
  (define (iter variables values)
    (if (null? variables)
      '()
      (cons (cons (car variables) (car values))
            (iter (cdr variables) (cdr values)))))
  (list (iter variables values)))

(define (frame-variables frame)
  (map car (car frame)))

(define (frame-values frame)
  (map cdr (car frame)))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))


(define f (make-frame (list 'a 'b 'd) (list 1 2 4)))
(add-binding-to-frame! 'c 3 f)
(display f)
(frame-variables f)
(frame-values f)

(define f (make-frame '() '()))
(add-binding-to-frame! 'c 4 f)
(display f)
(frame-variables f)
(frame-values f)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan l)
      (cond ((null? l)
             (env-loop (enclosing-environment env)))
        ((eq? var (caar l))
         (cdar l))
        (else (scan (cdr l)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (car frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan l)
      (cond ((null? l)
             (env-loop (enclosing-environment env)))
        ((eq? var (caar l))
         (set-cdr! (car l) val))
        (else (scan (cdr l)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (car frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan l)
      (cond ((null? l)
             (add-binding-to-frame! var val frame))
        ((eq? var (caar l))
         (set-cdr! (car l)  val))
        (else (scan (cdr l)))))
    (scan (car frame))))


(define (setup-environment2)
  (let ((initial-env
          (extend-environment '(a)
                              '(1)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment2 (setup-environment2))

(define-variable! 'b 1 the-global-environment2)
(display the-global-environment2)

(set-variable-value! 'b 2 the-global-environment2)
(display the-global-environment2)

;error
(set-variable-value! 'd 2 the-global-environment2)

(lookup-variable-value 'a the-global-environment2)
(lookup-variable-value 'b the-global-environment2)
(display the-global-environment2)
