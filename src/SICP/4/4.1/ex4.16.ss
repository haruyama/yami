(load "./4.1_2.ss")

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
        ((eq? var (car vars))
         (let ((val (car vals)))
           (if (eq? val '*unassigned*)
             (error "Unassigned variable" var)
             val)))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (make-assignment var val)
  (list 'set! var val))


(use srfi-1)
(define (scan-out-defines body)
  ; global な make-let とは違う
  (define (make-let-internal binds body)
    (cons 'let (cons binds body)))
  (let ((defs (take-while definition? body))
        (rest (drop-while definition? body)))
    (if (not (null? (filter definition? rest)))
      (error "syntax error: no toplevel definition"))
    (if (null? defs)
      body
      (let ((vars (map definition-variable defs))
            (vals (map definition-value defs)))
        (list (make-let-internal (map (lambda (x) (list x ''*unassigned*)) vars)
                         (append (map make-assignment vars vals)
                                 rest)))))))

(scan-out-defines
  '((define u (display v)) (define v (+ 1 2)) (display "aaa")))

(scan-out-defines
  '((define u (display v)) (define v (+ 1 2)) (display "aaa") (define o 'a)))

(scan-out-defines
  '((define (even? n)
      (if (= n 0)
        true
        (odd? (- n 1))))
    (define (odd? n)
      (if (= n 0)
        false
        (even? (- n 1))))
    (even? x)
    ))

;c
;(define (make-procedure parameters body env)
;  (list 'procedure parameters body env))
;(define (procedure-body p) (caddr p))

;1
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-body p) (caddr p))

(driver-loop)
(let ((a 3) (b 2))
  (+ a b))
(let ((x 1))
  x)


(define (f x)
  (define (even? n)
    (if (= n 0)
      true
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      false
      (even? (- n 1))))
  (even? x))

(f 3)
end

;2
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-body p)
  (scan-out-defines (caddr p)))


(driver-loop)
(let ((a 3) (b 2))
  (+ a b))
(let ((x 1))
  x)


(define (f x)
  (define (even? n)
    (if (= n 0)
      true
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      false
      (even? (- n 1))))
  (even? x))

(f 3)
end
