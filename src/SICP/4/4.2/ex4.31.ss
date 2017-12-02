(load "./4.2.ss")

(define (procedure-parameters p) (map (lambda (x) (if (pair? x) (car x) x)) (cadr p)))
(define (procedure-parameters-with-type p) (cadr p))

(procedure-parameters '(a (b c d)))
(procedure-parameters '(a (b (c lazy) (d lazy-memo))))

(define (first-parameter params) (car params))
(define (rest-parameters params) (cdr params))

(define (parameter-type param)
  (display param)
  (if (pair? param)
    (let ((type (cadr param)))
      (cond
        ((eq? type 'lazy) 'lazy)
        ((eq? type 'lazy-memo) 'lazy-memo)
        (else
          (error "unknown type: " type))))
    'actual))

(parameter-type 'a)
(parameter-type '(b lazy))
(parameter-type '(c lazy-memo))
;(parameter-type '(d lazy-memo000))

(define (delay-it-unmemo exp env)
  (list 'thunk-unmemo exp env))

(define (thunk-unmemo? obj)
  (tagged-list? obj 'thunk-unmemo))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        ((thunk-unmemo? obj)
         (actual-value (thunk-value obj) (thunk-env obj)))
        (else obj)))

(define (handle-arg type arg env)
  (cond ((eq? type 'actual) (actual-value arg env))
    ((eq? type 'lazy) (delay-it-unmemo arg env))
    ((eq? type 'lazy-memo) (delay-it arg env))
    (else (error "Unknown type: " type))))

(define (list-of-delayed-args params exps env)
  (if (no-operands? exps)
    '()
    (cons
      (handle-arg (parameter-type (first-parameter params)) (first-operand exps) env)
      (list-of-delayed-args (rest-parameters params) (rest-operands exps) env))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args (procedure-parameters-with-type procedure) arguments env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(driver-loop)
(define x 1)
(define (p e) e x)
(p (set! x (cons x '(2))))
x
end

(driver-loop)
(define x 1)
(define (p (e lazy-memo)) e x)
(p (set! x (cons x '(2))))
x
end
