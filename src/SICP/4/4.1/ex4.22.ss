(load "./4.1_3.ss")
(driver-loop)
(define x 1)
(if (eq? x 1)
    (display 1)
    (display 2))
(set! x 2)
(if (eq? x 1)
    (display 3)
    (display 5))
end

;ex4.22

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
        (expand-let-clauses (let-clauses exp)))

(define (expand-let-clauses clauses)
        (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
              (map cadr (let-bindings clauses))))


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp)
         (analyze (let->combination exp)))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(driver-loop)
(let ((x 1))
  (display x))
end
