(load "../4.1/4.1_3.ss")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((unless? exp) (analyze (unless->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp)
         (analyze (let->combination exp)))
        ((begin? exp)
         (display (analyze-sequence (begin-actions exp)))
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (cons (make-lambda
          (map car (cadr exp)) (cddr exp))
        (map cadr (cadr exp))))


(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))

(unless->if
  '(unless (= n 1)
           (* n (factorial (- n 1)))
           1)
  )

(driver-loop)
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)
end
