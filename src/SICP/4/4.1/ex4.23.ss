(load "./4.1_3.ss")

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


;ex4.23
;http://d.hatena.ne.jp/tmurata/20100423/1272021704
;http://www.serendip.ws/archives/2122

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
        ((begin? exp) 
         (analyze-sequence (begin-actions exp))
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


(driver-loop)
(begin
  (+ 1 1))
(begin
  (+ 1 1)
  (+ 2 2))
end


(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))


(driver-loop)
(begin
  (+ 1 1))
(begin
  (+ 1 1)
  (+ 2 2))
end

