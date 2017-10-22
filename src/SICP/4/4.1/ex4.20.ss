(load "./4.1_2.ss")
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

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-body p) (caddr p))

;4.20
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp)
     (eval-definition exp env))
;    ((unbind!? exp)
;     (eval-unbind exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body exp)
                     env))
    ((let? exp)
     (eval (let->combination exp) env))
    ((let*? exp)
     (eval (let*->nested-lets exp) env))
    ((letrec? exp)
     (eval (letrec->let exp) env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((and? exp) (eval-and exp env))
    ((or? exp) (eval-or exp env))
    ((while? exp) (eval (while->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))

(define (letrec? exp) (tagged-list? exp 'letrec))

;(define (letrec->let exp)
;  (define (make-let-internal binds body)
;    (cons 'let (cons binds body)))
;  (let ((bindings (cadr exp))
;        (body (cddr exp)))
;    (let ((vars (map car bindings))
;          (vals (map cadr bindings)))
;      (make-let-internal (map (lambda (x) (list x ''*unassigned*)) vars)
;                (append (map make-assignment vars vals)
;                        body)))))
;(letrec->let
;  (quote
;    (letrec ((even?
;               (lambda (n)
;                 (if (= n 0)
;                   true
;                   (odd? (- n 1)))))
;             (odd?
;               (lambda(n)
;                 (if (= n 0)
;                   false
;                   (even? (- n 1))))))
;      (even? x))))


(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
        (exps (map cdr (cadr exp)))
        (body (cddr exp)))
    (cons 'let
          (cons (map (lambda (x) (list x ''*unassigned*)) vars)
                (append (map (lambda (x y) (cons 'set! (cons x y))) vars exps)
                        body)))))

(letrec->let
  (quote
    (letrec ((even?
               (lambda (n)
                 (if (= n 0)
                   true
                   (odd? (- n 1)))))
             (odd?
               (lambda(n)
                 (if (= n 0)
                   false
                   (even? (- n 1))))))
      (even? x))))

(let->combination
  (letrec->let
    (quote
      (letrec ((even?
                 (lambda (n)
                   (if (= n 0)
                       true
                       (odd? (- n 1)))))
               (odd?
                 (lambda(n)
                   (if (= n 0)
                       false
                       (even? (- n 1))))))
        (even? x)))))

(let->combination
  (quote
    (let ((even?
               (lambda (n)
                 (if (= n 0)
                     true
                     (odd? (- n 1)))))
             (odd?
               (lambda(n)
                 (if (= n 0)
                     false
                     (even? (- n 1))))))
      (even? x))))


(driver-loop)

(define (f x)
  (letrec ((even?
             (lambda (n)
               (if (= n 0)
                 true
                 (odd? (- n 1)))))
           (odd?
             (lambda(n)
               (if (= n 0)
                 false
                 (even? (- n 1))))))
    (even? x)))
(f 3)
end

;b
; http://www.serendip.ws/archives/2010
