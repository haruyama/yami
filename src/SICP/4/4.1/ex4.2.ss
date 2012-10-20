(load "./4.1.ss")
(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
              ((application? exp) ;前に出した
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              ((assignment? exp) (eval-assignment exp env))
              ((definition? exp) (eval-definition exp env))
              ((if? exp) (eval-if exp env))
              ((lambda? exp)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env))
              ((begin? exp)
               (eval-sequence (begin-actions exp) env))
              ((cond? exp) (eval (cond->if exp) env))
              (else
                (error "Unknown expression type -- EVAL" exp))))

(define the-global-environment (setup-environment))
(eval '(define x 3) the-global-environment)

;b
(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))

(eval '(call + 2 3) the-global-environment)
(eval '(+ 2 3) the-global-environment)
