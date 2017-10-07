(load "./4.1.ss")
;ex4.1
(define (list-of-values exps env)
        (if (no-operands? exps)
            '()
            (cons (eval (first-operand exps) env)
                  (list-of-values (rest-operands exps) env))))

(define the-global-environment (setup-environment))
(eval '(define x 1) the-global-environment)

(list-of-values

  '((begin (set! x (+ x 3)) (newline) (display x))
    (begin (set! x (* x 2)) (newline) (display x)))

  the-global-environment)

(define (list-of-values-l2r exps env)
        (if (no-operands? exps)
            '()
            (let ((left-eval (eval (first-operand exps) env)))
                 (cons left-eval
                       (list-of-values-l2r (rest-operands exps) env)))))

(define the-global-environment (setup-environment))
(eval '(define x 1) the-global-environment)

(list-of-values-l2r

  '((begin (set! x (+ x 3)) (newline) (display x))
    (begin (set! x (* x 2)) (newline) (display x)))

  the-global-environment)


(define (list-of-values-r2l exps env)
        (if (no-operands? exps)
            '()
            (let ((right-eval (list-of-values-r2l (rest-operands exps) env)))
                 (cons (eval (first-operand exps) env)
                       right-eval))))

(define the-global-environment (setup-environment))
(eval '(define x 1) the-global-environment)

(list-of-values-r2l
  '((begin (set! x (+ x 3)) (newline) (display x))
    (begin (set! x (* x 2)) (newline) (display x)))
  the-global-environment)


;ex4.4
(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
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
              ((and? exp) (eval-and exp env))
              ((or? exp) (eval-or exp env))
              ((application? exp)
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              (else
                (error "Unknown expression type -- EVAL" exp))))

(define (and? exp)
        (tagged-list? exp 'and))

(define (eval-and exp env)
        (eval-and-conds (cdr exp) env))

(define (eval-and-conds conds env)
        (if (null? conds)
            true
            (let ((value (eval (car conds) env)))
                 (if (true?  value)
                     (if (null? (cdr conds))
                         value
                         (eval-and-conds (cdr conds) env))
                     false))))

(define the-global-environment (setup-environment))
(eval '(and true true) the-global-environment)
(eval '(and false true) the-global-environment)
(eval '(and true false) the-global-environment)
(eval '(and true true false) the-global-environment)
(eval '(and true false true) the-global-environment)
(eval '(and true) the-global-environment)
(eval '(and 1) the-global-environment)
(eval '(and true 1) the-global-environment)
(eval '(and false) the-global-environment)
(eval '(and) the-global-environment)

(define (or? exp)
        (tagged-list? exp 'or))

(define (eval-or exp env)
        (eval-or-conds (cdr exp) env))

(define (eval-or-conds conds env)
        (if (null? conds)
            false
            (let ((value (eval (car conds) env)))
                 (if (true?  value)
                     value
                     (eval-or-conds (cdr conds) env)))))

(define the-global-environment (setup-environment))
(eval '(or true true) the-global-environment)
(eval '(or 1 true) the-global-environment)
(eval '(or false true) the-global-environment)
(eval '(or true false) the-global-environment)
(eval '(or true true false) the-global-environment)
(eval '(or true false true) the-global-environment)
(eval '(or false) the-global-environment)
(eval '(or false false) the-global-environment)
(eval '(or) the-global-environment)


;ex4.5

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

(define x 1)
(cond ((set! x (+ x 1)) => display)
      (else false))

;これだと 副作用のある式が<test>にあると2回評価してしまう
;(define (expand-clauses clauses)
;        (if (null? clauses)
;            'false
;            (let ((first (car clauses))
;                  (rest (cdr clauses)))
;                 (cond ((cond-else-clauses? first)
;                        (if (null? rest)
;                            (sequence->exp (cond-actions first))
;                            (error "ELSE clause isn't last -- COND->IF"
;                                   clauses)))
;                       ((eq? (cadr first) '=>)
;                        (make-if (cond-predicate first)
;                                 (list (caddr first) (cond-predicate first))
;                                 (expand-clauses rest)))
;                       (else
;                         (make-if (cond-predicate first)
;                                  (sequence->exp (cond-actions first))
;                                  (expand-clauses rest)))))))

;(driver-loop)
;(define x 1)
;(cond ((set! x (+ x 1)) => display)
;      (else false))
;(display x)
;end

(define (cond-recipient-clause? clause)
        (eq? (cadr clause) '=>))
(define (cond-recipient clause)
        (caddr clause))

(define (expand-clauses clauses)
        (if (null? clauses)
            'false
            (let ((first (car clauses))
                  (rest (cdr clauses)))
                 (cond ((cond-else-clauses? first)
                        (if (null? rest)
                            (sequence->exp (cond-actions first))
                            (error "ELSE clause isn't last -- COND->IF"
                                   clauses)))
                       ((cond-recipient-clause? first)
                        (list
                          (make-lambda
                            '(<test>)
                            (list
                              (make-if '<test>
                                 (list (cond-recipient first) '<test>)
                                 (expand-clauses rest))))
                          (cond-predicate first)))
                       (else
                         (make-if (cond-predicate first)
                                  (sequence->exp (cond-actions first))
                                  (expand-clauses rest)))))))
(driver-loop)
(define x 1)
(cond ((= x 1) 1)
      (else 0))
(cond ((= x 2) 2)
      (else 0))
(cond ((set! x (+ x 1)) => display)
      (else false))
(display x)
(cond (true (display 10)))

(+ 1 2)
(cond ((assoc 'b '((a 1) (b 2))) true)
      (else false))

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

(cond ((assoc 'b '((a 1) (b 2))) => (lambda (x) (+ 3 (cadr x))))
      (else false))

(cond ((assoc 'c '((a 1) (b 2))) => (lambda (x) (+ 3 (cadr x))))
      (else false))
end

;ex4.6

(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
              ((assignment? exp) (eval-assignment exp env))
              ((definition? exp) (eval-definition exp env))
              ((if? exp) (eval-if exp env))
              ((lambda? exp)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env))
              ((let? exp)
               (eval (let->combination exp) env))
              ((begin? exp)
               (eval-sequence (begin-actions exp) env))
              ((cond? exp) (eval (cond->if exp) env))
              ((and? exp) (eval-and exp env))
              ((or? exp) (eval-or exp env))
              ((application? exp)
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              (else
                (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
        (expand-let-clauses (let-clauses exp)))

(define (expand-let-clauses clauses)
        (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
              (map cadr (let-bindings clauses))))

(let ((a 3) (b 2))
     (+ a b))
(let ()
     (+ 1 2))

(let->combination
  '(let ((a 3) (b 2))
        (+ a b))
  )

(let->combination
  '(let ()
        (+ 1 2))
  )


(driver-loop)
(let ((a 3) (b 2))
     (+ a b))
(let ()
     (+ 1 2))
end

;ex4.7
(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
              ((assignment? exp) (eval-assignment exp env))
              ((definition? exp) (eval-definition exp env))
              ((if? exp) (eval-if exp env))
              ((lambda? exp)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env))
              ((let? exp)
               (eval (let->combination exp) env))
              ((let*? exp)
               (eval (let*->nested-lets exp) env))
              ((begin? exp)
               (eval-sequence (begin-actions exp) env))
              ((cond? exp) (eval (cond->if exp) env))
              ((and? exp) (eval-and exp env))
              ((or? exp) (eval-or exp env))
              ((application? exp)
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              (else
                (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-clauses exp) (cdr exp))

(define (let*-bindings clauses) (car clauses))

(define (let*-body clauses) (cadr clauses))

(define (make-let clauses body) (list 'let clauses body))

(define (let*->nested-lets exp)
        (let ((clauses (let*-clauses exp)))
             (let ((bindings (let*-bindings clauses))
                   (body (let*-body clauses)))
                  (define (iter bindings)
                          (if (null? bindings)
                              body
                              (make-let (list (car bindings))
                                        (iter (cdr bindings)))))
                  (iter bindings))))

(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
      (* x z))

(driver-loop)
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))
end

;ex4.8
(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
              ((assignment? exp) (eval-assignment exp env))
              ((definition? exp)
               ;(display exp)
               (eval-definition exp env))
              ((if? exp) (eval-if exp env))
              ((lambda? exp)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env))
              ((let? exp)
               ;(display exp)
               ;(newline)
               (eval (let->combination exp) env))
              ((let*? exp)
               (eval (let*->nested-lets exp) env))
              ((begin? exp)
               (eval-sequence (begin-actions exp) env))
              ((cond? exp) (eval (cond->if exp) env))
              ((and? exp) (eval-and exp env))
              ((or? exp) (eval-or exp env))
              ((application? exp)
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
              (else
                (error "Unknown expression type -- EVAL" exp))))

(define (let->combination exp)
        (if (pair? (car (let-clauses exp)))
            (expand-let-clauses (let-clauses exp))
            (expand-named-let-clauses (let-clauses exp))))

(define (named-let-var clauses) (car clauses))

(define (named-let-bindings clauses) (cadr clauses))

(define (named-let-body clauses) (caddr clauses))

(define (expand-named-let-clauses clauses)
        (make-begin
          (list
            (list 'define (cons (named-let-var clauses)
                                (map car (named-let-bindings clauses)))
                  (named-let-body clauses))
            (cons (named-let-var clauses)
                  (map cadr (named-let-bindings clauses))))))

(define (fib n)
        (let fib-iter ((a 1)
                       (b 0)
                       (count n))
             (if (= count 0)
                 b
                 (fib-iter (+ a b) a (- count 1)))))
(fib 7)
(fib 8)
(fib 9)

(let->combination
  '(let fib-iter ((a 1)
                  (b 0)
                  (count n))
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))))
  )

(driver-loop)
(let ((x 3) (y 2))
     (* x y))
(define (fib n)
        (let fib-iter ((a 1)
                       (b 0)
                       (count n))
             (if (= count 0)
                 b
                 (fib-iter (+ a b) a (- count 1)))))
(fib 7)
(fib 8)
(fib 9)
end

;ex4.9
(define i 0)
(while (< i 10)
       (display i)
       (newline)
       (set! i (+ i 1)))

(define (eval exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((quoted? exp) (text-of-quotation exp))
              ((assignment? exp) (eval-assignment exp env))
              ((definition? exp)
               ;(display exp)
               (eval-definition exp env))
              ((if? exp) (eval-if exp env))
              ((lambda? exp)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env))
              ((let? exp)
               (eval (let->combination exp) env))
              ((let*? exp)
               (eval (let*->nested-lets exp) env))
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


(define (while? exp) (tagged-list? exp 'while))

(define (sequence->exp seq)
        (cond ((null? seq) seq)
              ((last-exp? seq) (first-exp seq))
              (else (make-begin seq))))


(define (while->if exp)
        (make-if (cadr exp)
                 (sequence->exp (append (cddr exp) (list exp)))
                 'ok))

(while->if
  '(while (< i 10)
          (display i)
          (newline)
          (set! i (+ i 1))))

(driver-loop)
(define i 0)
(while (< i 10)
       (display i)
       (newline)
       (set! i (+ i 1)))
end
