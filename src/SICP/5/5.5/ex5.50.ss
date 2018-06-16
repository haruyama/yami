(load "./5.5.ss")
(load "./5.5.7.ss")

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp)
     (compile-variable exp target linkage))
    ((assignment? exp)
     (compile-assignment exp target linkage))
    ((definition? exp)
     (compile-definition exp target linkage))
    ((let? exp) (compile (let->combination exp) target linkage)) ; 5.50
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp)
     (compile-sequence (begin-actions exp)
                       target
                       linkage))
    ((cond? exp) (compile (cond->if exp) target linkage))
    ((application? exp)
     (compile-application exp target linkage))
    (else
      (error "Unknown expression type -- COMPILE" exp))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'apply-in-underlying-scheme apply-primitive-procedure)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list 'display display)
        (list 'newline newline)
        (list 'assoc assoc)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'list list)
        (list 'caddr caddr)
        (list 'cadr cadr)
        (list 'length length)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'read read)
        (list 'number? number?)
        (list 'pair? pair?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list 'cddr cddr)
        (list 'cdadr cdadr)
        (list 'caadr caadr)
        (list 'cadddr cadddr)
        (list 'not not)
        (list 'cdddr cdddr)
        ;...
        ))

(define the-global-environment (setup-environment))

(compile-and-go
  '(begin
     (define (map proc list)
       (if (null? list)
         '()
         (cons (proc (car list))
               (map proc (cdr list)))))
     (define (apply procedure arguments)
       (cond ((primitive-procedure? procedure)
              (apply-primitive-procedure procedure arguments))
         ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
         (else
           (error
             "Unknown procedure type -- APPLY" procedure))))

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
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         (else
           (error "Unknown expression type -- EVAL" exp)))
       )


     (define (list-of-values exps env)
       (if (no-operands? exps)
         '()
         (cons (eval (first-operand exps) env)
               (list-of-values (rest-operands exps) env))))

     (define (eval-if exp env)
       (if (true? (eval (if-predicate exp) env))
         (eval (if-consequent exp) env)
         (eval (if-alternative exp) env)))


     (define (eval-sequence exps env)
       (cond ((last-exp? exps) (eval (first-exp exps) env))
         (else (eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))

     (define (eval-assignment exp env)
       (set-variable-value! (assignment-variable exp)
                            (eval (assignment-value exp) env)
                            env)
       'ok)

     (define (eval-definition exp env)
       (define-variable! (definition-variable exp)
                         (eval (definition-value exp) env)
                         env)
       'ok)

     ;;4.1.2
     (define (self-evaluating? exp)
       (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))

     (define (variable? exp) (symbol? exp))

     (define (quoted? exp)
       (tagged-list? exp 'quote))

     (define (text-of-quotation exp) (cadr exp))

     (define (tagged-list? exp tag)
       (if (pair? exp)
         (eq? (car exp) tag)
         false))

     (define (assignment? exp)
       (tagged-list? exp 'set!))

     (define (assignment-variable exp) (cadr exp))
     (define (assignment-value exp) (caddr exp))

     (define (definition? exp)
       (tagged-list? exp 'define))

     (define (definition-variable exp)
       (if (symbol? (cadr exp))
         (cadr exp)
         (caadr exp)))

     (define (definition-value exp)
       (if (symbol? (cadr exp))
         (caddr exp)
         (make-lambda (cdadr exp)
                      (cddr exp))))

     (define (lambda? exp) (tagged-list? exp 'lambda))

     (define (lambda-parameters exp) (cadr exp))
     (define (lambda-body exp) (cddr exp))

     (define (make-lambda parameters body)
       (cons 'lambda (cons parameters body)))

     (define (if? exp) (tagged-list? exp 'if))

     (define (if-predicate exp) (cadr exp))

     (define (if-consequent exp) (caddr exp))

     (define (if-alternative exp)
       (if (not (null? (cdddr exp)))
         (cadddr exp)
         'false))

     (define (make-if predicate consequent alternative)
       (list 'if predicate consequent alternative))

     (define (begin? exp) (tagged-list? exp 'begin))

     (define (begin-actions exp) (cdr exp))
     (define (last-exp? seq) (null? (cdr seq)))
     (define (first-exp seq) (car seq))
     (define (rest-exps seq) (cdr seq))

     (define (sequence->exp seq)
       (cond ((null? seq) seq)
         ((last-exp? seq) (first-exp seq))
         (else (make-begin seq))))

     (define (make-begin seq)
       (cons 'begin seq))

     (define (application? exp) (pair? exp))

     (define (operator exp) (car exp))

     (define (operands exp) (cdr exp))

     (define (no-operands? ops) (null? ops))

     (define (first-operand ops) (car ops))

     (define (rest-operands ops) (cdr ops))

     (define (cond? exp) (tagged-list? exp 'cond))

     (define (cond-clauses exp) (cdr exp))

     (define (cond-else-clauses? clause)
       (eq? (cond-predicate clause) 'else))

     (define (cond-predicate clause) (car clause))

     (define (cond-actions clause) (cdr clause))

     (define (cond->if exp)
       (expand-clauses (cond-clauses exp)))

     (define (expand-clauses clauses)
       (if (null? clauses)
         'false
         (let ((first (car clauses))
               (rest (cdr clauses)))
           (if (cond-else-clauses? first)
             (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last -- COND->IF"
                      clauses))
             (make-if (cond-predicate first)
                      (sequence->exp (cond-actions first))
                      (expand-clauses rest))))))

     (define (true? x)
       (not (eq? x false)))

     (define (false? x)
       (eq? x false))

     (define (make-procedure parameters body env)
       (list 'procedure parameters body env))

     (define (compound-procedure? p)
       (tagged-list? p 'procedure))

     (define (procedure-parameters p) (cadr p))
     (define (procedure-body p) (caddr p))
     (define (procedure-environment p) (cadddr p))

     (define (enclosing-environment env) (cdr env))
     (define (first-frame env) (car env))
     (define the-empty-environment '())

     (define (make-frame variables values)
       (cons variables values))

     (define (frame-variables frame) (car frame))
     (define (frame-values frame) (cdr frame))

     (define (add-binding-to-frame! var val frame)
       (set-car! frame (cons var (car frame)))
       (set-cdr! frame (cons val (cdr frame))))

     (define (extend-environment vars vals base-env)
       (if (= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)
         (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

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

     (define (primitive-procedure? proc)
       (tagged-list? proc 'primitive))

     (define (primitive-implementation proc) (cadr proc))

     (define primitive-procedures
       (list (list 'car car)
             (list 'cdr cdr)
             (list 'cadr cadr)
             (list 'caddr caddr)
             (list 'cons cons)
             (list 'null? null?)
             (list '+ +)
             (list '- -)
             (list '* *)
             (list '/ /)
             (list '= =)
             (list '< <)
             (list '> >)
             (list 'display display)
             (list 'newline newline)
             (list 'assoc assoc)
             (list 'eq? eq?)
             (list 'equal? equal?)
             (list 'list list)
             ;...
             ))

     (define (primitive-procedure-names)
       (map car
            primitive-procedures))

     (define (primitive-procedure-objects)
       (map (lambda (proc) (list 'primitive (cadr proc)))
            primitive-procedures))

     (define (apply-primitive-procedure proc args)
       (apply-in-underlying-scheme
         (primitive-implementation proc) args))

     (define (setup-environment)
       (let ((initial-env
               (extend-environment (primitive-procedure-names)
                                   (primitive-procedure-objects)
                                   the-empty-environment)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
         initial-env))

     (define the-global-environment (setup-environment))

     (define input-prompt ";;; M-Eval input:")
     (define output-prompt ";;; M-Eval output:")

     (define (driver-loop)
       (prompt-for-input input-prompt)
       (let ((input (read)))
         (let ((output (eval input the-global-environment)))
           (announce-output output-prompt)
           (user-print output)))
       (driver-loop))

     (define (prompt-for-input string)
       (newline) (newline) (display string) (newline))

     (define (announce-output string)
       (newline) (display string) (newline))

     (define (user-print object)
       (if (compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))
         (display object)))

     (define the-global-environment (setup-environment))

     (define (eval exp env)
       (cond ((self-evaluating? exp) exp)
         ((variable? exp) (lookup-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp))
         ((assignment? exp) (eval-assignment exp env))
         ((definition? exp)
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
                'true))

     (define (and? exp)
       (tagged-list? exp 'and))

     (define (eval-and exp env)
       (eval-and-conds (cdr exp) env))

     (define (eval-and-conds conds env)
       (if (null? conds)
         true
         (let ((value (eval (car conds) env)))
           (if (true ? value)
             (if (null? (cdr conds))
               value
               (eval-and-conds (cdr conds) env))
             false))))

     (define (or? exp)
       (tagged-list? exp 'or))

     (define (eval-or exp env)
       (eval-or-conds (cdr exp) env))

     (define (eval-or-conds conds env)
       (if (null? conds)
         false
         (let ((value (eval (car conds) env)))
           (if (true? value)
             value
             (eval-or-conds (cdr conds) env)))))

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

     (define (let? exp) (tagged-list? exp 'let))

     (define (let-clauses exp) (cdr exp))

     (define (let-bindings clauses) (car clauses))

     (define (let-body clauses) (cdr clauses))

     (define (let->combination exp)
       (expand-let-clauses (let-clauses exp)))

     (define (expand-let-clauses clauses)
       (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
             (map cadr (let-bindings clauses))))

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
     ))

(driver-loop)

1
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
(factorial 5)