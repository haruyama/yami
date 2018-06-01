(load "../5.2/5.2.ss")
(load "../5.4/5.4.ss")

(define eceval-operations
  (list
    (list 'adjoin-arg adjoin-arg)
    (list 'announce-output announce-output)
    (list 'application? application?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'assignment-value assignment-value)
    (list 'assignment-variable assignment-variable)
    (list 'assignment? assignment?)
    (list 'begin-actions begin-actions)
    (list 'begin? begin?)
    (list 'compound-procedure? compound-procedure?)
    (list 'define-variable! define-variable!)
    (list 'definition-value definition-value)
    (list 'definition-variable definition-variable)
    (list 'definition? definition?)
    (list 'empty-arglist empty-arglist)
    (list 'extend-environment extend-environment)
    (list 'first-exp first-exp)
    (list 'first-operand first-operand)
    (list 'get-global-environment get-global-environment)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)
    (list 'if-predicate if-predicate)
    (list 'if? if?)
    (list 'lambda-body lambda-body)
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda? lambda?)
    (list 'last-exp? last-exp?)
    (list 'last-operand? last-operand?)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'make-procedure make-procedure)
    (list 'no-operands? no-operands?)
    (list 'operands operands)
    (list 'operator operator)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'procedure-body procedure-body)
    (list 'procedure-environment procedure-environment)
    (list 'procedure-parameters procedure-parameters)
    (list 'prompt-for-input prompt-for-input)
    (list 'quoted? quoted?)
    (list 'read read)
    (list 'rest-exps rest-exps)
    (list 'rest-operands rest-operands)
    (list 'self-evaluating? self-evaluating?)
    (list 'set-variable-value! set-variable-value!)
    (list 'text-of-quotation text-of-quotation)
    (list 'true? true?)
    (list 'user-print user-print)
    (list 'variable? variable?)
    (list 'cond? cond?)
    (list 'cond-clauses cond-clauses)
    (list 'cond-else-clauses? cond-else-clauses?)
    (list 'cond-predicate cond-predicate)
    (list 'cond-actions cond-actions)
    (list 'let? let?)
    (list 'let->combination let->combination)
    (list 'null? null?)
    (list 'car car)
    (list 'cdr cdr)
    (list 'symbol? symbol?)
    )
  )

(define
  eceval-body
  '(
    ;; 5.4.4

    read-eval-print-loop
    (perform (op initialize-stack))
    (perform
      (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))
    print-result
    (perform
      (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    ;;5.4.1
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))

    ;;ex5.23
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op let?) (reg exp))
    (branch (label ev-let))

    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))


    ;;ex5.24
    ev-cond
    (save continue)
    (assign unev (op cond-clauses) (reg exp))

    ev-cond-loop
    (test (op null?) (reg unev))
    (branch (label ev-cond-null))
    (assign exp (op car) (reg unev)); cond-first-clause
    (test (op cond-else-clauses?) (reg exp))
    (branch (label ev-cond-actions))
    (save exp)
    (assign exp (op cond-predicate) (reg exp))
    (save unev)
    (save env)
    (assign continue (label ev-cond-predicate-done))
    (goto (label eval-dispatch))

    ev-cond-predicate-done
    (restore env)
    (restore unev)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-cond-actions))
    (assign unev (op cdr) (reg unev))
    (goto (label ev-cond-loop))

    ev-cond-actions
    (assign unev (op cond-actions) (reg exp))
    (goto (label ev-sequence))

    ev-cond-null
    (assign val (const #f))
    (restore continue)
    (goto (reg continue))







    ;;ex5.23
    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))


    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (test (op symbol?) (reg exp))
    (branch (label ev-operand-symbol))
    (save env)
    (assign continue (label ev-appl-did-operator-nosymbol))
    (goto (label eval-dispatch))

    ev-operand-symbol
    (assign continue (label ev-appl-did-operator-symbol))
    (goto (label eval-dispatch))

    ev-appl-did-operator-nosymbol
    (restore env)
    ev-appl-did-operator-symbol
    (restore unev)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)

    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))

    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

    ;;5.4.2

    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))

    ;;5.4.3
    ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))

    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ;;5.4.4


    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))

    unknown-procedure-type
    (restore continue)
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))

    signal-error
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))
    ))

(define eceval
  (make-machine
    '(exp env val proc argl continue unev)
    eceval-operations
    eceval-body))

(start eceval)
(define a 0)
(+ 1 1)

(cond ((eq? a 1) 1)
  ((eq? a 2) 2)
  (else a))

(define (nyo a)
  (cond ((eq? a 1) 1)
    ((eq? a 2) 2)
    (else a)))
(nyo 1)
(nyo 2)
(nyo 3)

(define (nya)
  (let ((a 0))
    (cond ((eq? a 1) 1)
      ((eq? a 2) 2)
      (else a))))

(nya)
(define (nya)
  (let ((a 1))
    (cond ((eq? a 1) 1)
      ((eq? a 2) 2)
      (else a))))
(nya)