(load "./4.1.ss")
;ex4.3
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false ))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))
              ))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define exp-table (make-table))
(define get (exp-table 'lookup-proc))
(define put (exp-table 'insert-proc!))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    (else
      (if (get 'eval (tag exp))
        ((get 'eval (tag exp)) exp env)
        (if (application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env))
          (error "Unknown expression type -- EVAL" exp))))))

(define (tag exp)
  (if (pair? exp)
    (car exp)
    (error "not tagged expression" exp)))

(define (eval-assignment2 exp env)
  (newline)
  (display exp)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


(put 'eval 'set! eval-assignment2)
(put 'eval 'define eval-definition)
(define the-global-environment (setup-environment))
(eval '(define x 1) the-global-environment)
(eval '(display x) the-global-environment)
(eval '(set! x 3) the-global-environment)
(eval '(display x) the-global-environment)

;ex4.10
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

(put 'eval 'and eval-and)
(define the-global-environment (setup-environment))
(eval '(and true true) the-global-environment)
(eval '(and false true) the-global-environment)
(eval '(and true false) the-global-environment)
(eval '(and true true false) the-global-environment)
(eval '(and true false true) the-global-environment)
(eval '(and true) the-global-environment)
(eval '(and 1) the-global-environment)
