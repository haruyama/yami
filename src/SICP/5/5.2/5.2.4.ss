(load "./5.2.ss")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  ;追加
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))

    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          (set! current-depth (- current-depth 1))
          top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushed  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics) (print-statistics))
        (else (error "Unknown request -- STACK"
                     message))))
    dispatch))



(define (fib n)
  (define fib-machine-2
    (make-machine
      '(continue n val)
      (list (list '< <) (list '- -) (list '+ +))
      '(
        ;(perform (op initialize-stack))
        (assign continue (label fib-done))
        fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
        afterfib-n-1
        (restore n)
        (restore continue)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label afterfib-n-2))
        (save val)
        (goto (label fib-loop))
        afterfib-n-2
        (assign n (reg val))
        (restore val)
        (restore continue)
        (assign val
                (op +) (reg val) (reg n))
        (goto (reg continue))
        immediate-answer
        (perform (op print-stack-statistics))
        (assign val (reg n))
        (goto (reg continue))
        fib-done)
      ))

  ((fib-machine-2 'stack) 'initialize)
  (set-register-contents! fib-machine-2 'n n)
  (set-register-contents! fib-machine-2 'val 0)
  (start fib-machine-2)
  (display (get-register-contents fib-machine-2 'val))
  (display ((fib-machine-2 'stack) 'print-statistics))
  (newline)
  )
(fib 7)
(fib 8)
(fib 9)

;ex 5.14

(define (fact n)
  (define fact-machine
    (make-machine
      '(continue val n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label fact-done))
         fact-loop
         (test (op =) (reg n) (const 1))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-fact))
         (goto (label fact-loop))
         after-fact
         (restore n)
         (restore continue)
         (assign val (op *) (reg n) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         fact-done)))

  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (display (get-register-contents fact-machine 'val))
  (display ((fact-machine 'stack) 'print-statistics))
  (newline))

(fact 5)
(fact 6)
(fact 7)

;2*(n-1)

;ex5.15
;http://www.serendip.ws/archives/3282

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        ;;
        (ic (make-register 'ic))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (**init** ((ic 'set) 0)))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              ((ic 'set) (+ 1 (ic 'get)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          ;;
          ((eq? message 'reset-instruction-counting) ((ic 'set) 0))
          ((eq? message 'get-instruction-counting) (ic 'get))
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (fact n)
  (define fact-machine
    (make-machine
      '(continue val n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label fact-done))
         fact-loop
         (test (op =) (reg n) (const 1))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-fact))
         (goto (label fact-loop))
         after-fact
         (restore n)
         (restore continue)
         (assign val (op *) (reg n) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         fact-done)))

  ((fact-machine 'stack) 'initialize)
  (fact-machine 'reset-instruction-counting)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (display (get-register-contents fact-machine 'val))
  (newline)
  (display (fact-machine 'get-instruction-counting))
  (newline)
  )

(fact 1)
(fact 5)
(fact 6)
(fact 7)


;ex5.16
;http://www.serendip.ws/archives/3285
;make-new-machine の executeもいじる必要あり

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (ic (make-register 'ic))
        ;;
        (trace-p #f)
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (**init** ((ic 'set) 0)))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              ((ic 'set) (+ 1 (ic 'get)))
              (if trace-p
                (begin
                  (display (caar insts))
                  (newline)
                  ))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          ((eq? message 'reset-instruction-counting) ((ic 'set) 0))
          ((eq? message 'get-instruction-counting) (ic 'get))
          ;;
          ((eq? message 'trace-on) (set! trace-p #t))
          ((eq? message 'trace-off) (set! trace-p #f))
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))



(define (fact n trace-p)
  (define fact-machine
    (make-machine
      '(continue val n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label fact-done))
         fact-loop
         (test (op =) (reg n) (const 1))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-fact))
         (goto (label fact-loop))
         after-fact
         (restore n)
         (restore continue)
         (assign val (op *) (reg n) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         fact-done)))

  (if trace-p
    (fact-machine 'trace-on)
    (fact-machine 'trace-off))
  ((fact-machine 'stack) 'initialize)
  (fact-machine 'reset-instruction-counting)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  )

(fact 3 #t)
(fact 3 #f)
;ex5.17
;http://www.serendip.ws/archives/3304

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          (if (assoc next-inst labels)
                            (error "Multiply defined label: " next-inst)
                            (let ((insts
                                    (cons (list (list 'label next-inst)) insts))) ;; ex5.17
                              ;(list (list (list 'label next-inst)) insts))) ;; ex5.17
                              (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels))))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto)
     (make-goto inst machine labels pc))
    ((eq? (car inst) 'save)
     (make-save inst machine stack pc))
    ((eq? (car inst) 'restore)
     (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc))
    ((eq? (car inst) 'label)
     (lambda () (advance-pc pc)))
    (else (error "Unknown instruction type -- ASSEMBLE"
                 inst))))


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (ic (make-register 'ic))
        (trace-p #f)
        (trace-label "")
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (**init** ((ic 'set) 0)))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))

              (if (eq? 'label (caaar insts))
                (set! trace-label (cadaar insts))
                (begin
                  ((ic 'set) (+ 1 (ic 'get)))
                  (if trace-p
                    (begin
                      (display trace-label)
                      (display ": ")
                      (display (caar insts))
                      (newline)
                      ))))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          ((eq? message 'reset-instruction-counting) ((ic 'set) 0))
          ((eq? message 'get-instruction-counting) (ic 'get))
          ;;
          ((eq? message 'trace-on) (set! trace-p #t))
          ((eq? message 'trace-off) (set! trace-p #f))
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))



(define (fact n trace-p)
  (define fact-machine
    (make-machine
      '(continue val n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label fact-done))
         fact-loop
         (test (op =) (reg n) (const 1))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-fact))
         (goto (label fact-loop))
         after-fact
         (restore n)
         (restore continue)
         (assign val (op *) (reg n) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         fact-done)))

  (if trace-p
    (fact-machine 'trace-on)
    (fact-machine 'trace-off))
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (newline)
  (display (fact-machine 'get-instruction-counting))
  (newline)
  )

(fact 1 #t)
(fact 5 #t)
(fact 1 #f)

;; ;ex5.18
;; ;http://www.serendip.ws/archives/3307

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-p #f))
    (define (dispatch message)
      (cond ((eq? message 'get)
             contents)
        ((eq? message 'set)
         (lambda (value)
           (if trace-p
             (begin
               (display name)
               (display ": ")
               (display contents)
               (display " to ")
               (display value)
               (newline)))
           (set! contents value)))
        ((eq? message 'trace-on) (set! trace-p #t))
        ((eq? message 'trace-off) (set! trace-p #f))
        (else
          (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (fact n)
  (define fact-machine
    (make-machine
      '(continue val n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label fact-done))
         fact-loop
         (test (op =) (reg n) (const 1))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-fact))
         (goto (label fact-loop))
         after-fact
         (restore n)
         (restore continue)
         (assign val (op *) (reg n) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         fact-done)))

  ((fact-machine 'stack) 'initialize)
  (fact-machine 'reset-instruction-counting)
  ((get-register fact-machine 'val) 'trace-on)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (newline)
  )

(fact 1)
(fact 5)

;; ;ex5.19
;; ;http://www.serendip.ws/archives/3380

(use srfi-1)

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (ic (make-register 'ic))
        (trace-p #f)
        (trace-label "")
        (stack (make-stack))
        (the-instruction-sequence '())
        (label '())
        (breakpoints '())
        (count-from-label 0)
        )
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (**init** ((ic 'set) 0)))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (begin
              (allocate-register name)
              (lookup-register name)))))
      (define (execute resume-p)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              (if (eq? (caaar insts) 'label)
                (begin
                  (set! count-from-label 0)
                  (set! trace-label (cadaar insts)))
                (begin
                  (if (not resume-p)
                    (begin
                      (set! count-from-label (+ 1 count-from-label))
                      ((ic 'set) (+ 1 (ic 'get)))))
                  (if trace-p
                    (begin
                      (display trace-label)
                      (display ": ")
                      (display (caar insts))
                      (newline)
                      ))))
              (if (and (not resume-p) (member (cons trace-label count-from-label) breakpoints))
                (begin
                  (display "***** BREAK! *****")
                  (newline)
                  )
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute #f)))))))
      (define (set-bpoint label n)
        (if (not (member (cons label n) breakpoints))
          (set! breakpoints (cons (cons label n) breakpoints))))
      (define (del-bpoint label n)
        (if (member (cons label n) breakpoints)
          (set! breakpoints (delete (cons label n) breakpoints))
          (error "DEL BREAKPOINT")))
      (define (del-all-bpoint)
        (set! breakpoints '()))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute #f))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          ((eq? message 'set-bpoint) set-bpoint)
          ((eq? message 'del-bpoint) del-bpoint)
          ((eq? message 'del-all-bpoint) del-all-bpoint)
          ((eq? message 'proceed) (execute #t))

          ((eq? message 'reset-instruction-counting) ((ic 'set) 0))
          ((eq? message 'get-instruction-counting) (ic 'get))
          ;;
          ((eq? message 'trace-on) (set! trace-p #t))
          ((eq? message 'trace-off) (set! trace-p #f))


          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define (set-breakpoint machine label n)
  ((machine 'set-bpoint) label n)
  'set-breakpoint-done)

(define (cancel-breakpoint machine label n)
  ((machine 'del-bpoint) label n)
  'delete-breakpoint-done)


(define (cancel-all-breakpoint machine)
  ((machine 'del-all-bpoint))
  'delete-all-breakpoint-done)

(define (proceed-machine machine)
  (machine 'proceed))

(define gcd-machine
  (make-machine
    '()
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(gcd-machine 'trace-on)
(set-breakpoint gcd-machine 'test-b 4)
(set-breakpoint gcd-machine 'test-b 6)
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)

(start gcd-machine)
(get-register-contents gcd-machine 'a)
(proceed-machine gcd-machine)

(get-register-contents gcd-machine 'a)
(proceed-machine gcd-machine)
(get-register-contents gcd-machine 'a)

(cancel-breakpoint gcd-machine 'test-b 4)
(cancel-all-breakpoint gcd-machine)
(proceed-machine gcd-machine)
(get-register-contents gcd-machine 'a)
