(load "./5.5.ss")
(load "./5.5.7.ss")

(compile-and-go
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))
(factorial 5)
end
(start-eceval)
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
(factorial 5)
end

(compile-and-go
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2)))))) 
(fib 5)
end

;;ex5.45
;;http://www.serendip.ws/archives/3926
(compile-and-go
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1))  n))))
(factorial 4)
(factorial 5)
quit

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(start-eceval)
(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1))  n)))
(factorial 4)
(factorial 5)
q

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

(fact 4)
(fact 5)


;;ex5.46
;;http://www.serendip.ws/archives/3962
(compile-and-go
  '(define (fib n)
     (if (< n 2)
       n
       (+ (fib (- n 1)) (fib (- n 2))))))
(fib 3)
(fib 4)
quit

(start-eceval)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
(fib 3)
(fib 4)
q

(define (fib n)
  (define fib-machine
    (make-machine
      '(continue n val)
      (list (list '< <) (list '- -) (list '+ +))
      '((assign continue (label fib-done))
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
        (assign val (reg n))
        (goto (reg continue))
        fib-done)
      ))
  ((fib-machine 'stack) 'initialize)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (display (get-register-contents fib-machine 'val))
  (display ((fib-machine 'stack) 'print-statistics))
  (newline))

(fib 2)
(fib 3)

;;ex5.47
(compile-and-go
  '(begin (define (f x) (g x))
     ))
(define (g x) (+ x 2))
(f 1)
quit

(define all-regs '(env proc val argl continue compapp))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch))))
        (make-instruction-sequence '(proc) '()
                                   `((test (op compiled-procedure?) (reg proc))
                                     (branch (label ,compiled-branch))))
        (parallel-instruction-sequences
          (cond ((and (eq? target 'val) (not (eq? compiled-linkage 'return)))
                 (make-instruction-sequence '(proc) all-regs
                                            `((assign continue (label ,compiled-linkage))
                                              (save continue)
                                              (goto (reg compapp)))))
            ((and (not (eq? target 'val))
               (not (eq? compiled-linkage 'return)))
             (let ((proc-return (make-label 'proc-return)))
               (make-instruction-sequence '(proc) all-regs
                                          `((assign continue (label ,proc-return))
                                            (save continue)
                                            (goto (reg compapp))
                                            ,proc-return
                                            (assign ,target (reg val))
                                            (goto (label ,compiled-linkage))))))
            ((and (eq? target 'val) (eq? compiled-linkage 'return))
             (make-instruction-sequence '(proc continue) all-regs
                                        '((save continue)
                                          (goto (reg compapp)))))
            ((and (not (eq? target 'val)) (eq? compiled-linkage 'return))
             (error "return linkage, target not val -- COMPILE"
                    target)))
          (parallel-instruction-sequences
            (append-instruction-sequences
              compiled-branch
              (compile-proc-appl target compiled-linkage))
            (append-instruction-sequences
              primitive-branch
              (end-with-linkage linkage
                                (make-instruction-sequence '(proc argl)
                                                           (list target)
                                                           `((assign ,target
                                                                     (op apply-primitive-procedure)
                                                                     (reg proc)
                                                                     (reg argl))))))))
        after-call))))


(define eceval
  (make-machine
    '(exp env val proc argl continue unev compapp)
    eceval-operations
    (cons '(assign compapp (label compound-apply))
          eceval-body)))

(compile-and-go
  '(begin (define (f x) (g x))
     ))
(define (g x) (+ x 2))
(f 1)
quit
