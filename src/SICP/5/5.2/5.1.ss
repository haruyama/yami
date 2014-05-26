(load "./5.2.ss")

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(get-register-contents gcd-machine 'a)
(get-register-contents gcd-machine 'b)

(start gcd-machine)

(get-register-contents gcd-machine 'a)

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list '< <) (list '= =) (list '- -))
    '(test-b
       (test (op =)  (reg b)  (const 0))
       (branch (label gcd-done))
       (assign t (reg a))
       rem-loop
       (test  (op <)  (reg t)  (reg b))
       (branch (label rem-done))
       (assign t (op -)  (reg t)  (reg b))
       (goto (label rem-loop))
       rem-done
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(get-register-contents gcd-machine 'a)
(get-register-contents gcd-machine 'b)

(start gcd-machine)

(get-register-contents gcd-machine 'a)

(define fact-machine
  (make-machine
    '(p c n)
    (list (list '> >) (list '* *) (list '+ +))
    '(
      (assign p (const 1))
      (assign c (const 1))
      test-c
      (test (op >) (reg c) (reg n))
      (branch (label factorial-done))
      (assign p (op *) (reg p) (reg c))
      (assign c (op +) (reg c) (const 1))
      (goto (label test-c))
      factorial-done)))

(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'p)

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
  (newline))

(fact 5)
(fact 6)
(fact 7)


(define (fib n)
  (define fib-machine
    (make-machine
      '(continue n val)
      (list (list '< <) (list '- -) (list '+ +)
            (list 'display display)
            )
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
  (newline))

(fib 3)
(fib 4)
(fib 5)
(fib 6)

(define (my-expt b n)
  (define expt-machine
    (make-machine
      '(continue val b n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign continue (label expt-done))
         expt-loop
         (test (op =) (reg n) (const 0))
         (branch (label base-case))
         (save continue)
         (save n)
         (assign n (op -) (reg n) (const 1))
         (assign continue (label after-expt))
         (goto (label expt-loop))
         after-expt
         (restore n)
         (restore continue)
         (assign val (op *) (reg b) (reg val))
         (goto (reg continue))
         base-case
         (assign val (const 1))
         (goto (reg continue))
         expt-done)))
  ((expt-machine 'stack) 'initialize)
  (set-register-contents! expt-machine 'b b)
  (set-register-contents! expt-machine 'n n)
  (start expt-machine)
  (display (get-register-contents expt-machine 'val))
  (newline))

(my-expt 2 10)
(my-expt 3 3)

(define (my-expt2 b n)
  (define expt-machine
    (make-machine
      '(counter product b n)
      (list (list '= =) (list '- -) (list '* *))
      '(start
         (assign counter (reg n))
         (assign product (const 1))
         expt-loop
         (test (op =) (reg counter) (const 0))
         (branch (label expt-done))
         (assign counter (op -) (reg counter) (const 1))
         (assign product (op *) (reg b) (reg product))
         (goto (label expt-loop))
         expt-done)))
  (set-register-contents! expt-machine 'b b)
  (set-register-contents! expt-machine 'n n)
  (start expt-machine)
  (display (get-register-contents expt-machine 'product))
  (newline))

(my-expt2 2 10)
(my-expt2 3 3)


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (square x)
  (* x x))

(define sqrt-machine
  (make-machine
    '(g n)
      (list
        (list 'good-enough? good-enough?)
        (list 'improve improve)
        (list 'square square))
      '(
        (assign n (const 2.0))
        (assign g (const 1.0))
        test-good-enough?
        (test (op good-enough?) (reg g) (reg n))
        (branch (label newton-done))
        (assign g (op improve) (reg g) (reg n))
        (goto (label test-good-enough?))
        newton-done)))

(start sqrt-machine)
(get-register-contents sqrt-machine 'g)

(define sqrt2-machine
  (make-machine
    '(g x t1 t2 t3)
      (list
        (list '* *)
        (list '- -)
        (list '>= >=)
        (list '< <)
        (list '/ /)
        (list '+ +)
        )
      '(
        (assign g (const 1.0))
        test-good-enough?
        ;; good-enough? の展開
        (assign t1 (op *) (reg g) (reg g))
        (assign t2 (op -) (reg t1) (reg x))
        (test (op >=) (reg t2) (const 0)) ; abs
        (branch (label skip-abs))
        (assign t2 (op *) (reg t2) (const -1)) ; 負数は正数に変換
        skip-abs
        (test (op <) (reg t2) (const 0.001))
        ;; good-enouh? 完了
        (branch (label newton-done))
        ;; improveの展開
        (assign t1 (op /) (reg x) (reg g))
        (assign t2 (op +) (reg g) (reg t1))
        (assign t3 (op /) (reg t2) (const 2)) ; average
        ;; improve 完了
        (assign g (reg t3))
        (goto (label test-good-enough?))
        newton-done)))

(set-register-contents! sqrt2-machine 'x 3)
(start sqrt2-machine)
(get-register-contents sqrt2-machine 'g)

