(load "./5.2.ss")

;ex 5.8

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          (receive insts
                                   (cons (make-label-entry next-inst
                                                           insts)
                                         labels))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

(define ex5.8-text '(start
                      (goto (label here))
                      here
                      (assign a (const 3))
                      (goto (label there))
                      here
                      (assign a (const 4))
                      (goto (label there))
                      there))

(define ex5.8-machine1 (make-machine '(a) '() ex5.8-text))

(start ex5.8-machine1)
(get-register-contents ex5.8-machine1 'a)


(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          (if (assoc next-inst labels)
                            ;ex5.8
                            (error "Multiple labels -- ASSEMBLE" next-inst)
                            (receive insts
                                     (cons (make-label-entry next-inst
                                                             insts)
                                           labels)))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

(make-machine '(a) '() ex5.8-text)


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

;ex5.9
(define (make-operation-exp exp machine labels ops)
  (let ((op (lookup-prim (operation-exp-op exp) ops))
        (aprocs
          (map (lambda (e)
                 (if (or (register-exp? e) (constant-exp? e))
                   (make-primitive-exp e machine labels)
                   (error "You can only operate a register or a constant" e)))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;ex5.10

;ex5.11
;b

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (let ((head-of-stack (pop stack)))
          (if (eq? (car head-of-stack) reg-name)
            (set-contents! reg (cdr head-of-stack))
            (error "Wrong register name - RESTORE" reg-name (car head-of-stack))))
        (advance-pc pc)))))



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

(set-register-contents! fib-machine 'n 7)
(set-register-contents! fib-machine 'continue 0)
(set-register-contents! fib-machine 'val 0)
(start fib-machine)
(get-register-contents fib-machine 'val)

;c
;http://www.serendip.ws/archives/3120

;ex 5.12

;http://www.serendip.ws/archives/3125

;ex 5.13
;http://www.serendip.ws/archives/3268
