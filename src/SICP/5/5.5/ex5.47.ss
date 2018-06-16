(load "./5.5.ss")
(load "./5.5.7.ss")

;;ex5.47
(compile-and-go
  '(begin (define (f x) (g x))
     ))
(define (g x) (+ x 2))
(f 1)
quit

(define all-regs '(env proc val argl continue compapp))

;; これはもともとあるもの
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
    ((and (not (eq? target 'val))
       (not (eq? linkage 'return)))
     (let ((proc-return (make-label 'proc-return)))
       (make-instruction-sequence '(proc) all-regs
                                  `((assign continue (label ,proc-return))
                                    (assign val (op compiled-procedure-entry)
                                            (reg proc))
                                    (goto (reg val))
                                    ,proc-return
                                    (assign ,target (reg val))
                                    (goto (label ,linkage))))))
    ((and (eq? target 'val) (eq? linkage 'return))
     (make-instruction-sequence '(proc continue) all-regs
                                '((assign val (op compiled-procedure-entry)
                                          (reg proc))
                                  (goto (reg val)))))
    ((and (not (eq? target 'val)) (eq? linkage 'return))
     (error "return linkage, target not val -- COMPILE" target))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (save continue)
                                      (goto (reg compapp)))))
    ((and (not (eq? target 'val))
       (not (eq? linkage 'return)))
     (let ((proc-return (make-label 'proc-return)))
       (make-instruction-sequence '(proc) all-regs
                                  `((assign continue (label ,proc-return))
                                    (save continue)
                                    (goto (reg compapp))
                                    ,proc-return
                                    (assign ,target (reg val))
                                    (goto (label ,linkage))))))
    ((and (eq? target 'val) (eq? linkage 'return))
     (make-instruction-sequence '(proc continue) all-regs
                                '((save continue)
                                  (goto (reg compapp)))))
    ((and (not (eq? target 'val)) (eq? linkage 'return))
     (error "return linkage, target not val -- COMPILE" target))))


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
          (compound-proc-appl target compiled-linkage)
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
