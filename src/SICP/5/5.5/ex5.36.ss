(load "../5.4/5.4.ss")
(load "./5.5.ss")

; original
(define (construct-arglist operand-codes)
  (let ((operand-codes  (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env)
                      code-to-get-last-arg
                      (code-to-get-rest-args
                        (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign argl
                                                           (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

; answer
; reverse を取る
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
    (make-instruction-sequence '() '(argl)
                               '((assign argl (const ()))))
    (let ((code-to-get-last-arg
            (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
                                         '((assign argl (op list) (reg val)))))))
      (if (null? (cdr operand-codes))
        code-to-get-last-arg
        (tack-on-instruction-sequence
          (preserving '(env)
                      code-to-get-last-arg
                      (code-to-get-rest-args
                        (cdr operand-codes)))
            (make-instruction-sequence '() '() '((assign argl (op reverse) (reg argl))))))))) ; reverse する

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign argl
                                                           (op cons) (reg val) (reg argl))))))) ; ここで cons ではなく append(adjoin-arg) していくというのもあり
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

(print-after-compiler
  (compile
    '(define (factorial-alt n)
       (if (= n 1)
         1
         (* n (factorial (- n 1)))))
    'val
    'next))

