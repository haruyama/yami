(load "./5.5.ss")
(load "../5.4/5.4.ss")
(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (preserving (cdr regs)
                  (make-instruction-sequence
                    (list-union (list first-reg)
                                (registers-needed seq1))
                    (list-difference (registers-modified seq1)
                                     (list first-reg))
                    (append `((save ,first-reg))
                            (statements seq1)
                            `((restore ,first-reg))))
                  seq2))))

(print-after-compiler
  (compile
    '(define (factorial n)
       (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    'val
    'next))


(print-after-compiler
  (compile
    '(define (factorial n)
       (define (iter product counter)
         (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
       (iter 1 1))
    'val
    'next))

(print-after-compiler
  (compile
    '(define (f x)
       (+ x (g (+ x 2))))
    'val
    'next))
