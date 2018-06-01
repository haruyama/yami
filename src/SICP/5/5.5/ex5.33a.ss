(load "../5.4/5.4.ss")
(load "./5.5.ss")
(print-after-compiler
  (compile
    '(define (factorial n)
       (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    'val
    'next))

