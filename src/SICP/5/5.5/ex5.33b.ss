(load "../5.4/5.4.ss")
(load "./5.5.ss")
(print-after-compiler
  (compile
    '(define (factorial-alt n)
       (if (= n 1)
         1
         (* n (factorial (- n 1)))))
    'val
    'next))

