(load "../5.4/5.4.ss")
(load "./5.5.ss")
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
