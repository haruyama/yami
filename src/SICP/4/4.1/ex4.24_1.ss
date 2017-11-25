(load "./4.1_2.ss")

(define the-global-environment (setup-environment))
(eval '(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n)) the-global-environment)

(define n 0)
(while (< n 100)
       (eval '(fib 1000) the-global-environment)
       (set! n (+ n 1)))
