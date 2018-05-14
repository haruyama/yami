(load "./5.4perf.ss")
(start eceval)
(define (factorial n)
  (if (= n 1)
    1
    (* (factorial (- n 1)) n)))
(factorial 5)
(factorial 6)

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))
(factorial-iter 5)
(factorial-iter 6)

;; #q
;;ex5.26
;;http://www.serendip.ws/archives/3510
;T = (35 * n) + 29
;D = 10

;;ex5.27
;;http://www.serendip.ws/archives/3515
;T = (32 * n) - 16
;D = (5 * n) + 3

;;ex5.28
;;http://www.serendip.ws/archives/3520

;;ex5.29
;;http://www.serendip.ws/archives/3525
(start eceval)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
(fib 3)
(fib 4)
(fib 5)
(fib 6)
#q
; D = (5 * n) + 3
; T = 56 * Fib(n + 1) - 40

;;ex5.30
;;http://www.serendip.ws/archives/3572
