(load "./number.ss")

(load "./poly.ss")

(load "./ex2-87.ss")

(add
  (make-polynomial 'x '((1 1)))
  (make-polynomial 'x '((1 1))))

(add
  (make-polynomial 'x '((1 (rational 1 . 2))))
  (make-polynomial 'x '((1 1))))

(mul
  (make-polynomial 'x '((1 1)))
  (make-polynomial 'x '((1 1))))

(=zero? (make-polynomial 'x '((1 1))))

(=zero? (make-polynomial 'x '((1 0))))

(=zero? (make-polynomial 'x '()))

(=zero? (make-polynomial 'x '((5 0) (4 0))))

(define c (make-polynomial 'y '((5 0) (4 0))))

(=zero? c)

(define c (make-polynomial 'y '((5 1) (4 0))))

(=zero? c)

(sub
  (make-polynomial 'x '((1 2)))
  (make-polynomial 'x '((1 1))))

(sub
  (sub  (make-polynomial 'x '((1 1)))
        (make-polynomial 'x '((1 1))))
  (make-polynomial 'x '((1 1))))

(sub
  (make-polynomial 'x '((1 (rational 1 . 2))))
  (make-polynomial 'x '((1 1))))

(load "./ex2-91.ss")

(div (make-polynomial 'x '((1 1)))
     (make-polynomial 'x '((1 1))))

(div (make-polynomial 'x '((5 1) (0 -1)))
     (make-polynomial 'x '((2 1)(0 -1))))

;ex 2.93

(load "./ex2-93.ss")

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rt (make-rational p2 p1))
(add rt rt)

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (remainder-terms a b))))

(load "./ex2-94.ss")

(div-terms '((3 1)(0 1)) '((2 1)(0 1)))
(remainder-terms '((3 1)(0 1)) '((2 1)(0 1)))

(gcd-terms '((3 1)(0 1)) '((2 1)(0 1)))
(gcd-terms '((5 2) (3 2) (2 2) (0 2)) '((4 1) (2 2) (0 1)))

(greatest-common-divisor 8 12)

(define p1 (make-polynomial 'x '((4 1)(3 -1)(2 -2)(1 2))))
(define p2 (make-polynomial 'x '((3 1)(1 -1))))

(greatest-common-divisor p1 p2)

(div-terms '((4 1)(3 -1)(2 -2)(1 2)) '((2 -1)(1 1)))
(div-terms '((3 1)(1 -1)) '((2 -1)(1 1)))

;ex2.95

(define p1 (make-polynomial 'x '((2 1)(1 -2)(0 1))))
(define p2 (make-polynomial 'x '((2 11)(0 7))))
(define p3 (make-polynomial 'x '((1 13)(0 5))))


(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
(use slib)
(require 'trace)
;(trace remainder-terms)

(greatest-common-divisor q1 q2)
(gcd-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))

(remainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))
;(untrace gcd-terms)
;(untrace remainder-terms)
(load "./ex2-96.ss")

(remainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))


(pseudoremainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))


(greatest-common-divisor q1 q2)

;ex 2.97
(load "./ex2-97.ss")

(gcd-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))
(reduce-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))


(define p1 (make-polynomial 'x '((2 1)(1 -2)(0 1))))
(define p2 (make-polynomial 'x '((2 11)(0 7))))
(define p3 (make-polynomial 'x '((1 13)(0 5))))

(reduce-poly (mul p2 p1) (mul p1 p3))
(reduce-poly (mul p2 p3) (mul p2 p1))
(reduce-poly (mul p1 p3) (mul p2 p3))

(reduce-integers 10 2)

(define (reduce x y) (apply-generic 'reduce x y))

(reduce 10 5)
(reduce (mul p1 p2) (mul p1 p3))



(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(add
  (make-rational 10 2)
  (make-rational 10 2))

(make-rational 10 2)

(make-rational p2 p1)
(define rf1 (make-rational p1 p2))
(display rf1)


(define rf2 (make-rational p3 p4))
(display rf2)

(add rf1 rf2)
(reduce p1 p2)
(reduce p3 p4)
(reduce p1 p4)
(reduce p2 p4)



(sub
  (make-polynomial 'x
                   '((1 2)))
  (make-polynomial 'x
                   '((1 1))))
