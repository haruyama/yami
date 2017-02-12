(define (pseudoremainder-terms p q)
  (let ((o1 (caar p))
        (o2 (caar q))
        (c  (cadar q)))
    (let ((factor (expt c (+ 1 o1 (- o2)))))
      (cadr (div-terms
              (map (lambda (x) (list (car x) (mul factor (cadr x)))) p)
              q)))))

;(define (gcd-terms a b)
;  (if (empty-termlist? b)
;    a
;    (gcd-terms b (pseudoremainder-terms a b))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;(accumulate gcd 1458 '(-2916 1458))
;(accumulate gcd 0 '(10 4 6))
(define (mul-coeff-term a n)
  (map (lambda (x) (list (car x) (mul (cadr x) n))) a))
(define (div-coeff-term a n)
  (map (lambda (x) (list (car x) (div (cadr x) n))) a))


(define (gcd-terms a b)
  (if (empty-termlist? b)
    (let ((coeffs (map coeff a)))
      (let ((div-number (accumulate gcd 0 coeffs)))
        (div-coeff-term a div-number)))

    (gcd-terms b (pseudoremainder-terms a b))))
