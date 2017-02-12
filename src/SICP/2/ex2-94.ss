(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))


  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (negate-term L) (map
                            (lambda (x) (make-term (order x)
                                                   (negate (coeff x)))) L))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))


  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((result (div-terms (term-list p1) (term-list p2))))
        (cons
          (make-poly (variable p1)
                     (car result))
          (make-poly (variable p1)
                     (cdr result))))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))

  (define (=zero-poly? p)
    (let ((terms (term-list p)))
      (= (length terms)
         (length (filter (lambda (x) (=zero? x))
                         (map coeff terms))))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))


  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero-poly? '(polynomial)
       =zero-poly?)

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'gcd-poly '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (greatest-common-divisor p1 p2)
  (let ((t1 (type-tag p1))
        (t2 (type-tag p2)))
    (cond ((and (eq? t1 'polynomial) (eq? t2 'polynomial))
           (gcd-poly p1 p2))
      ((and (eq? t1 'scheme-number) (eq? t2 'scheme-number))
       (gcd p1 p2))
      (else
        (error "greatest-common-divisor Error" (list p1 p2))))))

(define (gcd-poly x y) (apply-generic 'gcd-poly x y))
