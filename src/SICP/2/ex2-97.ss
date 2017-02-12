(define (reduce-terms n d)
  (let ((gcd-term (gcd-terms n d)))
    (let ((g1 (coeff (first-term  gcd-term)))
          (o2 (order (first-term  gcd-term)))
          (on (order (first-term  n)))
          (dn (order (first-term  d))))
      (let ((mul-value
              (if (> on dn)
                (expt g1 (+ 1 on (- o2)))
                (expt g1 (+ 1 dn (- o2)))
                )))

        (let ((mn (mul-coeff-term n mul-value))
              (md (mul-coeff-term d mul-value)))

          (let ((tn (car (div-terms mn gcd-term)))
                (td (car (div-terms md gcd-term))))
            (let ((div-number
                    (gcd
                      (accumulate gcd 0 (map coeff tn))
                      (accumulate gcd 0 (map coeff td)))))
              (list
                (div-coeff-term tn div-number)
                (div-coeff-term td div-number)))))))))

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

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((reduced
              (reduce-terms (term-list p1)
                            (term-list p2))))
        (list
          (tag (make-poly (variable p1) (car reduced)))
          (tag (make-poly (variable p1) (cadr reduced)))))
      (error "Polys not in same var -- REDUCE-POLY"
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

  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (reduce-poly p1 p2)))

  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))


  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


(install-polynomial-package)

(define (reduce-poly x y) 
  (apply-generic 'reduce x y))

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'reduce '(scheme-number scheme-number)
       reduce-integers)

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((r (reduce n d)))
          (cons (car r) (cadr r))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x)
    (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

