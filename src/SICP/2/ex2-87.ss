;ex2.87
;ex2.88
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
                     (cdr result))
          )
        )
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))


  ;  (define (=zero-poly? p)
  ;    (let ((terms (term-list p)))
  ;      (= (length terms)
  ;         (length (filter (lambda (x) (=zero? x))
  ;                         (map coeff terms))))))

  (define (=zero-poly? p)
    (every =zero?  (map coeff (term-list p))))

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
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result)) (tag (cdr result))))))

  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (equal-terms? L1 L2)
  (cond ((empty-termlist? L1) (every =zero? (map coeff L2)))
    ((empty-termlist? L2) (every =zero? (map coeff L1)))
    (else
      (let ((t1 (first-term L1)) (t2 (first-term L2)))
        (cond ((> (order t1) (order t2))
               (if (=zero? (coeff t1))
                 (equal-terms? (rest-terms L1) L2)
                 #f))
          ((< (order t1) (order t2))
           (if (=zero? (coeff t2))
             (equal-terms? L1 (rest-terms L2))
             #f))
          ((= (coeff t1) (coeff t2))
           (equal-terms? (rest-terms L1) (rest-terms L2)))
          (else #f))))))

(put 'equ? '(polynomial polynomial)
     (lambda (p1 p2)
       (and
         (same-variable? p1 p2)
         (equal-terms? (term-list p1) (term-list p2)))))

(define (=zero? x)
  (if (eq? (type-tag x) 'polynomial)
    ((get '=zero-poly? '(polynomial)) (contents x))
    (equ? 0 x)))
