(load "./number.ss")

(load "./poly.ss")

(load "./ex2-87.ss")

;http://www.serendip.ws/archives/1137

(define (install-sparse-term-package)
  ;; 内部手続き
  (define (make-sparse-term order coeff) (list order coeff))
  (define (the-empty-sparse-termlist) '())
  (define (empty-sparse-termlist? term-list) (null? term-list))
  (define (first-sparse-term term-list) (car term-list))
  (define (rest-sparse-terms term-list) (cdr term-list))
  (define (order-sparse term) (car term))
  (define (coeff-sparse term) (cadr term))
  (define (adjoin-sparse-term term term-list)
    (if (=zero? (coeff-sparse term))
        term-list
        (cons term term-list)))
  (define (=zero-sparse-term? L)
    (or (empty-sparse-termlist? L)
        (and (=zero? (coeff-sparse (first-sparse-term L)))
             (=zero-sparse-term? (rest-sparse-terms L)))))
  (define (add-sparse-terms L1 L2)
    (cond ((empty-sparse-termlist? L1) L2)
          ((empty-sparse-termlist? L2) L1)
          (else
            (let ((t1 (first-sparse-term L1)) (t2 (first-sparse-term L2)))
                 (cond ((> (order-sparse t1) (order-sparse t2))
                        (adjoin-sparse-term
                          t1 (add-sparse-terms (rest-sparse-terms L1) L2)))
                       ((< (order-sparse t1) (order-sparse t2))
                        (adjoin-sparse-term
                          t2 (add-sparse-terms L1 (rest-sparse-terms L2))))
                       (else
                         (adjoin-sparse-term
                           (make-sparse-term (order-sparse t1)
                                             (add (coeff-sparse t1) (coeff-sparse t2)))
                           (add-sparse-terms (rest-sparse-terms L1)
                                             (rest-sparse-terms L2)))))))))
  (define (mul-sparse-terms L1 L2)
    (if (empty-sparse-termlist? L1)
        (the-empty-sparse-termlist)
        (add-sparse-terms (mul-sparse-term-by-all-sparse-terms
                            (first-sparse-term L1) L2)
                          (mul-sparse-terms (rest-sparse-terms L1) L2))))
  (define (mul-sparse-term-by-all-sparse-terms t1 L)
    (if (empty-sparse-termlist? L)
        (the-empty-sparse-termlist)
        (let ((t2 (first-sparse-term L)))
             (adjoin-sparse-term
               (make-sparse-term (+ (order-sparse t1) (order-sparse t2))
                                 (mul (coeff-sparse t1) (coeff-sparse t2)))
               (mul-sparse-term-by-all-sparse-terms t1 (rest-sparse-terms L))))))
  (define (negate-sparse-term L)
    (if (empty-sparse-termlist? L)
        (the-empty-sparse-termlist)
        (let ((t (first-sparse-term L)))
             (adjoin-sparse-term
               (make-sparse-term (order-sparse t) (negate (coeff-sparse t)))
               (negate-sparse-term (rest-sparse-terms L))))))
  ;; 外部とのインターフェース
  (define (tag x) (attach-tag 'sparse-term x))
  (put '=zero-term? '(sparse-term) =zero-sparse-term?)
  (put 'order '(sparse-term) order-sparse)
  (put 'add-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (add-sparse-terms x y))))

  ;;;; sparse-dense
  (put 'add-terms '(sparse-term dense-term)
       (lambda (x y) (tag (add-sparse-terms x (dense->sparse y)))))
  (put 'add-terms '(dense-term sparse-term)
       (lambda (x y) (tag (add-sparse-terms (dense->sparse x) y))))

  (put 'mul-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (mul-sparse-terms x y))))

  ;;;; sparse-dense
  (put 'mul-terms '(sparse-term dense-term)
       (lambda (x y) (tag (mul-sparse-terms x (dense->sparse y)))))
  (put 'mul-terms '(dense-term sparse-term)
       (lambda (x y) (tag (mul-sparse-terms (dense->sparse x) y))))

  (put 'negate-term '(sparse-term)
       (lambda (x) (tag (negate-sparse-term x))))
  (put 'make-from-sparse 'sparse-term
       (lambda (sparse-term-list) (tag sparse-term-list)))
  (put 'make-from-dense 'sparse-term
       (lambda (dense-term-list) (tag (dense->sparse dense-term-list))))
  'done)

(define (make-sparse-term term-list)
  ((get 'make-from-sparse 'sparse-term) term-list))

(install-sparse-term-package)

(define (install-dense-term-package)
  ;; 内部手続き
  (define (adjoin-dense-term term term-list)
    (cons term term-list))
  (define (the-empty-dense-termlist) '())
  (define (empty-dense-termlist? term-list) (null? term-list))
  (define (first-dense-term term-list) (car term-list))
  (define (rest-dense-terms term-list) (cdr term-list))
  (define (order-dense-term term-list) (length (rest-dense-terms term-list)))
  (define (coeff-dense-term term-list) (first-dense-term term-list))
  (define (=zero-dense-term? L)
    (or (empty-dense-termlist? L)
        (and (=zero? (coeff-dense-term L))
             (=zero-dense-term? (rest-dense-terms L)))))
  (define (normalize-dense-term L)
    (cond ((empty-dense-termlist? L) L)
          ((=zero? (first-dense-term L))
           (normalize-dense-term (rest-dense-terms L)))
          (else L)))
  (define (add-dense-terms L1 L2)
    (define (add-rterms R1 R2)
      (cond ((empty-dense-termlist? R1) R2)
            ((empty-dense-termlist? R2) R1)
            (else
              (adjoin-dense-term (add (first-dense-term R1)
                                      (first-dense-term R2))
                                 (add-rterms (cdr R1) (cdr R2))))))
    (cond ((empty-dense-termlist? L1) L2)
          ((empty-dense-termlist? L2) L1)
          (else
            (normalize-dense-term (reverse (add-rterms (reverse L1)
                                                       (reverse L2)))))))
  (define (expand-dense-term L n)
    (if (= n 0)
        L
        (expand-dense-term
          (adjoin-dense-term 0 L) (- n 1))))
  (define (mul-dense-terms L1 L2)
    (define (mul-dense-terms-sub n L1 L2)
      (if (= n 0)
          (mul-dense-term-by-all-dense-terms 0 (first-dense-term L1) L2)
          (add-dense-terms
            (mul-dense-term-by-all-dense-terms n (first-dense-term L1) L2)
            (mul-dense-terms-sub (- n 1) (rest-dense-terms L1) L2))))
    (if (or (empty-dense-termlist? L1) (empty-dense-termlist? L2))
        (the-empty-dense-termlist)
        (mul-dense-terms-sub (order-dense-term L1) L1 L2)))
  (define (mul-dense-term-by-all-dense-terms n t1 L)
    (reverse (expand-dense-term (map (lambda (t) (mul t1 t)) (reverse L)) n)))
  (define (negate-dense-term L) (map negate L))
  ;; 外部とのインターフェース
  (define (tag x) (attach-tag 'dense-term x))
  (put '=zero-term? '(dense-term) =zero-dense-term?)
  (put 'add-terms '(dense-term dense-term)
       (lambda (x y) (tag (add-dense-terms x y))))
  (put 'mul-terms '(dense-term dense-term)
       (lambda (x y) (tag (mul-dense-terms x y))))
  (put 'negate-term '(dense-term)
       (lambda (x) (tag (negate-dense-term x))))
  (put 'make-from-sparse 'dense-term
       (lambda (sparse-term-list) (tag (sparse->dense sparse-term-list))))
  (put 'make-from-dense 'dense-term
       (lambda (dense-term-list) (tag dense-term-list)))
  'done)

(define (make-dense-term term-list)
  ((get 'make-from-dense 'dense-term) term-list))

(install-dense-term-package)

(define (dense->sparse term-list)
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (iter (cons (list i (car term-list)) result) (+ i 1) (cdr term-list))))
  (iter '() 0 (reverse term-list)))
(define (sparse->dense term-list)
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (let ((term (car term-list)))
             (let ((j (car term)))
                  (if (= i j)
                      (iter (cons (cadr term) result) (+ i 1) (cdr term-list))
                      (iter (cons 0 result) (+ i 1) term-list))))))
  (iter '() 0 (reverse term-list)))

(define (install-palynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=zero-poly? p) (=zero-term? (term-list p)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  ;; 外部とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-palynomial-package)


(define (add-terms x y) (apply-generic 'add-terms x y))
(define (mul-terms x y) (apply-generic 'mul-terms x y))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (if (= 1 (length args))
      (let ((proc (get op type-tags)))
        (if proc
          (drop (apply proc (map contents args)))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))
      (let ((proc (get op type-tags)))
        (if proc
          (drop (apply proc (map contents args)))
          (if (equal-level? type-tags)
            (let ((proc2 (get op type-tags)))
              (if proc2
                (drop (apply proc2 (map contents args)))
                (error "No method for these types -- APPLY-GENERIC"
                       (list op type-tags))))
            (let ((max-level (max-type-level type-tags)))
              (apply apply-generic (cons op
                                         (map
                                           (lambda (x) (raise-level max-level x))
                                           args))))))))))

(define (drop x)
  ; boolean, scheme-number の場合は処理しない
  (if (not  (pair? x)) x
    (let ((tag (type-tag x)))
      (if (or (eq? tag 'sparse-term) (eq? tag 'dense-term)) x
        (if (= (type-level tag) 0) x
      (let ((project-n (project-number x)))
        (let ((raise-n (raise-number project-n)))
          (if (equ? x raise-n)
            (drop project-n)
            x))))))))

(define p1 (make-polynomial 'x (make-sparse-term '((1 2) (0 1)))))
(define p2 (make-polynomial 'x (make-dense-term '(-1 -1))))
(add p1 p1)
(add p2 p2)


(add p1 p2)
(add p2 p1)
(mul p1 p1)
(mul p2 p2)
(mul p1 p2)
(mul p2 p1)
