(define (square x) (* x x))
(define pi (atan  0 -1.0))
(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else  (filter predicate (cdr sequence)))))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else
          (error  "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum)
         datum)
        (else
          (error  "Bad tagged datum -- TYPE-TAG" datum))))

(define true #t)
(define false #f)
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false ))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (raise-number x)
  ((get 'raise-number (type-tag x)) x))

(define (project-number x)
  ((get 'project-number (type-tag x)) x))

(define package-types '(scheme-number rational complex polynomial))

(define (type-level type)
  (define (type-level-sub type types level)
    (cond ((null? types) (error "no type" type))
          ((eq? type (car types)) level)
          (else
            (type-level-sub type (cdr types) (+ level 1)))))
  (type-level-sub type package-types 0))

(define (max-type-level types)
  (apply max (map type-level types)))

(define (equal-level? types)
  (apply = (map type-level types)))

(define (raise-level l arg)
  (if (= l (type-level (type-tag arg))) arg
      (raise-level l (raise-number arg))))

(define (drop x)
  ; boolean, scheme-number の場合は処理しない
  (if (not  (pair? x))
    x
    (if (= (type-level (type-tag x)) 0) x
      (let ((project-n (project-number x)))
        (let ((raise-n (raise-number project-n)))
          (if (equ? x raise-n)
            (drop project-n)
            x))))))

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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))

(define (install-scheme-number-package)

  (define (raise-number x)
    (make-rational (contents x) 1))

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

  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'raise-number 'scheme-number
       (lambda (x) (raise-number x)))
  'done)

(define (make-scheme-number n)
  ((get' make 'scheme-number) n))

(install-scheme-number-package)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (raise-number x)
    (make-complex-from-real-imag
      (/ (numer (contents x)) (denom (contents x))) 0))

  (define (project-number x)
    (make-scheme-number
      (numer (contents x))))

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

  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (- (numer x)) (denom x)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'raise-number 'rational
       (lambda (x) (raise-number x)))

  (put 'project-number 'rational
       (lambda (x) (project-number x)))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done )

(install-rectangular-package)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done )
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (raise-number z)
    (make-polynomial 'x (list (list  0 z))))

  ; 小数に対応するためのdirty hack
  (define (project-number z)
    (make-rational (round->exact (* (real-part z) 1000000000)) 1000000000))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'negate '(complex)
       (lambda (x) (tag (make-from-real-imag (- (real-part x)) (- (imag-part x))))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'raise-number 'complex
       (lambda (x) (raise-number x)))

  (put 'project-number 'complex
       (lambda (x) (project-number x)))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

(define (install-equ?-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)
         (= x y)))

  (put 'equ? '(rational rational)
       (lambda (x y)
         (and
           (= (car x) (car y))
           (= (cdr x) (cdr y)))))

  (put 'equ? '(complex complex)
       (lambda (x y)
         (and
           (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y)))))

  (put 'equ? '(scheme-number rational)
       (lambda (x y)
         (and
           (= x (car y))
           (= 1 (cdr y)))))


  (put 'equ? '(rational scheme-number)
       (lambda (y x)
         (and
           (= x (car y))
           (= 1 (cdr y)))))


  (put 'equ? '(scheme-number complex)
       (lambda (x y)
         (and
           (= x (real-part y))
           (= 0 (imag-part y)))))

  (put 'equ? '(complex scheme-number)
       (lambda (y x)
         (and
           (= x (real-part y))
           (= 0 (imag-part y)))))

  (put 'equ? '(rational complex)
       (lambda (x y)
         (and
           (= (/ (car x) (cdr x)) (real-part y))
           (= 1 (imag-part y)))))

  (put 'equ? '(complex rational)
       (lambda (y x)
         (and
           (= (/ (car x) (cdr x)) (real-part y))
           (= 0 (imag-part y)))))
  'done)


(define (equ? x y)
  (apply-generic 'equ? x y))

(install-equ?-package)

;(equ? 1 1)
;(equ? s1 1)
;(equ? 1 2)

;(equ? r1 r2)
;(equ? r1 r1)

;(equ? c1 c2)
;(equ? c2 c2)

;(equ? 2 r2)

;(define r3 (make-rational 4 2))
;(equ? 2 r3)
;(equ? r3 2)

;(equ? 1 c1)
;(equ? c1 1)
;(equ? 1 c2)

;(define c3 (make-complex-from-mag-ang 2 0))

;(equ? r3 c3)
;(equ? c3 r3)
;(equ? r3 c2)
;(equ? c2 r3)


(define (=zero? x)
  (equ? 0 x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
