;(make-from-real-imag (real-part z) (imag-part z))
;(make-from-mag-ang (magnitude z) (angle z))
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

(define (square x) (* x x))
(define pi (atan  0 -1.0))

;; ;Ben
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
   (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

(define z1 (make-from-real-imag 1 -1))
(define z2 (make-from-mag-ang 1 (/ pi 4)))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)

(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)

(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)

(magnitude (add-complex z1 z2))
(magnitude (sub-complex z1 z2))
(magnitude (mul-complex z1 z2))
(magnitude (div-complex z1 z2))

;; ;Alyssa
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))

;2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error  "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error  "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;Ben-2
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;Alyssa-2
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(make-from-real-imag 1.0 0.0)
(make-from-mag-ang 1.0 (/ pi 2))

(add-complex
  (make-from-real-imag 0.0 0.0)
  (make-from-mag-ang 1.0 (/ pi 2)))

(mul-complex
  (make-from-real-imag 1.0 0.0)
  (make-from-real-imag 0.0 1.0))

; 2.4.3
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define r (make-from-real-imag 0 1))
(define p (make-from-mag-ang 1 0))

(real-part r)
(real-part p)

(add-complex r p)
(sub-complex r p)
(mul-complex r p)
(div-complex r p)
;2.73
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)  (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(operator '(+ x y))
(operands '(+ x y))

; b

(define (deriv-sum operands var)
  (make-sum (deriv (car operands) var)
            (deriv (cadr operands) var)))

(put 'deriv '+ deriv-sum)


(deriv '(+ x y) 'x)
(deriv '(+ x y) 'y)
(deriv '(+ x (+ x y)) 'x)
(deriv '(+ x x y) 'x)

;(deriv '(* x x) 'x)

(define (deriv-mul operands var)
  (make-sum
    (make-product (car operands)
                  (deriv
                    (cadr operands) var))
    (make-product (deriv (car operands) var)
                  (cadr operands))))



(put 'deriv '* deriv-mul)

(deriv '(* x y) 'x)
(deriv '(* x x) 'x)

(deriv '(+ x (* y x)) 'x)

; c
(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-exponentiation m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((=number? m1 1) 1)
        (else (list '** m1 m2))))

(define (deriv-exp operands var)
  (make-product (cadr operands)
                (make-product
                  (make-exponentiation
                    (car operands) (make-sum (cadr operands) -1)
                    )
                  (deriv (car operands) var))))

(put 'deriv '** deriv-exp)
(deriv '(** x y) 'x)
(deriv '(** x 2) 'x)

; d
(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))

(put '+ 'deriv  deriv-sum)
(put '* 'deriv  deriv-mul)
(put '** 'deriv  deriv-exp)


(deriv '(+ x y) 'x)
(deriv '(+ x y) 'y)
(deriv '(* x y) 'x)
(deriv '(* x x) 'x)
(deriv '(+ x (* y x)) 'x)
(deriv '(** x y) 'x)
(deriv '(** x 2) 'x)


(deriv2 '(+ x y) 'x)
(deriv2 '(+ x y) 'y)
(deriv2 '(* x y) 'x)
(deriv2 '(* x x) 'x)
(deriv2 '(+ x (* y x)) 'x)
(deriv2 '(** x y) 'x)
(deriv2 '(** x 2) 'x)
;2.74
;('pex (address name define))
(define (install-pex-package)
  (define (get-name record)
    (cadr record))
  (define (get-address record)
    (car record))
  (define (get-salary record)
    (caddr record))
  (define (make-record name address salary)
    (list address name salary))

  (define (tag records) (attach-tag 'pex records))
  (define (get-record name records)
    (cond ((null? records) #f)
      ((eq? name (get-name (car records))) (car records))
      (else 
        (get-record name (cdr records)))))

  (put 'get-name 'pex get-name)
  (put 'get-address 'pex get-address)
  (put 'get-salary 'pex get-salary)

  (put 'make-record 'pex make-record)
  (put 'attach-tag 'pex tag)
  (put 'get-record 'pex get-record)
  )

(install-pex-package)

(define pex-records
  ((get 'attach-tag 'pex)
   (let ((make-record (get 'make-record 'pex)))
     (list
       (make-record 's-tanno 'mansion 100000000)
       (make-record 'taro    'koya    100)
       ))))
pex-records


;('ecnavi name (address salary))
(define (install-ecnavi-package)
  (define (get-name record)
    (car record))
  (define (get-address record)
    (caadr record))
  (define (get-salary record)	(cadadr record))
  (define (make-record name address salary)
    (list name (list address salary)))

  (define (tag record) (attach-tag 'ecnavi record))

  (define (get-record name records)
    (cond ((null? records) #f)
      ((eq? name (get-name (car records))) (car records))
      (else 
        (get-record name (cdr records)))))


  (put 'get-name 'ecnavi get-name)
  (put 'get-address 'ecnavi get-address)
  (put 'get-salary 'ecnavi get-salary)

  (put 'make-record 'ecnavi make-record)
  (put 'attach-tag 'ecnavi tag)
  (put 'get-record 'ecnavi get-record)
  )

(install-ecnavi-package)

(define ecnavi-records
  ((get 'attach-tag 'ecnavi)
   (let ((make-record (get 'make-record 'ecnavi)))
     (list
       (make-record 'haruyama 'underground 1)
       ))))
ecnavi-records

; a
(define (get-record name tagged-records)
  (let ((office (type-tag tagged-records))
        (records (cdr tagged-records)))
    ((get 'get-record office) name records)))

(get-record 'stanno pex-records)
(get-record 's-tanno pex-records)
(get-record 'haruyama ecnavi-records)


; b
(define  (get-salary name tagged-records)
  (let ((record (get-record name tagged-records)))
    (if record ((get 'get-salary (type-tag tagged-records)) record)
      #f
      )))


(get-salary 's-tanno pex-records)
(get-salary 'stanno pex-records)
(get-salary 'haruyama ecnavi-records)

(define (find-employee-record name all-tagged-records)
  (if (null? all-tagged-records) #f
    (let ((tagged-records (car all-tagged-records)))
      (let ((record (get-record name tagged-records)))
        (if record record
          (find-employee-record name (cdr all-tagged-records)))))))

(find-employee-record 's-tanno (list pex-records ecnavi-records))
(find-employee-record 'stanno (list pex-records ecnavi-records))
(find-employee-record 'haruyama (list pex-records ecnavi-records))

; d
; パッケージ追加

;p109 message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

((make-from-real-imag 1 0) 'real-part)
((make-from-real-imag 1 0) 'imag-part)
((make-from-real-imag 1 1) 'magnitude)


; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


((make-from-mag-ang 1 1) 'real-part)
((make-from-mag-ang 1 3.1) 'imag-part)
((make-from-mag-ang 1 1.5) 'imag-part)
((make-from-mag-ang 1 1) 'magnitude)

(real-part (make-from-mag-ang 1 1))
