(load "./number.ss")

(load "./poly.ss")

(load "./ex2-87.ss")

(load "./ex2-91.ss")

(define package-types '(scheme-number rational complex polynomial-x polynomial-y))

(define (my-symbol-append . arg)
  (string->symbol
  (apply string-append (map symbol->string arg))))

(define (install-polynomial-package v)
  (let ((package (my-symbol-append 'polynomial '- v)))
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


    (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

    ;; 面倒なので適当
    (define (project-number p)
      (make-complex-from-real-imag 0 0))

    (define (tag p) (attach-tag package p))
    (put 'add (list package package)
         (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul (list package package)
         (lambda (p1 p2) (tag (mul-poly p1 p2))))

    (put 'make package
         (lambda (var terms) (tag (make-poly var terms))))

    (put 'project-number package
         (lambda (x) (project-number x)))

    (put 'equ? (list package package)
         (lambda (p1 p2)
           (and
             (same-variable? p1 p2)
             (equal-terms? (term-list p1) (term-list p2)))))
    )

'done)

(install-polynomial-package 'x)
(install-polynomial-package 'y)

(define (make-polynomial var terms)
  ((get 'make (my-symbol-append 'polynomial '- var)) var terms))

(put 'raise-number 'polynomial-x
     (lambda (x) (make-polynomial 'y (list (list 0 x)))))

(add
  (make-polynomial 'x '((1 1)))
  (make-polynomial 'x '((1 1))))

(add
  (make-polynomial 'y '((1 1)))
  (make-polynomial 'x '((1 1))))
