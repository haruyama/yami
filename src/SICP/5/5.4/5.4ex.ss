(load "./5.4.ss")
(start eceval)
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(a b c) '(d e f))
#q

(start eceval)
(if (eq? 0 0)
  0
  1)

(define a 0)
(eq? a 1)
(if (eq? a 0)
  0
  1)

(cond ((eq? 0 1) 1)
  ((eq? 0 2) 2)
  (else 0))
(define a 0)

(cond ((eq? a 1) 1)
  ((eq? a 2) 2)
  (else a))

(define (nyo a)
  (cond ((eq? a 1) 1)
    ((eq? a 2) 2)
    (else a)))
(nyo 1)
(nyo 2)
(nyo 3)

(define (nya)
  (let ((a 0))
    (cond ((eq? a 1) 1)
      ((eq? a 2) 2)
      (else a))))

(nya)
(define (nya)
  (let ((a 1))
    (cond ((eq? a 1) 1)
      ((eq? a 2) 2)
      (else a))))
(nya)
#q
