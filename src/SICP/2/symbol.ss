;(a b c d)
;(23 45 17)
;((Norah 17)  (Molly 14)  (Anna 11)  (Lauren 11)  (Charlotte 8))

;(* (+ 23 45) (+ x 9))
;(define (fact n) (if  (= n 1) 1 (* n (fact (- n 1)))))
(define a 1)

(define b 2)

(list a b)

(list 'a 'b)

(list 'a b)

(car '(a b c))

(cdr '(a b c))

(define (mymemq item x)
  (cond ((null? x) #f)
    ((eq? item (car x)) x)
    (else (mymemq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sause) y apple pear))

(mymemq 'apple '(pear banana prune))
(mymemq 'apple '(x (apple sause) y apple pear))

;ex2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;ex2.54
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(eq? '(this is a list) '(this is a list))
(eq? '(this is a list) '(this (is a) list))
(eq? '(this) '(this))

(define (myequal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (number? a) (number? b))
         (= a b))
        ((and (pair? a) (pair? b) (myequal? (car a) (car b)))
         (myequal? (cdr a) (cdr b)))
        (else #f)))

(eq? '(is a) '(is a))

(equal? '(this a) '(this b))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this is a))
(equal? '(3 is a list) '(3 is a list))
(equal? 3 'a)
(equal? '(a) 'a)

(myequal? '(this a) '(this b))
(myequal? '(this is a list) '(this (is a) list))
(myequal? '(this is a list) '(this is a list))
(myequal? '(this is a list) '(this is a))
(myequal? '(3 is a list) '(3 is a list))
(myequal? 3 'a)
(myequal? '(a) 'a)

;ex2.55
''abracadabra
(car ''abracadabra)
(cdr ''abracadabra)
(car (quote (quote abracadabra)))

(eq? ''abracadabra (quote (quote abracadabra)))
(equal? ''abracadabra (quote (quote abracadabra)))

(car (list 'quote 'a))
(cdr ''abracadabra)
(display ''abracadabra)
(display '('a))
(car (car '('a)))

;2.3.2
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
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv
                           (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

;ex2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (make-exponentiation m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((=number? m1 1) 1)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list '** m1 m2))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv
                           (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                         (make-exponentiation
                           (base exp) (make-sum (exponent exp) -1))
                         (deriv (base exp) var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x (** x 3)) 'x)

(deriv '(* x (** x 3)) 'x)

;ex2.57
(define (augend s)
  (if (null? (cdddr s)) (caddr s)
      (cons '+ (cddr s))))

(augend '(+ 1 2))
(augend '(+ 1 2 3))
(augend '(+ 1 2 3 4))

(define (multiplicand s)
  (if (null? (cdddr s)) (caddr s)
      (cons '* (cddr s))))

(multiplicand '(* 1 2))
(multiplicand '(* 1 2 3))
(multiplicand '(* 1 2 3 4))

(deriv '(* x y (+ x 3)) 'x)
(deriv '(* x y (+ x 3 x)) 'x)


;ex2.58
;a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base exponentiation) (car exponentiation))


(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x + (3 * (x * (y + 2)))) 'x)

;b
;演算子の優先順位を持たせることで できるっぽい
;http://d.hatena.ne.jp/awacio/20100803/1280843906
(define (sum? x)
  (if (memq '+ x) #t #f))
(sum? '(x * y * z + 3 * x * (y + 2)))
(sum? '(x * y * z * 3 * x * (y + 2)))

(use srfi-1)

(define (addend x)
  (let ((ans (take-while (lambda (e) (not (eq? '+ e))) x)))
    (if (null? (cdr ans)) (car ans)
    ans)))

(addend '(x * y * z + 3 * x * (y + 2)))
(addend '(y + 3))

(define (augend x)
  (let ((ans (cdr (memq '+ x))))
    (if (null? (cdr ans))
      (car ans)
      ans)))

(augend '(x * y * z + 3 * x * (y + 2)))
(augend '(x * y * z + 3))
(augend '(y + 3))

; deriv では sum? が先にチェックされるので sum? のチェックはなくても動くはず
(define (product? x)
  (cond ((sum? x) #f)
    ((pair?  (memq '* x)) #t)
    (else #f)))

(product? '(x * y * z + 3 * x * (y + 2)))
(product? '(x * y * z * 3 * x * (y + 2)))

(define (multiplier p)
    (car p))

(multiplier '(x * y * z * 3 * x * (y + 2)))

(define (multiplicand p)
  (let ((ans (cdr (memq '* p))))
    (if (null? (cdr ans))
      (car ans)
      ans)))

(multiplicand '(x * y * z * 3 * x * (y + 2)))
(multiplicand '(x * (y + 2)))
(multiplicand '(x * 2))

; exponentiation を除いている
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv
                           (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))


(deriv '(x + (3 * ((x * x) + (x * (y + 2))))) 'x)
(deriv '(x * y * z + 3 * x * (y + 2)) 'x)
