(load "./4.2.ss")

;ex4.25
;無限ループ

(driver-loop)

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
(display (factorial 5))

(define (try a b)
  (if (= a 0) 1 b))
(try 0 (/ 1 0))
end

;4.27

(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define w (id (id 10)))
count
w
count


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define w (id (id 10)))
count
w
count
end

;ex4.28
(driver-loop)
(define (foo x)
  (+ 1 x))
(define (hoge f)
  (f 1))
(hoge foo)
end

;ex4.29
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-value obj) (thunk-env obj))
      obj))


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))
(square (id 10))
count
end

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))
(square (id 10))
count
end

;ex4.30
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(driver-loop)
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
end

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(driver-loop)
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
(p1 1)
(p2 1)
end

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(driver-loop)
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
(p1 1)
(p2 1)
end

;ex4.32
;この定義ではconsのcarの部分も遅延評価する
;以前は (cons-stream <a> <b>) => (cons <a> (delay <b>))
;x が未定義の場合に (cons-stream x <b>)はできない.

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
    ((null? list2) list1)
    (else (cons (+ (car list1) (car list2))
                (add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))

(define integers (cons 1 (add-lists ones integers)))

(list-ref integers 17)

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)
                     int)))
  int)


(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

(list-ref (solve (lambda (x) x) 1 0.001) 1000)
end

(driver-loop)

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(define y (cons z (* x x)))
(define x 2)
(define z 3)
(display (car y))
(display (cdr y))
end

;ex4.33

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(car '(a b c))
end

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((while? exp) (eval (while->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (text-of-quotation exp env)
  (if (pair? (cadr exp))
      (eval (make-quotation-list (cadr exp)) env)
      (list 'quote (cadr exp))))

(define (make-quotation-list l)
  (cond ((null? l) '())
    ((pair? l)
         (list 'cons (list 'quote (car l))
                 (make-quotation-list (cdr l))))
        (else (list 'quote l))))

(make-quotation-list '(1 2 3))
(make-quotation-list '(1 . 2))

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(car '(a b c))
(cdr '(a b c))
(car '(a . b ))
(cdr '(a . b ))
'a
'1
end

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(car '((b c)))
(car (car '((b c))))
(car (car (cdr '(a
                  (b c)
                  )
               )))
end
