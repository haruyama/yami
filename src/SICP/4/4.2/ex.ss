(load "./4.2.ss")

;ex4.25
;無限ループ

(driver-loop)
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
  (if (list? (cadr exp))
      (eval (make-quotation-list (cadr exp)) env)
      (cadr exp)))

(define (make-quotation-list l)
  (if (null? l)
      '()
      (let ((first-list (car l))
            (rest-list (cdr l)))
        (list 'cons (list 'quote first-list)
              (make-quotation-list rest-list)))))

(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(car '(a b c))
(cdr '(a b c))
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
