(load "./5.5.ss")
(load "../5.4/5.4.ss")

(define all-regs '(env proc val argl continue arg1 arg2))

; 0 引数の場合
; =, - の場合 error

; 1 引数の場合
; = の場合は error
; (+ a)
; (assign arg1 (op lookup-variable-value) (const a) (reg env))
; (assign val (op +) (reg arg1))

; 2 引数の場合
; (+ a 1)
; (assign arg1 (op lookup-variable-value) (const a) (reg env))
; (assign arg2 (const 1))
; (assign val (op +) (reg arg1) (reg arg2))

; (+ a 1)
; (assign arg1 (op lookup-variable-value) (const a) (reg env))
; (assign arg2 (const 1))
; (assign val (op +) (reg arg1) (reg arg2))

; (+ (+ 1 2) 3)
; (assign arg1 (const 1))
; (assign arg2 (const 2))
; (assign arg1 (op +) (reg arg1) (reg arg2))
; (assign arg2 (const 3))
; (assign val (op +) (reg arg1) (reg arg2))

; (+ (+ 1 (+ 2 3) 4)
; (assign arg1 (const 1))
; (save arg1)
; (assign arg1 (const 2))
; (assign arg2 (const 3))
; (assign arg2 (op +) (reg arg1) (reg arg2))
; (restore arg1)
; (assign arg1 (op +) (reg arg1) (reg arg2))
; (assign arg2 (const 3))
; (assign val (op +) (reg arg1) (reg arg2))

; 3 引数の場合
; (+ a 1 b)
; 問題にはないが / のことを考えると左から処理するのが妥当
; (assign arg1 (op lookup-variable-value) (const a) (reg env))
; (assign arg2 (const 1))
; (assign arg1 (op +) (reg arg1) (reg arg2))
; (assign arg2 (op lookup-variable-value) (const b) (reg env))
; (assign arg1 (op +) (reg arg1) (reg arg2))

; d を見た感じ spread-arguments では2引数のみを考え
; 3 引数の場合は その上でこのようにするのが題意か

;; 一応3引数以上も意識
(define (compile-arguments arg-list)
  (define (iter al first)
    (cond 
      ((null? al) '())
      ((true? first) (cons (compile (car al) 'arg1 'next) (iter (cdr al) false)))
      (else
        (cons (compile (car al) 'arg2 'next) (iter (cdr al) false)))))
  (iter arg-list true))

(compile-arguments '(a b))
(compile-arguments '((+ 1 2) 3)) ; この時点では + を オープンコードで扱っていない
(compile-arguments '(a b 1))

; ここからは2引数前提としてみる
; いろいろやってみたが spread-arguments の中で preserving しようとするとうまくいかない
; compile-arguments のままにして b で preserving するほうがよさそう

;(define (spread-arguments arg-list)
;  (if (= 2 (length arg-list))
;    (let ((cal (compile-arguments arg-list)))
;      (append-instruction-sequences
;        (car cal)
;        (preserving
;          '(arg1 arg2) ; arg2 は不要かも
;          (cadr cal)
;          (make-instruction-sequence
;            '(arg1 arg2) '() '()))))
;    (error "ERROR: spread-arguments: length of arg-list is not 2: " (length arg-list))))

;(spread-arguments '(b 1))
;(spread-arguments '(a b))
;(spread-arguments '((+ 1 2) b)) ; まだ オープンコードで扱っていない
;(spread-arguments '(b (+ 1 2))) ; まだ オープンコードで扱っていない

(define (spread-arguments arg-list)
  (if (= 2 (length arg-list))
    (compile-arguments arg-list)
    (error "ERROR: spread-arguments: length of arg-list is not 2: " (length arg-list))))

(spread-arguments '(a b))
(spread-arguments '((+ 1 2) 3)) ; この時点では + を オープンコードで扱っていない
(spread-arguments '(a b 1))

;b
(define (open-code? exp)
  (memq (car exp) '(= * - +)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp)
     (compile-variable exp target linkage))
    ((assignment? exp)
     (compile-assignment exp target linkage))
    ((definition? exp)
     (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp)
     (compile-sequence (begin-actions exp)
                       target linkage))
    ((cond? exp) (compile (cond->if exp) target linkage))
    ((open-code? exp)
     (compile-open-code exp target linkage))
    ((application? exp)
     (compile-application exp target linkage))
    (else
      (error "Unknown expression type -- COMPILE" exp))))

(define (compile-open-code exp target linkage)
  (if (= (length exp) 3)
    (let ((proc (operator exp))
          (sas (spread-arguments (operands exp))))
      (end-with-linkage linkage
                        (append-instruction-sequences
                          (car sas)
                          (preserving
                            '(arg1) ; おそらく arg2 はいらない
                            (cadr sas)
                            (make-instruction-sequence
                              '(arg1 arg2)
                              (list target)
                              `((assign ,target (op ,proc) (reg arg1) (reg arg2))))))))
    (compile-application exp target linkage))) ; 3 項でなければ application として扱う

(print-after-compiler
  (compile '(+ (+ 1 2) a) 'val 'next)
  )

(print-after-compiler
  (compile '(+ b (+ 1 2)) 'val 'next)
  )

;c
(print-after-compiler
  (compile
    '(define (factorial n)
       (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    'val
    'next))

;d

(define (multi->2 exp)
  (if (> 4 (length exp))
    exp
    (let ((proc (car exp))
          (operands (cdr exp)))
      (cons proc (cons (first-operand operands) (list (multi->2 (cons proc (rest-operands operands)))))))))

(multi->2 '(+ 1 2))
(multi->2 '(+ 1 2 3))
(multi->2 '(+ 1 2 3 4))

(define (open-multi-code? exp)
  (memq (car exp) '(* +)))

(define (open-code? exp)
  (memq (car exp) '(= -)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp)
     (compile-variable exp target linkage))
    ((assignment? exp)
     (compile-assignment exp target linkage))
    ((definition? exp)
     (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp)
     (compile-sequence (begin-actions exp)
                       target linkage))
    ((cond? exp) (compile (cond->if exp) target linkage))
    ((open-multi-code? exp)
     (compile-open-multi-code exp target linkage))
    ((open-code? exp)
     (compile-open-code exp target linkage))
    ((application? exp)
     (compile-application exp target linkage))
    (else
      (error "Unknown expression type -- COMPILE" exp))))

(define (compile-open-multi-code exp target linkage)
  (compile-open-code (multi->2 exp) target linkage))

(print-after-compiler
  (compile '(+ b 1 2) 'val 'next)
  )

(print-after-compiler
  (compile '(+ b 1 2 a) 'val 'next)
  )
