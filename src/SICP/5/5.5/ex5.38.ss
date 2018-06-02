(load "./5.5.ss")
(load "../5.4/5.4.ss")

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
(define (spread-arguments arg-list)
  (let ((cal (compile-arguments arg-list)))
    (preserving '(arg1)
                (car cal)
                (cadr cal))))

(spread-arguments '(b 1))
(spread-arguments '(a b))
(spread-arguments '((+ 1 2) b)) ; まだ オープンコードで扱っていない
