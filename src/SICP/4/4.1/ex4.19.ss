(load "./4.1_2.ss")

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

(driver-loop)
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
end

(define (make-assignment var val)
  (list 'set! var val))

(use srfi-1)
(define (scan-out-defines body)
  (let ((defs (take-while definition? body))
        (rest (drop-while definition? body)))
    (if (null? defs)
      body
      (let ((vars (map definition-variable defs))
            (vals (map definition-value defs)))
        (list (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
                        (append (map make-assignment vars vals)
                                rest)))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-body p) (caddr p))

(driver-loop)
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
end
;error

;http://practical-scheme.net/wiliki/wiliki.cgi?Scheme%3A%E5%86%85%E9%83%A8define%E3%81%AE%E8%A9%95%E4%BE%A1%E9%A0%86
