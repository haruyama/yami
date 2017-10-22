(use srfi-1)
(define (scan-out-defines body)
  (let ((defs (take-while definition? body))
        (rest (drop-while definition? body)))
    (if (not (null? (filter definition? rest)))
      (error "syntax error: no toplevel definition"))
    (if (null? defs)
      body
      (let ((vars (map definition-variable defs))
            (vals (map definition-value defs)))
        (append (map (lambda (x) (list 'define x ''*unassigned*)) vars)
                (map make-assignment vars vals)
                rest)))))

(scan-out-defines
  '((define u (display v)) (define v (+ 1 2)) (display "aaa")))

(scan-out-defines
  '((define u (display v)) (define v (+ 1 2)) (display "aaa") (define o 'a)))

(scan-out-defines
  '((define (even? n)
      (if (= n 0)
        true
        (odd? (- n 1))))
    (define (odd? n)
      (if (= n 0)
        false
        (even? (- n 1))))
    (even? x)
    ))
