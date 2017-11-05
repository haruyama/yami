(load "./4.1_2.ss")

(driver-loop)
(define (my-map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (my-map proc (cdr items)))))
(my-map (lambda (x) (* x x))  '(1 2 3))

(display (lambda (x) (* x x)))
(map (lambda (x) (* x x))  '(1 2 3))
end
