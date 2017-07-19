(load "./stream_my_lib.ss")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;ex3.77
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))) 
;(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;ex3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(stream-ref (solve-2nd 4 -4 0.001 1 2) 1000)

(stream-ref (solve-2nd 4 -4 0.001 1 2) 1000)
(stream-ref (solve-2nd 3 -2 0.001 2 3) 1000)
(stream-ref (solve-2nd 3 -2 0.001 2 3) 1000)
(stream-ref (solve-2nd 1 6 0.001 3 -1) 1000)
(stream-ref (solve-2nd 0 1 0.001 1 1) 1000)

;ex3.79
(define (solve-2nd-2 f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(stream-take (solve-2nd-2 (lambda (dy y) (+ (* 1 dy) (* 2 y) )) 0.001 1 0) 10)
(stream-ref (solve-2nd-2 (lambda (dy y) (+ (* 4 dy) (* -4 y))) 0.001 1 2) 1000)

;ex3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)

    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))

    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams
                  (scale-stream vC (/ 1 L))
                  (scale-stream iL (- (/ R L)))))
    (cons vC iL)
    ;(stream-map cons vC iL)
    ))

(stream-take
  (car 
    ((RLC 1 1 0.2 0.1) 10 0))
  50)

(stream-take
  (cdr
    ((RLC 1 1 0.2 0.1) 10 0))
  50)

(stream-take
  (cdr
    ((RLC 1 1 1 0.01) 1 0))
  500))
