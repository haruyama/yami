(load "./stream_my_lib.ss")

(define random-init 1)

(define (rand-update x)
  (modulo (+ (* 2423707 x) 7) 100000001))


(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(rand)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(stream-take random-numbers 10)

(define (map-succesive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-succesive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-succesive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                       random-numbers))


(stream-take cesaro-stream 10)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(stream-take pi 100)

;ex3.81
(define (rand-3-81 request-stream random-init)
  (if (stream-null? request-stream)
      the-empty-system
      (cond ((eq? 'generate (stream-car request-stream))
             (let ((new-number (rand-update random-init)))
               (cons-stream new-number
                            (rand-3-81 (stream-cdr request-stream) new-number))))

            ((number? (stream-car request-stream))
             (let ((new-number (rand-update (stream-car request-stream))))
               (cons-stream
                 new-number
                 (rand-3-81 (stream-cdr request-stream) new-number))))
            (else
              (display (stream-car request-stream))
              (error "aaa")))))

(define rs (cons-stream    'generate
                           (cons-stream 'generate
                                        (cons-stream 1
                                                     (cons-stream 'generate
                                                                  (cons-stream 'generate
                                                                               (cons-stream '100
                                                                                            (cons-stream 'generate
                                                                                                         the-empty-system))))))))

(define rs2 (list->stream '(generate generate 1 generate generate 100 generate)))
(display-stream (rand-3-81 rs 1))
(display-stream (rand-3-81 rs2 1))

(define (random)
  (let ((a (rand))
        (b (rand)))
    (if (> a b)
        (/ (* 1.0 b) a)
        (/ (* 1.0 a) b))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (random-stream-in-range a b)
  (cons-stream
    (random-in-range a b)
    (random-stream-in-range a b)))

(stream-take (random-stream-in-range 0 10) 10)

(define (square n) (* n n))
(define (circle-P-stream x1 x2 y1 y2)
  (define (circle-experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2))
          (cx (/ (+ x1 x2) 2))
          (cy (/ (+ y1 y2) 2))
          (r (/ (- x2 x1) 2)))
      (>= (square r)
          (+ (square (- x cx))
             (square (- y cy))))))
  (cons-stream
    (circle-experiment)
    (circle-P-stream x1 x2 y1 y2)))

(stream-take (circle-P-stream 2 8 4 10) 100)
(stream-take
  (monte-carlo (circle-P-stream 2 8 4 10) 0 0) 100)

(define (estimate-integral P x1 x2 y1 y2)
  (if (not (= (- x2 x1) (- y2 y1))) (error "not square")
      (scale-stream
        (monte-carlo (P x1 x2 y1 y2) 0 0)
        (* 1.0 (- x2 x1)  (- y2 y1)))))


(stream-take (estimate-integral circle-P-stream 2 8 4 10) 100)
(stream-take (estimate-integral circle-P-stream -1 1 -1 1) 100)
;(stream-take (estimate-integral circle-P-stream -1 1 -1 1) 1000)


(define nyo
  (stream-map cons random-numbers
              random-numbers))

(stream-take nyo 10)
