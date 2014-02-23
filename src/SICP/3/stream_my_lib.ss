(define true #t)
(define false #f)

;(define-syntax delay
;  (syntax-rules ()
;    ((_ exp) (lambda () exp))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;; define delay
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define the-empty-system '())

(define (stream-null? s) (null? s))

;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc . argstreams)
;  (if (stream-null? (car argstreams))
;      (apply proc (map stream-car argstreams))
;      (apply stream-map
;             (cons proc (map stream-cdr argstreams)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-system)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line s)
  (newline)
  (display s))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-take s n)
  (define (iter s i)
    (if (>= i n)
        'done
        (begin
          (display (stream-car s))
          (display " ")
          (iter (stream-cdr s) (+ i 1)))))
  (iter s 0))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-system
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (list->stream l)
  (if (pair? l)
      (cons-stream (car l)
                   (list->stream (cdr l)))
      the-empty-system))

