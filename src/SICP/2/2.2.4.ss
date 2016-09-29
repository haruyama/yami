(use gl)
(use gl.glut)

(define nil ())

; ex2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))


(define (ycor-vect v)
  (cdr v))

(define (op-vect op v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (op x1 x2) (op y1 y2))))

(define (add-vect v1 v2)
  (op-vect + v1 v2))

(define (sub-vect v1 v2)
  (op-vect - v1 v2))

(define (scale-vect s v)
  (op-vect * (make-vect s s) v))

;(add-vect (make-vect 1 2) (make-vect 3 4))
;(sub-vect (make-vect 1 2) (make-vect 3 4))
;(scale-vect 5 (make-vect 3 4))

; ex2.47
(define  (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define  (frame-origin frame)
  (car frame))

(define  (frame-edge1 frame)
  (cadr frame))

(define  (frame-edge2 frame)
  (caddr frame))

(define f (make-frame (make-vect 10 20) (make-vect 30 40) (make-vect 50 60)))
(frame-origin f)
(frame-edge1 f)
(frame-edge2 f)


;(define  (make-frame origin edge1 edge2)
;  (list origin (cons edge1 edge2)))

;(define  (frame-origin frame)
;  (car frame))

;(define  (frame-edge1 frame)
;  (caadr frame))

;(define  (frame-edge2 frame)
;  (cdadr frame))
;(define f (make-frame (make-vect 10 20) (make-vect 30 40) (make-vect 50 60)))
;(frame-origin f)
;(frame-edge1 f)
;(frame-edge2 f)

