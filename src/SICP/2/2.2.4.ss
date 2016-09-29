(use gl)
(use gl.glut)

(define nil ())

; http://d.hatena.ne.jp/awacio/20100612/1276351549

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

(define  (origin-frame frame)
  (car frame))

(define  (edge1-frame frame)
  (cadr frame))

(define  (edge2-frame frame)
  (caddr frame))

;(define f (make-frame (make-vect 10 20) (make-vect 30 40) (make-vect 50 60)))
;(origin-frame f)
;(edge1-frame f)
;(edge2-frame f)

;(define  (make-frame origin edge1 edge2)
;  (list origin (cons edge1 edge2)))

;(define  (origin-frame frame)
;  (car frame))

;(define  (edge1-frame frame)
;  (caadr frame))

;(define  (edge2-frame frame)
;  (cdadr frame))
;(define f (make-frame (make-vect 10 20) (make-vect 30 40) (make-vect 50 60)))
;(origin-frame f)
;(edge1-frame f)
;(edge2-frame f)

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (draw-line v1 v2)
  (define (t z)
    (- (* 2 z) 1))
  (gl-vertex (t (xcor-vect v1)) (t (ycor-vect v1)))
  (gl-vertex (t (xcor-vect v2)) (t (ycor-vect v2))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define diamond
  (segment->painter (list
                      (make-segment (make-vect 0.0 0.5)
                                    (make-vect 0.5 1.0))
                      (make-segment (make-vect 0.5 1.0)
                                    (make-vect 1.0 0.5))
                      (make-segment (make-vect 1.0 0.5)
                                    (make-vect 0.5 0.0))
                      (make-segment (make-vect 0.5 0.0)
                                    (make-vect 0.0 0.5)))))

(define frame  (make-frame  (make-vect 0 0)  (make-vect 1 0)  (make-vect 0 1)))

(define  (init)
  (gl-clear-color 1.0 1.0 1.0 1.0))

(define  (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color 0.0 0.0 0.0 0.0)
  (gl-begin GL_LINE_LOOP)

  (diamond frame)

  (gl-end)
  (gl-flush))

(define (main args)
  (glut-init args) 
  (glut-init-display-mode GLUT_RGBA)
  (glut-create-window  "Painter Line Test")
  (glut-display-func disp)
  (init)
  (glut-main-loop)
  0)
