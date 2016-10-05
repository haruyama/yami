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

;ex2.48
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

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;ex2.49
(define rim
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0)
                                    (make-vect 0.0 1.0))
                      (make-segment (make-vect 0.0 1.0)
                                    (make-vect 1.0 1.0))
                      (make-segment (make-vect 1.0 1.0)
                                    (make-vect 1.0 0.0))
                      (make-segment (make-vect 1.0 0.0)
                                    (make-vect 0.0 0.0)))))

(define rimx
  (segments->painter (list
                      (make-segment (make-vect 0.1 0.1)
                                    (make-vect 0.1 0.9))
                      (make-segment (make-vect 0.1 0.9)
                                    (make-vect 0.9 0.9))
                      (make-segment (make-vect 0.9 0.9)
                                    (make-vect 0.9 0.1))
                      (make-segment (make-vect 0.9 0.1)
                                    (make-vect 0.1 0.1)))))

(define batu
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0)
                                    (make-vect 1.0 1.0))
                      (make-segment (make-vect 1.0 0.0)
                                    (make-vect 0.0 1.0)))))

(define diamond
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.5)
                                    (make-vect 0.5 1.0))
                      (make-segment (make-vect 0.5 1.0)
                                    (make-vect 1.0 0.5))
                      (make-segment (make-vect 1.0 0.5)
                                    (make-vect 0.5 0.0))
                      (make-segment (make-vect 0.5 0.0)
                                    (make-vect 0.0 0.5)))))

;http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html 
(define wave-segments
  (list
    (make-segment
      (make-vect 0.006 0.840)
      (make-vect 0.155 0.591))
    (make-segment
      (make-vect 0.006 0.635)
      (make-vect 0.155 0.392))
    (make-segment
      (make-vect 0.304 0.646)
      (make-vect 0.155 0.591))
    (make-segment
      (make-vect 0.298 0.591)
      (make-vect 0.155 0.392))
    (make-segment
      (make-vect 0.304 0.646)
      (make-vect 0.403 0.646))
    (make-segment
      (make-vect 0.298 0.591)
      (make-vect 0.354 0.492))
    (make-segment
      (make-vect 0.403 0.646)
      (make-vect 0.348 0.845))
    (make-segment
      (make-vect 0.354 0.492)
      (make-vect 0.249 0.000))
    (make-segment
      (make-vect 0.403 0.000)
      (make-vect 0.502 0.293))
    (make-segment
      (make-vect 0.502 0.293)
      (make-vect 0.602 0.000))
    (make-segment
      (make-vect 0.348 0.845)
      (make-vect 0.403 0.999))
    (make-segment
      (make-vect 0.602 0.999)
      (make-vect 0.652 0.845))
    (make-segment
      (make-vect 0.652 0.845)
      (make-vect 0.602 0.646))
    (make-segment
      (make-vect 0.602 0.646)
      (make-vect 0.751 0.646))
    (make-segment
      (make-vect 0.751 0.646)
      (make-vect 0.999 0.343))
    (make-segment
      (make-vect 0.751 0.000)
      (make-vect 0.597 0.442))
    (make-segment
      (make-vect 0.597 0.442)
      (make-vect 0.999 0.144))))

(define wave  (segments->painter wave-segments))

(define frame  (make-frame  (make-vect 0 0)  (make-vect 1 0)  (make-vect 0 1)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-invards painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0))) 
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;ex2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;ex2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point
                               ))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;(define (below painter1 painter2)
;  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;(define (below painter1 painter2) 
;  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))


(define wave2 (beside wave (flip-vert wave)))
;(define wave2 (beside wave (rotate270 wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0) painter 
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;ex2.44
(define (up-split painter n)
  (if (= n 0) painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                 identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;ex2.45
(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0) painter
      (let ((smaller ((split proc1 proc2) painter (- n 1))))
        (proc1 painter (proc2 smaller smaller))))))

;(define right-split  (split beside below))
;(define up-split  (split below beside))


(define  (init)
  (gl-clear-color 1.0 1.0 1.0 1.0))

(define  (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color 0.0 0.0 0.0 0.0)
  (gl-begin GL_LINES)

  ;(rim frame)
  ;(rimx frame)
  ;(batu frame)
  ;(diamond frame)
  ;(wave frame)
  ;(wave2 frame)
  ;(wave4 frame)
  ;((right-split wave 4) frame)
  ;((corner-split wave 4) frame)
  ((square-limit wave 4) frame)

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
