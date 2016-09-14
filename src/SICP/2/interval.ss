(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))


(define (lower-bound x)  (car x))

(define (upper-bound x)  (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(sub-interval (make-interval -1 2) (make-interval -1 5))


;ex 2.11
; http://www.serendip.ws/archives/553
(define (mul-interval-2.11 x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
       (cond ((> lbx 0) ; x が正
              (cond ((> lby 0) ; y が正
                     (make-interval (* lbx lby)
                                    (* ubx uby)))
                    ((< uby 0) ; y が負
                     (make-interval (* ubx lby)
                                    (* lbx uby)))
                    (else ; y が零を跨る
                      (make-interval (* ubx lby)
                                     (* ubx uby)))))
             ((< ubx 0) ; x が負
              (cond ((> lby 0) ; y が正
                     (make-interval (* lbx uby)
                                    (* ubx lby)))
                    ((< uby 0) ; y が負
                     (make-interval (* ubx uby)
                                    (* lbx lby)))
                    (else ; y が零を跨る
                      (make-interval (* lbx uby)
                                     (* lbx lby)))))
             (else ; x が零を跨る
               (cond ((> lby 0) ; y が正
                      (make-interval (* lbx uby)
                                     (* ubx uby)))
                     ((< uby 0) ; y が負
                      (make-interval (* ubx lby)
                                     (* lbx lby)))
                     (else ; y が零を跨る
                       (make-interval (min (* lbx uby) (* ubx lby))
                                      (max (* lbx lby) (* ubx uby)))))))))

(mul-interval (make-interval 1 2) (make-interval 1 5))
(mul-interval-2.11 (make-interval 1 2) (make-interval 1 5))

(mul-interval (make-interval 1 2) (make-interval -1 5))
(mul-interval-2.11 (make-interval 1 2) (make-interval -1 5))

(mul-interval (make-interval -1 2) (make-interval -1 5))
(mul-interval-2.11 (make-interval -1 2) (make-interval -1 5))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; ex2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define i (make-center-width 5 5))
(percent i)

(define i (make-center-percent 100.0 10))
(display i)
(percent i)


; ex2.13
; (x + px * x) * (y + py * y) =~ x*y + px*x*y +  py*y*x = x*y(1 + px + py)

(mul-interval (make-center-percent 100.0 1) (make-center-percent 100.0 2))
(mul-interval (make-center-percent 100.0 5) (make-center-percent 100.0 2))
(mul-interval (make-center-percent 100.0 5) (make-center-percent 100.0 10))
(mul-interval (make-center-percent 100.0 10) (make-center-percent 100.0 10))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one 
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 1005.0 1))
(display r1)
(newline)
(percent r1)

(define r2 (make-center-percent 300.0 0.2))
(par1 r1 r2)
(par2 r1 r2)
(par1 r1 r1)
(par2 r1 r1)

(div-interval r1 r1)
(div-interval r1 r2)
