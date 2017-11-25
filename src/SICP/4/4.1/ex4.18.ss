(use srfi-1)
(define (scan-out-defines body)
  ; global な make-let とは違う
  (define (make-let-internal binds body)
    (cons 'let (cons binds body)))
  (let ((defs (take-while definition? body))
        (rest (drop-while definition? body)))
    (if (not (null? (filter definition? rest)))
      (error "syntax error: no toplevel definition"))
    (if (null? defs)
      body
      (let ((vars (map definition-variable defs))
            (vals (map definition-value defs)))
        (list (make-let-internal (map (lambda (x) (list x ''*unassigned*)) vars)
                         (append (map make-assignment vars vals)
                                 rest)))))))

(scan-out-defines
  '((define y (integral (delay dy) y0 dt)) (define dy (stream-map f y)) y))

;((let ((y '*unassigned*)
;       (dy '*unassigned*))
;   (set! y (integral (delay dy) y0 dt))
;   (set! dy (stream-map f y))
;   y))

;((let ((y '*unassigned*)
;       (dy '*unassigned*))
;   (let ((a (integral (delay dy) y0 dt))
;         (b (stream-map f y)))
;     (set! y a)
;     (set! dy b))
;   y))
;
;         (b (stream-map f y))) で y の準備ができてない
