(load "./4.4.ss")
;(load "./4.4_microshaft.ss")
;ex4.70
;問題文中の定義では無限ストリームになる

;ex4.71
;無限ループの場合に結果を表示できなくなる場合がある
;https://github.com/suzuken/sicp/blob/master/chapter4/q4.71.scm
;Louisの定義

;ex4.72
;http://wat-aro.hatenablog.com/entry/2016/01/21/013942
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append
        (find-assertions query-pattern frame)
        (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
        (qeval (first-disjunct disjuncts) frame-stream)
        (disjoin (rest-disjuncts disjuncts)
                 frame-stream))))

(query-driver-loop)
(assert! (married Minnie Mickery))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))

(married Mickery ?x)
end


(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
        (qeval (first-disjunct disjuncts) frame-stream)
        (delay (disjoin (rest-disjuncts disjuncts)
                        frame-stream)))))
;; (query-driver-loop)
;; (married Mickery ?x)
;; end

;ex4.72,4.73
;無限のストリームに対応するため.

; ex4.73
;(define (flatten-stream stream)
;  (if (stream-null? stream)
;    the-empty-stream
;    (interleave
;      (stream-car stream)
;      (flatten-stream (stream-cdr stream)))))

;(define (flatten-stream stream)
;  (if (stream-null? stream)
;      the-empty-stream
;      (interleave-delayed
;        (stream-car stream)
;        (delay (flatten-stream (stream-cdr stream))))))



;ex4.74
;a
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map
    stream-car
    (stream-filter
      (lambda (f) (not (stream-null? f)))
      stream)))

;; (define (stream-flatmap proc s)
;;   (flatten-stream (stream-map proc s)))

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave-delayed
;;        (stream-car stream)
;;        (delay (flatten-stream (stream-cdr stream))))))

;b
;空ストリームと単一のストリームしかこないので, 影響しない.

;ex4.75
(query-driver-loop)
(job ?x (computer wizard))
(job ?x (computer programmer))
(and (job ?x ?j) (job ?anyone ?j))
end

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (uniqued-query operands)
                           (singleton-stream frame))))
        (if (and (not (stream-null? result))
                 (stream-null? (stream-cdr result)))
            result
            the-empty-stream)))
    frame-stream))

(define (uniqued-query exps) (car exps))

(put 'unique 'qeval uniquely-asserted)


(query-driver-loop)
(unique (job ?x (computer wizard)))
(unique (job ?x (computer programmer)))

(and (job ?x ?j) (job ?anyone ?j))
(and (job ?x ?j) (unique (job ?anyone ?j)))
end

(query-driver-loop)
(and (job ?x ?j)
     (unique (supervisor ?anyone ?x)))
end
