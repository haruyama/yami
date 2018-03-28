(load "./4.4.ss")
(load "./4.4_microshaft.ss")
;ex4.70
;問題文中の定義では無限ストリームになる

;ex4.71
;https://github.com/suzuken/sicp/blob/master/chapter4/q4.71.scm
;Louisの定義
;(query-driver-loop)
;(assert! (married Minnie Mickey))
;(assert! (rule (married ?x ?y)
               (married ?y ?x)))
;(married Mickey ?who)

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append (find-assertions query-pattern frame)
                     (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave
      (qeval (first-disjunct disjuncts) frame-stream)
      (disjoin (rest-disjuncts disjuncts) frame-stream))))


(query-driver-loop)
(married Mickey ?who)

(load "./4.4.ss")
(load "./4.4_microshaft.ss")


;ex4.72
;無限ストリームに対応するため.
;http://wat-aro.hatenablog.com/entry/2016/01/21/013942

;ex4.72,4.73
;無限ストリームに対応するため.

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

;ex4.76
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts)
                    frame-stream))))

;(query-driver-loop)
;(and (job ?x (computer programmer)) (address ?x ?w))
;end
(conjoin '((job (? x) (computer programmer)) (address (? x) (? w)))
         (singleton-stream '()))
(display-stream
  (conjoin '((job (? x) (computer programmer)) (address (? x) (? w)))
           (singleton-stream '())))

; not と lisp-value があるとだめ. ex4.77 で扱う
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (unify-frame-streams
      (stream-map
        (lambda (conjunct)
          (qeval conjunct frame-stream))
        (list->stream conjuncts)))))

(put 'and 'qeval conjoin)

(define (unify-frame-streams frame-streams)
  (cond 
    ((stream-null? (stream-cdr frame-streams)) (stream-car frame-streams))
    (else
      (let ((s1 (stream-car frame-streams))
            (s2 (stream-car (stream-cdr frame-streams))))
        (unify-frame-streams
          (cons-stream
            (unify-two-frame-streams s1 s2)
            (stream-cdr (stream-cdr frame-streams))))))))

;(define (unify-two-frame-streams s1 s2)
;  (cond
;    ((stream-null? s1) the-empty-stream)
;    ((stream-null? s2) the-empty-stream)
;    (else
;      (let ((f1 (stream-car s1)))
;        (stream-append-delayed
;          (unify-frame-frame-stream f1 s2)
;          (delay (unify-two-frame-streams (stream-cdr s1) s2)))))))

;(define (unify-frame-frame-stream f s)
;  (if (stream-null? s) the-empty-stream
;    (let ((unify-result (unify-frames f (stream-car s))))
;      (if (eq? unify-result 'failed)
;        (unify-frame-frame-stream f (stream-cdr s))
;        (cons-stream unify-result (unify-frame-frame-stream f (stream-cdr s)))))))

;(display-stream
;  (unify-frame-frame-stream '(((? x) Hacker Alyssa P))
;                            (list->stream '((((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P))))))

(define (unify-two-frame-streams s1 s2)
  (stream-flatmap (lambda (f1)
                    (stream-filter
                      (lambda (f) (not (eq? f 'failed)))
                      (stream-map
                        (lambda (f2)
                          (unify-frames f1 f2))
                        s2)))
                  s1))


;(define (unify-frames f1 f2)
;  (define (iter f acc)
;    (cond
;      ((eq? acc 'failed) 'failed)
;      ((null? f) acc)
;      (else
;        (let ((vv1 (car f)))
;          (let ((vv2 (assoc (car vv1) (cdr f))))
;            (if (or (eq? vv2 #f) (equal? vv1 vv2))
;              (if (eq? #f (assoc (car vv1) acc))
;                (iter (cdr f) (cons vv1 acc))
;                (iter (cdr f) acc))
;              (iter (cdr f) (unify-match (cdr vv1) (cdr vv2) acc)))
;            )))))
;  (iter (append f1 f2) '()))

(define (unify-frames f1 f2)
  (if (null? f1)
    f2
    (let ((var (caar f1))
          (val (cdar f1)))
      (let ((ex (extend-if-possible var val f2)))
        (if (eq? ex 'failed)
          'failed
          (unify-frames (cdr f1) ex))))))


(display-stream
  (conjoin '((job (? x) (computer programmer)) (address (? x) (? w)))
           (singleton-stream '())))

;(assoc '(? x)  '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P)))
;(assoc '(? y)  '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P)))

(unify-frames '(((? x) Hacker Alyssa P))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P))
             )

(unify-frames '(((? x) ? y))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P))
             )

(unify-frames '(((? x) Hacker Alyssa P))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) ? z))
             )

(unify-frames '(((? x) Hacker Anonymous))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P)))


;ex4.77
(define (separate-conjuncts conjuncts)
  (define (iter cs front back)
    (if (null? cs) (cons front back)
        (let ((first (first-conjunct cs))
              (rest (rest-conjuncts cs)))
          (if (or (eq? 'not (type first)) (eq? 'lisp-value (type first)))
            (iter rest front (append back (list first)))
            (iter rest (append front (list first)) back)))))
  (iter conjuncts '() '()))


;(define (conjoin conjuncts frame-stream)
;  (if (empty-conjunction? conjuncts)
;    frame-stream
;    (let ((scs (separate-conjuncts conjuncts)))
;      (let ((front-frame-stream
;              (if (not (null? (car scs)))
;                (unify-frame-streams
;                  (stream-map
;                    (lambda (conjunct) (qeval conjunct frame-stream))
;                    (list->stream (car scs))))
;                frame-stream)))
;        (if (not (null? (cdr scs)))
;          (unify-frame-streams
;            (stream-map
;              (lambda (conjunct) (qeval conjunct front-frame-stream))
;              (list->stream (cdr scs))))
;          front-frame-stream)
;        )
;      )))

; フィルターする部分は並列に扱ってあとでマージするのはよくなさそう
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (let ((scs (separate-conjuncts conjuncts)))
      (let ((front-frame-stream
              (if (not (null? (car scs)))
                (unify-frame-streams
                  (stream-map
                    (lambda (conjunct) (qeval conjunct frame-stream))
                    (list->stream (car scs))))
                frame-stream)))
        (if (not (null? (cdr scs)))
          (original-conjoin (cdr scs) front-frame-stream)
          front-frame-stream)
        ))))

(define (original-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (original-conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts)
                    frame-stream))))

(put 'and 'qeval conjoin)

(display-stream
  (conjoin '((job (? x) (computer programmer)) (address (? x) (? w)))
           (singleton-stream '())))


(display-stream
  (conjoin '((salary (? person) (? amount)) (lisp-value > (? amount) 30000))
           (singleton-stream '())))

(display-stream
  (conjoin '((lisp-value > (? amount) 30000) (salary (? person) (? amount)))
           (singleton-stream '())))

;ex 4.79
; http://sioramen.sub.jp/blog/2010/01/sicp-4445-ex-479.html
; http://community.schemewiki.org/?sicp-ex-4.79
