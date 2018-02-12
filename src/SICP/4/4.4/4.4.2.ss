(load "./4.4.ss")
;(load "./4.4_microshaft.ss")
;ex4.70
;問題文中の定義では無限ストリームになる

;ex4.71
;無限ループの場合に結果を表示できなくなる場合がある
;https://github.com/suzuken/sicp/blob/master/chapter4/q4.71.scm
;Louisの定義

;ex4.72
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

;; (query-driver-loop)
;; (or (married Mickery ?x)
;;     (same Mickery ?x))
;; end

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave
;;        (stream-car stream)
;;        (flatten-stream (stream-cdr stream)))))


; ex4.73
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))



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
(and (supervisor ?x ?y)
     (unique (supervisor ?anyone ?y)))

(and (supervisor ?middle-manager ?person)
     (supervisor ?x ?middle-manager))

(and (supervisor ?middle-manager ?person)
     (unique (supervisor ?x ?middle-manager)))
end

;;;;
(use slib)
(require 'trace)

;;;
(query-syntax-process '(job ?x (computer programmer)))

(display-stream
  (qeval (query-syntax-process '(job ?x (computer programmer)))
         (singleton-stream '())))

;; (display-stream
;;  (simple-query (query-syntax-process '(job ?x (computer programmer)))
;;         (singleton-stream '())))


;; (define (instantiate exp frame unbound-var-handler)
;;   (define (copy exp)
;;     (cond ((var? exp)
;;            (let ((binding (binding-in-frame exp frame)))
;;              (if binding
;;                  (copy (binding-value binding))
;;                  (unbound-var-handler exp frame))))
;;           ((pair? exp)
;;            (cons (copy (car exp)) (copy (cdr exp))))
;;           (else exp)))
;;   (newline)
;;   (display "exp: ")
;;   (display exp)
;;   (newline)
;;   (display "frame: ")
;;   (display frame)
;;   (newline)
;;   (copy exp))
;(trace query-syntax-process)
(trace qeval)

(query-driver-loop)
(job ?x (computer programmer))
end

(untrace qeval)
(untrace query-syntax-process)




;; (define (instantiate exp frame unbound-var-handler)
;;   (define (copy exp)
;;     (cond ((var? exp)
;;            (let ((binding (binding-in-frame exp frame)))
;;              (if binding
;;                  (copy (binding-value binding))
;;                  (unbound-var-handler exp frame))))
;;           ((pair? exp)
;;            (cons (copy (car exp)) (copy (cdr exp))))
;;           (else exp)))
;;   (copy exp))

;(trace conjoin)

(query-driver-loop)
(and (job ?person (computer programmer))
     (address ?person ?where))

end

(untrace conjoin)

;4.4.4.3
;
(find-assertions (query-syntax-process '(job ?x (computer programmer)))
                 (singleton-stream '()))

(display-stream
  (find-assertions (query-syntax-process '(job ?x (computer programmer)))
                   (singleton-stream '())))


(display-stream
  (fetch-assertions (query-syntax-process '(job ?x (computer programmer)))
                    (singleton-stream '())))

(check-an-assertion '(job (Hacker Alyssa P) (computer programmer))
                    (query-syntax-process '(job ?x (computer programmer)))
                    (singleton-stream '()))

(check-an-assertion '(job (Bitdiddle Ben) (computer wizard))
                    (query-syntax-process '(job ?x (computer programmer)))
                    (singleton-stream '()))

(pattern-match
  (query-syntax-process '(job ?x (computer programmer)))
  '(job (Hacker Alyssa P) (computer programmer))
  (singleton-stream '()))

(pattern-match
  (query-syntax-process '(job ?x ?y))
  '(job (Hacker Alyssa P) (computer programmer))
  (singleton-stream '()))

(binding-in-frame '(? y)
                  (pattern-match
                    (query-syntax-process '(job ?x ?y))
                    '(job (Hacker Alyssa P) (computer programmer))
                    (singleton-stream '())))

;4.4.4.4

(query-driver-loop)
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))
(wheel ?p)
end

(fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(apply-rules (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
              (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(display-stream
  (stream-map
    (lambda (frame)
      (instantiate
        (query-syntax-process '(wheel ?p))
        frame
        (lambda (v f) (v))))
    (apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
                  (query-syntax-process '(wheel ?p)) (singleton-stream '()))))

(rename-variables-in
  (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))))

;(trace unify-match)
(query-driver-loop)
(wheel ?p)
end
;(untrace unify-match)

(unify-match 1 '(? x)
             (singleton-stream '()))

(unify-match '(? x) 1
             (singleton-stream '()))

(unify-match '(? x) '(? x)
             (singleton-stream '()))

;(query-syntax-process '(?x ?x))
;(?x ?x) (?y ?y)
(unify-match '((? x) (? x)) '((? y) (? y))
             (singleton-stream '()))

(unify-match '((? y) (? x)) '((? x) (? y))
             (singleton-stream '()))


(unify-match '(? x) 1
             (extend '(? x) 2
                     (singleton-stream '())))

(unify-match '(? x) 2
             (extend '(? x) 2
                     (singleton-stream '())))

(unify-match '((? x) (? y)) '((? y) 1)
             (extend '(? x) 2
                     (singleton-stream '())))

(depends-on? '((? x) (? x)) '(? x)
             (singleton-stream '()))
(depends-on? '(? x) '(? x)
             (singleton-stream '()))
(depends-on? '(? x) '(? y)
             (singleton-stream '()))
