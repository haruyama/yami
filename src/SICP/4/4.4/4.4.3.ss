(load "./4.4.ss")
(load "./4.4_microshaft.ss")

;;;;
(use slib)
(require 'trace)

;;;
(query-syntax-process '(job ?x (computer programmer)))

(display-stream
  (qeval (query-syntax-process '(job ?x (computer programmer)))
         (singleton-stream '())))

(trace query-syntax-process)
(trace qeval)

(query-driver-loop)
(job ?x (computer programmer))
end

(untrace qeval)
(untrace query-syntax-process)

(trace conjoin)

(query-driver-loop)
(and (job ?person (computer programmer))
     (address ?person ?where))
end

(untrace conjoin)

(display-stream
  (conjoin '((job (? x) (computer programmer)) (address (? x) (? w)))
           (singleton-stream '())))

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

(unify-match '(((? x) Hacker Alyssa P))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P))
             (singleton-stream '()))


(unify-match '(? x) 1
             (extend '(? x) 2
                     (singleton-stream '())))

(unify-match '(? x) 1
             (extend '(? y) 2
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
