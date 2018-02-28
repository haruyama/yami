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
                 '())

(display-stream
  (find-assertions (query-syntax-process '(job ?x (computer programmer)))
                   '()))

(display-stream
  (fetch-assertions (query-syntax-process '(job ?x (computer programmer)))
                    '()))

(check-an-assertion '(job (Hacker Alyssa P) (computer programmer))
                    (query-syntax-process '(job ?x (computer programmer)))
                    '())

(check-an-assertion '(job (Bitdiddle Ben) (computer wizard))
                    (query-syntax-process '(job ?x (computer programmer)))
                    '())

(pattern-match
  (query-syntax-process '(job ?x (computer programmer)))
  '(job (Hacker Alyssa P) (computer programmer))
  '())

(pattern-match
  (query-syntax-process '(job ?x (computer programmer)))
  '(job (Hacker Alyssa P) (computer programmer))
  '(((? x) Hacker Anonymous)))

(pattern-match
  (query-syntax-process '(job ?x (computer programmer)))
  '(job (Hacker Alyssa P) (computer programmer))
  '())
(pattern-match
  (query-syntax-process '(job ?x (computer programmer)))
  '(job (Hacker Alyssa P) (computer programmer))
  '())

(pattern-match
  (query-syntax-process '(job ?x ?y))
  '(job (Hacker Alyssa P) (computer programmer))
  '())

(binding-in-frame '(? y)
                  (pattern-match
                    (query-syntax-process '(job ?x ?y))
                    '(job (Hacker Alyssa P) (computer programmer))
                    '()))

;4.4.4.4

(query-driver-loop)
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))
(wheel ?p)
end

(fetch-rules  (query-syntax-process '(wheel ?p)) '())

(apply-rules (query-syntax-process '(wheel ?p)) '())

(apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) '()))
              (query-syntax-process '(wheel ?p)) '())

(display-stream
  (stream-map
    (lambda (frame)
      (instantiate
        (query-syntax-process '(wheel ?p))
        frame
        (lambda (v f) (v))))
    (apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) '()))
                  (query-syntax-process '(wheel ?p)) '())))

(rename-variables-in
  (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) '())))

;(trace unify-match)
(query-driver-loop)
(wheel ?p)
end
;(untrace unify-match)

(unify-match '(Hacker) '(? x)
             '())

(unify-match '(? x) 1
             '())

(unify-match '(? x) '(? x)
             '())

(unify-match '(? x) '(? y)
             '())

;(query-syntax-process '(?x ?x))
;(?x ?x) (?y ?y)
(unify-match '((? x) (? x)) '((? y) (? y))
             '())

(unify-match '((? y) (? x)) '((? x) (? y))
             '())

(unify-match '(((? x) Hacker Alyssa P))
             '(((? w) Cambridge (Mass Ave) 78) ((? x) Hacker Alyssa P))
             '())


(unify-match '(? x) 1
             (extend '(? x) 2
                     '()))

(unify-match '(? x) 1
             (extend '(? y) 2
                     '()))

(unify-match '(? x) 2
             (extend '(? x) 2
                     '()))

(unify-match '((? x) (? y)) '((? y) 1)
             (extend '(? x) 2
                     '()))

(unify-match '((? x) (? y)) '((? y) 1)
             (extend '(? x) 1
                     '()))

(unify-match '((? x)) '((+ 1 2))
             (extend '(? x) '(+ 1 (? y))
                     '()))

(unify-match '((? x)) '((+ 2 2))
             (extend '(? x) '(+ 1 (? y))
                     '()))

(depends-on? '((? x) (? x)) '(? x)
             '())
(depends-on? '(? x) '(? x)
             '())
(depends-on? '(? x) '(? y)
             '())
