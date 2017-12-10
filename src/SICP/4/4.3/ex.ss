(load "./4.3.ss")

(driver-loop)
(list (amb 1 2 3) (amb 'a 'b))
try-again
try-again
try-again
try-again
try-again

try-again
end

(driver-loop)
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
(an-integer-starting-from 2)
try-again
try-again
try-again
end

(driver-loop)
(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))


(prime-sum-pair '(1 3 5 8) '(20 35 110))

try-again
try-again
try-again

(prime-sum-pair '(19 27 30) '(11 36 58))
try-again
try-again

end

(driver-loop)
(let ((a (an-integer-starting-from 10)))
  (require (prime? a))
  a)
try-again
try-again
end


;ex4.35-36
(driver-loop)
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 3 13)
try-again
try-again
try-again

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 5)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between 1 k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple)
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
end
;ex4.37
;kについて探索しないでよい

(driver-loop)

(define (distinct? items)
  (cond ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
try-again
end

;ex4.38
(driver-loop)
(define (multiple-dwelling2)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;(require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling2)
try-again
try-again
try-again
try-again
try-again

end

;ex4.39
;https://wizardbook.wordpress.com/2011/01/12/exercise-4-39/

;ex4.40
(driver-loop)
(define (multiple-dwelling3)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((baker (amb 1 2 3 4 5))   ;手抜き
            (miller (amb 1 2 3 4 5))
            (smith (amb 1 2 3 4 5)))
        (require (not (= baker 5)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require
          (distinct? (list baker cooper fletcher miller smith)))

        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

(multiple-dwelling3)
try-again

end

;ex4.41

(define (distinct? items)
  (cond ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))


(define (multiple-dwelling4-condition items)
  (let ((baker (car items))
        (cooper (cadr items))
        (fletcher (caddr items))
        (miller (cadddr items))
        (smith  (car (cddddr items))))
    (cond ((= fletcher 5) false)
      ((= fletcher 1) false)
      ((= cooper 1) false)
      ((= (abs (- fletcher cooper)) 1) false)
      ((= baker 5) false)
      ((< miller cooper) false)
      ((= (abs (- smith fletcher)) 1) false)
      ;          ((not (distinct? items)) false)
      (else true))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (permutations s)
  (if (null? s) (list '())
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else  (filter predicate (cdr sequence)))))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;(permutations (list 1 2 3))

(define (multiple-dwelling4)
  (filter multiple-dwelling4-condition
          (permutations (list 1 2 3 4 5))))

(multiple-dwelling4)

(driver-loop)
(and true true)
(and true false)
(and)
(or true false)
(or true true)
(or false true)
(or false false)
end

;ex4.42
(driver-loop)

(define (xor a b)
  (if a (not b) b))

;(define (require-ex4-42 cond1 cond2)
;  (require (or (and cond1 (not cond2))
;               (and (not cond1) cond2))))

(define (ex4-42)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (xor  (= kitty 2) (= betty 3)))
    (require (xor  (= ethel 1) (= joan 2)))
    (require (xor  (= joan 3) (= ethel 5)))
    (require (xor  (= kitty 2) (= mary 4)))
    (require (xor  (= mary 4) (= betty 1)))
    (require
      (distinct?     (list betty ethel joan kitty mary)))
    (list betty ethel joan kitty mary)))

(ex4-42)
try-again
end

;ex4.43
(driver-loop)

(define (distinct? items)
  (cond ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (ex4-43)
  (let (
        (moore (cons 'lorna 'mary-ann))
        (downing (cons 'melissa (amb 'lorna 'rosalind 'gabrielle 'mary-ann)))
        (hall (cons 'rosalind (amb 'lorna 'gabrielle 'mary-ann)))
        (barnacle (cons 'gabrielle 'melissa))
        (parker (cons 'mary-ann (amb 'lorna 'rosalind 'gabrielle)))
        )
    (let ((fathers
            (list moore downing hall barnacle parker)))
      ;      (require
      ;       (distinct? (map car fathers)))
      (require
        (distinct? (map cdr fathers)))
      (require
        (eq? (cdr parker)
             (car (car (filter (lambda (x) (eq? (cdr x) 'gabrielle)) fathers)))))
      (list 'moore moore 'downing downing 'hall hall 'barnacle barnacle 'parker parker))))


(ex4-43)
try-again

end

(driver-loop)

(define (distinct? items)
  (cond ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (yacht f) (car f))
(define (daughter f) (cdr f))

(define (ex4-43-raw)
  (let ((moore (cons (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))))
    (require (eq? (yacht moore) 'lorna))
    (require (eq? (daughter moore) 'mary-ann))
    (let ((barnacle (cons (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))))
      (require (eq? (yacht barnacle) 'gabrielle))
      (require (eq? (daughter barnacle) 'melissa))
      (let ((downing (cons (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))))
        (require (eq? (yacht downing) 'melissa))
        (require (not (member (daughter downing) '(melissa mary-ann))))
        (let ((hall (cons (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))))
          (require (eq? (yacht hall) 'rosalind))
          (require (not (member (daughter hall) '(melissa mary-ann rosalind))))
          (let ((parker (cons (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))))
            (require (not (member (yacht parker) '(lorna gabrielle melissa rosalind))))
            (require (not (member (daughter parker) '(melissa mary-ann))))
            (let ((fathers-without-parker
                    (list moore downing hall barnacle))
                  (fathers
                    (list moore downing hall barnacle parker)))
              (let ((gabrielle-fathers (filter (lambda (x) (eq? (daughter x) 'gabrielle)) fathers-without-parker)))
                (require (not (null? gabrielle-fathers)))
                (require (null? (cdr  gabrielle-fathers)))
                (require
                  (eq? (daughter parker)
                       (yacht (car gabrielle-fathers))))
                (require
                  (distinct? (map daughter fathers)))
                (list 'moore moore 'downing downing 'hall hall 'barnacle barnacle 'parker parker)))))))))

(ex4-43-raw)
try-again
end


(driver-loop)

(define (distinct? items)
  (cond ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))
(define (ex4-43-2)
  (let (
        (moore (cons 'lorna (amb 'rosalind 'gabrielle 'mary-ann)))
        (downing (cons 'melissa (amb 'lorna 'rosalind 'gabrielle 'mary-ann)))
        (hall (cons 'rosalind (amb 'lorna 'gabrielle 'mary-ann)))
        (barnacle (cons 'gabrielle 'melissa))
        (parker (cons 'mary-ann (amb 'lorna 'rosalind 'gabrielle)))
        )
    (let ((fathers
            (list moore downing hall barnacle parker)))
      ;      (require
      ;       (distinct? (map car fathers)))
      (require
        (distinct? (map cdr fathers)))
      (require
        (eq? (cdr parker)
             (car (car (filter (lambda (x) (eq? (cdr x) 'gabrielle)) fathers)))))
      (list 'moore moore 'downing downing 'hall hall 'barnacle barnacle 'parker parker))))


(ex4-43-2)
try-again
try-again
end


(driver-loop)

(define (eight-queen-diag row1 rest-rows)
  (if (null? rest-rows) true
    (if
      (= (abs (- (car row1) (caar rest-rows)))
         (abs (- (cdr row1) (cdar rest-rows))))
      false
      (eight-queen-diag row1 (cdr rest-rows)))))


(define (eight-queen-sub rows)
  (if (null? rows) true
    (let ((first-row (car rows))
          (rest-rows (cdr rows)))
      (if
        (eight-queen-diag first-row rest-rows)
        (eight-queen-sub rest-rows)
        false))))





(define (four-queen)
  (let (
        (row1 (cons 1 (amb 1 2 3 4 )))
        (row2 (cons 2 (amb 1 2 3 4 )))
        (row3 (cons 3 (amb 1 2 3 4 )))
        (row4 (cons 4 (amb 1 2 3 4 )))
        )
    (let ((rows (list row1 row2 row3 row4)))
      (require
        (distinct? (map cdr rows)))
      (require
        (eight-queen-sub rows))
      rows)))

(four-queen)
try-again
try-again
end


(driver-loop)

(define (eight-queen-diag first-row rest-rows)
  (if (null? rest-rows) true
    (if
      (= (abs (- (car first-row) (caar rest-rows)))
         (abs (- (cdr first-row) (cdar rest-rows))))
      false
      (eight-queen-diag first-row (cdr rest-rows)))))


(define (eight-queen-sub rows)
  (if (null? rows) true
    (let ((first-row (car rows))
          (rest-rows (cdr rows)))
      (if
        (eight-queen-diag first-row rest-rows)
        (eight-queen-sub rest-rows)
        false))))


(define (n-queens n)
  (define (generate-positions k l)
    (if (> k n) l
      (generate-positions (+ k 1)  (cons (cons k (an-integer-between 1 n)) l))))
  (let ((rows (generate-positions 1 '())))
    (require
      (distinct? (map cdr rows)))
    (require
      (eight-queen-sub rows))
    rows))
(n-queens 4)
try-again
try-again
;(n-queens 6)
end

;; (driver-loop)
;; (define (eight-queen)
;;   (let (
;;         (row1 (cons 1 (amb 1 2 3 4 5 6 7 8)))
;;         (row2 (cons 2 (amb 1 2 3 4 5 6 7 8)))
;;         (row3 (cons 3 (amb 1 2 3 4 5 6 7 8)))
;;         (row4 (cons 4 (amb 1 2 3 4 5 6 7 8)))
;;         (row5 (cons 5 (amb 1 2 3 4 5 6 7 8)))
;;         (row6 (cons 6 (amb 1 2 3 4 5 6 7 8)))
;;         )
;;     (let ((rows-a (list row1 row2 row3 row4 row5 row6)))
;;       (require
;;        (distinct? (map cdr rows-a)))
;;       (require
;;        (eight-queen-sub rows-a))
;;       (let
;;           (
;;            (row7 (cons 7 (amb 1 2 3 4 5 6 7 8)))
;;            )
;;         (let ((rows-b (list row1 row2 row3 row4 row5 row6 row7)))
;;           (require
;;            (distinct? (map cdr rows-b)))
;;           (require
;;            (eight-queen-sub rows-b))

;;           (let (
;;                 (row8 (cons 8 (amb 1 2 3 4 5 6 7 8)))
;;                 )
;;             (let ((rows (list row1 row2 row3 row4 row5 row6 row7 row8)))
;;               (require
;;                (distinct? (map cdr rows)))
;;               (require
;;                (eight-queen-sub rows))
;;               rows)))))))


;; (eight-queen)

(driver-loop)
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

end

(driver-loop)
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    ;    (newline) (display "unparsed: " )(display *unparsed*) (newline)
    (require (null? *unparsed*))
    sent))
(parse '(the cat eats))
end

(driver-loop)
(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    ;    (newline) (display "verb-maybe-extend: ")
    ;    (display verb-phrase) (newline)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    ;    (newline) (display "noun-maybe-extend: ")
    ;    (display noun-phrase) (newline)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(parse '(the student with the cat sleeps in the class))
(parse '(the professor lectures to the student with the cat))
try-again
try-again
end

(driver-loop)
(parse '(the professor lectures to the student with the cat))
try-again
try-again
end


(driver-loop)
(parse '(the professor lectures to the student in the class with the cat))
try-again
try-again
try-again
try-again
try-again
end


;ex4.46
;先に動詞句をパースしちゃうから

;ex4.47
;http://www.serendip.ws/archives/2540

;; (driver-loop)
;; (define (parse-word word-list)
;;   (display *unparsed*)
;;   (newline)
;;   (display word-list)
;;   (newline)
;;   (require (not (null? *unparsed*)))
;;   (require (memq (car *unparsed*) (cdr word-list)))
;;   (let ((found-word (car *unparsed*)))
;;     (set! *unparsed* (cdr *unparsed*))
;;     (list (car word-list) found-word)))

;; (parse '(the cat eats))
;; try-again
;; end

;; (driver-loop)
;; (define (parse-verb-phrase)
;;   (amb (parse-word verbs)
;;        (list 'verb-phrase
;;              (parse-verb-phrase)
;;              (parse-prepositional-phrase))))
;; (parse '(the cat eats))
;; try-again
;; end



;ex4.49

(driver-loop)
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (let ((found-word (an-element-of (cdr word-list))))
    (list (car word-list) found-word)))
(parse-sentence)
try-again
try-again
try-again
try-again
try-again
end

(parse '(the professor lectures to the student in the class with the cat))
;ex4.50
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((lambda? exp) (analyze-lambda exp))
    ((let? exp)
     (analyze (let->combination exp)))
    ((begin? exp)
     (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((amb? exp) (analyze-amb exp))
    ((ramb? exp) (analyze-ramb exp))
    ((and? exp) (analyze-and exp))
    ((or? exp) (analyze-or exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE" exp))))


(define (ramb? exp) (tagged-list? exp 'ramb))

(use math.mt-random)
(define m (make <mersenne-twister> :seed (sys-time)))
(mt-random-integer m 10)
(define (random a) (mt-random-integer m a))
(use srfi-1)
(list-ref '(1 2) 1)
(take '(1 2) 1)
(drop '(1 2) 1)
(list-ref '(1 2 3) 2)
(mt-random-integer m 10)
(random 110)
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          (let ((i (random (length choices))))
            ((list-ref choices i) env
                                  succeed
                                  (lambda ()
                                    (try-next (append (take choices i) (drop choices (+ i 1)))
                                              ))))))
      (try-next cprocs))))

(driver-loop)
(ramb 1 2 3 4 5)
try-again
try-again
try-again
try-again
end

(driver-loop)
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (ramb verb-phrase
          (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
          (maybe-extend (list 'noun-phrase
                              noun-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (ramb verb-phrase
          (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (random-word items)
  (require (not (null? items)))
  (ramb (car items) (an-element-of (cdr items))))
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (list (car word-list) (random-word (cdr word-list))))

(parse-sentence)
try-again
try-again
;try-again
;try-again
;try-again
end

;ex4.51
(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((permanent-set? exp) (analyze-permanent-set exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((lambda? exp) (analyze-lambda exp))
    ((let? exp)
     (analyze (let->combination exp)))
    ((begin? exp)
     (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((amb? exp) (analyze-amb exp))
    ((ramb? exp) (analyze-ramb exp))
    ((and? exp) (analyze-and exp))
    ((or? exp) (analyze-or exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE" exp))))

(driver-loop)
(define count 0)
(define p-count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (permanent-set! p-count (+ p-count 1))
  (require (not (eq? x y)))
  (list x y count p-count))
try-again
end

(driver-loop)
(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
try-again
end

;ex4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (analyze-if-fail exp)
  (let ((normal (analyze (cadr exp)))
        (alternative (analyze (caddr exp))))
    (lambda (env succeed fail)
      (normal env
              succeed
              (lambda ()
                (alternative env succeed fail))))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((permanent-set? exp) (analyze-permanent-set exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((if-fail? exp) (analyze-if-fail exp))
    ((lambda? exp) (analyze-lambda exp))
    ((let? exp)
     (analyze (let->combination exp)))
    ((begin? exp)
     (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((amb? exp) (analyze-amb exp))
    ((ramb? exp) (analyze-ramb exp))
    ((and? exp) (analyze-and exp))
    ((or? exp) (analyze-or exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE" exp))))

(driver-loop)
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
(if-fail (let ((x (an-element-of '(1 3 8))))
           (require (even? x))
           x)
         'all-odd)
end

;ex4.53
(driver-loop)
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
end

;ex4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))



(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((permanent-set? exp) (analyze-permanent-set exp))
    ((definition? exp) (analyze-definition exp))
    ((require? exp) (analyze-require exp))
    ((if? exp) (analyze-if exp))
    ((if-fail? exp) (analyze-if-fail exp))
    ((lambda? exp) (analyze-lambda exp))
    ((let? exp)
     (analyze (let->combination exp)))
    ((begin? exp)
     (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((amb? exp) (analyze-amb exp))
    ((ramb? exp) (analyze-ramb exp))
    ((and? exp) (analyze-and exp))
    ((or? exp) (analyze-or exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                 (fail2)
                 (succeed 'ok fail2)))
             fail))))

(driver-loop)
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(prime-sum-pair '(1 3 5 8) '(20 35 110))
try-again
try-again
try-again
end
