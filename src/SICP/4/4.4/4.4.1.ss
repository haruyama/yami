(load "./4.4.ss")

(load "./4.4_microshaft.ss")

(query-driver-loop)
(job ?x (computer programmer))
(address ?x ?y)
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(supervisor ?x ?x)
(job ?x (computer ?type))
(job ?x (computer . ?type))
end

;ex4.55
(query-driver-loop)
;a
(supervisor ?x (Bitdiddle Ben))
;b
(job ?x (accounting . ?y))
;c
(address ?x (Slumerville . ?y))
end

(query-driver-loop)
(and (job ?person (computer programmer))
  (address ?person ?where))
(or (supervisor ?x (Bitdiddle Ben))
  (supervisor ?x (Hacker Alyssa P)))

(and (supervisor ?x (Bitdiddle Ben))
  (not (job ?x (computer programmer))))

(and (salary ?persion ?amount)
  (lisp-value > ?amount 30000))
end

;ex4.56
(query-driver-loop)
;a
(and (supervisor ?x (Bitdiddle Ben))
  (address ?x ?y))
;b
(and (salary (Bitdiddle Ben) ?ben-amount)
  (salary ?x ?amount)
  (lisp-value < ?amount ?ben-amount))
;c
(and (supervisor ?person ?supervisor)
  (not (job ?supervisor (computer . ?x)))
  (job ?supervisor ?y))
end

(query-driver-loop)

(assert! (rule (lives-near ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                 (address ?person-2 (?town . ?rest-2))
                 (not (same ?person-1 ?person-2)))))

(assert! (rule (same ?x ?x)))

(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                 (supervisor ?x ?middle-manager))))

(assert! (rule (outranked-by ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                 (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss)))))
end

(query-driver-loop)
(lives-near ?x (Bitdiddle Ben))
(lives-near ?x ?y)
(wheel ?x)
(and (job ?x (computer programmer))
  (lives-near ?x (Bitdiddle Ben)))
(and (job ?x ?type)
  (lives-near ?x (Bitdiddle Ben)))
(outranked-by ?person ?boss)
end

;ex4.57
(query-driver-loop)
(assert! (rule (can-replace-job ?job-1 ?job-2)
               (or
                 (can-do-job ?job-1 ?job-2)
                 (and (can-do-job ?job-1 ?job-m)
                   (can-replace-job ?job-m ?job-2))
                 )))
(assert! (rule (replace ?person-1 ?person-2)
               (and (job ?person-1 ?job-1)
                 (job ?person-2 ?job-2)
                 (or (can-replace-job ?job-1 ?job-2)
                   (same ?job-1 ?job-2))
                 (not (same ?person-1 ?person-2)))))

(can-replace-job ?j (computer programmer trainee))
(can-replace-job (computer wizard) ?j)

(replace ?p (Fect Cy D))

(and (replace ?p1 ?p2)
  (salary ?p1 ?s1)
  (salary ?p2 ?s2)
  (lisp-value < ?s1 ?s2))
end

;ex4.58
(query-driver-loop)

(assert! (rule (big-shot ?person)
               (or
                 (and
                   (job ?person (?div-p . ?rest-p))
                   (not (supervisor ?person ?supervisor)))
                 (and
                   (job ?person (?div-p . ?rest-p))
                   (supervisor ?person ?supervisor)
                   (job ?supervisor (?div-s . ?rest-s))
                   (not (same ?div-p ?div-s))))))
(big-shot ?p)

(and (job ?w ?s)
  (big-shot ?w))
end

;ex4.59
(query-driver-loop)
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

;a
(meeting ?d (Friday . ?t))

;b
(assert!
  (rule (meeting-time ?person ?day-and-time)
        (and (job ?person (?div . ?rest-p))
          (or
            (meeting whole-company ?day-and-time)
            (meeting ?div ?day-and-time)))))

(meeting-time (Hacker Alyssa P) ?dt)
(meeting-time (Scrooge Eben) ?dt)
(meeting-time (Aull DeWitt) ?dt)
;c
(meeting-time (Hacker Alyssa P) (Wednesday . ?t))
end

;ex4.60
(query-driver-loop)
(lives-near ?p1 ?p2)

;名前に順序を付ける
;http://www.serendip.ws/archives/2660
(assert! (id 0 (Warbucks Oliver)))
(assert! (id 1 (Bitdiddle Ben)))
(assert! (id 2 (Hacker Alyssa P)))
(assert! (id 3 (Fect Cy D)))
(assert! (id 4 (Tweakit Lem E)))
(assert! (id 5 (Reasoner Louis)))
(assert! (id 6 (Scrooge Eben)))
(assert! (id 7 (Cratchet Robert)))
(assert! (id 8 (Aull DeWitt)))
(assert! (rule (lives-near2 ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                 (address ?person-2 (?town . ?rest-2))
                 (id ?id-1 ?person-1)
                 (id ?id-2 ?person-2)
                 (lisp-value > ?id-1 ?id-2)
                 ;(not (same ?person-1 ?person-2)
                 )))

(lives-near2 ?p1 ?p2)
end

;プログラムとしての論理
(query-driver-loop)
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))
(append-to-form (a b) (c d) ?z)

(append-to-form (a b) ?y (a b c d))

(append-to-form ?x ?y (a b c d))
end

;ex4.61
(query-driver-loop)
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))
(?x next-to ?y in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))
(1 next-to ?x in (2 1 3 1))
end


;ex4.62
(query-driver-loop)
(assert! (rule (last-pair (?x . ?y) (?z))
               (last-pair ?y (?z))))
(assert! (rule (last-pair (?y) (?y))))
(last-pair (3) ?x)

(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))

(last-pair (?x) (3))
;(last-pair ?x (3))
end


;ex4.63
(query-driver-loop)
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson-of ?g ?s)
               (and
                 (son-of ?p ?s)
                 (son-of ?g ?p))))

(assert! (rule (son-of ?m ?s)
               (or
                 (son ?m ?s)
                 (and (wife ?m ?w)
                   (son ?w ?s)))))

(grandson-of ?g ?s)
(son-of ?m ?s)
(grandson-of Cain ?s)
(grandson-of Methushael ?s)
end

(display-stream
  (qeval '(job (? x) (? y)) (singleton-stream '())))

(display-stream
  (find-assertions '(job (? x) (? y)) (singleton-stream '())))

(display-stream
  (apply-rules '(job (? x) (? y)) (singleton-stream '())))

(display-stream
  (qeval '(lives-near (? x) (? y)) (singleton-stream '())))

(display-stream
  (find-assertions '(lives-near (? x) (? y)) (singleton-stream '())))

(display-stream
  (apply-rules '(lives-near (? x) (? y)) (singleton-stream '())))

(query-driver-loop)
(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))
(married Mickey ?who)

;ex 4.64
; outranked-by が無限に呼ばれる

(query-driver-loop)
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))
(outranked-by (Bitdiddle Ben) ?who)
end

;ex 4.65
(query-driver-loop)
(wheel ?who)

end
; Warbucks が wheel である人が4人いる

;ex 4.66
; SICP 4.4.3 Ex. 4.66 - nakayama-blog - http://sioramen.sub.jp/blog/2009/12/sicp-443-ex-466.html
; distinct 機構を入れて解決できる

; ex 4.67
; http://sioramen.sub.jp/blog/2009/12/sicp-443-ex-467.html
; 履歴を残し履歴のパターンと同じ質問が繰替えされていたら打ち切る

(define rule-history 'dummy)
(define rule-loop-count 0)
(define (query-driver-loop)
  (set! rule-history 'dummy)
  (set! rule-loop-count 0)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input 'end) (display "stopped.")
      (let ((q (query-syntax-process input)))
        (cond ((assertion-to-be-added? q)
               (add-rule-or-assertion! (add-assertion-body q))
               (newline)
               (display "Assertion added to data base.")
               (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate q
                               frame
                               (lambda(v f)
                                 (contract-quetion-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))))

;(define (apply-a-rule rule query-pattern query-frame)
;  (let ((clean-rule (rename-variables-in rule)))
;    (let ((unify-result
;            (unify-match query-pattern
;                         (conclusion clean-rule)
;                         query-frame)))
;      (if (eq? unify-result 'failed)
;        the-empty-stream
;        (qeval (rule-body clean-rule)
;               (singleton-stream unify-result))))))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
            (unify-match query-pattern
                         (conclusion clean-rule)
                         query-frame)))
      (if (eq? unify-result 'failed)
        the-empty-stream
        ;        (begin
        ;          (display rule-history) (newline)
        ;          (display (conclusion rule)) (newline)
        (begin
          (if (eq? rule-history (conclusion rule))
            (if (> rule-loop-count 1000)
              (error "rule loop detected" rule-history)
              (set! rule-loop-count (+ 1 rule-loop-count)))
            (begin
              (set! rule-history (conclusion rule))
              (set! rule-loop-count 0)))
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))))


;ex4.68
(query-driver-loop)
(assert! (rule (reverse () ())))
(assert! (rule (reverse (?u . ?v) ?y)
               (and (reverse ?v ?w)
                 (append-to-form ?w (?u) ?y))))

(reverse (1 2 3) ?x)

(assert! (rule (reverse2 () ())))
(assert! (rule (reverse2 ?y (?u . ?v))
               (and (reverse2 ?w ?v)
                 (append-to-form ?w (?u) ?y))))
(reverse2 ?x (1 2 3))

;ex4.69

(assert! (rule ((great . ?rel) ?x ?y)
               (and
                 (son-of ?x ?w)
                 (?rel ?w ?y))))


(assert! (rule ((grandson) ?x ?y)
               (grandson-of ?x ?y)))

(assert! (rule (is-last-grandson? ?rel)
               (last-pair ?rel (grandson))
               ))

((great grandson) ?g ?ggs)
(?rel Adam Irad)
(and
  (?rel Adam Irad)
  (is-last-grandson? ?rel)
  )
((great great grandson) ?g ?ggs)
end
