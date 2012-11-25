(load "./4.4.ss")

(query-driver-loop)
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))

(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))

(assert! (can-do-job (computer programmer)
            (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
            (administration big wheel)))

(assert! (rule (same ?x ?x)))

(assert! (rule (lives-near ?person-1 ?person-2)
			   (and (address ?person-1 (?town . ?rest-1))
					(address ?person-2 (?town . ?rest-2))
					(not (same ?person-1 ?person-2)))))

(assert! (rule (wheel ?person)
			   (and (supervisor ?middle-manager ?person)
					(supervisor ?x ?middle-manager))))

(assert! (rule (outranked-by ?staff-person ?boss)
			   (or (supervisor ?staff-person ?boss)
				   (and (supervisor ?staff-person ?middle-manager)
						(outranked-by ?middle-manager ?boss)))))

;ex4.57
(assert! (rule (replace-job ?job-1 ?job-2)
			   (or (can-do-job ?job-1 ?job-2)
				   (and (can-do-job ?job-1 ?job-m)
						(replace-job ?job-m ?job-2))
				   )))
(assert! (rule (replace ?person-1 ?person-2)
			   (and (job ?person-1 ?job-1)
					(job ?person-2 ?job-2)
					(or (replace-job ?job-1 ?job-2)
						(same ?job-1 ?job-2))
					(not (same ?person-1 ?person-2)))))

;ex4.58
(assert! (rule (big-shot ?person)
			   (and
				(job ?person (?div-p . ?rest-p))
				(supervisor ?person ?supervisor)
				(job ?supervisor (?div-s . ?rest-s))
				(not (same ?div-p ?div-s))
				)
			   ))
;ex4.59
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

(assert!
 (rule (meeting-time ?person ?day-and-time)
	   (and (job ?person (?div . ?rest-p))
			(or (meeting whole-company ?day-and-time)
				(meeting ?div ?day-and-time)))))

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
			   (append-to-form ?v ?y ?z)))

;ex4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
			   (?x next-to ?y in ?z)))


;(assert! (rule (last-pair (?x . ?y) ?z)
;			   (last-pair ?y ?z)))
(assert! (rule (last-pair (?x . ?y) (?z))
			   (last-pair ?y (?z))))
(assert! (rule (last-pair (?y) (?y))))


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
			   (and (son-of ?p ?s)
					(son-of ?g ?p))))

(assert! (rule (son-of ?m ?s)
			   (or
				(son ?m ?s)
				(and (wife ?m ?w)
					 (son ?w ?s)))))

;ex4.68
(assert! (rule (reverse () ())))
(assert! (rule (reverse (?u . ?v) ?y)
			   (and (reverse ?v ?w)
					(append-to-form ?w (?u) ?y))))

;ex4.69

(assert! (rule ((great . ?rel) ?x ?y)
               (and (son-of ?x ?w)
                    (?rel ?w ?y))))


;; (assert! (rule ((son) ?x ?y)
;;                (son-of ?x ?y)))


(assert! (rule ((grandson) ?x ?y)
               (grandson-of ?x ?y)))

;; (rule (ends-with ?lis ?x)
;; 	  (last-pair ?lis (?x . ?unused)))

;; (rule ((great  . ?rel) ?a ?d)
;; 	  (and
;; 	   (and
;; 		(son ?a ?m)
;; 		(?rel  ?m ?d))
;; 	   (ends-with ?rel grandson)))

end


(query-driver-loop)
(job ?x (computer programmer))
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

(assert! (married Minnie Mickery))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))

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

(replace-job ?j (computer programmer trainee))
(replace-job (computer wizard) ?j)

(replace ?p (Fect Cy D))

(and (replace ?p1 ?p2)
	 (salary ?p1 ?s1)
	 (salary ?p2 ?s2)
	 (lisp-value < ?s1 ?s2))
end

;ex4.58
(query-driver-loop)

(and
 (job ?person (?div-p . ?rest-p))
 (supervisor ?person ?supervisor)
 (job ?supervisor (?div-s . ?rest-s))
 (not (same ?div-p ?div-s))
 )

(big-shot ?p)

end

;ex4.59
(query-driver-loop)
(meeting ?d (Friday . ?t))
;; (and (job ?person (?div . ?rest-p))
;; 	 (or (meeting whole-company ?day-and-time)
;; 		 (meeting ?div ?day-and-time)))
;(meeting-time ?p ?dt)
(meeting-time (Hacker Alyssa P) ?dt)
(meeting-time (Hacker Alyssa P) (Wednesday ?t))
end

;ex4.60
;名前に順序を付けるなどする必要あり
(query-driver-loop)
(lives-near ?p1 ?p2)
end
;;
(query-driver-loop)
(append-to-form (a b) (c d) ?z)

(append-to-form (a b) ?y (a b c d))

(append-to-form ?x ?y (a b c d))
end

;ex4.61
(query-driver-loop)
(?x next-to ?y in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))
end

;ex4.62
(query-driver-loop)
(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))

(last-pair (?x) (3))
end

;ex4.63
(query-driver-loop)
(grandson-of ?g ?s)
(son-of ?m ?s)
end


(query-driver-loop)
(not (baseball-fan (Bitdiddle Ben)))
end


;ex4.65
(query-driver-loop)
(wheel ?who)
(supervisor ?middle-manager ?person)
;; (supervisor (Aull DeWitt) (Warbucks Oliver)) ; 0
;; (supervisor (Cratchet Robert) (Scrooge Eben)) ; 0
;; (supervisor (Scrooge Eben) (Warbucks Oliver)) ; 1
;; (supervisor (Bitdiddle Ben) (Warbucks Oliver)) ; 3
;; (supervisor (Reasoner Louis) (Hacker Alyssa P)) ; 0
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben)) ;0
;; (supervisor (Fect Cy D) (Bitdiddle Ben)) ; 0
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) ; 1
(supervisor ?x (Warbucks Oliver))
(supervisor ?x (Bitdiddle Ben))
end

;ex4.68
(query-driver-loop)
(reverse (1) ?y)
(reverse (1 2) ?y)
(reverse (1 2 3) ?x)
;(reverse ?x (1 2 3))
;loop
end

;ex4.69

(query-driver-loop)
((great grandson) ?g ?ggs)
;((?rel grandson) Adam Irad)
;(?relationship Adam Cain)
;(?relationship Adam Irad)
end


;ex4.70
;問題文中の定義では無限ストリームになる

;ex4.71
;無限ループの場合に結果を表示できなくなる場合がある
;Louisの定義

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
;; 	(same Mickery ?x))
;; end

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;; 	  the-empty-stream
;; 	  (interleave
;; 	   (stream-car stream)
;; 	   (flatten-stream (stream-cdr stream)))))



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
;; 	  the-empty-stream
;; 	  (interleave-delayed
;; 	   (stream-car stream)
;; 	   (delay (flatten-stream (stream-cdr stream))))))

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
;; 		(singleton-stream '())))


;; (define (instantiate exp frame unbound-var-handler)
;;   (define (copy exp)
;; 	(cond ((var? exp)
;; 		   (let ((binding (binding-in-frame exp frame)))
;; 			 (if binding
;; 				 (copy (binding-value binding))
;; 				 (unbound-var-handler exp frame))))
;; 		  ((pair? exp)
;; 		   (cons (copy (car exp)) (copy (cdr exp))))
;; 		  (else exp)))
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
;; 	(cond ((var? exp)
;; 		   (let ((binding (binding-in-frame exp frame)))
;; 			 (if binding
;; 				 (copy (binding-value binding))
;; 				 (unbound-var-handler exp frame))))
;; 		  ((pair? exp)
;; 		   (cons (copy (car exp)) (copy (cdr exp))))
;; 		  (else exp)))
;;   (copy exp))

;(trace conjoin)

(query-driver-loop)
(and (job ?person (computer programmer))
	 (address ?person ?where))

end

(untrace conjoin)

;4.4.4.3
(display-stream
 (find-assertions (query-syntax-process '(job ?x (computer programmer)))
                                  (singleton-stream '())))


(fetch-assertions (query-syntax-process '(job ?x (computer programmer)))
                                  (singleton-stream '()))


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

(query-syntax-process '(?x ?x))
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
