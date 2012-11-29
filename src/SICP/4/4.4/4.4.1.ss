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
end

(query-driver-loop)
(job ?x (computer programmer))
(address ?x ?y)
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
(assert! (rule (replace-job ?job-1 ?job-2)
			   (or
                 (can-do-job ?job-1 ?job-2)
				   (and (can-do-job ?job-1 ?job-m)
						(replace-job ?job-m ?job-2))
				   )))
(assert! (rule (replace ?person-1 ?person-2)
			   (and (job ?person-1 ?job-1)
					(job ?person-2 ?job-2)
					(or (replace-job ?job-1 ?job-2)
						(same ?job-1 ?job-2))
					(not (same ?person-1 ?person-2)))))
end

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
(assert! (rule (big-shot ?person)
			   (and
				(job ?person (?div-p . ?rest-p))
				(supervisor ?person ?supervisor)
				(job ?supervisor (?div-s . ?rest-s))
				(not (same ?div-p ?div-s))
				)
			   ))
(big-shot ?p)
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
			(or (meeting whole-company ?day-and-time)
				(meeting ?div ?day-and-time)))))

(meeting-time (Hacker Alyssa P) ?dt)
(meeting-time (Scrooge Eben) ?dt)
(meeting-time (Aull DeWitt) ?dt)
;c
(meeting-time (Hacker Alyssa P) (Wednesday ?t))
end

;ex4.60
(query-driver-loop)
(lives-near ?p1 ?p2)

;名前に順序を付ける
;http://www.serendip.ws/archives/2660
(assert! (id (Warbucks Oliver) 0))
(assert! (id (Bitdiddle Ben) 1))
(assert! (id (Hacker Alyssa P) 2))
(assert! (id (Fect Cy D) 3))
(assert! (id (Tweakit Lem E) 4))
(assert! (id (Reasoner Louis) 5))
(assert! (id (Scrooge Eben) 6))
(assert! (id (Cratchet Robert) 7))
(assert! (id (Aull DeWitt) 7))
(assert! (rule (lives-near2 ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (id ?person-1 ?id-1)
                    (id ?person-2 ?id-2)
                    (lisp-value > ?id-1 ?id-2)
                    (not (same ?person-1 ?person-2)))))

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
			   (and (son-of ?p ?s)
					(son-of ?g ?p))))

(assert! (rule (son-of ?m ?s)
			   (or
				(son ?m ?s)
				(and (wife ?m ?w)
					 (son ?w ?s)))))

(grandson-of ?g ?s)
(son-of ?m ?s)
end

