(add-rule-or-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))

(add-rule-or-assertion! '(job (Bitdiddle Ben) (computer wizard)))
(add-rule-or-assertion! '(salary (Bitdiddle Ben) 60000))

(add-rule-or-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-rule-or-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-rule-or-assertion! '(salary (Hacker Alyssa P) 40000))
(add-rule-or-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(add-rule-or-assertion! '(job (Fect Cy D) (computer programmer)))
(add-rule-or-assertion! '(salary (Fect Cy D) 35000))
(add-rule-or-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-rule-or-assertion! '(job (Tweakit Lem E) (computer technician)))
(add-rule-or-assertion! '(salary (Tweakit Lem E) 25000))
(add-rule-or-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-rule-or-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
(add-rule-or-assertion! '(salary (Reasoner Louis) 30000))
(add-rule-or-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))

(add-rule-or-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(add-rule-or-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-rule-or-assertion! '(job (Warbucks Oliver) (administration big wheel)))
(add-rule-or-assertion! '(salary (Warbucks Oliver) 150000))

(add-rule-or-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(add-rule-or-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
(add-rule-or-assertion! '(salary (Scrooge Eben) 75000))
(add-rule-or-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(add-rule-or-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-rule-or-assertion! '(job (Cratchet Robert) (accounting scrivener)))
(add-rule-or-assertion! '(salary (Cratchet Robert) 18000))
(add-rule-or-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))

(add-rule-or-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-rule-or-assertion! '(job (Aull DeWitt) (administration secretary)))
(add-rule-or-assertion! '(salary (Aull DeWitt) 25000))
(add-rule-or-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(add-rule-or-assertion! '(can-do-job (computer wizard) (computer programmer)))
(add-rule-or-assertion! '(can-do-job (computer wizard) (computer technician)))

(add-rule-or-assertion! '(can-do-job (computer programmer)
                     (computer programmer trainee)))

(add-rule-or-assertion! '(can-do-job (administration secretary)
                     (administration big wheel)))
