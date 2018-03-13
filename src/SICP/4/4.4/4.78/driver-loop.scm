(load "./evaluator.scm")
(load "./environment.scm")

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (query-syntax-process (read))))
      (cond ((assertion-to-be-added? input)
             (add-rule-or-assertion! (add-assertion-body input))
             (newline)
             (display "Assertion added to data base.")
             (internal-loop try-again))
            ((eq? input 'try-again) (try-again))
            (else
             (begin
               (newline)
               (display ";;; Starting a new problem ")
               (ambeval input
                        ;; ambeval success
                        (lambda (val next-alternative)
                          ;; succeed take two parameters: value and fail.
                          ;; So the interval-loop will call fail to get
                          ;; another value or end up with no values.(fail
                          ;; is a procedure trying get values from other
                          ;; process)
                          (let ((res (instantiate
                                       input
                                       val
                                       (lambda (v f)
                                         (contract-question-mark v)))))
                            (announce-output output-prompt)
                            (display res)
                            (internal-loop next-alternative)))
                        ;; ambeval failure
                        ;; This is the basic fail procedure, would be passed
                        ;; as the fail procedure of process in most cases.
                        ;; But when it hits amb procedure it will be kept in
                        ;; a local procedure of the result returned by anal-
                        ;; yze-amb. After that, the fail procedure passed in
                        ;; the process woulb be the next value of amb-choices.
                        ;; And this basic fail procedure won't be released,
                        ;; until the amb-choices is run out.
                        (lambda ()
                          (announce-output
                           ";;; There are no more values of")
                          (display input)
                          (driver-loop))
                        '()))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))
