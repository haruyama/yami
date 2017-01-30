;ex2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) L1)
        (let ((new-c (div (coeff t1) (coeff t2)))
              (new-o (- (order t1) (order t2))))
          (let ((rest-of-result
                  (div-terms
                    ; L1 - new-o * L2
                    (add-terms L1
                               (mul-terms L2 
                                          (adjoin-term (make-term new-o (negate new-c)) (the-empty-termlist))))
                    L2)
                  ))
            (cons (adjoin-term (make-term new-o new-c) (car rest-of-result))
                  (cdr rest-of-result))
            ))))))
