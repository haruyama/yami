(define nil '())
; p61
(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))
;
(length x)
;
(list x x)
;
;(length (list x x))
;
(define (count-leaves x)
 (cond ((null? x) 0)
       ((not (pair? x)) 1)
       (else (+ (count-leaves (car x))
                (count-leaves (cdr x))))))
;
(count-leaves x)
;
(count-leaves (list x x))

; 2.24

(list 1 (list 2 (list 3 4)))

; 2.25
(define x (list 1 3 (list 5 7) 9))
(display x)
(newline)
(car (cdr (car (cdr (cdr x)))))
(car (cdaddr x))

(define x (list (list 7)))
(display x)
(newline)
(car (car x))
(caar x)

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display x)
(newline)
;(car (cdr (car (cdr x))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
(cadadr (cadadr (cadadr x )))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

; 2.27
(define x (list (list 1 2) (list 3 4)))
;(display x)
;(newline)
(reverse x)
;
(define (deep-reverse x)
 (define (iter l a)
   (cond ((null? l) a)
         ((not (pair? (car l)))
          (iter (cdr l) (cons (car l) a)))
         (else
          (iter (cdr l)
           (if (null? a)
               (list (deep-reverse (car l)))
               (cons (deep-reverse (car l)) a))))))
 (iter x nil))

(deep-reverse x)

(define (deep-reverse x)
  (if (pair? x) (reverse (map deep-reverse x))
      x))
(deep-reverse x)
;
(deep-reverse (list (list 1 2) (list 3 4) (list 5 6 (list 7 8))))
(deep-reverse (list (list 1 2) (list 3 4) (list 5 6 (list 7 8) 9)))

; 2.28
;
(define x (list (list 1 2) (list 3 4)))
;(display x)
;; ;(newline)
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(fringe x)

(define (fringe x)
  (let rec ((x x) (acc '()))
    (cond ((null? x) acc)
          ((not (pair? x)) (cons x acc))
          (else (rec (car x) (rec (cdr x) acc))))))

(fringe x)

(define (fringe x)
  (define (loop x acc)
    (cond ((null? x) acc)
          ((not (pair? x)) (cons x acc))
          (else (loop (car x) (loop (cdr x) acc)))))
  (loop x '()))


(fringe x)

(display (list x x))
(newline)
(fringe (list x x))

; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
;
(define mb1 (make-branch 1 10))

(define mb2 (make-branch 2 20))

(define mb3 (make-branch 3 30))
(define mb4 (make-branch 4 (make-mobile mb2 mb3)))
;
(define m1 (make-mobile mb1 mb4))
;
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
;
(left-branch m1)
(right-branch m1)
;
(define (branch-length mb)
  (car mb))
;
(define (branch-structure mb)
  ;  (begin (display mb) (newline))
  (car (cdr mb)))
;
(branch-length mb4)
;
(branch-structure mb4)
;
(define mobile? pair?)


;; (define (total-weight-sub mobile weight)
;;   (define (iter mb w)
;; ;    (begin (display mb) (newline))
;;     (if (mobile? (branch-structure mb))
;;      (total-weight-sub (branch-structure mb) w)
;;      (+ w (branch-structure mb))))
;;   (+ (iter (left-branch mobile) weight) (iter (right-branch mobile) 0)))
;; (total-weight-sub m1 0)

;(define (total-weight m)
;  (if (not (mobile? m)) m
;      (+ (total-weight (branch-structure (left-branch m)))
;         (total-weight (branch-structure (right-branch m))))))

(define (total-weight m)
  (if (mobile? m)
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))
      m))

(total-weight m1)

(define (balanced? m)
  (cond ((not (mobile? m)) #t)
        ((= (* (branch-length (left-branch m))
               (total-weight (branch-structure (left-branch m))))
            (* (branch-length (right-branch m))
               (total-weight (branch-structure (right-branch m)))))
         (and (balanced? (branch-structure (left-branch m)))
              (balanced? (branch-structure (right-branch m)))))
        (else #f)))

(balanced? m1)

(balanced? (make-mobile (make-branch 5 20) (make-branch 10 10)))

(balanced? (make-mobile (make-branch 5 20)
                        (make-branch 10 (make-mobile (make-branch 5 5)
                                                     (make-branch 5 5)))))

;; (define (calc-mobile-torque-sub mb length)

;;   (if (mobile? (branch-structure mb))
;;       (+
;;        (calc-mobile-torque-sub
;;         (left-branch (branch-structure mb)) (+ length (branch-length mb)))
;;        (calc-mobile-torque-sub
;;         (right-branch (branch-structure mb)) (+ length (branch-length mb))))
;;        (* (+ length (branch-length mb)) (branch-structure mb))))

;; ;
;; ;
;; ;
;; (define (calc-mobile-torque mb)
;;   (calc-mobile-torque-sub mb 0))
;; (calc-mobile-torque mb1)
;; (calc-mobile-torque mb4)

(define  (scale-tree tree factor)
  (cond  ((null? tree) nil)
    ((not  (pair? tree))  (* tree factor))
    (else  (cons  (scale-tree  (car tree) factor)
                  (scale-tree  (cdr tree) factor)))))

(scale-tree  (list 1  (list 2  (list 3 4) 5)  (list 6 7))
             10)

(define  (scale-tree tree factor)
  (map  (lambda  (sub-tree)
          (if  (pair? sub-tree)
            (scale-tree sub-tree factor)
            (* sub-tree factor)))
        tree))

(scale-tree  (list 1  (list 2  (list 3 4) 5)  (list 6 7))
             10)

; 2.30

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define x (cons (list 1 2) (list 3 4)))
(square-tree x)

; 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree))) tree))
;
(define (square-tree tree)
  (tree-map square tree))

(square-tree x)
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

; 2.32
(define (subsets s)
  (if (null? s) (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l))
                          rest)))))

(subsets (list 1 2 3))
(display (subsets (list 1 2 3)))
