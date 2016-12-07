(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define weights '((A 4) (B 2) (C 1) (D 1)))

(make-leaf-set weights)

;ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
sample-tree

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
(decode '(0) sample-tree)
(decode '(1 0) sample-tree)
(decode '(1 1 1) sample-tree)
(decode '(1 1 0) sample-tree)

(symbols sample-tree)
(symbols (make-leaf 'E 1))
(weight sample-tree)

;ex2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(cadr sample-tree)

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((symbol-exists? symbol left)
               (cons 0 (encode-symbol symbol left)))
              ((symbol-exists? symbol right)
               (cons 1 (encode-symbol symbol right)))
              (else (error "Invalid symbol -- ENCODE-SYMBOL" symbol))))))

(define (symbol-exists? symbol tree)
  (memq symbol (symbols tree)))

(decode sample-message sample-tree)
(encode (decode sample-message sample-tree) sample-tree)
;(encode '(E) sample-tree)

;ex2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge
        (adjoin-set (make-code-tree (car set) (cadr set))
                    (cddr set)))))

(define test (make-leaf-set weights))
test
(successive-merge test)

(define sample-tree2
  (generate-huffman-tree weights))

(encode (decode sample-message sample-tree2) sample-tree2)

;ex2.70
(define weights-2-70 '((A 2) (BOOM 1 ) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define tree-2-70
  (generate-huffman-tree weights-2-70))

(define message-2-70
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA 
        GET A JOB SHA NA NA NA NA NA NA NA NA 
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
        SHA BOOM))

(encode message-2-70 tree-2-70)
(length (encode message-2-70 tree-2-70))
(decode (encode message-2-70 tree-2-70) tree-2-70)
(* 3 (length message-2-70))

(define weights-2-71-1 '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define weights-2-71-2 '((A 1) (B 2) (C 4) (D 8) (E 16)
                               (F 32) (G 64) (H 128) (I 256) (J 512)))

(define tree-2-71-1
  (generate-huffman-tree weights-2-71-1))

(display tree-2-71-1)

(define tree-2-71-2
  (generate-huffman-tree weights-2-71-2))

(display tree-2-71-2)


(use slib)
(require 'trace)
(untrace encode-symbol)
(trace symbol-exists?)

(encode '(E) tree-2-71-1)
(encode '(A) tree-2-71-1)

(encode '(J) tree-2-71-2)
(encode '(A) tree-2-71-2)
; http://community.schemewiki.org/?sicp-ex-2.71
; http://community.schemewiki.org/?sicp-ex-2.72
; 最大頻度: weights で頻度が高いものを先頭に置くように設計すれば Θ(1) そのような前提はないので θ (n)
; 最小頻度 θ(n^2)

