; 2.3 Symbolic Data

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(display (memq 'apple '(pear banana prune)))
(display (memq 'apple '(pear banana prune apple orange)))
(memq 'red '((red shoes) (blue socks)))
(display (memq 'red '(red shoes blue socks)))

; Ex 2.54

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this is (a) list))
(equal? '(this is a list) '(this a is list))

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else
          false)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this is a list1))

; Ex 2.55
; skip

; 2.3.2 Example Symbolic Differentiation

; x^2*2y
; -> 4xy
; 
; (* (* x x) (* 2 y))
; (+ (* (* 2 y) deriv('(* x x)))
;    (* (* x x) deriv('(* 2 y)))
; = (+ (* (* 2 y)
;         (+ (* x deriv(x))
;            (* x deriv(x))))
;      (* (* x x)
;         (+ (* 2 deriv(y))
;            (* y deriv(2)))
;       
; = (+ (* (* 2 y)
;         (+ (* x 1)
;            (* x 1))
;      (* (* x x)
;         (+ (* 2 0)
;            (* y 0))
; = (+ (* (* 2 y)
;         (* 2 x))
; = 4xy

; 積の微分公式(ライプニッツルール)(product rule)
; {f(x)g(x)}' = f'(x)g(x) + f(x)g'(x)
; https://ja.wikipedia.org/wiki/%E7%A9%8D%E3%81%AE%E5%BE%AE%E5%88%86%E6%B3%95%E5%89%87 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type --DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(display (deriv '(+ x 3) 'x))

(display (deriv '(* x y) 'x))

(display (deriv '(+ (* (* x x) y) (* 2 x)) 'x))

(display (deriv '(* (* x y) (+ x 3)) 'x))
; (+ (* (* x y) (+ 1 0))
;    (* (+ (* x 0) (* 1 y))
;       (+ x 3)))
; xy + y * (3 + x)
; xy + 3y + xy
; 2xy + 3y
; y(2x + 3)

(display (deriv '(* (* x x) (* 2 y)) 'x))
; 4y

(display (deriv '(* (* x x) x) 'x))
; f  = x^3
; f' = 3x^2
; (+ (* (* x x) 1) (* (+ (* x 1) (* 1 x)) x))
; = (+ x^2 (* 2x x))
; = (+ x^2 2x^2)
; = 3x^2

; Reduce answer to simplest form
(define (make-sum a1 a2)
  (cond((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Skip Ex 2.56 ~ Ex 2.58 for saving time

; 2.3.3 Example: Representing Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Ex 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))
(display (union-set '(a b c e) '(b c d f e)))

; Ex 2.60

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1)
                               set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(display (intersection-set '(1 2 3) '(2 3 4)))

; Ex 2.61
(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set)))
         (cons x set))
        ((= x (car set))
         set)
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))
(display (adjoin-set 5 '(1 3 4 7 8 10)))

; Ex 2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((< (car s2) (car s1))
         (cons (car s2) (union-set s1 (cdr s2))))
        ((= (car s2) (car s1))
         (union-set s1 (cdr s2)))
        (else
         (cons (car s1) (union-set (cdr s1) s2)))))
(display (union-set '(1 2 3 5 7 9) '(0 4 7 10)))

; [Sets as binary trees](バイナリツリー)
; We can do better than the ordered-list representation by
; arranging the set elements in the form of a tree.

; Each node of the tree holds one element of the set called
; "entry" and a link to each of two other nodes.

; The "left" link points to smaller than the one at the node,
; and the "right" link to elements greater than the one at
; the node.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(display 
  (adjoin-set
    5
    (adjoin-set 15 (make-tree 10 '() '()))))
; (10 (5 () ()) (15 () ()))

; Ex 2.63

; Convert binary tree to ordered list

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define t1
  (adjoin-set
    5
    (adjoin-set 15 (make-tree 10 '() '()))))

(display (tree->list-1 t1))
(display (tree->list-2 t1))

(define t2
  (adjoin-set
    4
    (adjoin-set
      3
      (adjoin-set
        2
        (make-tree 1 '() '())))))

(display t2)
(display (tree->list-1 t2))
(display (tree->list-2 t2))

(define t3
  (adjoin-set
    1
    (adjoin-set
      2
      (adjoin-set
        3
        (make-tree 4 '() '())))))

(display t3)
(display (tree->list-1 t3))
(display (tree->list-2 t3))

; Ex 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    ; left-size = (length-1)/2
    (let ((left-size (quotient (- n 1) 2)))
      ; create left result (recursive)
      ; -> (left tree, remains)
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          ; create right result (recursive)
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              ; return tree and remaining-elements
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(display (list->tree '(1 3 5 7 9 11)))

; Ex 2.65 ~ Ex 2.66
; Skipping for now

; 2.3.4 Example Huffman Encoding Trees

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
                (decode-1 (cdr bits) next-branch))))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit == SHOOSE BRANCH" bit))))



