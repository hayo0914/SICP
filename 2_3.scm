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




