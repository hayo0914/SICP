; 3.3 Modeling with Mutable Data

; Compound data
; -> means for constructing computational objects
;    that have several parts.
;    which is used for modeling real-world objects
;    that has several aspects.

; Data abstraction
; -> According to which data structures are specified
;    in terms of "constructors",
;      which create data objects,
;    and "selectors",
;      which access the parts of compound data objects.
 
; In order to model compound objects with changing
; state, we will design data abstractions to include,
; in addition to selectors and constructors,
; operations called "mutators", which modify data objects

; Data objects for which mutators are defined are
; known as "mutable data objects".

; 3.3.1 Mutable List Structure

; Ex 3.12

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

; mutator version of append
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)

; Ex 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
; (last-pair z) ; it results infinite loop

; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
v

(define w (mystery v))
w
v

; Ex 3.16

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; orig
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(count-pairs (list 1 2 3 (list 4 5 6)))
(define X (list 1 2 3))
(set-cdr! (last-pair X) X)
X
(count-pairs X)

; fixed
(define (count-pairs x)
  (define (f x encountered)
    (if (or (not (pair? x)) (memq x encountered)) 0
        (+ (f (car x) (cons x encountered))
           (f (cdr x) (cons x encountered)) 1)))
  (f x '()))
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(count-pairs (list 1 2 3 (list 4 5 6)))

(define X (list 1 2 3))
(set-cdr! (last-pair X) X)
X
(count-pairs X)

