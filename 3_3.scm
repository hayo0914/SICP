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


; 3.3.2 Representing Queues

; 3.3.3 Representing Tables

; table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

; Usage of Assoc
(assoc 'c (list '(a 2) '(b 1) '(c 3) '(d 5) '(e 0)))

; Two-dimentional table
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; Creating local tables

; Two-dimentional table generator
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; get and put definition for Data Directed Programming
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; Ex 3.24
; Ex 3.25
; Ex 3.26
; Skip

; Ex 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; memonized procedure

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
              (lookup x table)))
				(if previously-computed-result
          (begin
            (display "cache hit: ")
            (display x)
            (newline))
          (begin
            (display "cache miss: ")
            (display x)
            (newline)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

; This is not working obviously.
(define memo-fib (memoize fib))
(memo-fib 10)

; 3.3.4 A Simulator for Digital Circuits


