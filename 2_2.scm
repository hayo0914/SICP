; 2.2 Hierachical Data and the Closure Property
;
(cons 1 2)
(list 1 2)

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

(append squares odds)

; Ex 2.17
(define (last-pair items)
  (if (equal? (cdr items) nil)
    items
    (last-pair (cdr items))))
(last-pair odds)
(last-pair (list 23 71 149 34))

; Ex 2.18
(define (reverse items)
  (let ((len (length items)))
    (define (iter result rest)
      (if (null? rest)
        result
        (iter
          (cons (car rest) result)
          (cdr rest))))
    (iter nil items)))
(display (reverse odds))

; Ex 2.19
; Skip

; Ex 2.20
(define (f x y . z)
  (display x)
  (newline)
  (display y)
  (newline)
  (display z)
  (newline))
(f 1 2 3 4 5)
; 1
; 2
; (3 4 5)

(define (same-parity x . others)
  (let ((parity (even? x)))
    (define (iter result rest)
      (if (null? rest)
        result
        (iter
          (if (equal? parity (even? (car rest)))
            (append result (list (car rest)))
            result)
          (cdr rest))))
    (iter (list x) others)))
(display (same-parity 1 2 3 4 5 6 7))
(display (same-parity 2 3 4 5 6 7))


(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(display (map abs (list -10 2.5 -11.6 17)))
(display (map (lambda (x) (* x x))
              (list 1 2 3 4)))

; Ex 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))
(square-list (list 1 2 3))

(define (square-list items)
  (map (lambda (item)
         (square item))
       items))
(square-list (list 1 2 3))

; Ex 2.22
; skip

; Ex 2.23
(define (for-each proc items)
  (proc (car items))
  (if (not (null? (cdr items)))
    (for-each proc (cdr items))))
(for-each
  (lambda (x)
    (newline)
    (display x))
  (list 57 321 88))

; 2.2.2 Hierarchical Structures


