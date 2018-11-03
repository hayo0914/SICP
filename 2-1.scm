;; 2 Abstraction using data
; 
; We want compound data and compound procedure 
; for the same reasons:
; - to elevate the conceptual level at which 
;   we can design our programs
; - to increase the modularity of our designs
; - to enhance the expressive power of our 
;   language.
;
; the ability to construct compound data objects 
; enables us to deal with data at a higher 
; conceptual level

; 2.1 Data Abstraction
; The basic idea it to structure the programs
; that are to use compound objects so that
; they operate on 'abstract data'.
;
; Our programs should use data without
; assumptions about the data that are not
; strictly necessary for performing the task
; at hand.
;
; At the same time, the concrete data
; representation is defined independant of 
; the programs that use the data.
;
; The interface between these two parts
; will be a set of procedures, called
; "selectors" and "constructors", that
; implement the abstract data.

; 2.1.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-harf (make-rat 1 2))
(print-rat one-harf)
(define one-third (make-rat 1 3))
(print-rat one-third)
(print-rat (add-rat one-harf one-third))
(print-rat (mul-rat one-harf one-third))
(print-rat (add-rat one-third one-third))

; Exercise 2.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< n 0) (< d 0))
               (and (> n 0) (> d 0)))
           (cons (abs (/ n g)) (abs (/ d g))))
           ((< n 0)
            (cons (/ n g) (/ d g)))
           (else
             (cons (* -1 (/ n g))
                   (* -1 (/ d g)))))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
(print-rat
  (mul-rat (make-rat 1 -2) (make-rat 2 3)))

;; Improve version
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d)
  (let ((g (* (if (< d 0) -1 1) (gcd n d))))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
(print-rat
  (mul-rat (make-rat 1 -2) (make-rat 2 3)))

