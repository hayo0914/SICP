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

; 2.1.2 Abstraction Barriers

; Exercise 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (midpoint-segment s)
  (define (average x y) (/ (+ x y) 2.0))
  (make-point
    (average (x-point (start-segment s))
             (x-point (end-segment s)))
    (average (y-point (start-segment s))
             (y-point (end-segment s)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(print-point 
  (midpoint-segment
    (make-segment
      (make-point 2 3)
      (make-point 10 24))))

; Exercise 2.3
; skipped easy problem

; 2.1.3 What is meant by data ?

; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

; Exercise 2.5
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (devides? a b)
  (= (remainder a b) 0))
(define (count-division n divisor)
  (define (iter z divisions)
    (if (devides? z divisor)
      (iter (/ z divisor) (+ divisions 1))
      divisions))
  (iter n 0))
(define (car z)
  (count-division z 2))
(define (cdr z)
  (count-division z 3))
(car (cons 33 22))
(cdr (cons 33 22))

; Ex 2.6
(define zero
  (lambda (f)
    (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
(define (inc n) (+ n 1))

((add-1 zero) inc)

(((add-1 zero) inc) 3)
;
; ((((lambda (add-1 n)
;       (lambda (f)
;         (lambda (x)
;           (f ((n f) x)))))
;     zero) inc) 3)
;
; ((((lambda (f)
;       (lambda (x)
;         (f ((zero f) x))))
;     inc) 3)
;
; (((((lambda (x)
;        (inc ((zero inc) x))))
;     3)
;
; (((((lambda (x)
;        (inc ((zero inc) x))))
;     3)
;
; (inc ((zero inc) 3))
;
; (inc ((lambda (x) x)) 3))
;
; (inc (3))
;
; (+ 3 1)
;
; 4

; 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; Ex 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(div-interval (make-interval 10 15)
              (make-interval 20 30))

; Ex 2.8
; The minimum value would be the smallest
; possible value.
; The maximum value would be the largest
; possible value.
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
(sub-interval (make-interval 30 55)
              (make-interval 20 30))

; Ex 2.9
(define (print-bound x)
  (display (format "[~a,~a]\n"
                   (lower-bound x)
                   (upper-bound x))))
(define (width-bound x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2.0))
(define (print-width x)
  (print-bound x)
  (display (format "width=~a\n"
                   (width-bound x))))

(define x (make-interval 10 20))
(define y (make-interval 5 10))

(print-width x)
; [10,20]
; width=5.0
(print-width y)
; [5,10]
; width=2.5

(print-width (add-interval x y))
; [15,30]
; width=7.5

(print-width (sub-interval x y))
; [0,15]
; width=7.5

; Width for added interval.
; (xH + yH) - (xL + yL)
; = (xH - xL) + (yH - yL)

; Width for subbed interval.
; (xH - yL) - (xL - yH)
; = (xH - xL) + (yH - yL)
; it's same as the result of added interval.

(print-width (mul-interval x y))
; [50,200]
; width=75.0

; Ex 2.10
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (>= 0 (* (upper-bound y)
               (lower-bound y)))
    "Error(Interval spans 0)"
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(div-interval
  (make-interval 10 20)
  (make-interval -10 20))

; Ex 2.11
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
(define (mul-interval x y)
  (let ((xL (lower-bound x))
        (xH (upper-bound x))
        (yL (lower-bound y))
        (yH (upper-bound y)))
    (cond ((and (<= 0 xL)
                (<= 0 xH)
                (> 0 yL)
                (> 0 yH))
           ;; ++ * --
           (make-interval (* xL yL)
                          (* xH yH)))
          ((and (<= 0 xL)
                (<= 0 xH)
                (> 0 yL)
                (<= 0 yH))
           ;; ++ * -+
           (make-interval (* xL yL)
                          (* xH yH)))
          ((and (<= 0 xL)
                (<= 0 xH)
                (<= 0 yL)
                (<= 0 yH))
           ;; ++ * ++
           (make-interval (* xL yL)
                          (* xH yH)))
          ((and (> 0 xL)
                (<= 0 xH)
                (> 0 yL)
                (> 0 yH))
           ;; -+ * --
           (make-interval (* xH yL)
                          (* xL yL)))
          ((and (> 0 xL)
                (<= 0 xH)
                (> 0 yL)
                (<= 0 yH))
           ;; -+ * -+
           (make-interval
             (min (* xL yH) (* xH yL))
             (max (* xH yH) (* xL yL))))
          ((and (> 0 xL)
                (<= 0 xH)
                (<= 0 yL)
                (<= 0 yH))
           ;; -+ * ++
           (make-interval (* xL yH)
                          (* xH yH)))
          ((and (> 0 xL)
                (> 0 xH)
                (> 0 yL)
                (> 0 yH))
           ;; -- * --
           (make-interval (* xH yH)
                          (* xL yL)))
          ((and (> 0 xL)
                (> 0 xH)
                (> 0 yL)
                (> 0 yH))
           ;; -- * --
           (make-interval (* xH yH)
                          (* xL yL)))
          ((and (> 0 xL)
                (> 0 xH)
                (> 0 yL)
                (<= 0 yH))
           ;; -- * -+
           (make-interval (* xL yH)
                          (* xL yL)))
          ((and (> 0 xL)
                (> 0 xH)
                (<= 0 yL)
                (<= 0 yH))
           ;; -- * ++
           (make-interval (* xL yH)
                          (* xH yL))))))
(define (div-interval x y)
  (if (>= 0 (* (upper-bound y)
               (lower-bound y)))
    "Error(Interval spans 0)"
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (print-interval x)
  (display (format "[~a,~a]\n"
                   (lower-bound x)
                   (upper-bound x))))

(div-interval
  (make-interval 10 20)
  (make-interval -10 20))

(print-interval (mul-interval
                  (make-interval -15 25)
                  (make-interval -5 20)))
(print-interval (mul-interval
                  (make-interval 15 25)
                  (make-interval -15 -5)))
(print-interval (mul-interval
                  (make-interval -20 -15)
                  (make-interval -15 -5)))

; Ex 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-interval
      (- c width)
      (+ c width))))
(define (percent i)
  (let ((c (center i)))
    (if (= c 0)
      (display "Error(center is 0)")
      (abs (* (/ (width i) c)
              100.0)))))

; Quick checks
(print-interval (make-center-percent 35 20))
(percent (make-center-percent 35 20))
(percent (make-interval -10 10))
(percent (make-interval 80 100))
(percent (make-interval 10 20))

; Ex 2.13
;
; Pt(a*b)
; = ((abH - abL)/2) / center(a*b)
; = (abH - abL) / (abH + abL)
; = [a(1 + P1) * b(1 + P2) - a(1 - P1) * b(1 - P2)]
;   / [a(1 + P1) * b(1 + P2) + a(1 - P1) * b(1 - P2)]
; = 2ab[P1 + P2] / 2ab[1 + P1P2)
; = (P1 + P2) / (1 + P1P2)
; よってP1>0, P2>0かつP1,P2が十分小さい場合,
; 区間誤差はP1 + P2に近似する.

; 確認
(percent
  (mul-interval
    (make-center-percent 35 8)
    (make-center-percent 50 5)))
(/ (+ 0.08 0.05) (+ 1 (* 0.08 0.05)))




