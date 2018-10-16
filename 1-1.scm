#lang racket

;; 1.1.4
;; 複合手続き(compound precedure)の定義
;; (define (<name> <formal parameters>)
;;   <body>)

;; 1.1.5

;; 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))
(sqrt 1000000)
(sqrt 13904823)

; this does not work
(define (new-if predicate then-clause else-clause)
  (print predicate)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(sqrt 9)


