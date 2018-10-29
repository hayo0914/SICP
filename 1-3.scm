;; 1.3 Formulating Abstractions with High-Order Procedures
;;     高階プロシージャによる抽象化
;; One of the things we should demand from a powerful programming language is 
;; the ability to build abstractions by assigning names to common patterns 
;; and then to work in terms of the abstractions directly. 
;; Procedures provide this ability. 
;; 
;; This is why all but most primitive programming language include
;; mechanism for defining procedures.

;; High-Order Procedures
;; Procedures that manipulate procedures called higher-order procedures.
;;

;; 1.3.1
(define (sum-integers a b)
  (if (> a b) 0
    (+ a (sum-integers (+ a 1) b))))
(sum-integers 1 3)

(define (cube a) (* a a a))
(define (sum-cubes a b)
  (if (> a b) 0
    (+ (cube a)
       (sum-cubes (+ 1 a) b))))
(sum-cubes 1 3)

;; pi-sum converges to pi/8 (very slowly)
(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))
(* (pi-sum 1 1000) 8)

;; Procedures as Arguments
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (cube a) (* a a a))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; 定積分：∫ x^3dx = [1/4 * x^4](1,0) = (1/4*1^4) - (1/4*0^4) = 0.25
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; 3x^2
(define (cube2 a)
  (* 3 (* a a)))
(integral cube2 0 10 0.01)
(sum cube 0 add-dx 0.01)

;; Exercise 1.29
;; Simpson's Rule
;; which is a more accurate method of numerical integration.

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(define (sr f a b n)
  (define (inc n) (+ n 1))
  (define h (/ (- b a) n))
  (define (y-val k)
    (f (+ a (* k h))))
  (define (term k)
    (* (y-val k) (if (even? k) 2 4)))
  (* h (/ (+ (sum term 1 inc (- n 1))
             (y-val 0) (y-val n))
          3)))

(sr cube 0 1 100)
; > (sr cube 0 1 100)
; 1/4

;; Exercise 1.30

;; Improved sum procedure to be a Iterative Process
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))
(define (sr f a b n)
  (define (inc n) (+ n 1))
  (define h (/ (- b a) n)) (define (y-val k)
    (f (+ a (* k h))))
  (define (term k)
    (* (y-val k) (if (even? k) 2 4)))
  (* h (/ (+ (sum term 1 inc (- n 1))
             (y-val 0) (y-val n))
          3)))
(sr cube 0 1 100)

;; Exercise 1.31

; a)
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
(define (factorial x)
  (define (inc a) (+ a 1))
  (define (term a) a)
  (product term 2 inc x))
(factorial 5)

; Calc approximations to Pi
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
(define (calc-pi x)
  (define (inc a) (+ a 1))
  (define (square a) (* a a))
  (define (term a)
    (let ((numer (* 2 a)))
      (/ (* numer (+ 2 numer))
         (square (+ numer 1)))))
  (* (product term 1 inc x) 4.0))
(calc-pi 1000)

; b)
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))
(define (calc-pi x)
  (define (inc a) (+ a 1))
  (define (square a) (* a a))
  (define (term a)
    (let ((numer (* 2 a)))
      (/ (* numer (+ 2 numer))
         (square (+ numer 1)))))
  (* (product term 1 inc x) 4.0))
(calc-pi 1000)

;; Exercise 1.32

; a)
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (identity a) a)
(define (inc a) (+ 1 a))
(define (f b)
  (sum identity 0 inc b))
(f 10)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
(define (product term a next b)
  (accumulate * 1 term a next b))
(define (calc-pi x)
  (define (inc a) (+ a 1))
  (define (square a) (* a a))
  (define (term a)
    (let ((numer (* 2 a)))
      (/ (* numer (+ 2 numer))
         (square (+ numer 1)))))
  (* (product term 1 inc x) 4.0))
(calc-pi 1000)

;; Exercise 1.33


