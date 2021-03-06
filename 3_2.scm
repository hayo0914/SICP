; 3.2 The Environment Model of Evaluation

; In the presence of assignment, a variable can no longer
; be cosidered to be merely a name for a value.

; In structure, called "environments", a variable 
; designate a place in which values can be stored.

; Indeed, one could say that expressions in a
; programming language do not have any meaning.
; Rather, an expressions acquires a meaning
; only with respect to some environment in which
; it is evaluated.

; 3.2.1 The Rules for Evaluation

; 3.2.2 Applying Simple Procedures
; 3.2.3
; 3.2.4 Internal Definition

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))



