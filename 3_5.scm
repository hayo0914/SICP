; Streams

; Streams can mitigate some of the complexity of modeling
; state.

; From an abstract point of view, a stream is simply a
; sequene.

; Stream processing lets us model systems that have state 
; without ever using assignment or mutable data.
; This has important implications.

; --------------------
; 3.5.1 Streams Are Delayed Lists

; Streams are a clever idea that allows one to use
; sequence manipulations without incurring the costs of
; manipulating sequences as lists.

; The basic idea is to arrange to construct a
; stream only partially, and to pass the partial construction
; to the program that consumes the stream.
; If the consuper attempts to access a part of the stream
; that has not yet been constructed, the stream will
; automatically construct just enough more of itself ; to produce the required part, thus preserving ; the illusion that the entire stream exists.


; delay macro
(define-syntax delay
  (syntax-rules ()
                ((_ expr ...)
                 (lambda () expr ...))))
; force macro
(define-syntax force
  (syntax-rules ()
                ((_ x)
                 (x))))
; cons-stream macro
(define-syntax cons-stream
  (syntax-rules ()
                ((_ a b)
                 (cons a (delay b)))))

(define (stream-ref s n)
  (display s)
  (newline)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
  (delayed-object))

; Usage of delay and force
(define x 0)
(define f (delay
  (set! x (+ x 1))
  (display x)
  (newline)))

(force f)

; Use of cons-stream
(define s
  (cons-stream 1
               2))
(stream-car s)
(stream-cdr s)

; Ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; Ex 3.51
(define (show x)
  (display-line x)
  x)
(define x (stream-map
            show
            (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; Ex 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter
            even? seq))
(define z (stream-filter
            (lambda (x)
              (= (remainder x 5) 0))
            seq))

(stream-ref y 7)
(display-stream z)

(display-stream seq)
(display-stream (stream-enumerate-interval 1 20))

; --------------------
; 3.5.2 Infinite Streams

; We have seen how to support the illusion of
; manipulating streams as complete entities
; even though, in actuality, we compute only
; as much of the stream as we need to access.

; What is more striking, we can use streams
; to represent sequences that are infinitely long.

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(stream-car integers)
(stream-car (stream-cdr integers))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7)))
                                    integers))
(stream-ref no-sevens 30)

; Check Result
(display-stream-to no-sevens 30)

(define (display-stream-to s n)
  (define (it s c)
    (display (stream-car s))
    (newline)
    (if (< c n)
      (it
        (stream-cdr s) (+ c 1))))
  (it s 0))

(define (f n r)
  (if (= n 0)
    r
    (f (if (not (divisible? (+ r 1) 7)) (- n 1) n)
       (+ r 1))))
(f 30 1)

; --------------------
; The sieve of Eratosthenes
; which construct the infinite stream of
; prime numbers.

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 10)

; (sieve (s 2 3 4))

; (sieve 
;   (cons
;     2
;     (sieve (filter x:2 (s 3 4)))))

; (sieve 
;   (cons
;     2
;     (sieve (filter x:2 (s 3 4)))))

; (sieve 
;   (cons
;     2
;     (sieve
;        (filter x:2
;               (s 3 4)))))

; (sieve 
;   (cons
;     2
;     (cons
;       3
;       (sieve
;         (filter x: 3
;           (filter x:2
;             (s 3 4)))))

; (sieve 
;   (cons
;     2
;     (cons
;       3
;       (cons 4
;         (sieve
;           (filter x: 4
;             (filter x: 3
;               (filter x:2
;                 (s 3 4)))))
;                  

; --------------------
; Defining streams implicityly

(define ones (cons-stream 1 ones))

(stream-car ones)
(stream-car (stream-cdr (stream-cdr ones)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers
  (cons-stream 1
               (add-streams ones integers)))

(define (stream-map proc s1 s2)
  (if (or (stream-null? (car s1))
          (stream-null? (car s2)))
      the-empty-stream
      (cons-stream (proc (stream-car s1) (stream-car s2))
                   (stream-map
                     proc
                     (stream-cdr s1)
                     (stream-cdr s2)))))

(stream-car integers)
(stream-car (stream-cdr (stream-cdr integers)))

; Fibonacci numbers

(define fibs
  (cons-stream
    0
    (cons-stream
      1
      (add-streams
        (stream-cdr fibs)
        fibs))))

(define (display-stream-to s n)
  (define (it s c)
    (display (stream-car s))
    (newline)
    (if (< c n)
      (it
        (stream-cdr s) (+ c 1))))
  (it s 0))

(display-stream-to fibs 12)

; Exp
(define s
  (cons-stream 0
               (cons-stream 1
                            s)))
(display-stream-to s 5)
; 0 1 0 1 0


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(display-stream-to double 5)
; 1 2 4 8 16 32

; Test where a number n is prime
; by checking whethere n is divisible by
; a prime (not by just any integer)
; less than or equal to root n
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define (square x) (* x x))

(prime? 131)

; Ex 3.53
(define s (cons-stream 1 (add-streams s s)))
(display-stream-to s 5)

; Ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define integers
  (cons-stream 1
               (add-streams ones integers)))
(define factorials
  (cons-stream 1
               (mul-streams factorials
                            (stream-cdr integers))))

(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)

; Ex 3.55 - Ex3.62
; Skip

; --------------------
; 3.5.3 Exploiting the Stream Paradigm

; Streams with delayed evalueation
; can be powerful modeling tool,
; providing many of benefits of
; local state and assignment.

; Formulating iterations as stream processes

; In this sqrt, these gesses are the successive
; values of state variable.
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2.0))

(sqrt-improve (sqrt-improve 1 4) 4)

; Instead of it, we can generate the
; infinite stream of guesses.
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                   (lambda (guess)
                     (sqrt-improve guess x))
                   guesses)))
  guesses)
(display-stream-to (sqrt-stream 2) 7)

; Generate approximation to pi.
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams
                 (stream-cdr s)
                 (partial-sums s))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(display-stream-to pi-stream 100)

; The sequence accelerator converts a sequence
; of approximations to a new sequences
; that converges to the same value as the original,
; only faster.
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(display-stream-to (euler-transform pi-stream) 100)
