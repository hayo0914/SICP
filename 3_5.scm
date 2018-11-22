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


