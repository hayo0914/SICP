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

' delay macro
(define-syntax delay
  (syntax-rules ()
                ((_ expr ...)
                 (lambda () expr ...))))

(define x 0)
(define f (delay
  (set! x (+ x 1))
  (display x)
  (newline)))

(force f)

; Ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))






   




