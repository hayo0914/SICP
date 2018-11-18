; Concurrency: Time is of the Essence

; The central issue lurking beneath the complexity of state,
; sameness, and change is that by introducing assignment
; we are forced to admit time into our computational
; models.

; Making programs more modular, concurrent computation
; can provide a speed advantage over sequential computation

; Sequential computers execute only one operation at a time,
; so the amount of time it takes to perform a task
; is propotional to the total number of operations performed.

; Howerver, if it is possible to decompose a problem
; into pieces that are relatively independent and
; need to communicate only rarely, it may be possible to 
; allocate pieces to separate computing processors,
; producing a speed advantage proportional to the
; number of processors available.

; Unfortunately, the complexities introduced by 
; assignment become even more problematic in the
; precense of concurrency.

; --------------------
; 3.4.1 The nature of time in concurrent systems.

; The root of this complexity lies in the
; assignments to variables that are shared among
; the different processes.

; One possible restriction on concurrency would
; stipulate that no two operations that change any
; shared state variables can occur at the same time.
; This is an extremely strigent requirement.

; Ex 3.38
; Skip

; --------------------
; 3.4.2 Mechanisms for Controlling Concurrency

; Serializing access to shared state.
; We can use serialization to control access to
; shared variables.

; We ensure that no other procedure that assigns to
; the variable can run concurrently with this procedure
; by serializing all of these procedures with the
; same serializer.

; This guarantees that the value of the variable
; cannot be changed between an access and the
; correnspong assignment.

; --------------------
; Serializers in Scheme

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; Ex 3.39 - Ex 3.42
; Skip

; --------------------
; Complexity of using multiple shared resorces

; Exporting the serializer in this way gives us
; enough flexibility to implement a serialized
; exhange program.
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

; Ex 3.43 - Ex 3.45
; Skip

; --------------------
; Implementing serializers

; A mutex is an object that supports two operations
; - acuire
; - release

; In our implementation, each serializer has an
; associated mutex.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

; [Important]
; There is a crucial subtlety here, which is the essential
; place where concurrency control enters the system:
; The test-and-set! operation must be performed
; atomically.
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; The actual implementation of test-and-set! depends
; on the details of how our system runs concurrent processes.

; For example, we might be executing processes on a
; sequential processor using a "Time-slicing" mechanism
; that cycle through the processes.
; In that case, test-and-set! can work by disabling
; time slicing during the testing and setting.

; Alternatively, multiprocessing computers provide
; instructions that support atomic operaions directly
; in hardware.

; Ex 3.46 - Ex 3.47
; Skip

; --------------------
; Deadlock

; Ex 3.48 - Ex 3.49
; Skip

; --------------------
; Concurrency, time, and communication



