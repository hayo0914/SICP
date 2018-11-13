; 3 Modularity, Objects, and State
; - Abstraction is vital in helping us to cope with
;   the complexity of large systems
; - We need strategies to help us structure large
;   systems so that they will be modular, that is,
;   so that they can be divided "naturally" into
;   coherent parts that can be separately developed and
;   maintained.
;
; Two different organizational strategy
; - the object-based approach
; - the stream-processing approach

; 3.1 Local State (局所状態変数)
; Objectの状態を保存する.

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

; We can make balance internal to withdraw
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; Specify an initial amount of money in the account
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
(define W1 (make-withdraw 120))
(define W2 (make-withdraw 50))
(W1 50)
(W2 30)
(W1 20)
(W2 10)

; A procedure that returns a bank-account object
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; Take message as input
  ; This is precisely the message-passing style 
  ; of programming
  ; 
  ; Tye key idea of data-directed programming
  ; is to handle generic operations in programming
  ;
  ; オブジェクトにメッセージを送ると,
  ; 適切な手続きにディスパッチされる.
  ; 型が複数存在する場合は, 型, 汎用手続きを
  ; 実際の手続きに関連付けるテーブルが必要となる.
  (define (dispatch m) 
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 20)
((acc 'withdraw) 50) 
((acc 'deposit) 500) 


; Experiment
(define (f x)
  (define (Hello)
    (display "hello ")
    (display x)
    (display " !!")
    (newline))
  hello)
(define obj (f "Tom"))
(obj)
; -> Hello Tom!!

; Ex 3.1

(define (make-accumulator x)
  (define (add v)
    (set! x (+ x v))
    x)
  add)
(define A (make-accumulator 5))
(A 10)
(A 10)

; Ex 3.2
(define make-monitored
  (let ((count 0))
    (lambda (f)
      (lambda (m)
        (cond
          ((eq? m 'how-many-calls?) count)
          ((eq? m 'reset-count) (set! count 0))
          (else (f m) (set! count (+ 1 count))))))))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 100)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 100)
(s 100)
(s 'how-many-calls?)

; Ex 3.3 ~ Ex 3.4
; Skip

; 3.1.2 The benefits of introducing assignment

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))


(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; Ex 3.5 ~ Ex 3.6
; Skip


; 3.1.3 The Costs of Introducing Assignment
; 関数型プログラミングは代入を使わない

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
(D1 10)
(D2 10)
(D1 5)

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
(W1 20)
(W1 20)
(W2 10)

; 代入を多用するプログラミングは, 
; 命令的プログラミング(imperative programming)という.
; 命令形の流儀で書いたプログラムは, 
; 関数型プログラムでは発生し得ないバグを入れやすい.

; 一般的に代入を使うプログラムでは, 代入の
; 相対的順序を注意深く考え, 正しいバージョンの変数を
; 使うよう注意深く確認しなければならない.
; こういうことは関数型プログラミングにはない.

; Ex 3.7 ~ Ex 3.8
; Skip


