#lang racket

;; 1.1
;; Combining simple ideas to form more complex ideas.
;; Every powerful language has three mechanism for accomplishing this.
;; - primitive expressions
;; - means of combination
;; - means of abstraction

;; 1.1.2
;; A critical aspect of a programming language is the means it provides ;; for using names to refer computational objects.
;; The name identifies a variable whose value is the object.
;; defineは言語の最も単純な抽象化の手段

;; 1.1.3
;; Isolate issues about thinking procedually.
;; As a case in point, in evaluating combinations, the interpreter itself following a procedure.
;; To evaluate combination, do the following.
;; 1. Evaluate the subexpressions(部分式) of the combination
;;    部分式の評価
;; 2. Apply the procedure that is the value of left most subexpression(the operator)
;;    to the arguments that are the value of the other expressions(the operands)
;;    手続きへの適用(オペレータにオペランドを適用する)

;; 1.1.4
;; Compound precedure(複合手続き)
;; (define (<name> <formal parameters>)
;;   <body>)
;; The <formal parameters>(形式パラメタ) are the names used within the 
;; body of the procedure to refer to the corresponding arguments of the procedure.

;; 1.1.5
;; For compound procedures, the application process is as follows.
;; - evaluate the body to of the procedure with each formal parameter
;;   replaced by the correspoinding arguments.
;;
;; The process we have described is called 
;; the substition model for procedure application(置換モデル, 代入モデル)
;; 
;; - Applicative order(適用順)
;;   引数を評価してから適用する
;;   evaluate the argumens and then apply
;; - Normal order(正規順)
;;   完全に展開してから簡約する
;;   Fully expand and then reduce
;;
;; Lisp uses applicative-order evaluation.
;; On the other hand, normal-order evaluation can be an extremely 
;; valuable tool.

;; 1.1.6
;; case analysis(ケース分析, 事例分析)
;; cond("conditional")
;; 形式
;; (cond (<p1> <e1>)
;;       (<p2> <e2>)
;;       ...
;;       (<pn> <en>))
;; 複数の式のペア(<p> <e>)はclauses(クローズ, 節)と呼ばれる
;; 各ペアの最初の式はpredicate(述語: 値が真か偽になる式)
;; 条件式は次のように評価される.
;; 1. 述語<p1>を最初に評価する
;; 2. その値が真であれば, <p2>が評価される
;; 3. その値が偽であれば, その次は<p3>が評価される.
;; 真となる場合, インタプリタは対応するクローズの
;; consequent expressions(結果式) <e>の値を条件式の値として返す.
;; 真となる<p>が見つからない場合, condの値は未定義.
;;
;; 

;; Exercise 1.3
(define (test x y z)
  (cond ((and (>= x z) (>= y z)) (+ x y))
        ((and (>= x y) (>= z y)) (+ x z))
        (else (+ y z))))
(test 1 2 3)
(test 2 3 1)
(test 3 2 1)
(test 3 3 3)
(test 5 3 3)
(test 3 5 3)
(test 3 3 5)

;; Exercise 0.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p))

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
  (< (abs (- (square guess) x)) 1e-8))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (test-sqrt x)
  (define result
    (sqrt x))
  (display (format "x = ~a\n" x))
  (display (format "result: ~a\n" x))
  (display (format "diff: ~a\n" (-(square result)
                                  x))))
(test-sqrt 2)
(test-sqrt 1)
(test-sqrt 0.001)
(test-sqrt 0.0001)
(test-sqrt 0.00001)
(test-sqrt 0.000001)
(test-sqrt 0.0000001)
(test-sqrt 0.00000001)


;; Exercise 1.6
; this does not work
(define (square x)
  (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 1e-8))
(define (new-if predicate then-clause else-clause)
  (print predicate)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    ((print x)(sqrt-iter (improve guess x) x))))
(sqrt 9)

;; Exercise 1.7
(define (square x)
  (* x x))
(define (sqrt-iter guess x)
  (sqrt-iter2 guess x (* 2 guess)))
(define (sqrt-iter2 guess x prior-guess)
  (if (good-enough?-change guess x prior-guess)
    guess
    (sqrt-iter2 (improve guess x) x guess)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.01))
(define (good-enough?-change guess x prior-guess)
  (< (/ (abs (- guess prior-guess)) guess) 0.0000001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (test-sqrt x)
  (define result
    (sqrt x))
  (display (format "x = ~a\n" x))
  (display (format "result: ~a\n" result))
  (display (format "diff: ~a\n\n" (-(square result)
                                  x))))
(test-sqrt 2)
(test-sqrt 1)
(test-sqrt 0.001)
(test-sqrt 0.0001)
(test-sqrt 0.00001)
(test-sqrt 0.000001)
(test-sqrt 0.0000001)
(test-sqrt 0.00000001)

;; Exercise 1.8 (Using Newton Method)
(define (square x)
  (* x x))
(define (cube x)
  (* (square x) x))
(define (cube-iter guess x)
  (cube-iter2 guess x (* 2 guess)))
(define (cube-iter2 guess x prior-guess)
  (if (good-enough?-change guess x prior-guess)
    guess
    (cube-iter2 (improve guess x) x guess)))
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough?-change guess x prior-guess)
  (< (/ (abs (- guess prior-guess)) guess) 1e-8))
(define (sqrt x)
  (cube-iter 1.0 x))
(define (test-cube x)
  (define result
    (sqrt x))
  (display (format "x = ~a\n" x))
  (display (format "result: ~a\n" result))
  (display (format "diff: ~a\n\n" (-(cube result)
                                  x))))
(test-cube 27)
(test-cube 8)
(test-cube 1)
(test-cube 0.01)
(test-cube 0.0001)

;; 1.1.8
;; A formal parameter of a procedure has a special role in procedure definition,
;; in that it doesn't matter what name the formal parameter has.
;; Such a name is called "a bound variable(束縛変数)"
;; The parameters are local to the bodies of the respective procedures.
;;
;; The set of expressions for which a biding defines a name is called
;; "the scope" of that name.
;; In a procedure definition, the bound variables declared as the formal
;; parameters of the procedure have the body of the procedure as their scope.

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
(sqrt 10)

;; Such nesting of definitions, called "Block Structure", is basically
;; the right solution to the simplest name-packaging problem.
;; 
;; We can simplify them. Since "x" is bound in the definition of "sqrt",
;; the auxilary procedures are in the scope of "x".
;; Thus, it's not necessary to pass x explicitly to each of these procedures.
;; Instead, we allow x to be free variable in the internal definitions.
;; This discipline is called "Lexical Scoping".
;; 補足) 自由変数とは関数の中で参照される局所変数や引数以外の変数を意味する。

(define (average x y)
  (/ (+ x y) 2))
(define (square x)
  (* x x))
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
(sqrt 10)

;; We will use "Block Structure" extensively to help us break up
;; large programs into tractable pieces.

