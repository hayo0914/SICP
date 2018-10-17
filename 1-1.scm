#lang racket

;; 1.1
;; Combining simple ideas to form more complex ideas.
;; Every powerful language has three mechanism for accomplishing this.
;; - primitive expressions
;; - means of combination
;; - means of abstraction

;; 1.1.2
;; A critical aspect of a programming language is the means it provides
;; for using names to refer computational objects.
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
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
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


