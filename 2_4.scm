; 2.4 抽象データの多重表現
; ソフトウェアは長期にわたり, 大勢の人で設計するため
; 要求は時間とともに変わる.
; そういう環境では, データ表現についての選択を
; 誰もが前もって同意することはできない.
; そこで
; - 表現を使用から隔離するデータ抽象の壁
; に加えて,
; - 異なる設計選択を相互に隔離し, 一つのプログラムの中に
;   異なる選択が共存できるようにする抽象の壁
; が必要になる.
; 大きいプログラムは別々に設計された既存の部品を組み合わせ
; 作り出すから, プログラマが部品を加法的(additively),
; つまり部品を再設計, 再実装せずに, より大きいシステムへと
; 組み立てる方法が必要である.

(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))
(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y)
  (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a))
        (* r (sin a))))

(make-from-real-imag 10 5)



; 2.4.2 タグ付きデータ(Tagged Data)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
    (eq? (type-tag z) 'rectangular))
(define (polar? z)
    (eq? (type-tag z) 'polar))



