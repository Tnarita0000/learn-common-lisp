;; ===============
;; 1. Introduction
;; ===============
;; LET: 新しい変数を定義する
;;      LETの変数のスコープは, 変数を導入したフォームによって制限される(Lexical scope).
;;      
;; -- 構文 --
;; (let (variable*)
;;   body-form*)
;;
;; 例)
;;   (let ((x 10) (y 20) z)
;;     ...)

(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter LET: ~a~%" x))
(foo 5)
;; Parameter: 5
;; Outer LET: 2
;; Inner LET: 3
;; Outer LET: 2
;; Parameter: 5
;; NIL

(dotimes (x 10) (format t "~d " x))
;;0 1 2 3 4 5 6 7 8 9
;; NIL

;; LET*: 各変数が参照する初期値のフォームで,
;;       変数リストの中で前に出てきたものを使うことができる.
(let* ((x 10)
       (y (+ x 10)))
  (list x y))
;; (10 20)

;; ===============================
;; 2. Lexical Variable and Closure
;; ===============================
;;
;; ex) Lexical scope変数であるcountを内部的に使用する無名関数(LAMBA)をfnとして定義.
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(funcall *fn*)
;; 1
(funcall *fn*)
;; 2
(funcall *fn*)
;; 3

;; ex) 3つのclosureからなるリストを返す.
;;     1. 閉じ込めたcountの束縛を増やしていくclosure
;;     2. 減らしていくclosure
;;     3. 現在の値を返すclosure
(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))

;; =============================================
;; 3. Dincamic Variable.(a.k.a Special Variable)
;; =============================================
;;
;; DEFVAR:       変数が未定義だった場合のみ初期値を代入するグローバル変数を定義する.
;; DEFPARAMETER: 評価されるたびに常に初期値を代入するグローバル変数を定義する.
(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))
(foo)
;; X: 10
;; NIL
(let ((*x* 20)) (foo))
;; X 20
;; NIL

;; ===========
;; 4. Constant
;; ===========

;; DEFCONSTANT: 定数を定義する.
;;              定数変数はすべてグローバルであり, 束縛し直すことはできない.
;; (deconstant name initial-value-form)

;; =============
;; 5. Assignment
;; =============
;;
;; SETF: 汎用的に使える代入オペレータ.
;; (setf place value)

;; 束縛に新しい値を代入しても他の束縛には何の影響も与えない.
(defun foo (x) (setf x 10))
(let ((y 20))
  (foo y)
  (print y))
;; 20
