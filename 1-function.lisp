;; Function's statement
;;
;; -- 構文 --
;; (defun name (parameter*)
;;   "Optional documentation text"
;;   body-form*)
;;
;; -- 呼び出し --
;; (name (parameter*))

;; ===============
;; 1. Introduction
;; ===============
(Defun hello-world () (format t "Hello, world~%"))
(hello-world)
;;> Hello, world

(defun verbose-sum (x y)
  "After print message, add two numbers"
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
(verbose-sum 2 3)
;;> Summing 2 and 3.

;; ==================================
;; 2. Optional parameters [&optional]
;; ==================================
(defun foo (a b &optional c d) (list a b c d))
(foo 'a 'b)
;;> (A B NIL NIL)
(foo 'a 'b 'c 'd)
;;> (A B C D)

;; with default value
(defun foo-with-default-value (a &optional (b 10)) (list a b))
(foo-with-default-value 'a)
;;> (A 10)
(foo-with-default-value 'a 'b)
;;> (A B)

;; with same default value
(defun make-rectangle (width &optional (height width))
  (list height width))
(make-rectangle 100)
;;> (100 100)
(make-rectangle 100 200)
;;> (100 200)

;; supplied parameter
;;   `-supplied-p`というprefixを付与するのがsupplied parameterの規則.
;;   もし呼び出し元でoptional parameterへ引数を指定していれば T,
;;   そうでなければ NIL を返す.
(defun foo-with-supplied-param (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
(foo-with-supplied-param 1 2)
;;> (1 2 3 NIL)
(foo-with-supplied-param 1 2 3)
;;> (1 2 3 T)
(foo-with-supplied-param 1 2 4)
;;> (1 2 4 T)

;; =========================
;; 3. Rest parameter [&rest]
;; =========================
(defun foo-rest (&rest numbers)
  (list numbers))
(foo-rest)
;;> (NIL)
(foo-rest 1 'a 'b)
;;> ((1 A B))

;; ===========================
;; 4. Keyword parameter [&key]
;; ===========================
(defun foo-key (&key a b c)
  (list a b c))
(foo-key :a 1)
;;> (1 NIL NIL)
(foo-key :a 2 :c 10)
;;> (2 NIL 10)

;; =========================
;; 5. Function Return Values
;; =========================

;; RETURN-FROM: 関数の途中から値を返す.
(defun fuga (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
	(return-from fuga (list i j))))))

;; ==================================================
;; 6. Functions As Data, a.k.a. Higher-Order Functions
;;    データとしての関数, または高階関数
;; ===================================================

;; FUNCTION: 関数オブジェクトを取得する.
;;           「'#」の構文糖.
(defun foo (x) (* 2 x))
(function foo)
;;> #<FUNCTION FOO>
#'foo
;;> #<FUNCTION FOO>

;; FUNCALL: 関数オブジェクトを通じて関数を呼び出す.
(funcall #'foo 3)
;;> 6

;; FUNCALL example)
(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))
(plot #'exp 0 4 1/2)
;;> *
;;> **
;;> ***
;;> *****
;;> ********
;;> *************
;;> *********************
;;> **********************************
;;> *******************************************************
;;> NIL

;; ======================
;; 7. Anonymous Functions
;; ======================
;; LAMBDA: 無名関数を生成する
;;   (lambda (parameters) body)

(funcall #'(lambda (x y) (+ x y)) 2 3)
;;> 5

(plot #'(lambda (x) (* 2 x)) 0 10 1)
;;> **
;;> ****
;;> ******
;;> ********
;;> **********
;;> ************
;;> **************
;;> ****************
;;> ******************
;;> ********************
;;> NIL
