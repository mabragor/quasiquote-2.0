
(in-package #:quasiquote-2.0-tests)

(in-suite quasiquote-2.0)

(enable-quasiquote-2.0)

(defmacro define-sample-macro (name args &body body)
  `(defmacro ,name ,args
     `(sample-thing-to-macroexpand-to
       ,,@body)))

(define-sample-macro sample-macro-1 (x y)
  ,x ,y)

(define-sample-macro sample-macro-2 (&body body)
  ,@body)

(test macro-defined-macroexpansions
  (is (equal '(sample-thing-to-macroexpand-to a b) (macroexpand-1 '(sample-macro-1 a b))))
  (is (equal '(sample-thing-to-macroexpand-to a b c) (macroexpand-1 '(sample-macro-2 a b c)))))