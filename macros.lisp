
(in-package #:quasiquote-2.0)

(defmacro define-dig-like-macro (name)
  `(defmacro ,name (n-or-form &optional (form nil form-p) &environment env)
     (if (not form-p)
	 `(,',name 1 ,n-or-form)
	 (let ((*env* env))
	   (transform-dig-form `(,',name ,n-or-form ,form))))))


(define-dig-like-macro dig)
(define-dig-like-macro odig)


