;;;; package.lisp

(defpackage #:quasiquote-2.0
  (:use #:cl #:iterate)
  (:export #:%codewalk-dig-form
	   #:dig #:inject #:splice #:odig #:oinject #:osplice))

