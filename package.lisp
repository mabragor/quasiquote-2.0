;;;; package.lisp

(defpackage #:quasiquote-2.0
  (:use #:cl #:iterate)
  (:export #:%codewalk-dig-form #:transform-dig-form
	   #:dig #:inject #:splice #:odig #:oinject #:osplice
	   #:macro-inject #:omacro-inject #:macro-splice #:omacro-splice
	   #:enable-quasiquote-2.0 #:disable-quasiquote-2.0))


