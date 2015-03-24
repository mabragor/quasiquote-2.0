;;;; quasiquote-2.0.asd

(defpackage :quasiquote-2.0-system
  (:use :cl :asdf))

(in-package quasiquote-2.0-system)

(asdf:defsystem #:quasiquote-2.0
  :serial t
  :description "Writing macros that write macros. Effortless."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :depends-on (#:iterate)
  :version "0.1"
  :components ((:file "package")
               (:file "quasiquote-2.0")
	       (:file "macros")))

(defsystem :quasiquote-2.0-tests
  :description "Tests for QUASIQUOTE-2.0"
  :licence "MIT"
  :depends-on (:quasiquote-2.0 :fiveam)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :quasiquote-2.0))))
  (load-system :quasiquote-2.0)
  (funcall (intern "RUN-TESTS" :quasiquote-2.0)))
