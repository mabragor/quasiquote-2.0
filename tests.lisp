(in-package :cl-user)

(defpackage :quasiquote-2.0-tests
  (:use :cl :quasiquote-2.0 :fiveam)
  (:export #:run-tests))

(in-package :quasiquote-2.0-tests)

(def-suite quasiquote-2.0)
(in-suite quasiquote-2.0)

(defun run-tests ()
  (let ((results (run 'quasiquote-2.0)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (equal '(nil :just-quote-it!) (multiple-value-list (%codewalk-dig-form '(dig nil))))))
