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
  (is (equal '(nil :just-quote-it!) (multiple-value-list (%codewalk-dig-form '(dig nil)))))
  (is (equal '(nil :just-form-it!) (multiple-value-list (%codewalk-dig-form '(dig (inject a))))))
  (is (equal '(nil :just-form-it!) (multiple-value-list (%codewalk-dig-form '(dig 2 (inject 2 a))))))
  (is (equal '(((((inject d)) car cdr cdr cdr car) (((inject b) c (inject d)) car cdr car)) nil)
	     (multiple-value-list (%codewalk-dig-form '(dig (a (inject b) c (inject d)))))))
  (is (equal '(nil nil)
	     (multiple-value-list (%codewalk-dig-form '(dig (dig (a (inject b) c (inject d))))))))
  (is (equal '(((((inject 2 d)) car cdr cdr cdr car cdr car)) nil)
	     (multiple-value-list (%codewalk-dig-form '(dig (dig (a (inject b) c (inject 2 d)))))))))
  
(test transform
  (is (equal '(quote a) (transform-dig-form '(dig a))))
  (is (equal '(quote a) (transform-dig-form '(dig 2 a))))
  (is (equal 'a (transform-dig-form '(dig (inject a)))))
  (is (equal 'a (transform-dig-form '(dig 2 (inject 2 a))))))

(defun foo (b d)
  (dig (a (inject b) c (inject d))))

(test foos
  (is (equal '(a 1 c 2) (foo 1 2)))
  (is (equal '(a 100 c 200) (foo 100 200))))
