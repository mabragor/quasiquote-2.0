
(in-package #:quasiquote-2.0)

(defmacro dig (n-or-form &optional (form nil form-p))
  (if (not form-p)
      `(dig 1 ,n-or-form)
      (transform-dig-form `(dig ,n-or-form ,form))))


