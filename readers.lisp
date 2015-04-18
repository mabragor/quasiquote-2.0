

(in-package #:quasiquote-2.0)

(defun read-n-chars (stream char)
  (let (new-char
	(n 0))
    (loop
       (setf new-char (read-char stream nil :eof t))
       (if (not (char= new-char char))
	   (progn (unread-char new-char stream)
		  (return n))
	   (incf n)))))

(defmacro define-dig-reader (name symbol)
  `(defun ,name (stream char)
     (let ((depth (1+ (read-n-chars stream char))))
       (if (equal 1 depth)
	   (list ',symbol (read stream t nil t))
	   (list ',symbol
		 depth
		 (read stream t nil t))))))

(define-dig-reader dig-reader dig)
(define-dig-reader odig-reader odig)

(defun expect-char (char stream)
  (let ((new-char (read-char stream t nil t)))
    (if (char= char new-char)
	t
	(unread-char new-char stream))))

(defun guess-injector-name (opaque-p macro-p all-p splicing-p)
  (intern (concatenate 'string
		       (if opaque-p "O" "")
		       (if macro-p "MACRO-" "")
		       (if splicing-p "SPLICE" "INJECT")
		       (if all-p "-ALL" ""))))

(defun inject-reader (stream char)
  (let ((anti-depth (1+ (read-n-chars stream char)))
	(extended-syntax (expect-char #\! stream)))
    (let ((injector-name (if (not extended-syntax)
			     (guess-injector-name nil nil nil (expect-char #\@ stream))
			     (guess-injector-name (expect-char #\o stream)
						  (expect-char #\m stream)
						  (expect-char #\a stream)
						  (expect-char #\@ stream)))))
      `(,injector-name ,@(if (not (equal 1 anti-depth)) `(,anti-depth))
		       ,(read stream t nil t)))))



(defvar *previous-readtables* nil)

(defun %enable-quasiquote-2.0 ()
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-macro-character #\` #'dig-reader)
  (set-macro-character #\, #'inject-reader)
  (values))

(defun %disable-quasiquote-2.0 ()
  (if *previous-readtables*
      (setf *readtable* (pop *previous-readtables*))
      (setf *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-quasiquote-2.0 ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-quasiquote-2.0)))
(defmacro disable-quasiquote-2.0 ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-quasiquote-2.0)))
  
