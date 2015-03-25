

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

(defmacro define-inject-reader (name inject-symbol splice-symbol)
  `(defun ,name (stream char)
     (let ((anti-depth (1+ (read-n-chars stream char))))
       (if (equal 1 anti-depth)
	   (list (if (char= #\@ (peek-char nil stream t nil t))
		     (progn (read-char stream t nil t) ',splice-symbol)
		     ',inject-symbol)
		 (read stream t nil t))
	   (list (if (char= #\@ (peek-char nil stream t nil t))
		     (progn (read-char stream t nil t) ',splice-symbol)
		     ',inject-symbol)
		 anti-depth
		 (read stream t nil t))))))

(define-inject-reader inject-reader inject splice)
(define-inject-reader oinject-reader oinject osplice)


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
  
