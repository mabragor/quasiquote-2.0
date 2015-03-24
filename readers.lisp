

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
       (list ',symbol
	     depth
	     (read stream t nil t)))))

(define-dig-reader dig-reader dig)
(define-dig-reader odig-reader odig)

(defmacro define-inject-reader (name inject-symbol splice-symbol)
  `(defun ,name (stream char)
     (let ((anti-depth (1+ (read-n-chars stream char))))
       (list (if (char= #\@ (peek-char nil stream t nil t))
		 ',splice-symbol
		 ',inject-symbol)
	     anti-depth
	     (read stream t nil t)))))

(define-inject-reader inject-reader inject splice)
(define-inject-reader oinject-reader oinject osplice)


(defvar *previous-readtables* nil)
