;;;; quasiquote-2.0.lisp

(in-package #:quasiquote-2.0)


(defmacro nonsense-error (str)
  `(error ,(concatenate 'string
			str
			" appears as a bare, non DIG-enclosed form. "
			"For now I don't know how to make sense of this.")))

(defmacro define-nonsense-when-bare (name)
  `(defmacro ,name (n-or-form &optional form)
     (declare (ignore n-or-form form))
     (nonsense-error ,(string name))))

(define-nonsense-when-bare inject)
(define-nonsense-when-bare oinject)
(define-nonsense-when-bare splice)
(define-nonsense-when-bare osplice)

(defparameter *depth* 0)


(defparameter *injectors* nil)

;; (defmacro with-injector-parsed (form)
;;   `(let ((kwd (intern (string 

(defun reset-injectors ()
  (setf *injectors* nil))

(defun injector-form-p (form)
  (and (consp form)
       (eq (car form) 'inject)))

(defun injector-level (form)
  (if (equal 2 (length form))
      1
      (cadr form)))

(defun injector-subform (form)
  (if (equal 2 (length form))
      (values (cdr form) '(cdr))
      (values (cddr form) '(cddr))))

(defun transparent-p (form)
  (or (eq (car form) 'dig)
      (eq (car form) 'inject)))

(defun look-into-injector (form path)
  (let ((*depth* (- *depth* (injector-level form))))
    (multiple-value-bind (subform subpath) (injector-subform form)
      (search-all-active-sites subform (append subpath path) nil))))

(defun dig-form-p (form)
  (and (consp form)
       (eq 'dig (car form))))

(defun look-into-dig (form path)
  (let ((*depth* (+ *depth* (injector-level form))))
    (multiple-value-bind (subform subpath) (injector-subform form)
      (search-all-active-sites subform (append subpath path) nil))))

(defun search-all-active-sites (form path toplevel-p)
  (format t "SEARCH-ALL-ACTIVE-SITES: got form ~a~%" form)
  (if (not form)
      nil
      (if toplevel-p
	  (cond ((atom (car form)) (format t "imhere~%") :just-quote-it!)
		((injector-form-p (car form)) (if (equal *depth* (injector-level (car form)))
						  :just-form-it!
						  (if (transparent-p (car form))
						      (look-into-injector (car form) (cons 'car path)))))
		((dig-form-p (car form))
		 (format t "Got dig form ~a~%" form)
		 (if (transparent-p (car form))
		     (look-into-dig (car form) (cons 'car path))))
		(t (search-all-active-sites (car form) (cons 'car path) nil)
		   (search-all-active-sites (cdr form) (cons 'cdr path) nil)))
	  (when (consp form)
	    (cond ((dig-form-p (car form))
		   (format t "Got dig form ~a~%" form)
		   (if (transparent-p (car form))
		       (look-into-dig (car form) (cons 'car path))))
		  ((injector-form-p (car form))
		   (format t "Got injector form ~a ~a ~a~%" form *depth* (injector-level (car form)))
		   (if (equal *depth* (injector-level (car form)))
		       (progn (push (cons form (cons 'car path)) *injectors*)
			      nil)
		       (if (transparent-p (car form))
			   (look-into-injector (car form) (cons 'car path)))))
		  (t (search-all-active-sites (car form) (cons 'car path) nil)))
	    (search-all-active-sites (cdr form) (cons 'cdr path) nil)))))

	  
	      
(defun codewalk-dig-form (form)
  (reset-injectors)
  (let ((it (search-all-active-sites form nil t)))
    (values *injectors* it)))

(defun %codewalk-dig-form (form)
  (if (not (dig-form-p form))
      (error "Supposed to be called on dig form")
      (let ((*depth* (+ (injector-level form) *depth*)))
	(codewalk-dig-form (injector-subform form)))))

(defun path->setfable (path var)
  (let ((res var))
    ;; First element is artifact of extra CAR-ing
    (iter (for spec in (cdr (reverse path)))
	  (setf res (list spec res)))
    res))

(defun tree->cons-code (tree)
  (if (atom tree)
      `(quote ,tree)
      `(cons ,(tree->cons-code (car tree))
	     ,(tree->cons-code (cdr tree)))))

(defun transform-dig-form (form)
  (let ((the-form (copy-tree form)))
    (multiple-value-bind (site-paths cmd) (%codewalk-dig-form the-form)
      (cond ((eq cmd :just-quote-it!)
	     `(quote ,(car (injector-subform the-form))))
	    ((eq cmd :just-form-it!)
	     (car (injector-subform (car (injector-subform the-form)))))
	    (t (let ((gensyms (mapcar (lambda (x)
					(declare (ignore x))
					(gensym "INJECTOR"))
				      site-paths))
		     (g!-list (gensym "LIST")))
		 (iter (for (site . path) in site-paths)
		       (for gensym in gensyms)
		       (collect `(,gensym ,(car (injector-subform (car site)))) into lets)
		       (setf (car site) nil)
		       (collect `(setf ,(path->setfable path g!-list) ,gensym) into setfs)
		       (finally (return `(let ,lets
					   (let ((,g!-list ,(tree->cons-code (car (injector-subform the-form)))))
					     ,@setfs
					     ,g!-list)))))))))))
    
      
;;       (labels ((rec (smth)
;; 		 (if (and (consp smth)
;; 			  (consp (car smth)))
;; 		     (cond ((eq 'dig (caar smth) (let ((*depth* (1+ *depth*)))
;; 						   (rec (cdr smth))))
;; 			    ;; OK, let's try to write inject first
;; 			    (eq 'inject (caar smth) ...)
;; 			    (t (rec (car smth))
;; 			       (rec (cdr smth))))))))
;; 	(rec form))))


;; How this macroexpansion should work in general?
;;   * We go over the cons-tree, keeping track of the depth level, which is
;;   controlled by DIG's
;;   * Once we find the INJECT with matching level, we remember the place, where
;;     this happens
;;   * We have two special cases:
;;     * cons-tree is an atom
;;     * cons-tree is just a single INJECT
