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
      (cadr form)
      (caddr form)))

(defun transparent-p (form)
  (or (eq (car form) 'dig)
      (eq (car form) 'inject)))

(defun look-into-injector (form)
  (let ((*depth* (- *depth* (injector-level form))))
    (search-all-active-sites (injector-subform form) nil)))

(defun dig-form-p (form)
  (and (consp form)
       (eq 'dig (car form))))

(defun look-into-dig (form)
  (let ((*depth* (+ *depth* (injector-level form))))
    (search-all-active-sites (injector-subform form) nil)))

(defun search-all-active-sites (form toplevel-p)
  (if toplevel-p
      (cond ((atom form) (format t "imhere~%") :just-quote-it!)
	    ((injector-form-p form) (if (equal *depth* (injector-level form))
					:just-form-it!
					(if (transparent-p form)
					    (look-into-injector form))))
	    (t (search-all-active-sites (car form) nil)
	       (search-all-active-sites (cdr form) nil)))
      (if (consp form)
	  (cond ((dig-form-p form) (if (transparent-p form)
				       (look-into-dig form)))
		((injector-form-p (car form)) (if (equal *depth* (injector-level form))
						  (progn (push form *injectors*)
							 nil)
						  (if (transparent-p form)
						      (look-into-injector form))))
		(t (search-all-active-sites (car form) nil)
		   (search-all-active-sites (cdr form) nil))))))
	  
	      
(defun codewalk-dig-form (form)
  (reset-injectors)
  (let ((it (search-all-active-sites form t)))
    (values *injectors* it)))

(defun %codewalk-dig-form (form)
  (if (not (dig-form-p form))
      (error "Supposed to be called on dig form")
      (let ((*depth* (+ (injector-level form) *depth*)))
	(codewalk-dig-form (injector-subform form)))))

    
;; (defmacro dig (n-or-form &optional (form nil form-p))
;;   (if (not form-p)
;;       `(dig 1 ,n-or-form)
;;       (let ((*depth* (+ n-or-form *depth*)))
;; 	(codewalk-dig-form form t))))
      
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
