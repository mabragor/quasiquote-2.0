;;;; quasiquote-2.0.lisp

(in-package #:quasiquote-2.0)

(defparameter *env* nil)

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
(define-nonsense-when-bare macro-inject)

(defparameter *depth* 0)


(defparameter *injectors* nil)

;; (defmacro with-injector-parsed (form)
;;   `(let ((kwd (intern (string 

(defun reset-injectors ()
  (setf *injectors* nil))

(defparameter *known-injectors* '(inject splice oinject osplice
				  macro-inject omacro-inject
				  macro-splice omacro-splice
				  macro-inject-all omacro-inject-all
				  macro-splice-all omacro-splice-all))

(defun injector-form-p (form)
  (and (consp form)
       (find (car form) *known-injectors* :test #'eq)))

(defun injector-level (form)
  (if (equal 2 (length form))
      1
      (cadr form)))

(defun injector-subform (form)
  (if (equal 2 (length form))
      (values (cdr form) '(cdr))
      (values (cddr form) '(cddr))))

(defparameter *opaque-injectors* '(odig oinject osplice omacro-inject))

(defun transparent-p (form)
  (not (find (car form) *opaque-injectors* :test #'eq)))

(defun look-into-injector (form path)
  (let ((*depth* (- *depth* (injector-level form))))
    (multiple-value-bind (subform subpath) (injector-subform form)
      (search-all-active-sites subform (append subpath path) nil))))

(defparameter *known-diggers* '(dig odig))

(defun dig-form-p (form)
  (and (consp form)
       (find (car form) *known-diggers* :test #'eq)))

(defun look-into-dig (form path)
  (let ((*depth* (+ *depth* (injector-level form))))
    (multiple-value-bind (subform subpath) (injector-subform form)
      (search-all-active-sites subform (append subpath path) nil))))

(defun handle-macro-1 (form)
  (if (atom form)
      (error "Sorry, symbol-macros are not implemented for now")
      (let ((fun (macro-function (car form) *env*)))
	(if (not fun)
	    (error "The subform of MACRO-1 injector is supposed to be macro, perhaps, something went wrong..."))
	(macroexpand-1 form *env*))))

(defun handle-macro-all (form)
  (if (atom form)
      (error "Sorry, symbol-macros are not implemented for now")
      (macroexpand form *env*)))


(defparameter *macro-handlers* `((macro-inject . ,#'handle-macro-1)
				 (omacro-inject . ,#'handle-macro-1)
				 (macro-splice . ,#'handle-macro-1)
				 (omacro-splice . ,#'handle-macro-1)
				 (macro-inject-all . ,#'handle-macro-all)
				 (omacro-inject-all . ,#'handle-macro-all)
				 (macro-splice-all . ,#'handle-macro-all)
				 (omacro-splice-all . ,#'handle-macro-all)))

(defun get-macro-handler (sym)
  (or (cdr (assoc sym *macro-handlers*))
      (error "Don't know how to handle this macro injector: ~a" sym)))

	

(defun macroexpand-macroinjector (place)
  (if (not (splicing-injector (car place)))
      (progn (setf (car place) (funcall (get-macro-handler (caar place))
					(car (injector-subform (car place)))))
	     nil)
      (let ((new-forms (funcall (get-macro-handler (caar place))
				(car (injector-subform (car place))))))
	(cond ((not new-forms)
	       (setf *void-filter-needed* t
		     (car place) *void-elt*))
	      ((atom new-forms) (error "We need to splice the macroexpansion, but got atom: ~a" new-forms))
	      (t (setf (car place) (car new-forms))
		 (let ((tail (cdr place)))
		   (setf (cdr place) (cdr new-forms)
			 (cdr (last new-forms)) tail))))
	t)))
	    

(defun search-all-active-sites (form path toplevel-p)
  ;; (format t "SEARCH-ALL-ACTIVE-SITES: got form ~a~%" form)
  (if (not form)
      nil
      (if toplevel-p
	  (cond ((atom (car form)) :just-quote-it!)
		((injector-form-p (car form)) (if (equal *depth* (injector-level (car form)))
						  :just-form-it!
						  (if (transparent-p (car form))
						      (look-into-injector (car form) (cons 'car path)))))
		((dig-form-p (car form))
		 ;; (format t "Got dig form ~a~%" form)
		 (if (transparent-p (car form))
		     (look-into-dig (car form) (cons 'car path))))
		(t (search-all-active-sites (car form) (cons 'car path) nil)
		   (search-all-active-sites (cdr form) (cons 'cdr path) nil)))
	  (when (consp form)
	    (cond ((dig-form-p (car form))
		   ;; (format t "Got dig form ~a~%" form)
		   (if (transparent-p (car form))
		       (look-into-dig (car form) (cons 'car path))))
		  ((injector-form-p (car form))
		   ;; (format t "Got injector form ~a ~a ~a~%" form *depth* (injector-level (car form)))
		   (if (equal *depth* (injector-level (car form)))
		       (if (macro-injector-p (car form))
			   (progn (macroexpand-macroinjector form)
				  (return-from search-all-active-sites
				    (search-all-active-sites form path nil)))
			   (progn (push (cons form (cons 'car path)) *injectors*)
				  nil))
		       (if (transparent-p (car form))
			   (look-into-injector (car form) (cons 'car path)))))
		  (t (search-all-active-sites (car form) (cons 'car path) nil)))
	    (search-all-active-sites (cdr form) (cons 'cdr path) nil)))))

	  
	      
(defun codewalk-dig-form (form)
  (reset-injectors)
  (let ((it (search-all-active-sites form nil t)))
    (values (nreverse *injectors*) it)))

(defun %codewalk-dig-form (form)
  (if (not (dig-form-p form))
      (error "Supposed to be called on dig form")
      (let ((*depth* (+ (injector-level form) *depth*)))
	(codewalk-dig-form (injector-subform form)))))

(defun path->setfable (path var)
  (let ((res var))
    ;; First element is artifact of extra CAR-ing
    (dolist (spec (cdr (reverse path)))
      (setf res (list spec res)))
    res))

(defun tree->cons-code (tree)
  (if (atom tree)
      `(quote ,tree)
      `(cons ,(tree->cons-code (car tree))
	     ,(tree->cons-code (cdr tree)))))

(defparameter *known-splicers* '(splice osplice
				 macro-splice omacro-splice
				 macro-splice-all omacro-splice-all))

(defun splicing-injector (form)
  (and (consp form)
       (find (car form) *known-splicers* :test #'eq)))

(defparameter *known-macro-injectors* '(macro-inject omacro-inject
					macro-splice omacro-splice
					macro-inject-all omacro-inject-all
					macro-splice-all omacro-splice-all
					))

(defun macro-injector-p (form)
  (and (consp form)
       (find (car form) *known-macro-injectors* :test #'eq)))

(defparameter *void-elt* nil)
(defparameter *void-filter-needed* nil)

(defun filter-out-voids (lst void-sym)
  (let (caars cadrs cdars cddrs)
    ;; search for all occurences of VOID
    (labels ((rec (x)
	       (if (consp x)
		   (progn (cond ((consp (car x))
				 (cond ((eq void-sym (caar x)) (push x caars))
				       ((eq void-sym (cdar x)) (push x cdars))))
				((consp (cdr x))
				 (cond ((eq void-sym (cadr x)) (push x cadrs))
				       ((eq void-sym (cddr x)) (push x cddrs)))))
			  (rec (car x))
			  (rec (cdr x))))))
      (rec lst))
    (if (or cdars cddrs)
	(error "Void sym found on CDR position, which should not have happened"))
    ;; destructively transform LST
    (dolist (elt caars)
      (setf (car elt) (cdar elt)))
    (dolist (elt cadrs)
      (setf (cdr elt) (cddr elt)))
    ;; check that we indeed filtered-out all VOIDs
    (labels ((rec (x)
	       (if (not (atom x))
		   (progn (rec (car x))
			  (rec (cdr x)))
		   (if (eq void-sym x)
		       (error "Not all VOIDs were filtered")))))
      (rec lst))
    lst))

(defun transform-dig-form (form)
  (let ((the-form (copy-tree form)))
    (let ((*void-filter-needed* nil)
	  (*void-elt* (gensym "VOID")))
      (multiple-value-bind (site-paths cmd) (%codewalk-dig-form the-form)
	(cond ((eq cmd :just-quote-it!)
	       `(quote ,(car (injector-subform the-form))))
	      ((eq cmd :just-form-it!)
	       (car (injector-subform (car (injector-subform the-form)))))
	      (t (let ((cons-code (if (not site-paths)
				      (tree->cons-code (car (injector-subform the-form)))
				      (really-transform-dig-form the-form site-paths))))
		   (if (not *void-filter-needed*)
		       cons-code
		       `(filter-out-voids ,cons-code ',*void-elt*)))))))))

(defmacro make-list-form (o!-n form)
  (let ((g!-n (gensym "N"))
	(g!-i (gensym "I"))
	(g!-res (gensym "RES")))
    `(let ((,g!-n ,o!-n)
	   (,g!-res nil))
       (dotimes (,g!-i ,g!-n)
	 (push ,form ,g!-res))
       (nreverse ,g!-res))))

(defun mk-splicing-injector-let (x)
  `(let ((it ,(car (injector-subform x))))
     (assert (listp it))
     (copy-list it)))



(defun mk-splicing-injector-setf (path g!-list g!-splicee)
  (assert (eq 'car (car path)))
  (let ((g!-rest (gensym "REST")))
    `(let ((,g!-rest ,(path->setfable (cons 'cdr (cdr path)) g!-list)))
       (assert (or (not ,g!-rest) (consp ,g!-rest)))
       (if (not ,g!-splicee)
	   (setf ,(path->setfable (cdr path) g!-list)
		 ,g!-rest)
	   (progn (setf ,(path->setfable (cdr path) g!-list) ,g!-splicee)
		  (setf (cdr (last ,g!-splicee)) ,g!-rest))))))


(defun really-transform-dig-form (the-form site-paths)
  (let ((gensyms (make-list-form (length site-paths) (gensym "INJECTEE"))))
    (let ((g!-list (gensym "LIST")))
      (let ((lets nil)
	    (splicing-setfs nil)
	    (setfs nil))
	(do ((site-path site-paths (cdr site-path))
	     (gensym gensyms (cdr gensym)))
	    ((not site-path))
	  (destructuring-bind (site . path) (car site-path)
	    (push `(,(car gensym) ,(if (not (splicing-injector (car site)))
				       (car (injector-subform (car site)))
				       (mk-splicing-injector-let (car site))))
		  lets)
	    (if (not (splicing-injector (car site)))
		(push `(setf ,(path->setfable path g!-list) ,(car gensym)) setfs)
		(push (mk-splicing-injector-setf path g!-list (car gensym)) splicing-setfs))
	    (setf (car site) nil)))
	`(let ,(nreverse lets)
	   (let ((,g!-list ,(tree->cons-code (car (injector-subform the-form)))))
	     ,@(nreverse setfs)
	     ,@(nreverse splicing-setfs)
	     ,g!-list))))))


;; There are few types of recursive injection that may happen:
;;   * compile-time injection:
;;     (dig (inject (dig (inject a)))) -- this type will be handled automatically by subsequent macroexpansions
;;   * run-time injection:
;;     (dig (dig (inject 2 a)))
;;     and A is '(dig (inject 3 'foo)) -- this one we guard against ? (probably, first we just ignore it
;;     -- do not warn about it, and then it wont really happen.
;;   * macroexpanded compile-time injection:
;;     (dig (inject (my-macro a b c))),
;;     where MY-MACRO expands into, say (splice (list 'a 'b 'c))
;;     This is *not* handled automatically, and therefore we must do it by hand.

      
;; OK, now how to implement splicing ?
;;   (dig (a (splice (list b c)) d))
;; should transform into code that yields
;;   (a b c d)
;; what this code is?
;;   (let ((#:a (copy-list (list b c))))
;;     (let ((#:res (cons 'a nil 'd)))
;;       ;; all non-splicing injects go here, as they do not spoil the path-structure
;;       (setf (cdr #:res) #:a)
;;       (setf (cdr (last #:a)) (cdr (cdr #:res)))
;;       #:res)))


;; How this macroexpansion should work in general?
;;   * We go over the cons-tree, keeping track of the depth level, which is
;;   controlled by DIG's
;;   * Once we find the INJECT with matching level, we remember the place, where
;;     this happens
;;   * We have two special cases:
;;     * cons-tree is an atom
;;     * cons-tree is just a single INJECT
