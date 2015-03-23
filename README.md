quasiquote-2.0
--------------

Why is it so hard to write macros, that write other macros?
Maybe, because we need to rethink quasiquote a little bit...


I have this piece of code:

(defmacro define-no-wrap-binop (name &body type-checks)
  `(defun ,name (op1 op2 &key no-signed-wrap no-unsigned-wrap)
     (let ((top1 (imply-type op1))
	   (top2 (imply-type op2)))
       (assert (llvm-same-typep top1 top2))
       ,@type-checks
       (emit-resulty (make-tmp-var ',name (slot-value top1 'type))
         ,(trim-llvm (underscorize name))
	 (if no-unsigned-wrap "nuw")
	 (if no-signed-wrap "nsw")
	 (join ", "
	       (emit-text-repr top1)
	       (emit-text-repr (slot-value top2 'value)))))))

And I want to be able to write

(defmacro define-binop-definer (name extra-args res-var-type &body format-strs)
  ...)

such that the example above would be written something like

(define-binop-definer define-no-wrap-binop (&key no-signed-wrap no-unsigned-wrap)
  nil
  (if no-unsigned-wrap "nuw")
  (if no-signed-wrap "nsw"))

and several others from the same project CG-LLVM (file basics.lisp) would be defined as

(define-binop-definer define-float-binop (&rest flags)
  nil ; defaults to (slot-value top1 'type)
  ;; And I want this thing to be correctly spliced!!!
  ,@(mapcar #'coerce-to-fast-math-flag flags))

(define-binop-definer define-exactable-binop (&key exact)
  nil
  (if exact "exact"))

(define-binop-definer define-simple-binop ()
  nil)
  

Let's first see, why and where the usual quasiquote approach is broken (and maybe it's not broken at all?)

(defmacro define-binop-definer (binop-name extra-args res-var-type &body list-things)
  `(defmacro ,binop-name (name &body type-checks)
    `(defun ,name (op1 op2 ,',@extra-args) ; what EXACTLY should I write here, I always wonder?
       (let ((top1 (imply-type op1))
             (top2 (imply-type op2)))
         (assert (llvm-same-typep top1 top2))
         ,@type-checks
         (emit-resulty (make-tmp-var ',name ,',(or res-var-type '(slot-value top1 'type)))
           ,(trim-llvm (underscorize name))
	   ,',@list-things ; and here ???
  	   (join ", "
	     (emit-text-repr top1)
	     (emit-text-repr (slot-value top2 'value))))))))

Almost surely that mapcar, that I wrote above, would not work with this, even
though after some changes ,',@ <-> ,,@ <-> ,@, the rest, probably, would.


So, how to fix this, how to make the rules such that transition from macro to macro-macro becomes
evident, transparent and not complicated at all?
(at least, macro-macro being on the same order of complexity as usual macro)