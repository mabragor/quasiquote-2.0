quasiquote-2.0
==============

Why should it be hard to write macros that write other macros?
Well, it shouldn't!

quasiquote-2.0 defines slightly different rules for quasiquotation,
that make writing macro-writing macros very smooth experience.

```lisp
(quasiquote-2.0:enable-quasiquote-2.0)

(defmacro define-my-macro (name args &body body)
  `(defmacro ,name ,args
     `(sample-thing-to-macroexpand-to
        ,,@body))) ; note the difference from usual way

(define-my-macro foo (x y)
  ,x ; now here injections of quotation constructs work
  ,y)

(define-my-macro bar (&body body)
  ,@body) ; splicing is also easy
```

The "injections" in macros FOO and BAR work as expected, as if I had written
(using usual quasiquote rules, but will work the same in quasiquote-2.0)
```lisp
(defmacro foo (x y)
  `(sample-thing-to-macroexpand-to ,x ,y))

(defmacro bar (&body body)
  `(sample-thing-to-macroexpand-to ,@body))
```


So, how is this effect achieved?


DIG and INJECT and SPLICE
-------------------------

Well, main technical difference from usual quasiquote is that all essential transformations
happen at macroexpansion-time, rather than at read-time.

At the core is a macro DIG, which expands in the code that generates the
expression according to the rules:
  * at the start of walking, evaluation is off (i.e. same as usual quasiquote)
  * the tree of a form is walked, with keeping track of the "depth"
  * each DIG, occuring on the way, increases depth by one (hence the name)
  * each INJECT or SPLICE:
    * decreases depth by one
    * if the resulting depth is zero, evaluation is turned on, the result is
      substituted in the corresponding place
    * SPLICE splices the form, similarly to ordinary ,@


The ENABLE-QUASIQUOTE-2.0 macro just installs reader that reads
`FORM as (DIG FORM), ,FORM as (INJECT FORM) and ,@FORM as (SPLICE FORM).
You can just as well type DIG's, INJECT's and SPLICE's directly, 
(in particular, when writing utility functions that generate macro-generating code)
or roll your own convenient reader syntax (pull requests are welcome).

So, these two lines (with ENABLE-QUASIQUOTE-2.0) read the same
```lisp
`(a (,b `,,c) d)

(dig (a ((inject b) (dig (inject 2 c))) d))
```

You may notice the (INJECT 2 ...) form appearing, which is described below.


At "level 1", i.e. when only \` , and ,@ are used, and not, say \`\` ,, ,', ,,@ ,',@
this behaves exactly as usual quasiquotation.

ODIG and OINJECT and OSPLICE
----------------------------

Sometimes you don't want DIG's macroexpansion to look further into the structure of
some INJECT or SPLICE or DIG in its subform,
if the depth does not match. In these cases you need "opaque" versions of
DIG, INJECT and SPLICE, named, respectively, ODIG, OINJECT and OSPLICE.

```lisp
;; here injection of B would occur
(defun foo (b)
  (dig (dig (inject (a (inject b))))))

;; and here not, because macroexpansion does not look into OINJECT form
(defun bar (b)
  (dig (dig (oinject (a (inject b))))))

(foo 1)

  '(DIG (INJECT (A 1)))

(bar 1)

  '(DIG (OINJECT (A (INJECT B))))
```


The N argument
--------------

All quasiquote-2.0 operators accept optional "depth" argument,
which goes before the form for human readability.

Namely, (DIG N FORM) increases depth by N instead of one and
(INJECT N FORM) decreases depth by N instead of one.

```lisp
(DIG 2 (INJECT 2 A))

; is the same as

(DIG (INJECT A))
```


In fact, with ENABLE-QUASIQUOTE-2.0, say, ,,,,,FORM (5 quotes) reads as (INJECT 5 FORM)
and ,,,,,@FORM as (SPLICE 5 FORM)


More examples
-------------

For fairly complicated example (using ,,,@) see DEFINE-BINOP-DEFINER macro
in CG-LLVM (https://github.com/mabragor/cg-llvm/src/basics.lisp),
desire to write which was a motivation for this whole project.

TODO
----

* WITH-QUASIQUOTE-2.0 read-macro-token for local enabling of ` and , overloading
* wrappers for convenient definition of custom overloading schemes
* some syntax for opaque operations

P.S. Name "quasiquote-2.0" comes from "patronus 2.0" spell from www.hpmor.com
     and has nothing to do with being "the 2.0" version of quasiquote.