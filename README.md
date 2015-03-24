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
        ,,@body)))

(define-my-macro foo (x y)
  ,x
  ,y)

(define-my-macro bar (&body body)
  ,@body)
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

The ENABLE-QUASIQUOTE-2.0 macro just installs reader that reads
`FORM as (DIG FORM), ,FORM as (INJECT FORM) and ,@FORM as (SPLICE FORM).
You can just as well type DIG's, INJECT's and SPLICE's directly, 
(in particular, when writing utility functions that generate macro-generating code)
or roll your own convenient reader syntax (pull requests are welcome).


Then DIG is just a macro with a peculiar macroexpansion, the rules
are these:
  * the tree of a form is walked, with keeping track of the "depth"
  * each DIG, occuring on the way, increases depth by one (hence the name)
  * each INJECT or SPLICE:
    * decreases depth by one
    * if the resulting depth is zero, is evaluates its subform
    * SPLICE splices the form, same as ordinary ,@ does

At "level 1", i.e. when only \` , and ,@ are used, and not, say \`\` ,, ,', ,,@ ,',@
this behaves exactly as usual quasiquotation.

ODIG and OINJECT and OSPLICE
----------------------------

Sometimes you don't want evaluation to look into the structure of INJECT or SPLICE or DIG,
if the depth does not match. In these cases you need "opaque" versions of
DIG, INJECT and SPLICE, named, respectively, ODIG, OINJECT and OSPLICE.

```lisp
(dig (dig (inject a (inject b)))) ; here injection of B would occur

(dig (dig (oinject a (inject b)))) ; and here not
```

The N argument
--------------

All quasiquote-2.0 operators accept optional "depth" argument, that is
(DIG N FORM) increases depth by N instead of one and
(INJECT N FORM) decreases depth by N instead of one.


TODO
----

* WITH-QUASIQUOTE-2.0 read-macro-token for local enabling of ` and , overloading
* wrappers for convenient definition of custom overloading schemes
* some syntax for opaque operations

P.S. Name "quasiquote-2.0" comes from "patronus 2.0" spell from www.hpmor.com
     and has nothing to do with being "the 2.0" version of quasiquote.