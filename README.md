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
     `(sample-thing-to-expand-to
        ,,@body))) ; note the difference from usual way

(define-my-macro foo (x y)
  ,x ; now here injections of quotation constructs work
  ,y)

(define-my-macro bar (&body body)
  ,@body) ; splicing is also easy
```

The "injections" in macros FOO and BAR work as naively expected, as if I had written
```lisp
(defmacro foo (x y)
  `(sample-thing-to-expand-to ,x ,y))

(defmacro bar (&body body)
  `(sample-thing-to-expand-to ,@body))

(macroexpand-1 '(foo a b))

  '(SAMPLE-THING-TO-EXPAND-TO A B)

(macroexpand-1 '(bar a b c))

  '(SAMPLE-THING-TO-EXPAND-TO A B C)
```


So, how is this effect achieved?


DIG, INJECT and SPLICE
-------------------------

The transformations of backquote occur at macroexpansion-time and not at read-time.
It is totally possible not to use any special reader syntax, but just
underlying macros directly!

At the core is a macro DIG, which expands to the code that generates the
expression according to the rules, which are roughly these:
  * each DIG increases "depth" by one (hence the name)
  * each INJECT or SPLICE decreases "depth" by one
  * if depth is 0, evaluation is turned on
  * if depth if not zero (even if it's negative!) evaluation is off
  * SPLICE splices the form, similarly to ordinary `,@`, INJECT simply injects, same as `,`

```lisp
;; The example using macros, without special reader syntax

(dig ; depth is 1 here
  (a b
     (dig ; depth is 2 here
       ((inject c) ; this inject is not evaluated, because depth is nonzero
        (inject (d ;depth becomes 1 here again
                (inject e) ; and this inject is evaluated, because depth becomes zero
                ))
        (inject 2 f) ; this inject with level specification is evaluated, because it
                     ; decreases depth by 2
        ))))


;; the same example using ENABLE-QUASIQUOTE-2.0 syntax is written as
`(a b `(,c ,(d ,e) ,,f)) ; note double comma acts different than usually
```


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


The optional N argument
--------------

All quasiquote-2.0 operators accept optional "depth" argument,
which goes before the form for human readability.

Namely, (DIG N FORM) increases depth by N instead of one and
(INJECT N FORM) decreases depth by N instead of one.

```lisp
(DIG 2 (INJECT 2 A))

; gives the same result as

(DIG (INJECT A))
```


In fact, with ENABLE-QUASIQUOTE-2.0, say, ,,,,,FORM (5 quotes) reads as (INJECT 5 FORM)
and ,,,,,@FORM as (SPLICE 5 FORM)


More examples
-------------

For fairly complicated example (using ,,,@) see DEFINE-BINOP-DEFINER macro
in CG-LLVM (https://github.com/mabragor/cg-llvm/src/basics.lisp),
desire to write which was a motivation for this whole project.


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



TODO
----

* WITH-QUASIQUOTE-2.0 read-macro-token for local enabling of ` and , overloading
* wrappers for convenient definition of custom overloading schemes
* some syntax for opaque operations

P.S. Name "quasiquote-2.0" comes from "patronus 2.0" spell from www.hpmor.com
     and has nothing to do with being "the 2.0" version of quasiquote.