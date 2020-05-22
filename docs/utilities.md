# utilities - A collection of some more utility libraries

A collection of smallish utility libraries.

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***

## PARSE-NUMBER

Version: 1.7
<br/>
Repository: [sharplispers/parse-number - Github](https://github.com/sharplispers/parse-number)

PARSE-NUMBER is a library of functions which accept an arbitrary
string and attempt to parse it, if possible into one of the standard
Common Lisp number types without using the reader, or else signal an
error of type `invalid-number`.

### invalid-number

```lisp
Condition
```

### invalid-number-reason

```lisp
Generic Function: (invalid-number-reason condition)
```

### invalid-number-value

```lisp
Generic Function: (invalid-number-value condition)
```

### parse-number

```lisp
Function: (parse-number string &key (start 0) (end NIL) (radix 10)
            ((:float-format *read-default-float-format*)
             *read-default-float-format*))
```

### parse-positive-real-number

```lisp
Function: (parse-positive-real-number string &key (start 0) (end NIL)
            (radix 10)
            ((float-format *read-default-float-format*)
             *read-default-float-format*))
```

### parse-real-number

```lisp
Function: (parse-real-number string &key (start 0) (end NIL) (radix 10)
            ((float-format *read-default-float-format*)
             *read-default-float-format*))
```

## SPLIT-SEQUENCE

Version: 2.0.0
<br/>
Repository: [sharplispers/split-sequence](https://github.com/sharplispers/split-sequence)

### split-sequence

```lisp
Function: (split-sequence delimiter sequence &key (start 0) (end NIL)
           (from-end NIL) (count NIL) (remove-empty-subseqs NIL)
           (test (function eql) test-p) (test-not NIL test-not-p)
           (key (function identity)))
```

Return a list of subsequences in seq delimited by delimiter.
If `:remove-empty-subseqs` is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for [cl:substitute](http://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm).  In particular, the
behaviour of `:from-end` is possibly different from other versions of
this function; `:from-end` values of NIL and T are equivalent unless
`:count` is supplied. `:count` limits the number of subseqs in the main
resulting list. The second return value is an index suitable as an
argument to [cl:subseq](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm) into the sequence indicating where processing
stopped.

### split-sequence-if

```lisp
Function: (split-sequence-if predicate sequence &key (start 0) (end NIL)
           (from-end NIL) (count NIL) (remove-empty-subseqs NIL)
           (key (function identity)))
```

Return a list of subsequences in seq delimited by items satisfying
predicate.
If `:remove-empty-subseqs` is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for [cl:substitute-if](http://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm).  In particular, the
behaviour of `:from-end` is possibly different from other versions of
this function; `:from-end` values of NIL and T are equivalent unless
`:count` is supplied. `:count` limits the number of subseqs in the main
resulting list. The second return value is an index suitable as an
argument to [cl:subseq](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm) into the sequence indicating where processing
stopped.

### split-sequence-if-not

```lisp
Function: (split-sequence-if-not predicate sequence &key (start 0) (end NIL)
           (from-end NIL) (count NIL) (remove-empty-subseqs NIL)
           (key (function identity)))
```

Return a list of subsequences in seq delimited by items satisfying
([cl:complement](http://www.lispworks.com/documentation/HyperSpec/Body/f_comple.htm) predicate).
If `:remove-empty-subseqs` is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for [cl:substitute-if-not](http://www.lispworks.com/documentation/HyperSpec/Body/f_sbs_s.htm).  In particular,
the behaviour of `:from-end` is possibly different from other versions
of this function; `:from-end` values of NIL and T are equivalent unless
`:count` is supplied. `:count` limits the number of subseqs in the main
resulting list. The second return value is an index suitable as an
argument to [cl:subseq](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm) into the sequence indicating where processing
stopped.

## TRIVIAL-TYPES

Version: 0.1
<br/>
Repository: [m2ym/trivial-types - Github](https://github.com/m2ym/trivial-types)
<br/>

TODO: This repository is archived; quickdocs points to this; a [branch of a fork](https://github.com/christophejunke/trivial-types/tree/patch-1) is ahead of this.

TRIVIAL-TYPES provides missing but important type
definitions such as `proper-list`, `association-list`, `property-list` and
`tuple`.

By using these types, you can keep type declarations more
accurate. For example, you may write a class definition like:

```lisp
(defclass person ()
  ((name :type string))
  ((age :type fixnum))
  ((friends :type list)))
```

However, it is not obvious for anyone except you that FRIENDS slot has
only a list of person. If you want declare `friends` slot more
accurately,` proper-list` is the best for that:

```lisp
(defclass person ()
  ((name :type string))
  ((age :type fixnum))
  ((friends :type (proper-list person))))
```

In addition, TRIVIAL-TYPES also provides standard designators defined
in ANSI standard such as `package-designator`. They are useful when you
write a function that takes a package-oid argument like:

```lisp
(defun list-external-symbols (package)
  (declare (package-designator package))
  (loop for symbol being the external-symbol of package
     collect symbol))
```

An exhaustive list of provided types includes:

- [association-list](#association-list-p)
- character-designator
- [file-associated-stream](#file-associated-stream-p)
- file-position-designator
- function-designator
- list-designator
- non-nil
- package-designator
- pathname-designator
- [proper-list](#proper-list-p)
- [property-list](#property-list-p)
- stream-designator
- string-designator

### association-list-p

```lisp
Function: (association-list-p var)
```

Returns true if OBJECT is an association list.

Examples:

```lisp
(association-list-p 1) => NIL
(association-list-p '(1 2 3)) => NIL
(association-list-p nil) => T
(association-list-p '((foo))) => T
(association-list-p '((:a . 1) (:b . 2))) => T
```

### file-associated-stream-p

```lisp
Function: (file-associated-stream-p stream)
```

Returns true if `stream` is a stream associated to a file.

### proper-list-p

```lisp
Function: (proper-list-p object)
```

Returns true if `object` is a proper list.

Examples:

```lisp
(proper-list-p 1) => NIL
(proper-list-p '(1 . 2)) => NIL
(proper-list-p nil) => T
(proper-list-p '(1 2 3)) => T
```

### property-list-p

```lisp
Function: (property-list-p object)
```

Returns true if `object` is a property list.

Examples:

```lisp
(property-list-p 1) => NIL
(property-list-p '(1 2 3)) => NIL
(property-list-p '(foo)) => NIL
(property-list-p nil) => T
(property-list-p '(foo 1)) => T
(property-list-p '(:a 1 :b 2)) => T
```

### tuple

```lisp
Function: (tuple &rest args)
```

Exactly same as LIST.

### tuplep

```lisp
Function: (tuplep object)
```

Returns true if `object` is a tuple, meaning a proper list.

Examples:

```lisp
(tuplep 1) => NIL
(tuplep '(1 . 2)) => NIL
(tuplep nil) => T
(tuplep '(1 2 3)) => T
```

### type-expand

```lisp
Function: (type-expand type-specifier &optional env)
```

Expand `type-specifier` in the lexical environment `env`.

### type-specifier-p

```lisp
Function: (type-specifier-p type-specifier)
```

Returns true if `type-specifier` is a valid type specfiier.


## TRIVIAL-PACKAGE-LOCAL-NICKNAMES

Version: 0.2
<br/>
Repository: [phoe/trivial-package-local-nicknames](https://github.com/phoe/trivial-package-local-nicknames)

A portability layer for package local nicknames.

### add-package-local-nickname

```lisp
Function: (add-package-local-nickname local-nickname actual-package &optional
           (package-designator (sane-package)))
```

Adds `local-nickname` for `actual-package` in the designated package, defaulting
to current package. `local-nickname` must be a string designator, and
`actual-package` must be a package designator.

Returns the designated package.

Signals a continuable error if `local-nickname` is already a package local
nickname for a different package, or if `local-nickname` is one of "CL",
"COMMON-LISP", or, "KEYWORD", or if `local-nickname` is a global name or
nickname for the package to which the nickname would be added.

When in the designated package, calls to FIND-PACKAGE with the `local-nickname`
will return the package the designated `actual-package` instead. This also
affects all implied calls to FIND-PACKAGE, including those performed by the
reader.

When printing a package prefix for a symbol with a package local nickname,
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: [package-local-nicknames](#package-local-nicknames), [package-locally-nicknamed-by-list](#package-locally-nicknamed-by-list),
[remove-package-local-nickname](#remove-package-local-nickname), and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change.

### package-local-nicknames

```lisp
Function: (package-local-nicknames package-designator)
```

Returns an alist of (local-nickname . actual-package) describing the
nicknames local to the designated package.

When in the designated package, calls to FIND-PACKAGE with the any of the
local-nicknames will return the corresponding actual-package instead. This
also affects all implied calls to FIND-PACKAGE, including those performed by
the reader.

When printing a package prefix for a symbol with a package local nickname, the
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: [add-package-local-nickname](#add-package-local-nickname), [package-locally-nicknamed-by-list](#package-locally-nicknamed-by-list),
[remove-package-local-nickname](#remove-package-local-nickname), and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change.

### package-locally-nicknamed-by-list

```lisp
Function: (package-locally-nicknamed-by-list package-designator)
```

Returns a list of packages which have a local nickname for the designated
package.

See also: [add-package-local-nickname](#add-package-local-nickname), [package-local-nicknames](#package-local-nicknames),
[remove-package-local-nickname](#remove-package-local-nickname), and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change.

### remove-package-local-nickname

```lisp
Function: (remove-package-local-nickname old-nickname &optional
           (package-designator (sane-package)))
```

If the designated package had `old-nickname` as a local nickname for
another package, it is removed. Returns true if the nickname existed and was
removed, and NIL otherwise.

See also: [add-package-local-nickname](#add-package-local-nickname), [package-local-nicknames](#package-local-nicknames),
[package-locally-nicknamed-by-list](#package-locally-nicknamed-by-list), and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change.

