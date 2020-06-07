# alexandria - Commonly Used Utilities

Version: 1.0.0
<br/>
Repository: [alexandria/alexandria - Gitlab](https://gitlab.common-lisp.net/alexandria/alexandria)

*This documentation is modified to markdown from [here](https://common-lisp.net/project/alexandria/draft/alexandria.html) on 01 March, 2020.*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

***

## ALEXANDRIA 

Alexandria software and associated documentation are in the public
domain:

Authors dedicate this work to public domain, for the benefit of the
public at large and to the detriment of the authors' heirs and
successors. Authors intends this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights under
copyright law, whether vested or contingent, in the work. Authors
understands that such relinquishment of all rights includes the
relinquishment of all rights to enforce (by lawsuit or otherwise)
those copyrights in the work.

Authors recognize that, once placed in the public domain, the work may
be freely reproduced, distributed, transmitted, used, modified, built
upon, or otherwise exploited by anyone for any purpose, commercial or
non-commercial, and in any way, including by methods that have not yet
been invented or conceived.

In those legislations where public domain dedications are not recognized
or possible, Alexandria is distributed under the following terms and
conditions:

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-------------

## 1. HASH TABLES

### ensure-gethash

```lisp
Macro: (ensure-gethash key hash-table &optional default)
```
Like `gethash`, but if `key` is not found
in the `hash-table` saves the `default` under key before returning it.
Secondary return value is true if key was already in the table.


### copy-hash-table

```lisp
Function: (copy-hash-table table &key key test size rehash-size rehash-threshold)
```
Returns a copy of hash table `table`, with
the same keys and values as the `table`. The copy has the same
properties as the original, unless overridden by the keyword
arguments.

Before each of the original values is set into the new hash-table,
`key` is invoked on the value. As `key` defaults to `cl:identity`, a
shallow copy is returned by default.

### maphash-keys

```lisp
Function: (maphash-keys function table)
```
Like `maphash`, but calls `function` with
each key in the hash table `table`.

### maphash-values

```lisp
Function: (maphash-values function table)
```
Like `maphash`, but calls `function` with
each value in the hash table `table`.

### hash-table-keys

```lisp
Function: (hash-table-keys table)
```
Returns a list containing the keys of
hash table `table`.

### hash-table-values

```lisp
Function: (hash-table-values table)
```
Returns a list containing the values of
hash table `table`.


### hash-table-alist

```lisp
Function: (hash-table-alist table)
```
Returns an association list containing
the keys and values of hash table `table`.



### hash-table-plist

```lisp
Function: (hash-table-plist table)
```
Returns a property list containing the
keys and values of hash table `table`.



### alist-hash-table

```lisp
Function: (alist-hash-table alist &rest hash-table-initargs)
```
Returns a hash table containing the keys
and values of the association list `alist`. Hash table is initialized
using the `hash-table-initargs`.



### plist-hash-table

```lisp
Function: (plist-hash-table plist &rest hash-table-initargs)
```
Returns a hash table containing the keys
and values of the property list `plist`. Hash table is initialized
using the `hash-table-initargs`.


-----------

## 2. DATA AND CONTROL FLOW

### define-constant

```lisp
Macro: (define-constant name initial-value &key test documentation)
```
Ensures that the global variable named by
`name` is a constant with a value that is equal under `test` to the
result of evaluating `initial-value`. `test` is a /function
designator/ that defaults to `eql`. If `documentation` is given, it
becomes the documentation string of the constant.

Signals an error if `name` is already a bound non-constant variable.

Signals an error if `name` is already a constant variable whose value
is not equal under `test` to result of evaluating `initial-value`.




### destructuring-case

```lisp
Macro: (destructuring-case keyform &body clauses)
```
`destructuring-case`, `-ccase`, and
`-ecase` are a combination of `case` and `destructuring-bind`.
`keyform` must evaluate to a `cons`.

Clauses are of the form:

```lisp
            ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)
     
```

The clause whose `case-keys` matches `car` of `key`, as if by `case`,
`ccase`, or `ecase`, is selected, and FORMs are then executed with
`cdr` of `key` is destructured and bound by the
`destructuring-lambda-list`.

Example:

```lisp
           (defun dcase (x)
             (destructuring-case x
               ((:foo a b)
                (format nil "foo: ~S, ~S" a b))
               ((:bar &key a b)
                (format nil "bar, ~S, ~S" a b))
               (((:alt1 :alt2) a)
                (format nil "alt: ~S" a))
               ((t &rest rest)
                (format nil "unknown: ~S" rest))))
     
```

```lisp
            (dcase (list :foo 1 2))        ; ="foo: 1, 2"
            (dcase (list :bar :a 1 :b 2))  ; ="bar: 1, 2"
            (dcase (list :alt1 1))         ; ="alt: 1"
            (dcase (list :alt2 2))         ; ="alt: 2"
            (dcase (list :quux 1 2 3))     ; ="unknown: 1, 2, 3"
     
```

```lisp
           (defun decase (x)
             (destructuring-case x
               ((:foo a b)
                (format nil "foo: ~S, ~S" a b))
               ((:bar &key a b)
                (format nil "bar, ~S, ~S" a b))
               (((:alt1 :alt2) a)
                (format nil "alt: ~S" a))))
     
```

```lisp
            (decase (list :foo 1 2))        ; ="foo: 1, 2"
            (decase (list :bar :a 1 :b 2))  ; ="bar: 1, 2"
            (decase (list :alt1 1))         ; ="alt: 1"
            (decase (list :alt2 2))         ; ="alt: 2"
            (decase (list :quux 1 2 3))     ; =| error
     
```




### ensure-functionf

```lisp
Macro: (ensure-functionf &rest places)
```
Multiple-place modify macro for
`ensure-function:` ensures that each of `places` contains a function.




### multiple-value-prog2

```lisp
Macro: (multiple-value-prog2 first-form second-form &body forms)
```
Evaluates `first-form`, then
`second-form`, and then `forms`. Yields as its value all the value
returned by `second-form`.




### named-lambda

```lisp
Macro: (named-lambda name lambda-list &body body)
```
Expands into a lambda-expression within
whose `body` `name` denotes the corresponding function.




### nth-value-or

```lisp
Macro: (nth-value-or nth-value &body forms)
```
Evaluates `form` arguments one at a time,
until the `nth-value` returned by one of the forms is true. It then
returns all the values returned by evaluating that form. If none of
the forms return a true nth value, this form returns `nil`.




### if-let

```lisp
Macro: (if-let bindings &body (then-form &optional else-form))
```
Creates new variable bindings, and conditionally
executes either `then-form` or `else-form`. `else-form` defaults to
`nil`.

`bindings` must be either single binding of the form:

```lisp
           (variable initial-form)
     
```

or a list of bindings of the form:

```lisp
           ((variable-1 initial-form-1)
            (variable-2 initial-form-2)
            ...
            (variable-n initial-form-n))
     
```

All initial-forms are executed sequentially in the specified order.
Then all the variables are bound to the corresponding values.

If all variables were bound to true values, the `then-form` is
executed with the bindings in effect, otherwise the `else-form` is
executed with the bindings in effect.




### when-let

```lisp
Macro: (when-let bindings &body forms)
```
Creates new variable bindings, and conditionally
executes `forms`.

`bindings` must be either single binding of the form:

```lisp
           (variable initial-form)
     
```

or a list of bindings of the form:

```lisp
           ((variable-1 initial-form-1)
            (variable-2 initial-form-2)
            ...
            (variable-n initial-form-n))
     
```

All initial-forms are executed sequentially in the specified order.
Then all the variables are bound to the corresponding values.

If all variables were bound to true values, then `forms` are executed
as an implicit `progn`.




```lisp
Macro: (when-let* bindings &body forms)
```
Creates new variable bindings, and
conditionally executes `forms`.

`bindings` must be either single binding of the form:

```lisp
           (variable initial-form)
     
```

or a list of bindings of the form:

```lisp
           ((variable-1 initial-form-1)
            (variable-2 initial-form-2)
            ...
            (variable-n initial-form-n))
     
```

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the `when-let*`.

Execution of `when-let*` stops immediately if any initial-form
evaluates to `nil`. If all initial-forms evaluate to true, then
`forms` are executed as an implicit `progn`.




### switch

```lisp
Macro: (switch whole (object &key test key) &body clauses)
```
Evaluates first matching clause, returning its
values, or evaluates and returns the values of `default` if no keys
match.




### cswitch

```lisp
Macro: (cswitch whole (object &key test key) &body clauses)
```
Like `switch`, but signals a continuable error if
no key matches.




### eswitch

```lisp
Macro: (eswitch whole (object &key test key) &body clauses)
```
Like `switch`, but signals an error if no key
matches.




### whichever

```lisp
Macro: (whichever &rest possibilities env)
```
Evaluates exactly one of `possibilities`,
chosen at random.




### xor

```lisp
Macro: (xor &rest datums)
```
Evaluates its arguments one at a time, from left to
right. If more then one argument evaluates to a true value no further
`datums` are evaluated, and `nil` is returned as both primary and
secondary value. If exactly one argument evaluates to true, its value
is returned as the primary value after all the arguments have been
evaluated, and `t` is returned as the secondary value. If no arguments
evaluate to true `nil` is retuned as primary, and `t` as secondary
value.



### disjoin

```lisp
Function: (disjoin predicate &rest more-predicates)
```
Returns a function that applies each of
`predicate` and `more-predicate` functions in turn to its arguments,
returning the primary value of the first predicate that returns true,
without calling the remaining predicates. If none of the predicates
returns true, `nil` is returned.



### conjoin

```lisp
Function: (conjoin predicate &rest more-predicates)
```
Returns a function that applies each of
`predicate` and `more-predicate` functions in turn to its arguments,
returning `nil` if any of the predicates returns false, without
calling the remaining predicates. If none of the predicates returns
false, returns the primary value of the last predicate.



### compose

```lisp
Function: (compose function &rest more-functions)
```
Returns a function composed of `function` and
`more-functions` that applies its arguments to to each in turn,
starting from the rightmost of `more-functions`, and then calling the
next one with the primary value of the last.



```lisp
Function: (ensure-function function-designator)
```
Returns the function designated by
`function-designator:` if `function-designator` is a function, it is
returned, otherwise it must be a function name and its `fdefinition`
is returned.



### multiple-value-compose

```lisp
Function: (multiple-value-compose function &rest more-functions)
```
Returns a function composed of
`function` and `more-functions` that applies its arguments to each in
turn, starting from the rightmost of `more-functions`, and then
calling the next one with all the return values of the last.



### curry

```lisp
Function: (curry function &rest arguments)
```
Returns a function that applies `arguments` and the
arguments it is called with to `function`.



### rcurry

```lisp
Function: (rcurry function &rest arguments)
```
Returns a function that applies the arguments it
is called with and `arguments` to `function`.

--------

## 3. CONSES


### proper-list

Type designator for proper lists. Implemented
as a `satisfies` type, hence not recommended for performance intensive
use. Main usefullness as a type designator of the expected type in a
`type-error`.



### circular-list

Type designator for circular lists.
Implemented as a `satisfies` type, so not recommended for performance
intensive use. Main usefullness as the expected-type designator of a
`type-error`.




### appendf

```lisp
Macro: (appendf place &rest lists env)
```
Modify-macro for `append`. Appends `lists` to the
place designated by the first argument.




### nconcf

```lisp
Macro: (nconcf place &rest lists env)
```
Modify-macro for `nconc`. Concatenates `lists` to
place designated by the first argument.




### remove-from-plistf

```lisp
Macro: (remove-from-plistf place &rest keys env)
```
Modify macro for `remove-from-plist`.




### delete-from-plistf

```lisp
Macro: (delete-from-plistf place &rest keys env)
```
Modify macro for `delete-from-plist`.




### reversef

```lisp
Macro: (reversef place env)
```
Modify-macro for `reverse`. Copies and reverses
the list stored in the given place and saves back the result into the
place.




### nreversef

```lisp
Macro: (nreversef place env)
```
Modify-macro for `nreverse`. Reverses the list
stored in the given place by destructively modifying it and saves back
the result into the place.




### unionf

```lisp
Macro: (unionf place list &rest args env)
```
Modify-macro for `union`. Saves the union of
`list` and the contents of the place designated by the first argument
to the designated place.




### nunionf

```lisp
Macro: (nunionf place list &rest args env)
```
Modify-macro for `nunion`. Saves the union of
`list` and the contents of the place designated by the first argument
to the designated place. May modify either argument.




### doplist

```lisp
Macro: (doplist (key val plist &optional values) &body body)
```
Iterates over elements of `plist`. `body` can be
preceded by declarations, and is like a `tagbody`. `return` may be
used to terminate the iteration early. If `return` is not used,
returns `values`.



### circular-list-p

```lisp
Function: (circular-list-p object)
```
Returns true if `object` is a circular
list, `nil` otherwise.



### circular-tree-p

```lisp
Function: (circular-tree-p object)
```
Returns true if `object` is a circular
tree, `nil` otherwise.



### proper-list-p

```lisp
Function: (proper-list-p object)
```
Returns true if `object` is a proper list.



### alist-plist

```lisp
Function: (alist-plist alist)
```
Returns a property list containing the same
keys and values as the association list `alist` in the same order.



### plist-alist

```lisp
Function: (plist-alist plist)
```
Returns an association list containing the
same keys and values as the property list `plist` in the same order.



### circular-list

```lisp
Function: (circular-list &rest elements)
```
Creates a circular list of `elements`.



### make-circular-list

```lisp
Function: (make-circular-list length &key initial-element)
```
Creates a circular list of `length`
with the given `initial-element`.



### ensure-car

```lisp
Function: (ensure-car thing)
```
If `thing` is a `cons`, its `car` is
returned. Otherwise `thing` is returned.



### ensure-cons

```lisp
Function: (ensure-cons cons)
```
If `cons` is a cons, it is returned.
Otherwise returns a fresh cons with `cons` in the car, and `nil` in
the cdr.



### ensure-list

```lisp
Function: (ensure-list list)
```
If `list` is a list, it is returned.
Otherwise returns the list designated by `list`.



### flatten

```lisp
Function: (flatten tree)
```
Traverses the tree in order, collecting non-null
leaves into a list.



### lastcar

```lisp
Function: (lastcar list)
```
Returns the last element of `list`. Signals a
type-error if `list` is not a proper list.



### (setf

```lisp
Function: ((setf lastcar))
```
Sets the last element of
`list`. Signals a type-error if `list` is not a proper list.



### proper-list-length

```lisp
Function: (proper-list-length list)
```
Returns length of `list`, signalling
an error if it is not a proper list.



### mappend

```lisp
Function: (mappend function &rest lists)
```
Applies `function` to respective element(s) of
each `list`, appending all the all the result list to a single list.
`function` must return a list.



### map-product

```lisp
Function: (map-product function list &rest more-lists)
```
Returns a list containing the results of
calling `function` with one argument from `list`, and one from each of
`more-lists` for each combination of arguments. In other words,
returns the product of `list` and `more-lists` using `function`.

Example:

```lisp
           (map-product 'list '(1 2) '(3 4) '(5 6))
            =((1 3 5) (1 3 6) (1 4 5) (1 4 6)
                (2 3 5) (2 3 6) (2 4 5) (2 4 6))
     
```



### remove-from-plist

```lisp
Function: (remove-from-plist plist &rest keys)
```
Returns a propery-list with same keys
and values as `plist`, except that keys in the list designated by
`keys` and values corresponding to them are removed. The returned
property-list may share structure with the `plist`, but `plist` is not
destructively modified. Keys are compared using `eq`.



### delete-from-plist

```lisp
Function: (delete-from-plist plist &rest keys)
```
Just like `remove-from-plist`, but
this version may destructively modify the provided plist.



### set-equal

```lisp
Function: (set-equal list1 list2 &key test key)
```
Returns true if every element of `list1`
matches some element of `list2` and every element of `list2` matches
some element of `list1`. Otherwise returns false.



### setp

```lisp
Function: (setp object &key test key)
```
Returns true if `object` is a list that denotes a
set, `nil` otherwise. A list denotes a set if each element of the list
is unique under `key` and `test`.

-----------

## 4. SEQUENCES


### proper-sequence 

Type designator for proper sequences,
that is proper lists and sequences that are not lists.




### deletef

```lisp
Macro: (deletef place item &rest remove-keywords env)
```
Modify-macro for `delete`. Sets place designated
by the first argument to the result of calling `delete` with `item`,
place, and the `remove-keywords`.




### removef

```lisp
Macro: (removef place item &rest remove-keywords env)
```
Modify-macro for `remove`. Sets place designated
by the first argument to the result of calling `remove` with `item`,
place, and the `remove-keywords`.



### rotate

```lisp
Function: (rotate sequence &optional n)
```
Returns a sequence of the same type as
`sequence`, with the elements of `sequence` rotated by `n:` `n`
elements are moved from the end of the sequence to the front if `n` is
positive, and `-n` elements moved from the front to the end if `n` is
negative. `sequence` must be a proper sequence. `n` must be an
integer, defaulting to `1`.

If absolute value of `n` is greater then the length of the sequence,
the results are identical to calling `rotate` with

```lisp
            (* (signum n) (mod n (length sequence))).
     
```

Note: the original sequence may be destructively altered, and result
sequence may share structure with it.



### shuffle

```lisp
Function: (shuffle sequence &key start end)
```
Returns a random permutation of `sequence`
bounded by `start` and `end`. Original sequece may be destructively
modified, and share storage with the original one. Signals an error if
`sequence` is not a proper sequence.



### random-elt

```lisp
Function: (random-elt sequence &key start end)
```
Returns a random element from `sequence`
bounded by `start` and `end`. Signals an error if the `sequence` is
not a proper non-empty sequence, or if `end` and `start` are not
proper bounding index designators for `sequence`.



### emptyp

```lisp
Function: (emptyp sequence)
```
Returns true if `sequence` is an empty sequence.
Signals an error if `sequence` is not a sequence.



### sequence-of-length-p

```lisp
Function: (sequence-of-length-p sequence length)
```
Return true if `sequence` is a
sequence of length `length`. Signals an error if `sequence` is not a
sequence. Returns `false` for circular lists.



### length=

```lisp
Function: (length= &rest sequences)
```
Takes any number of sequences or integers in
any order. Returns true iff the length of all the sequences and the
integers are equal. Hint: there's a compiler macro that expands into
more efficient code if the first argument is a literal integer.



### copy-sequence

```lisp
Function: (copy-sequence type sequence)
```
Returns a fresh sequence of `type`, which
has the same elements as `sequence`.



### first-elt

```lisp
Function: (first-elt sequence)
```
Returns the first element of `sequence`.
Signals a type-error if `sequence` is not a sequence, or is an empty
sequence.


### (setf first-elt)

```lisp
Function: ((setf first-elt))
```
Sets the first element of
`sequence`. Signals a type-error if `sequence` is not a sequence, is
an empty sequence, or if `object` cannot be stored in `sequence`.



### last-elt

```lisp
Function: (last-elt sequence)
```
Returns the last element of `sequence`. Signals
a type-error if `sequence` is not a proper sequence, or is an empty
sequence.



### (setf last-elt)

```lisp
Function: ((setf last-elt))
```
Sets the last element of
`sequence`. Signals a type-error if `sequence` is not a proper
sequence, is an empty sequence, or if `object` cannot be stored in
`sequence`.



### starts-with

```lisp
Function: (starts-with object sequence &key test key)
```
Returns true if `sequence` is a sequence
whose first element is `eql` to `object`. Returns `nil` if the
`sequence` is not a sequence or is an empty sequence.



### starts-with-subseq

```lisp
Function: (starts-with-subseq prefix sequence &rest args &key return-suffix &allow-other-keys)
```
Test whether the first elements of
`sequence` are the same (as per TEST) as the elements of `prefix`.

If `return-suffix` is `t` the functions returns, as a second value, a
displaced array pointing to the sequence after `prefix`.



### ends-with

```lisp
Function: (ends-with object sequence &key test key)
```
Returns true if `sequence` is a sequence whose
last element is `eql` to `object`. Returns `nil` if the `sequence` is
not a sequence or is an empty sequence. Signals an error if `sequence`
is an improper list.



### ends-with-subseq

```lisp
Function: (ends-with-subseq suffix sequence &key test)
```
Test whether `sequence` ends with
`suffix`. In other words: return true if the last (length SUFFIX)
elements of `sequence` are equal to `suffix`.



### map-combinations

```lisp
Function: (map-combinations function sequence &key start end length copy)
```
Calls `function` with each combination
of `length` constructable from the elements of the subsequence of
`sequence` delimited by `start` and `end`. `start` defaults to `0`,
`end` to length of `sequence`, and `length` to the length of the
delimited subsequence. (So unless `length` is specified there is only
a single combination, which has the same elements as the delimited
subsequence.) If `copy` is true (the default) each combination is
freshly allocated. If `copy` is false all combinations are `eq` to
each other, in which case consequences are specified if a combination
is modified by `function`.



### map-derangements

```lisp
Function: (map-derangements function sequence &key start end copy)
```
Calls `function` with each derangement
of the subsequence of `sequence` denoted by the bounding index
designators `start` and `end`. Derangement is a permutation of the
sequence where no element remains in place. `sequence` is not
modified, but individual derangements are `eq` to each other.
Consequences are unspecified if calling `function` modifies either the
derangement or `sequence`.



### map-permutations

```lisp
Function: (map-permutations function sequence &key start end length copy)
```
Calls function with each permutation of
`length` constructable from the subsequence of `sequence` delimited by
`start` and `end`. `start` defaults to `0`, `end` to length of the
sequence, and `length` to the length of the delimited subsequence.


## 5. IO
----


### read-file-into-string

```lisp
Function: (read-file-into-string pathname &key buffer-size external-format)
```
Return the contents of the file
denoted by `pathname` as a fresh string.

The `external-format` parameter will be passed directly to
`with-open-file` unless it's `nil`, which means the system default.



```lisp
Function: (read-file-into-byte-vector pathname)
```
Read `pathname` into a
freshly allocated (unsigned-byte 8) vector.


---------------

## 6. MACRO WRITING

### once-only

```lisp
Macro: (once-only specs &body forms)
```
Evaluates `forms` with symbols specified in
`specs` rebound to temporary variables, ensuring that each initform is
evaluated only once.

Each of `specs` must either be a symbol naming the variable to be
rebound, or of the form:

```lisp
            (symbol initform)
     
```

Bare symbols in `specs` are equivalent to

```lisp
            (symbol symbol)
     
```

Example:

```lisp
            (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
            (let ((y 0)) (cons1 (incf y))) =(1 . 1)
     
```




### with-gensyms

```lisp
Macro: (with-gensyms names &body forms)
```
Binds each variable named by a symbol in
`names` to a unique symbol around `forms`. Each of `names` must either
be either a symbol, or of the form:

```lisp
           (symbol string-designator)
     
```

Bare symbols appearing in `names` are equivalent to:

```lisp
           (symbol symbol)
     
```

The string-designator is used as the argument to `gensym` when
constructing the unique symbol the named variable will be bound to.




### with-unique-names

```lisp
Macro: (with-unique-names names &body forms)
```
Alias for `with-gensyms`.



### featurep

```lisp
Function: (featurep feature-expression)
```
Returns `t` if the argument matches the state
of the `*features*` list and `nil` if it does not.
`feature-expression` can be any atom or list acceptable to the reader
macros `#+` and `#-`.



### parse-body

```lisp
Function: (parse-body body &key documentation whole)
```
Parses `body` into (values remaining-forms
declarations doc-string). Documentation strings are recognized only if
`documentation` is true. Syntax errors in body are signalled and
`whole` is used in the signal arguments when given.



### parse-ordinary-lambda-list

```lisp
Function: (parse-ordinary-lambda-list lambda-list 
             &key normalize 
                  allow-specializers 
                  normalize-optional
                  normalize-keyword
                  normalize-auxilary)
```
Parses an ordinary
lambda-list, returning as multiple values:

`1`. Required parameters.

`2`. Optional parameter specifications, normalized into form:

```lisp
             (name init suppliedp)
     
```

`3`. Name of the rest parameter, or `nil`.

`4`. Keyword parameter specifications, normalized into form:

```lisp
             ((keyword-name name) init suppliedp)
     
```

`5`. Boolean indicating `&allow-other-keys` presence.

`6`. `&aux` parameter specifications, normalized into form

```lisp
             (name init).
     
```

Signals a `program-error` is the lambda-list is malformed.


---------

## 7. SYMBOLS


### ensure-symbol

```lisp
Function: (ensure-symbol name &optional package)
```
Returns a symbol with name designated by
`name`, accessible in package designated by `package`. If symbol is
not already accessible in `package`, it is interned there. Returns a
secondary value reflecting the status of the symbol in the package,
which matches the secondary return value of `intern`.

Example:

```lisp
            (ensure-symbol :cons :cl) =cl:cons, :external
     
```



### format-symbol

```lisp
Function: (format-symbol package control &rest arguments)
```
Constructs a string by applying
`arguments` to string designator `control` as if by `format` within
`with-standard-io-syntax`, and then creates a symbol named by that
string.

If `package` is `nil`, returns an uninterned symbol, if package is
`t`, returns a symbol interned in the current package, and otherwise
returns a symbol interned in the package designated by `package`.



### make-keyword

```lisp
Function: (make-keyword name)
```
Interns the string designated by `name` in
the `keyword` package.



### make-gensym

```lisp
Function: (make-gensym name)
```
If `name` is a non-negative integer, calls
`gensym` using it. Otherwise `name` must be a string designator, in
which case calls `gensym` using the designated string as the argument.



### make-gensym-list

```lisp
Function: (make-gensym-list length &optional x)
```
Returns a list of `length` gensyms,
each generated as if with a call to `make-gensym`, using the second
(optional, defaulting to "G") argument.



### symbolicate

```lisp
Function: (symbolicate &rest things)
```
Concatenate together the names of some
strings and symbols, producing a symbol in the current package.

--------

## 8. ARRAYS


### array-index
```
Type designator for an index into array of
`length:` an integer between `0` (inclusive) and `length` (exclusive).
`length` defaults to `array-dimension-limit`.



### array-length
```
Type designator for a dimension of an array
of `length:` an integer between `0` (inclusive) and `length`
(inclusive). `length` defaults to `array-dimension-limit`.



### copy-array

```lisp
Function: (copy-array array &key element-type fill-pointer
adjustable)
```
Returns an undisplaced copy of `array`, with
same fill-pointer and adjustability (if any) as the original, unless
overridden by the keyword arguments.

-------

## 9. TYPES


### string-designator

A string designator type. A string
designator is either a string, a symbol, or a character.




### coercef

```lisp
Macro: (coercef place type-spec env)
```
Modify-macro for `coerce`.



### of-type

```lisp
Function: (of-type type)
```
Returns a function of one argument, which
returns true when its argument is of `type`.



### type=

```lisp
Function: (type= type1 type2)
```
Returns a primary value of `t` is `type1` and
`type2` are the same type, and a secondary value that is true is the
type equality could be reliably determined: primary value of `nil` and
secondary value of `t` indicates that the types are not equivalent.



------------------------------------------------------------------------

## 10. NUMBERS


### maxf

```lisp
Macro: (maxf place &rest numbers env)
```
Modify-macro for `max`. Sets place designated by
the first argument to the maximum of its original value and `numbers`.




### minf

```lisp
Macro: (minf place &rest numbers env)
```
Modify-macro for `min`. Sets place designated by
the first argument to the minimum of its original value and `numbers`.



### binomial-coefficient

```lisp
Function: (binomial-coefficient n k)
```
Binomial coefficient of `n` and
`k`, also expressed as `n` choose `k`. This is the number of `k`
element combinations given `n` choises. `n` must be equal to or
greater then `k`.



### count-permutations

```lisp
Function: (count-permutations n &optional k)
```
Number of `k` element permutations
for a sequence of `n` objects. `k` defaults to `n`



### clamp

```lisp
Function: (clamp number min max)
```
Clamps the `number` into [min, max] range.
Returns `min` if `number` is lesser then `min` and `max` if `number`
is greater then `max`, otherwise returns `number`.



### lerp

```lisp
Function: (lerp v a b)
```
Returns the result of linear interpolation between
A and `b`, using the interpolation coefficient `v`.



### factorial

```lisp
Function: (factorial n)
```
Factorial of non-negative integer `n`.



### subfactorial

```lisp
Function: (subfactorial n)
```
Subfactorial of the non-negative integer
`n`.



### gaussian-random

```lisp
Function: (gaussian-random &optional min max)
```
Returns two gaussian random double
floats as the primary and secondary value, optionally constrained by
`min` and `max`. Gaussian random numbers form a standard normal
distribution around `0`.0d0.

Sufficiently positive `min` or negative `max` will cause the algorithm
used to take a very long time. If `min` is positive it should be close
to zero, and similarly if `max` is negative it should be close to
zero.



### iota

```lisp
Function: (iota n &key start step)
```
Return a list of n numbers, starting from `start`
(with numeric contagion from `step` applied), each consequtive number
being the sum of the previous one and `step`. `start` defaults to `0`
and `step` to `1`.

Examples:

```lisp
            (iota 4)                      =(0 1 2 3)
            (iota 3 :start 1 :step 1.0)   =(1.0 2.0 3.0)
            (iota 3 :start -1 :step -1/2) =(-1 -3/2 -2)
     
```



### map-iota

```lisp
Function: (map-iota function n &key start
step)
```
Calls `function` with `n` numbers, starting
from `start` (with numeric contagion from `step` applied), each
consequtive number being the sum of the previous one and `step`.
`start` defaults to `0` and `step` to `1`. Returns `n`.

Examples:

```lisp
            (map-iota #'print 3 :start 1 :step 1.0) =3
              ;;; 1.0
              ;;; 2.0
              ;;; 3.0
     
```



### mean

```lisp
Function: (mean sample)
```
Returns the mean of `sample`. `sample` must be a
sequence of numbers.



### median

```lisp
Function: (median sample)
```
Returns median of `sample`. `sample` must be a
sequence of real numbers.



### variance

```lisp
Function: (variance sample &key biased)
```
Variance of `sample`. Returns the biased
variance if `biased` is true (the default), and the unbiased estimator
of variance if `biased` is false. `sample` must be a sequence of
numbers.


### standard-deviation

```lisp
Function: (standard-deviation sample &key biased)
```
Standard deviation of `sample`.
Returns the biased standard deviation if `biased` is true (the
default), and the square root of the unbiased estimator for variance
if `biased` is false (which is not the same as the unbiased estimator
for standard deviation). `sample` must be a sequence of numbers.


