# numcl - Lispy clone of numpy

Version: 0.1

*(This documentation was possible due to the excellent 
[official documentation](https://numcl.github.io/numcl/).)*

## Introduction

This is a Numpy clone in Common Lisp. At the moment the library is
written in pure Common Lisp, focusing more on correctness and
usefulness, not speed. Track the progress at
<https://github.com/numcl/numcl/projects/1> .

### Goals

-   **Closely follow the numpy API, but still make it lispy.**
    -   Delegate the documentation effort to Numpy community.
-   **Replace the Common Lisp array interface.**
    -   We do not deviate from the traditional symbols/idioms in Common
        Lisp unless necessary. Therefore we provide symbols that
        conflicts the Common Lisp symbol. Math functions become aliases
        to the original CL functions when the inputs are not arrays.
    -   See *doc/DETAILS.org\#packages* .

### Features/Contracts

-   APIs are provided as functions, not macros.
    -   It is a design flaw otherwise.
    -   This does not mean the API is functional — we use procedural
        code.
-   Still, zero overhead.
    -   The APIs are simply the wrappers over simple functions and
        designed to be fully inlined.
    -   Optimization will be done on the compiler side, not by macros.
-   Operations are type-correct.
    -   They always return arrays of the most specific
        array-element-type. For example,
    -   (zeros 5) returns a bit vector.
    -   (asarray '(1 2 3)) returns an (unsigned-byte 2) vector.
    -   See *doc/DETAILS.org\#types* .
-   NUMCL Arrays are CL arrays.
    -   As this library aims to extend Common Lisp (not to replace part
        of it) in a compatible way, we do not introduce custom
        structures/classes for representing an array.
    -   See *doc/DETAILS.org\#representation* .

### Dependencies

NUMCL depends on the following libraries that must be installed manually
and other libraries that are automatically loaded by quicklisp.

-   <https://github.com/numcl/constantfold>
-   <https://github.com/numcl/gtype>
-   <https://github.com/numcl/specialized-function> .

With Roswell, installation can be done by

``` {.example}
ros install numcl/constantfold numcl/specialized-function numcl/gtype numcl/numcl
```

This library is at least tested on implementation listed below:

-   SBCL 1.4.12 on X86-64 Linux 4.4.0-141-generic (author's environment)
-   SBCL 1.5.1 on X86-64 Linux 4.4.0-141-generic (author's environment)
-   CI tested on CCL, ECL.

Dependency graph:

<img src="https://numcl.github.io/numcl/numcl.png">

## Getting Started

### Array Representation

NUMCL arrays are merely the displaced multidimentional arrays 
and no classes or structures are used.

However, in order to guarantee speed and simplify implementation,
arrays given to numcl functions must satisfy the following two
conditions:

-   Be a specialized array. Things of type `(array single-float)`,
    `(array (unsigned-byte 16))` etc.
-   Be an array displaced to a simple 1D specialized array. "[Simple
    array](http://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm)"
    means a non-displaced, non-adjustable array without fill pointer.

There are a few ways to create the required arrays:

```lisp
(reshape (arange 4.0) '(2 2))
(asarray #2A((0.0 1.0) (2.0 3.0)))
(asarray '((0.0 1.0) (2.0 3.0)))
(asarray '(#(0.0 1.0) #(2.0 3.0)))
(let ((a (zeros '(2 2) :type 'single-float)))
  (dotimes (i 2)
    (dotimes (j 2)
      (setf (aref a i j) ...))))
```

The names and the parameters of numcl functions mostly (rather strictly)
follows the numpy counterpart. There are even numpy names, such as
`dtype`, which are just aliases for `array-element-type`.

See [API Reference](#api-reference) for the complete list of functions.
Optionally, see [Array Representation Details](#array-representation-details) if required.

### Packages

NUMCL defines several symbols which have names identical to the
corresponding CL symbols. We call them **conflicting symbols**. To avoid
confusion in the code base, we use 3 packages:

-   `NUMCL.IMPL`: (internal package) for implementing numcl.
-   `NUMCL.EXPORTED`: (external package), for storing the numcl exported
    symbols,
-   `NUMCL`: package, that replaces `COMMON-LISP` package by
    shadowing-import symbols from `NUMCL.EXPORTED` on top of
    `COMMON-LISP` package.

### Types

In NUMCL, there is no `ratio` type:
 - CL prohibits `ratio` to have a denominator 1 (e.g. 3/1), and thus the
operations on ratios are not closed. 
  - No implementations provide a specialized array for `rational`.
  - Ratio computation requires
an additional simplification phase (e.g. 2/4 -&gt; 1/2) which does not
finish in a constant number of operations and is incompatible to SIMD
operations.

As a result, `ratios` are always converted to
`*numcl-default-float-format*`, which is single-float by default. This
means that `/` always returns a float array (except atomic numbers are
given).

We also force irrational functions to always return floats, by coercion.
(Implementations are allowed to return rationals for certain constants,
e.g. (sin pi).)

`(array bignum)` does not exist either. However, when the result of
numerical computation causes a fixnum overflow, it signals an error
instead of overflowing silently.

For complex arrays, only `(complex *-float)` exists (for each float
type). Both complex integers and complex ratios are converted into
floats. This is because CL does not allow rational complex with imagpart
0 (cf. <http://clhs.lisp.se/Body/t_comple.htm>), thus the numerical
operation always coerces the result into reals. This prevents us from
having (ARRAY (COMPLEX FIXNUM)).

This may be contrasted with that in [Common Lisp](#common-lisp-types) as provided.

### Examples

[example.lisp](https://github.com/numcl/numcl/blob/master/example.lisp) contains a script that you can explore
the functionality implemented so far in NUMCL.

## API Reference

As stated on the section on [Packages](#packages), NUMCL exports all the symbols in package CL, along with the ones with `numcl.exported`. Therefore, here, we only list the symbols exported by 
`numcl.exported`.

### \*

```lisp
Function: (* &rest args)
```

### \*\*

### \*\*\*

### +

```lisp
Function: (+ &rest args)
```

### ++

### +++

### -

```lisp
Function: (- first &rest args)
```

### /

```lisp
Function: (/ first &rest args)
```

### //

### ///

### /=

```lisp
Function: (/= x y)
```

### 1+

```lisp
Function: (1+ array)
```

### 1-

```lisp
Function: (1- array)
```

### <

```lisp
Function: (< x y)
```

### <=

```lisp
Function: (<= x y)
```

### =

```lisp
Function: (= x y)
```

### >

```lisp
Function: (> x y)
```

### >=

```lisp
Function: (>= x y)
```

### abs

```lisp
Function: (abs x)
```

### acos

```lisp
Function: (acos x)
```

### amax

```lisp
Function: (amax array &rest args &key axes type)
```

### amin

```lisp
Function: (amin array &rest args &key axes type)
```

### arange

```lisp
Function: (arange &rest args)
```
Arange's argument signature is irregular, following the API of numpy.
The interpretation of its arguments depends on the number of arguments.

 (arange stop            &key type)
 (arange start stop      &key type)
 (arange start stop step &key type)

Don't worry, we provide a compiler-macro to avoid the runtime dispatch.


### aref

```lisp
Function: (aref array &rest subscripts)
```
An extended `aref` that accepts ranges as lists, similar to numpy's array access.
For a 3D array x,

* range

```
x[1:5,2,3]   = (aref x '(1 5) 2 3)
x[2,1:5,3]   = (aref x 2 '(1 5) 3)
x[2,1:2:5,3] = (aref x 2 '(1 2 5) 3)
x[2,1:,3]    = (aref x 2 '(1 t) 3)
x[2,:1,3]    = (aref x 2 '(t 1) 3)
x[2,:,3]     = (aref x 2 '(t t) 3)
x[2,:,3]     = (aref x 2    t   3)
```

* insufficient axis

```commonlisp
(aref x '(1 5)) == (aref x '(1 5) t t)
(aref x 2 '(1 5)) == (aref x 2 '(1 5) t)
```

* newaxis

```commonlisp
(aref x '(1 2 5) nil 2 3)
```

* ellipsis

```commonlisp
(aref x '- 2) = (aref x t t 2) = x[...,2]
(aref x 2 '-) = (aref x 2 t t) = x[2,...]
(aref x 2 '- 3) = (aref x 2 t 3) = x[2,...,3]
(aref x 2 3 '-) = (aref x 2 3 t) = x[2,3,...]
```



### argmax

### argmin

### argwhere

```lisp
Function: (argwhere array fn)
```
Returns a list of the multidimentional indices of the elements which satisfies the predicate FN.
Note that the list elements are the multidimentional indices, even for a single-dimensional array.

### array-index-from-row-major-index

```lisp
Function: (array-index-from-row-major-index array row-major-index)
```
Takes a multidimentional array and a row-major-index.
 Returns a list containing the normal index.

### asarray

```lisp
Function: (asarray contents &key type)
```
Copy CONTENTS to a new array. NOTE: ASARRAY is *SLOW* as it recurses into the substructures.
When CONTENTS is a multidimentional array, its elements are copied to a new array that guarantees the NUMCL assumption.
When CONTENTS is a nested sequence, it is traversed up to the depth that guarantees the sane shape for an array.
When elements are copied, it is coerced to TYPE.
When TYPE is not given, it is replaced with the float-contagion type deduced from the elements of CONTENTS.
It may return a 0-dimensional array with CONTENTS being the only element.

For example:

```lisp
;; a vector of two lists.
(asarray '((1) (1 2)))               -> #((1) (1 2))
;; a 2D array of 4 lists.
(asarray '(((1) (1 2)) ((3) (3 4)))) -> #2A(((1) (1 2)) ((3) (3 4)))

(asarray '((1 2) (3 4)))    -> #2A((1 2) (3 4))
(asarray #(#(1 2) #(3 4)))  -> #2A((1 2) (3 4))
(asarray #((1 2) (3 4)))    -> #2A((1 2) (3 4))
```

However, this behavior may not be ideal because the resulting shape could be affected by the lengths of the strings.

```lisp
(asarray #(#(1 2) #(3 4)))   -> #2A((1 2) (3 4))
(asarray #(#(1 2) #(3 4 5))) -> #(#(1 2) #(3 4 5))

(asarray #("aa" "aa"))   -> #2A((#a #a) (#a #a))
(asarray #("aa" "aaa"))  -> #("aa" "aaa")
```

As a remedy to this problem, we allow TYPE to be a specifier for vector subtypes. Providing such a type specifier
will keep the leaf objects (e.g. strings) from split into individual elements.
We don't allow it to be a multidimentional array [at the moment.]

```lisp
(asarray #(#(1 2) #(3 4))   :type '(array fixnum (*))) -> #(#(1 2) #(3 4))
(asarray #(#(1 2) #(3 4 5)) :type '(array fixnum (*))) -> #(#(1 2) #(3 4 5))

(asarray #("aa" "aa")  :type 'string)    -> #("aa" "aa")
(asarray #("aa" "aaa") :type 'string)    -> #("aa" "aaa")

(asarray '((1 2) (3 4))   :type '(array fixnum (* *)))  -> error
```


### asin

```lisp
Function: (asin x)
```

### astype

```lisp
Function: (astype array type)
```

### atan

```lisp
Function: (atan x)
```

### avg

```lisp
Function: (avg array &key axes)
```

### bernoulli

```lisp
Function: (bernoulli p &optional shape)
```
Returns a bit array whose elements are 1 with probability P

### bernoulli-like

```lisp
Function: (bernoulli-like a)
```

### beta

```lisp
Function: (beta a b &optional shape (type
                                     (union-to-float-type (type-of a)
                                                          (type-of b))))
```

### binomial

```lisp
Function: (binomial n p &optional shape)
```

### broadcast

```lisp
Function: (broadcast function x y &key type (atomic function))
```
For binary functions

### ceiling

```lisp
Function: (ceiling number &optional (divisor 1))
```

### chisquare

### cis

```lisp
Function: (cis x)
```

### clip

```lisp
Function: (clip array min max)
```

### concatenate

```lisp
Function: (concatenate arrays &key (axis 0) out)
```

### conjugate

```lisp
Function: (conjugate x)
```

### copy

```lisp
Function: (copy array)
```

### cos

```lisp
Function: (cos x)
```

### cosh

```lisp
Function: (cosh x)
```

### denominator

```lisp
Function: (denominator x)
```

### diag

```lisp
Function: (diag a &optional result)
```
Return the diagonal element of a matrix as a vector

### dirichlet

### dot

### dtype

```lisp
Function: (dtype array)
```

### einsum

```lisp
Function: (einsum subscripts &rest args)
```
Performs Einstein's summation.
The SUBSCRIPT specification is significantly extended from that of Numpy
and can be seens as a full-brown DSL for array operations.

SUBSCRIPTS is a sequence of the form `(<SPEC>+ [-> <TRANSFORM>*] [-> [<SPEC>*])`.
The remaining arguments ARGS contain the input arrays and optionally the output arrays.

#### SPEC

The first set of SPECs specifies the input subscripts,
and the second set of SPECs specifies the output subscripts.
Unlike Numpy, there can be multiple output subscripts:
It can performs multiple operations in the same loop, then return multiple values.
The symbol `->` can be a string and can belong to any package because it is compared by STRING=.

Each SPEC is an alphabetical string designator, such as a symbol IJK or a string "IJK",
where each alphabet is considered as an index. It signals a type-error when it contains any
non-alpha char. 
Note that a symbol NIL is interpreted as an empty list rather than N, I and L.

Alternatively, each SPEC can be a list that contains a list of symbols.
For example, `((i j) (j k) -> (i k))` and `(ij jk -> ik)` are equivalent.

When -> and the output SPECs are omitted, a single output is assumed and its spec is
a union of the input specs.
For example, `(ij jk)` is equivalent to `(ij jk -> ijk)`.
Note that `(ij jk)` and `(ij jk ->)` have the different meanings:
The latter sums up all elements.

#### TRANSFORM

TRANSFORM is a list of element-wise operations.
The number of TRANSFORM should correspond to the number of outputs.
In each TRANSFORM, the elements in the input arrays can be referenced by $N, where N is a 1-indexed number.
Similarly the output array can be referred to by @N.

For example, `(ij ik -> (+ @1 (* $1 $2)) -> ik)` is equivalent to `(ij ik -> ik)` (a GEMM).

By default, TRANSFORM is `(+ @1 (* $1 ... $N))` for N inputs, which is equivalent to Einstein's summation.

#### ARGS

The shape of each input array should unify against the corresponding input spec. For example,
with a spec IJI, the input array should be of rank 3 as well as
the 1st and the 3rd dimension of the input array should be the same.

The shape of each output array is determined by the corresponding output spec.
For example, if SUBSCRIPTS is `(ij jk -> ik)`, the output is an array of rank 2,
and the output shape has the same dimension as the first input in the first axis,
and the same dimension as the second input in the second axis.

If the output arrays are provided, their shapes and types are also checked against the
corresponding output spec.  The types should match the result of the numerical
operations on the elements of the input arrays.

The outputs are calculated in the following rule.

+ The output array types are calculated based on the TRANSFORM, and 
  the shapes are calcurated based on the SPEC and the input arrays.
+ The output arrays are allocated and initialized by zeros.
+ Einsum nests one loop for each index in the input specs.
  For example, `(ij jk -> ik)` results in a triple loop.
+ In the innermost loop, each array element is bound to `$1..$N` / `@1..@N`.
+ For each `@i`, `i`-th TRANSFORM is evaluated and assigned to `@i`.

+ If the same index appears multiple times in a single spec,
  they share the same value in each iteration.
  For example, `(ii -> i)` returns the diagonal elements of the matrix.

When TRANSFORMs are missing, it follows naturally from the default TRANSFORM values that

+ When an index used in the input spec is missing in the output spec,
  the axis is aggregated over the iteration by summation.
+ If the same index appears across the different input specs,
  the element values from the multiple input arrays are aggregated by multiplication.
  For example, `(ij jk -> ik)` will perform
  `(setf (aref a2 i k) (* (aref a0 i j) (aref a1 j k)))`
  when a0, a1 are the input arrays and a2 is the output array.

For example, (einsum '(ij jk) a b) is equivalent to:

```commonlisp
 (dotimes (i <max> <output>)
   (dotimes (j <max>)
     (dotimes (k <max>)
       (setf (aref <output> i j k) (* (aref a i j) (aref b j k))))))
```

#### Performance

If SUBSCRIPTS is a constant, the compiler macro
builds an iterator function and make them inlined. Otherwise,
a new function is made in each call to einsum, resulting in a large bottleneck.
 (It could be memoized in the future.)

The nesting order of the loops are automatically decided based on the specs.
The order affects the memory access pattern and therefore the performance due to
the access locality. For example, when writing a GEMM which accesses three matrices
by `(setf (aref output i j) (* (aref a i k) (aref b k j)))`,
it is well known that ikj-loop is the fastest among other loops, e.g. ijk-loop.
EINSUM reorders the indices so that it maximizes the cache locality.


### empty

```lisp
Function: (empty shape &key (type 'bit))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### empty-like

```lisp
Function: (empty-like array &key (type (array-element-type array)))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### exp

```lisp
Function: (exp x)
```

### expand-dims

```lisp
Function: (expand-dims a axes)
```
axes: an int or a list of ints

### exponential

```lisp
Function: (exponential scale &optional shape (type
                                              (union-to-float-type
                                               (type-of scale))))
```

### eye

```lisp
Function: (eye n &key (m n) (k 0) (type 'bit))
```
Returns a matrix whose k-th diagnonal filled with 1.
 N,M specifies the shape of the return array. K will adjust the sub-diagonal -- positive K moves it upward.

### f

```lisp
Function: (f dfnum dfden &optional shape (type
                                          (union-to-float-type (type-of dfnum)
                                                               (type-of dfden))))
```

### fceiling

```lisp
Function: (fceiling number &optional (divisor 1))
```

### ffloor

```lisp
Function: (ffloor number &optional (divisor 1))
```

### flatten

```lisp
Function: (flatten a)
```

### floor

```lisp
Function: (floor number &optional (divisor 1))
```

### fround

```lisp
Function: (fround number &optional (divisor 1))
```

### ftruncate

```lisp
Function: (ftruncate number &optional (divisor 1))
```

### full

```lisp
Function: (full shape value &key (type (type-of value)))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### full-like

```lisp
Function: (full-like array value &key (type (array-element-type array)))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### gamma

```lisp
Function: (gamma k &optional (theta 1.0) shape (type
                                                (union-to-float-type
                                                 (type-of k) (type-of theta))))
```

### geometric

### gumbel

### histogram

```lisp
Function: (histogram array &key (low (amin array)) (high (amax array)) (split 1))
```
Returns a fixnum vector representing a histogram of values.
The interval between LOW and HIGH are split by STEP value.
All values less than LOW are put in the 0-th bucket;
All values greater than equal to HIGH are put in the last bucket.

### hypergeometric

### imagpart

```lisp
Function: (imagpart x)
```

### inner

```lisp
Function: (inner a b &optional result)
```
Inner product of two vectors.

### integer-length

```lisp
Function: (integer-length x)
```

### invalid-array-index-error


```lisp
NIL
```


  NUMCL.EXPORTED:SHAPE
Initargs: :shape
Readers: numcl.exported:invalid-array-index-error-shape
Writers: (setf numcl.exported:invalid-array-index-error-shape)

#### axis

```lisp
Initargs: :axis
Readers: numcl.exported:invalid-array-index-error-axis
Writers: (setf numcl.exported:invalid-array-index-error-axis)

#### subscripts

```lisp
Initargs: :subscripts
Readers: numcl.exported:invalid-array-index-error-subscripts
Writers: (setf numcl.exported:invalid-array-index-error-subscripts)### kron

```lisp
Function: (kron a b &optional result)
```
Compute the kronecker product of two vectors.

### laplace

### length

```lisp
Function: (length array)
```

### linspace

```lisp
Function: (linspace start stop length &key type endpoint)
```

### log

```lisp
Function: (log x)
```

### logand

```lisp
Function: (logand &rest args)
```

### logandc1

```lisp
Function: (logandc1 &rest args)
```

### logandc2

```lisp
Function: (logandc2 &rest args)
```

### logcount

```lisp
Function: (logcount x)
```

### logeqv

```lisp
Function: (logeqv &rest args)
```

### logior

```lisp
Function: (logior &rest args)
```

### logistic

### lognand

```lisp
Function: (lognand &rest args)
```

### lognor

```lisp
Function: (lognor &rest args)
```

### lognormal

### lognot

```lisp
Function: (lognot x)
```

### logorc1

```lisp
Function: (logorc1 &rest args)
```

### logorc2

```lisp
Function: (logorc2 &rest args)
```

### logseries

### logxor

```lisp
Function: (logxor &rest args)
```

### map

```lisp
Function: (map result-type function &rest sequences)
```

### map-array

```lisp
Function: (map-array function &rest sequences)
```

### map-array-into

```lisp
Function: (map-array-into result-sequence function &rest sequences)
```

### map-into

```lisp
Function: (map-into result-sequence function &rest sequences)
```

### matmul

```lisp
Function: (matmul a b &optional result)
```
Matrix product of two arrays.

### max

```lisp
Function: (max &rest args)
```

### mean

```lisp
Function: (mean array &key axes)
```

### min

```lisp
Function: (min &rest args)
```

### mod

```lisp
Function: (mod number &optional (divisor 1))
```

### multinomial

```lisp
Function: (multinomial n pvals &optional shape)
```
pvals is a sequence of probabilities summing up to 1.

### multivariate-normal

### negative-binomial

```lisp
Function: (negative-binomial n p &optional shape)
```

### noncentral-chisquare

### noncentral-f

### nonzero

```lisp
Function: (nonzero array)
```
collect multidimentional indices where the element is nonzero

### normal

```lisp
Function: (normal &optional (mean 0.0) (var 1.0) shape (type
                                                        (union-to-float-type
                                                         (type-of mean)
                                                         (type-of var))))
```

### numcl-array

### numcl-array-p

```lisp
Function: (numcl-array-p array)
```
Returns true when ARRAY satisfies the NUMCL assumption, that is,
an array displaced to a non-displaced 1D array.

### numerator

```lisp
Function: (numerator x)
```

### onehot

### ones

```lisp
Function: (ones shape &key (type 'bit))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### ones-like

```lisp
Function: (ones-like array &key (type (array-element-type array)))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### outer

```lisp
Function: (outer a b &optional result)
```
Compute the outer product of two vectors.

### pareto

### phase

```lisp
Function: (phase x)
```

### poisson

```lisp
Function: (poisson &optional (lambda 1.0) shape (type
                                                 (union-to-float-type
                                                  (type-of lambda))))
```

### power

### prod

```lisp
Function: (prod array &rest args &key axes type)
```

### rank

```lisp
Function: (rank array)
```

### rayleigh

### realpart

```lisp
Function: (realpart x)
```

### reduce-array

```lisp
Function: (reduce-array fn array &key axes (type
                                            (%reduce-array-result-type array
                                                                       fn)) (initial-element
                                                                             (zero-value
                                                                              type)))
```

### rem

```lisp
Function: (rem number &optional (divisor 1))
```

### reshape

```lisp
Function: (reshape a shape)
```
Reshape the array while sharing the backing 1D array.
-1 implies that the axis size is deduced from the other axes. At most one axis is allowed to be -1.
T  implies that the axis size is preserved. It can be used as many times, but only at the right/leftmost axes.

Example of reshaping (3 8 5):

valid:

    (6 -1 10)     = (6 2 10)
    (t 2 2 2 t)   = (3 2 2 2 5)
    (3 t t)       = (3 8 5)
    (2 -1 2 2 t)  = (2 3 2 2 5)

invalid:

    (2 t 2 2 t)


### round

```lisp
Function: (round number &optional (divisor 1))
```

### shape

```lisp
Function: (shape array)
```

### shuffle

```lisp
Function: (shuffle array-or-sequence &key (start 0) end)
```

This code extends alexandria:shuffle.
It additionally accepts arrays and shuffles the elements according to the first axis,
viewing the remaining axes as one "element".

Original documentation:

Returns a random permutation of SEQUENCE bounded by START and END.
Original sequence may be destructively modified, and (if it contains
CONS or lists themselv) share storage with the original one.
Signals an error if SEQUENCE is not a proper sequence.


### signum

```lisp
Function: (signum x)
```

### sin

```lisp
Function: (sin x)
```

### sinh

```lisp
Function: (sinh x)
```

### size

```lisp
Function: (size array)
```

### sqrt

```lisp
Function: (sqrt x)
```

### square

```lisp
Function: (square x)
```

### squeeze

```lisp
Function: (squeeze a)
```

### stack

```lisp
Function: (stack arrays &key (axis 0) out)
```

### standard-cauchy

### standard-deviation

```lisp
Function: (standard-deviation array &key axes)
```

### standard-exponential

### standard-gamma

### standard-normal

### standard-t

### stdev

```lisp
Function: (stdev array &key axes)
```

### sum

```lisp
Function: (sum array &rest args &key axes type)
```

### take

```lisp
Function: (take array indices)
```
Collect the elements using a list of multidimentional indices (in a format returned by WHERE).

### tan

```lisp
Function: (tan x)
```

### tanh

```lisp
Function: (tanh x)
```

### to-simple-array

```lisp
Function: (to-simple-array array)
```
Returns a simple array of the equivalent contents.

### transpose

```lisp
Function: (transpose matrix &optional result)
```
Reverses the axes of an array.

### tri

```lisp
Function: (tri n &key (m n) (k 0) (type 'bit))
```
Returns a triangle matrix whose lower diagnonal (including the diagonal) filled with 1.
 N,M specifies the shape of the return array. K will adjust the sub-diagonal -- positive K fills more 1s.

### triangular

### tril

```lisp
Function: (tril matrix &optional (k 0))
```
Returns the copy of matrix with elements above the k-th diagonal zeroed. Positive K fills less 0s.

### triu

```lisp
Function: (triu matrix &optional (k 0))
```
Returns the copy of matrix with elements below the k-th diagonal zeroed. Positive K fills more 0s.

### truncate

```lisp
Function: (truncate number &optional (divisor 1))
```

### uniform

```lisp
Function: (uniform &optional (low 0.0) (high 1.0) shape type)
```

### unstack

```lisp
Function: (unstack array &key (axis 0))
```

### vander

```lisp
Function: (vander v &key (n (length v)) increasing)
```
Returns a matrix where M[i,j] == V[i]^(N-j) when increasing is false (default), and 
 M[i,j] == V[i]^j when increasing is true.

### var

```lisp
Function: (var array &key axes)
```

### variance

```lisp
Function: (variance array &key axes)
```

### vdot

```lisp
Function: (vdot a b &optional result)
```
Dot product of two vectors. For complex values, the first value is conjugated.

### vonmises

### wald

### weibull

### where

```lisp
Function: (where array fn)
```
Returns a list of list of indices of the elements which satisfies the predicate FN.
The first list contains the indices for the 1st dimension,
the second list contains the indices for the 2nd dimension, and so on.

### zeros

```lisp
Function: (zeros shape &key (type 'bit))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### zeros-like

```lisp
Function: (zeros-like array &key (type (array-element-type array)))
```
Equivalent of the same function in numpy. Note the default `type` difference.

+ `empty` : does not explicitly fill the array. In an unsafe compiler setting,
  junk value may appear.
+ `full`  : fill the array with a certain value.
+ `zeros`, `ones` : fill the array with zeros / ones. `type` affects the actual
  value being filled.
+ `X-like` : similar to above functions, but takes another array and returns the
  array of the same shape.


### zipf

## More Discussion

### Common Lisp Types

Common Lisp has the following types for numbers.

```lisp
number = (or complex real)
real   = (or float rational)
rational = (or ratio integer)
integer  = (or fixnum bignum)
float    = (or short-float ... long-float) (== irrational).
```

Common Lisp defines several rules for the type of the values returned by
the numerical operations. The detail of the rules are explained in [CLHS
12.1 Number Concepts](http://clhs.lisp.se/Body/12_a.htm).

*Rational functions* behave as `rational* -> rational`,
`float* -> float`, `{rational,float}* -> float`. This rule is called
**float contagion** rule.

Rational functions do not guarantee `integer -> integer`, primarily due
to `/` , which returns `integer* -> (or ratio integer)`.

Irrational functions behaves as `rational -> (or rational float)`,
`float -> float`: For a certain irrational functions, implementations
are allowed to return the exact rational number or its float
approximation. Examples are `(sin pi) -> 1/2`. The behavior depends on
the implementation and is called **float substitution rule**.

### Array Representation Details

NUMCL arrays are not based on custom classes or structures. They are
merely the displaced multidimentional arrays.

In order to guarantee the speed and to simplify the implementation, the
arrays given to numcl functions must satisfy the following two
conditions:

-   It is a specialized array. Things of type `(array single-float)`,
    `(array (unsigned-byte 16))` etc.
-   It is an array displaced to a simple 1D specialized array. "[Simple
    array](http://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm)"
    means a non-displaced, non-adjustable array without fill pointer.

The base function for creating a new array is `%make-array`, but this is
not exported in NUMCL. You should use the wrapper functions like `ones`,
`zeros`, `ones-like`, `arange`, `linspace`, `asarray` etc. They are
always inline-expanded to `%make-array`, therefore there is no worry
about the performance. These functions analyze the input and return the
most specialized array for the input, but you can also specify the
element type.

`%make-array` instantiates a new flattened array and returns another
array displaced to it with the specified shape. The flattened array is
returned as the secondary value (as does most other numcl functions).

The justification for this scheme is that some implementations (esp.
SBCL) require an indirection for accessing the array element (e.g.
through array-header in SBCL) even for a simple multi-dimentional array
and thus using a displacing array has essentially no performance penalty
over using a simple multi-dimentional array.

We also ensure that the length of the base arrays are the multiples of
8. This ensures that the program can safely iterate over the extended
region with a future support for SIMD operations in mind.


## Author, License, Copyright

Masataro Asai (guicho2.71828@gmail.com)
Licensed under LGPL v3.
Copyright (c) 2019 IBM Corporation
