# cl-ppcre - Regular Expressions

Version: 2.1.1
<br/>
Licence: BSD
<br/>
Nickname: ppcre
<br/>
Repository: [edicl/cl-ppcre - Github](https://github.com/edicl/cl-ppcre)
<br>
See also: [awesome-cl/#regex](https://github.com/CodyReichert/awesome-cl#regex)

*This documentation is possible the [excellent official documentation](https://edicl.github.io/cl-ppcre) as of 4th May 2020.*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

***

CL-PPCRE is a Portable Perl-Compatible Regular Expressions library for Common Lisp.
This is also thread-safe, and allows specifying [regular expressions using
S-expressions](#using-s-expressions).


## GETTING STARTED

### Using perl regex

Per the name, `cl-ppcre` is more or less compatible with perl 5.8 including
extended features like non-greedy repetitions, positive and negative
look-ahead and look-behind assertions, "standalone" subexpressions,
and conditional subpatterns. The following Perl features are
(currently) **not** supported:

-   `(?{ code })` and `(??{ code })` because they obviously don't make
    sense in Lisp.
-   `\N{name}` (named characters), `\x{263a}` (wide hex characters),
    `\l`, `\u`, `\L`, and `\U` because they're actually not part of
    Perl's *regex* syntax - but see
    [CL-INTERPOL](https://github.com/edicl/cl-interpol/).
-   `\X` (extended Unicode), and `\C` (single character). But you can
    of course use all characters supported by your CL implementation.
-   Posix character classes like `[[:alpha]]`. Load `(asdf:load-system :cl-ppcre-unicode)`
    to install [unicode-preperty-resolver](#unicode-property-resolver) as your
    [property-resolver](#property-resolver). See [cl-unicode](https://edicl.github.io/cl-unicode/)
    for the supported unicode properties and their naming conventions.
-   `\G` for Perl's `pos()` because we don't have it.

Note, however, that `\t`, `\n`, `\r`, `\f`, `\a`, `\e`, `\033` (octal
character codes), `\x1B` (hexadecimal character codes), `\c[` (control
characters), `\w`, `\W`, `\s`, `\S`, `\d`, `\D`, `\b`, `\B`, `\A`,
`\Z`, and `\z` **are** supported.

Users can straightaway start with:

- [scan](#scan)
- [scan-to-strings](#scan-to-strings)
- [split](#split)
- [quote-meta-chars](#quote-meta-chars)
- [register-groups-bind](#register-groups-bind)
- [regex-replace](#regex-replace)

Register groups simply refer to the captured groups. For instance:

```lisp
CL-USER> (register-groups-bind (a b) ("([^ ]+) ([^ ]+)" "hello world")
           (list a b))
("hello" "world")
```

See the [other macros](#do-matches) for iterative versions of these.

Those wanting to get into perl regular expressions, might find [their official
documentation](https://perldoc.perl.org/perlre.html) useful.

Users comfortable with regular expressions might also want to try
[cl-interpol](https://github.com/edicl/cl-interpol/) - which provides string
interpolation facilities to the lisp reader. (Yes, it's defacto!)

### Using s-expressions

The basics:

```
+------- Common Lisp Equivalent -------+----------- Perl Equivalent -----------+
| String / Character                   | Literal treatment                     |
| :void                                | Empty string                          |
| :everything                          | Dot                                   |
| :(non-)word-boundary                 | \b, \B (non)                          |
| :(non-)digit-class                   | \d, \D (non)                          |
| :(non-)word-char-class               | \w, \W (non)                          |
| :(non-)whitespace-char-class         | \s, \S (non)                          |
| :start/end-anchor                    | ^ [start], $ [end                     |
| :modeless-start/end-anchor           | \A [start], \Z [end]                  |
| :modeless-end-anchor-no-newline      | \z                                    |
| :case-(in)sensitive-p                | (?i), (?-i) [insensitive]             |
| :(not)-multi-line-mode-p             | (?m), (?-m) [not]                     |
| :(not)-single-line-mode-p            | (?s), (?-s) [not]                     |
| (anything else)                      | Syntax Error                          |
+--------------------------------------+---------------------------------------+
```

Simple combinations include `:sequence`, `:group`, `:flags`, `:register`,
`:greedy-repetition`, `:non-greedy-repetition`. You could play around with
[parse-string](#parse-string) to learn more about the equivalence.

For instance:

```lisp
* (parse-string "(ab)*")
(:GREEDY-REPETITION 0 `NIL` (:REGISTER "ab"))

* (parse-string "(a(b))")
(:REGISTER (:SEQUENCE #\a (:REGISTER #\b)))

* (parse-string "(?:abc){3,5}")
(:GREEDY-REPETITION 3 5 (:GROUP "abc"))
;; (:GREEDY-REPETITION 3 5 "abc") would also be OK

* (parse-string "a(?i)b(?-i)c")
(:SEQUENCE #\a
 (:SEQUENCE (:FLAGS :CASE-INSENSITIVE-P)
  (:SEQUENCE #\b (:SEQUENCE (:FLAGS :CASE-SENSITIVE-P) #\c))))
;; same as (:SEQUENCE #\a :CASE-INSENSITIVE-P #\b :CASE-SENSITIVE-P #\c)

* (parse-string "(?=a)b")
(:SEQUENCE (:POSITIVE-LOOKAHEAD #\a) #\b)

* (parse-string "aa|aaa")
(:ALTERNATION "aa" "aaa")
```

See the [manual](https://edicl.github.io/cl-ppcre/#create-scanner2) for the
more detailed equivalence.

Thus, you could equivalently use parse-trees for the functions and macros that
expect regex.

```lisp
* (all-matches-as-strings '(:greedy-repetition 1 nil #\a) "aaaa")
("aaaa")
```

### Performance Aspects

`cl-ppcre` was intended to be fast. Indeed, [when it first appeared,
it was perhaps the fastest](http://web.archive.org/web/20080624164217/http://weitz.de/cl-ppcre/#bench).

However, in 2020, it can be five times as slow as Perl:

```sh
time perl -e '"@{['x' x 50000000]}" =~ /([xy])*/'

real	0m0.245s
user	0m0.161s
sys	    0m0.084s
```


```lisp
CL-USER> (time (progn
                 (scan "([xy])*"
                       (make-string 50000000 :element-type 'base-char
                                    :initial-element #\x))
                 nil))
Evaluation took:
  1.116 seconds of real time
  1.115901 seconds of total run time (1.111972 user, 0.003929 system)
  100.00% CPU
  2,463,959,814 processor cycles
  50,000,032 bytes consed
```

But still (more than) 5 times faster than python:

```sh
time python3 -c 'import re; x = re.search("([xy])*", "x"*50000000)'

real	0m7.458s
user	0m4.563s
sys	    0m2.892s
```

Or slower as well:

```sh
$ ~ time python3 -c 'import re; x = re.search("x*", "x"*50000000)'

real	0m0.146s
user	0m0.109s
sys	    0m0.036s
```

Honestly, though, it should be possible to use implementation specific means to speed things up
- or copying the developments in the perl and/or python world.

Perhaps, see the [manual](https://edicl.github.io/cl-ppcre/#blabla) for hints on speeding things
up.

## FUNCTIONS AND MACROS

### all-matches

```lisp
Function: (all-matches regex target-string
            &key (start 0) (end (length target-string)))
```
Returns a list containing the start and end positions of all
matches of `regex` against `target-string`, i.e. if there are N matches
the list contains (* 2 N) elements.  If `regex` matches an empty string
the scan is continued one position behind this match.

### all-matches-as-strings

```lisp
Function: (all-matches-as-strings regex target-string
            &key (start 0) (end (length target-string)) sharedp)
```
Returns a list containing all substrings of `target-string` which
match `regex`. If `regex` matches an empty string the scan is continued
one position behind this match. If `sharedp` is true, the substrings may
share structure with `target-string`.

### create-optimized-test-function

```lisp
Function: (create-optimized-test-function test-function
            &key (start 0) (end *regex-char-code-limit*)
              (kind *optimize-char-classes*))
```
Given a unary test function which is applicable to characters
returns a function which yields the same boolean results for all
characters with character codes from `start` to (excluding) `end`.  If
`kind` is `nil`, `test-function` will simply be returned.  Otherwise, `kind`
should be one of:

* `:hash-table` - builds a hash table representing all characters which
                satisfy the test and returns a closure which checks if
                a character is in that hash table

* `:charset` - instead of a hash table uses a "charset" which is a
             data structure using non-linear hashing and optimized to
             represent (sparse) sets of characters in a fast and
             space-efficient way (contributed by Nikodemus Siivola)

* `:charmap` - instead of a hash table uses a bit vector to represent
             the set of characters

You can also use `:hash-table*` or `:charset*` which are like `:hash-table`
and `:charset` but use the complement of the set if the set contains
more than half of all characters between `start` and `end`.  This saves
space but needs an additional pass across all characters to create the
data structure.  There is no corresponding `:charmap*` kind as the bit
vectors are already created to cover the smallest possible interval
which contains either the set or its complement.

### create-scanner

```lisp
Function: (create-scanner regex
            &key case-insensitive-mode multi-line-mode 
              single-line-mode extended-mode destructive)
```
Accepts a regular expression - either as a
parse-tree or as a string - and returns a scan closure which will scan
strings for this regular expression and a list mapping registers to
their names (`nil` stands for unnamed ones).  The "mode" keyword
arguments are equivalent to the imsx modifiers in Perl.  If
`destructive` is not `nil`, the function is allowed to destructively
modify its first argument (but only if it's a parse tree).

(More in the manual!)

### define-parse-tree-synonym

```lisp
Macro: (define-parse-tree-synonym name parse-tree)
```
Defines the symbol `name` to be a synonym for the parse tree
`parse-tree`.  Both arguments are quoted.

### do-matches

```lisp
Macro: (do-matches (match-start match-end regex target-string
                     &optional result-form &key start end)
         &body body)
```
Iterates over `target-string` and tries to match `regex` as often as
possible evaluating `body` with `match-start` and `match-end` bound to the
start/end positions of each match in turn.  After the last match,
returns `result-form` if provided or `nil` otherwise.  An implicit block
named `nil` surrounds `do-matches`; `return` may be used to terminate the
loop immediately.  If `regex` matches an empty string the scan is
continued one position behind this match. `body` may start with
declarations.

### do-matches-as-strings

```lisp
Macro: (do-matches-as-strings
           (match-var regex target-string 
             &optional result-form &key start end sharedp)
         &body body)
```
Iterates over `target-string` and tries to match `regex` as often as
possible evaluating `body` with `match-var` bound to the substring of
`target-string` corresponding to each match in turn.  After the last
match, returns `result-form` if provided or `nil` otherwise.  An implicit
block named `nil` surrounds `do-matches-as-strings`; `return` may be used to
terminate the loop immediately.  If `regex` matches an empty string the
scan is continued one position behind this match.  If `sharedp` is true,
the substrings may share structure with `target-string`.  `body` may start
with declarations.

### do-register-groups

```lisp
Macro: (do-register-groups
            var-list
            (regex target-string 
               &optional result-form &key start end sharedp)
          &body body)
```
Iterates over `target-string` and tries to match `regex` as often as
possible evaluating `body` with the variables in `var-list` bound to the
corresponding register groups for each match in turn, i.e. each
variable is either bound to a string or to `nil`.  For each element of
`var-list` which is `nil` there's no binding to the corresponding register
group. The number of variables in `var-list` must not be greater than
the number of register groups.  After the last match, returns
`result-form` if provided or `nil` otherwise.  An implicit block named `nil`
surrounds `do-register-groups`; `return` may be used to terminate the loop
immediately. If `regex` matches an empty string the scan is continued
one position behind this match.  If `sharedp` is true, the substrings
may share structure with `target-string`.  `body` may start with
declarations.

### do-scans

```lisp
Macro: (do-scans (match-start match-end reg-starts reg-ends regex target-string
                    &optional result-form &key start end) 
          &body body)
```
Iterates over `target-string` and tries to match `regex` as often as
possible evaluating BODY with `match-start`, `match-end`, `reg-starts`, and
`reg-ends` bound to the four return values of each match in turn.  After
the last match, returns `result-form` if provided or `nil` otherwise. An
implicit block named `nil` surrounds `do-scans`; `return` may be used to
terminate the loop immediately.  If `regex` matches an empty string the
scan is continued one position behind this match. `body` may start with
declarations.

### parse-string

```lisp
Function: (parse-string string)
```
Translate the regex string `string` into a parse tree.

### parse-tree-synonym

```lisp
Function: (parse-tree-synonym symbol)
```
Returns the parse tree the `symbol` symbol is a synonym for.  Returns
`nil` is `symbol` wasn't yet defined to be a synonym.

### ppcre-error

Every error signaled by CL-PPCRE is of type `ppcre-error`. This is a direct subtype of
[simple-error](http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_er.htm)
without any additional slots or options.

### ppcre-invocation-error

Errors of type `ppcre-invocation-error` are signaled if one of the exported functions of CL-PPCRE is called with wrong or inconsistent arguments. This is a direct subtype of `ppcre-error` without any additional slots or options.

### ppcre-syntax-error

An error of type `ppcre-syntax-error` is signaled if CL-PPCRE's parser encounters an error when trying to parse a regex string or to convert a parse tree into its internal representation. This is a direct subtype of `ppcre-error` with two additional slots. These denote the regex string which HTML-PPCRE was parsing and the position within the string where the error occurred. If the error happens while CL-PPCRE is converting a parse tree, both of these slots contain NIL. (See the next two entries on how to access these slots.)

### ppcre-syntax-error-pos

```lisp
Function: (ppcre-syntax-error-pos condition)
```
Returns the position within the string where the error occurred
(or `nil` if the error happened while trying to convert a parse tree

### ppcre-syntax-error-string

```lisp
Function: (ppcre-syntax-error-string condition)
```
Returns the string the parser was parsing when the error was
encountered (or `nil` if the error happened while trying to convert a
parse tree).


### quote-meta-chars

```lisp
Function: (quote-meta-chars string &key (start 0) (end (length string)))
```
Quote, i.e. prefix all non-word characters in `string` with `#\\`.

### regex-apropos

```lisp
Function: (regex-apropos regex &optional packages &key (case-insensitive t))
```
Similar to the standard function `apropos` but returns a list of all
symbols which match the regular expression REGEX.  If `case-insensitive`
is true and `regex` isn't already a scanner, a case-insensitive scanner
is used.

### regex-apropos-list

```lisp
Function: (regex-apropos-list regex &optional packages &key (case-insensitive t))
```
Similar to the standard function `apropos-list` but returns a list of
all symbols which match the regular expression `regex`.  If
`case-insensitive` is true and `regex` isn't already a scanner, a
case-insensitive scanner is used.

### regex-replace

```lisp
Function: (regex-replace regex target-string replacement
             &key (start 0) (end (length target-string))
               preserve-case simple-calls
               (element-type 'character))
```
Try to match `target-string` between `start` and `end` against `regex` and
replace the first match with `replacement`.  Two values are returned;
the modified string, and `t` if `regex` matched or `nil` otherwise.

  `replacement` can be a string which may contain the special substrings
"\&" for the whole match, "\`" for the part of `target-string`
before the match, "\'" for the part of `target-string` after the
match, "\N" or "\{N}" for the Nth register where N is a positive
integer.

  `replacement` can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by `replacement` with the arguments `target-string`, `start`,
`end`, `match-start`, `match-end`, `reg-starts`, and `reg-ends`. (`reg-starts` and
`reg-ends` are arrays holding the start and end positions of matched
registers or `nil` - the meaning of the other arguments should be
obvious.)

  Finally, `replacement` can be a list where each element is a string,
one of the symbols `:match`, `:before-match`, or `:after-match` -
corresponding to "\&", "\`", and "\'" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If `preserve-case` is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if `regex` doesn't
match.

  `element-type` is the element type of the resulting string.

### regex-replace-all

```lisp
Function: (regex-replace-all regex target-string replacement
             &key (start 0) (end (length target-string))
             preserve-case simple-calls 
             (element-type 'character))
```
Try to match `target-string` between `start` and `end` against `regex` and
replace all matches with `replacement`.  Two values are returned; the
modified string, and T if `regex` matched or `nil` otherwise.

  `replacement` can be a string which may contain the special substrings
"\&" for the whole match, "\`" for the part of `target-string`
before the match, "\'" for the part of `target-string` after the
match, "\N" or "\{N}" for the Nth register where N is a positive
integer.

  `replacement` can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by `replacement` with the arguments `target-string`, `start`,
`end`, `match-start`, `match-end`, `reg-starts`, and `reg-ends`. (`reg-starts` and
`reg-ends` are arrays holding the start and end positions of matched
registers or `nil` - the meaning of the other arguments should be
obvious.)

  Finally, `replacement` can be a list where each element is a string,
one of the symbols `:match`, `:before-match`, or `:after-match` -
corresponding to "\&", "\`", and "\'" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If `preserve-case` is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if `regex` doesn't
match.

  `element-type` is the element type of the resulting string.

### register-groups-bind

```lisp
Macro: (register-groups-bind var-list
            (regex target-string &key start end sharedp) 
          &body body)
```
Executes `body` with the variables in `var-list` bound to the
corresponding register groups after `target-string` has been matched
against `regex`, i.e. each variable is either bound to a string or to
`nil`.  If there is no match, `body` is _not_ executed. For each element
of `var-list` which is `nil` there's no binding to the corresponding
register group.  The number of variables in `var-list` must not be
greater than the number of register groups.  If `sharedp` is true, the
substrings may share structure with `target-string`.

### scan

```lisp
Function: (scan regex target-string &key start end real-start-pos)
```
Searches `target-string` from `start` to `end` and tries
to match `regex`.  On success returns four values - the start of the
match, the end of the match, and two arrays denoting the beginnings
and ends of register matches.  On failure returns `nil`.  `regex` can be a
string which will be parsed according to Perl syntax, a parse tree, or
a pre-compiled scanner created by [create-scanner](#create-scanner).  `target-string` will
be coerced to a simple string if it isn't one already.  The
`real-start-pos` parameter should be ignored - it exists only for
internal purposes.


### scan-to-strings

```lisp
Function: (scan-to-strings regex target-string
             &key (start 0) (end (length target-string)) sharedp)
```
Like `scan` but returns substrings of `target-string` instead of
positions, i.e. this function returns two values on success: the whole
match as a string plus an array of substrings (or `nil`s) corresponding
to the matched registers.  If `sharedp` is true, the substrings may
share structure with `target-string`.

### split

```lisp
Function: (split regex target-string
             &key (start 0) (end (length target-string))
               limit with-registers-p omit-unmatched-p sharedp)
```
Matches `regex` against `target-string` as often as possible and
returns a list of the substrings between the matches.  If
`with-registers-p` is true, substrings corresponding to matched
registers are inserted into the list as well.  If `omit-unmatched-p` is
true, unmatched registers will simply be left out, otherwise they will
show up as `nil`.  `limit` limits the number of elements returned -
registers aren't counted.  If `limit` is `nil` (or 0 which is
equivalent), trailing empty strings are removed from the result list.
If `regex` matches an empty string the scan is continued one position
behind this match.  If `sharedp` is true, the substrings may share
structure with `target-string`.


## CONFIGURATION VARIABLES

### \*allow-named-registers\*

Whether the parser should support AllegroCL's named registers
`(?<name>"<regex>")` and back-reference \k<name> syntax.

### \*allow-quoting\*

Whether the parser should support Perl's \Q and \E.

### \*look-ahead-for-suffix\*

Controls whether scanners will optimistically look ahead for a
  constant suffix of a regular expression, if there is one.

### \*optimize-char-classes\*

Whether character classes should be compiled into look-ups into
O(1) data structures.  This is usually fast but will be costly in
terms of scanner creation time and might be costly in terms of size if
`*regex-char-code-limit*` is high.  This value will be used as the `:kind`
keyword argument to `create-optimized-test-function` - see there for the
possible non-`NIL` values.

### \*property-resolver\*

Should be `NIL` or a designator for a function which accepts strings
and returns unary character test functions or `NIL`.  This 'resolver' is
intended to handle character properties' like \p{IsAlpha}.  If
`*property-resolver*` is `NIL`, then the parser will simply treat \p and
\P as #\p and #\P as in older versions of CL-PPCRE.

### \*regex-char-code-limit\*

The upper exclusive bound on the char-codes of characters which can
occur in character classes.  Change this value BEFORE creating
scanners if you don't need the (full) Unicode support of
implementations like AllegroCL, CLISP, LispWorks, or SBCL.

### \*use-bmh-matchers\*

Whether the scanners created by [create-scanner](#create-scanner) should use the (fast
but large) Boyer-Moore-Horspool matchers.



## SUPPORT

The development version of cl-ppcre can be found [on
github](https://github.com/edicl/cl-ppcre). Please use the github issue
tracking system to submit bug reports. Patches are welcome, please use
[GitHub pull requests](https://github.com/edicl/cl-ppcre/pulls). If you
want to make a change, please [read this
first](http://weitz.de/patches.html).
