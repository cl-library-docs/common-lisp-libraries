# fiveam - Regression Testing Framework

Version: 1.4.1
<br/>
Nickname: 5am
<br/>
Licence: BSD
<br/>
Repository: [sionescu/fiveam - Github](https://github.com/sionescu/fiveam)
<br>
See also: [awesome-cl#unit-testing](https://github.com/CodyReichert/awesome-cl#unit-testing)

*This documentation was possible due to [Tomek Kurez's tutorial on fiveam](http://turtleware.eu/posts/Tutorial-Working-with-FiveAM.html) and the [excellent official documentation](https://common-lisp.net/project/fiveam/docs/).*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

***

## GETTING STARTED

### Checks and Tests

A check, defined using the macro [is](#is), is a line of code that makes sure something is indeed true. A basic check definition is of the form `(is test &rest reason-args)`.

However, checks can only be defined in the context of tests:

```lisp
(test test-demo
  "This demonstrates the basic use of test and check."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3)) "This should pass.")  ; &rest reason-args
  (is (= 4 4.1) "~D and ~D are not = to each other." 4 4.1))
```

To run the test, simply, call [run!](#run_1):

```lisp
(run! 'test-demo)
;; Running test TEST-DEMO ..f
;;  Did 3 checks.
;;     Pass: 2 (66%)
;;     Skip: 0 ( 0%)
;;     Fail: 1 (33%)
;; 
;;  Failure Details:
;;  --------------------------------
;;  TEST-DEMO [This demonstrates the basic use of test and check.]: 
;;       4 and 4.1 are not = to each other.
;;       
;; (= 4 4.1)
;; 
;;  was NIL..
;;  --------------------------------
```

Tests can automatically be run on (re)compilation by setting [\*run-test-when-defined\*](#run-test-when-defined) to `t`. So, simply compiling

```lisp
(test another-test (is (eq t t)))
;; 
;; Running test ANOTHER-TEST .
;;  Did 1 check.
;;     Pass: 1 (100%)
;;     Skip: 0 ( 0%)
;;     Fail: 0 ( 0%)
```

runs the test. (Also see other [Configuration Variables](#configuration-variables).)

`(run!)` can be called without arguments to run all the tests.

### Suites

Tests can be grouped into suites. In fact, suites can also parent other suites.

Suites can be defined using [def-suite](#def-suite), and similar to* `in-package`, the current
suite can be specified using [in-suite](#in-suite).

```lisp
(def-suite suite-one)
(def-suite suite-two)

(in-suite suite-one)

(test first-test (is (= 1 1)))
(test (second-test :suite suite-two) ; perhaps, not recommended
  (is (null nil)))

(in-suite suite-two)

(test third-test (is (eq 'done 'done)))
```

All tests can be run with [run-all-tests](#run-all-tests). Also see the arguments to [run!](#run_1).

```lisp
(run-all-tests)
;; Running test suite NIL
;;  Running test FIRST-TEST .
;;  Running test THIRD-TEST .
;; Running test suite SUITE-TWO
;;  Running test SECOND-TEST .
;;  Did 3 checks.
;;     Pass: 3 (100%)
;;     Skip: 0 ( 0%)
;;     Fail: 0 ( 0%)
```

### More Things

- Tests can be removed using [rem-test](#rem-test).
- See [generators](#gen-buffer) for utilities for generating random data. These are to be used in
conjunction with the [for-all](#for-all) macro. Generators are merely closures and you can
define your own.
- fiveam also provides the ability to [def-fixture](#def-fixture) and
[with-fixture](#with-fixture); however, (as of 2016) it is [recommended to just use macros
instead](https://github.com/sionescu/fiveam/issues/31).
- The [signals](#signals) macro can be used to check if conditions are signalled appropriately.



## CONFIGURATION VARIABLES

### \*debug-on-error\*

T if we should drop into the debugger on error, NIL otherwise.
OBSOLETE: superseded by \*ON-ERROR\*

### \*debug-on-failure\*

T if we should drop into the debugger on a failing check, NIL otherwise.
OBSOLETE: superseded by \*ON-FAILURE\*

### \*default-test-compilation-time\*

### \*max-trials\*

Number of total times we attempt to run the body of the
  FOR-ALL test including when the body is skipped due to failed
  guard conditions.

Since we have guard conditions we may get into infinite loops
where the test code is never run due to the guards never
returning true. This second run limit prevents that.

### \*num-trials\*

Number of times we attempt to run the body of the FOR-ALL test.

### \*on-error\*

The action to perform on error:
- :DEBUG if we should drop into the debugger
- :BACKTRACE to print a backtrace
- NIL to simply continue

### \*on-failure\*

The action to perform on check failure:
- :DEBUG if we should drop into the debugger
- :BACKTRACE to print a backtrace
- NIL to simply continue

### \*print-names\*

T if we should print test running progress, NIL otherwise.

### \*run-test-when-defined\*

When non-NIL tests are run as soon as they are defined.

### \*test-dribble\*

### \*verbose-failures\*

T if we should print the expression failing, NIL otherwise.


## FUNCTIONS AND MACROS



### !

```lisp
Function: (!)
```
Rerun the most recently run test and explain the results.

### !!

```lisp
Function: (!!)
```
Rerun the second most recently run test and explain the results.

### !!!

```lisp
Function: (!!!)
```
Rerun the third most recently run test and explain the results.

### debug!

```lisp
Function: (debug! &optional (test-spec *suite*))
```
Calls (run! test-spec) but enters the debugger if any kind of error happens.

### def-fixture

```lisp
Macro: (def-fixture name (&rest args) &body body)
```
Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
&BODY to specify where the body should go.

See Also: [WITH-FIXTURE](#with-fixture).


### def-suite

```lisp
Macro: (def-suite name &key description in)
```
Define a new test-suite named NAME.

IN (a symbol), if provided, causes this suite te be nested in the
suite named by IN. NB: This macro is built on top of make-suite,
as such it, like make-suite, will overrwrite any existing suite
named NAME.

### def-suite\*

```lisp
Macro: (def-suite* name &rest def-suite-args)
```

### def-test

```lisp
Macro: (def-test name (&key depends-on (suite) fixture (compile-at) profile) &body body)
```
Create a test named NAME.

NAME is the symbol which names the test.

DEPENDS-ON is a list of the form:

 (AND . test-names) - This test is run only if all of the tests
 in TEST-NAMES have passed, otherwise a single test-skipped
 result is generated.

 (OR . test-names) - If any of TEST-NAMES has passed this test is
 run, otherwise a test-skipped result is generated.

 (NOT test-name) - This is test is run only if TEST-NAME failed.

AND, OR and NOT can be combined to produce complex dependencies.

If DEPENDS-ON is a symbol it is interpreted as `(AND
,depends-on), this is accomadate the common case of one test
depending on another.

FIXTURE specifies a fixture to wrap the body in.

If PROFILE is T profiling information will be collected as well.

### explain!

```lisp
Function: (explain! result-list)
```
Explain the results of RESULT-LIST using a
detailed-text-explainer with output going to *test-dribble*.
Return a boolean indicating whether no tests failed.

### fail

```lisp
Macro: (fail &rest message-args)
```
Simply generate a FAIL.

### finishes

```lisp
Macro: (finishes &body body)
```
Generates a pass if BODY executes to normal completion. In
other words if body does signal, return-from or throw this test
fails.

### for-all

```lisp
Macro: (for-all bindings &body body)
```
Bind `BINDINGS` to random variables and test `BODY` *num-trials* times.

`BINDINGS` is a list of binding forms, each element is a list
of `(BINDING VALUE &optional GUARD)`. Value, which is evaluated
once when the for-all is evaluated, must return a generator which
be called each time `BODY` is evaluated. `BINDING` is either a symbol
or a list which will be passed to destructuring-bind. `GUARD` is a
form which, if present, stops `BODY` from executing when `IT` returns
NIL. The GUARDS are evaluated after all the random data has been
generated and they can refer to the current value of any
binding. NB: Generator forms, unlike guard forms, can not contain
references to the bound variables.

Examples:

```lisp
  (for-all ((a (gen-integer)))
    (is (integerp a)))

  (for-all ((a (gen-integer) (plusp a)))
    (is (integerp a))
    (is (plusp a)))

  (for-all ((less (gen-integer))
            (more (gen-integer) (< less more)))
    (is (<= less more)))

  (for-all (((a b) (gen-two-integers)))
    (is (integerp a))
    (is (integerp b)))
```

### gen-buffer

```lisp
Function: (gen-buffer &key
                      (length (gen-integer min 0 max 50))
                      (element-type '(unsigned-byte 8))
                      (elements (gen-integer :min 0 :max (1- (expt 2 8)))))
```

### gen-character

```lisp
Function: (gen-character &key
                         (code-limit char-code-limit)
                         (code (gen-integer :min 0 :max (1- code-limit)))
                         (alphanumericp nil))
```
Returns a generator of characters.

CODE must be a generator of random integers. ALPHANUMERICP, if
non-NIL, limits the returned chars to those which pass
alphanumericp.

### gen-float

```lisp
Function: (gen-float &key bound (type 'short-float))
```
Returns a generator which produces floats of type TYPE. BOUND,
if specified, constrains the results to be in the range (-BOUND,
BOUND).

### gen-integer

```lisp
Function: (gen-integer &key
                       (max (1+ most-positive-fixnum))
                       (min (1- most-negative-fixnum)))
```
Returns a generator which produces random integers greater
than or equal to MIN and less than or equal to MAX.

### gen-list

```lisp
Function: (gen-list &key
                    (length (gen-integer :min 0 :max 10))
                    (elements (gen-integer :min -10 :max 10)))
```
Returns a generator which produces random lists. LENGTH must be
an integer generator and ELEMENTS must be a generator which
produces objects.

### gen-one-element

```lisp
Function: (gen-one-element &rest elements)
```

### gen-string

```lisp

Function: (gen-string &key
                      (length (gen-integer :min 0 :max 80))
                      (elements (gen-character))
                      (element-type 'character))
```
Returns a generator which produces random strings. LENGTH must
be a generator which produces integers, ELEMENTS must be a
generator which produces characters of type ELEMENT-TYPE.

### gen-tree

```lisp
Function: (gen-tree &key (size 20) (elements (gen-integer :min -10 :max 10)))
```
Returns a generator which produces random trees. SIZE controls
the approximate size of the tree, but don't try anything above
 30, you have been warned. ELEMENTS must be a generator which
will produce the elements.

### get-fixture

```lisp
Function: (get-fixture key &optional default)
```

### get-test

```lisp
Function: (get-test key &optional default)
```

### in-suite

```lisp
Macro: (in-suite suite-name)
```
Set the *suite* special variable so that all tests defined
after the execution of this form are, unless specified otherwise,
in the test-suite named SUITE-NAME.

See also: DEF-SUITE *SUITE*

### in-suite\*

```lisp
Macro: (in-suite* suite-name &key in)
```
Just like in-suite, but silently creates missing suites.

### is

```lisp
Macro: (is test &rest reason-args)
```
The DWIM checking operator.

If TEST returns a true value a test-passed result is generated,
otherwise a test-failure result is generated. The reason, unless
REASON-ARGS is provided, is generated based on the form of TEST:

 (predicate expected actual) - Means that we want to check
 whether, according to PREDICATE, the ACTUAL value is
 in fact what we EXPECTED.

 (predicate value) - Means that we want to ensure that VALUE
 satisfies PREDICATE.

 Wrapping the TEST form in a NOT simply produces a negated reason
 string.

### is-every

```lisp
Macro: (is-every predicate &body clauses)
```
The input is either a list of lists, or a list of pairs. Generates (is (,predicate ,expr ,value))
   for each pair of elements or (is (,predicate ,expr ,value) ,@reason) for each list.

### is-false

```lisp
Macro: (is-false condition &rest reason-args)
```
Generates a pass if CONDITION returns false, generates a
  failure otherwise. Like IS-TRUE, and unlike IS, IS-FALSE does
  not inspect CONDITION to determine what reason to give it case
  of test failure

### is-true

```lisp
Macro: (is-true condition &rest reason-args)
```
Like IS this check generates a pass if CONDITION returns true
  and a failure if CONDITION returns false. Unlike IS this check
  does not inspect CONDITION to determine how to report the
  failure.

### make-fixture

### make-suite

```lisp
Function: (make-suite name &key description ((in parent-suite)))
```
Create a new test suite object.

Overrides any existing suite named NAME.

### make-test

### pass

```lisp
Macro: (pass &rest message-args)
```
Simply generate a PASS.

### rem-fixture

```lisp
Function: (rem-fixture key)
```

### rem-test

```lisp
Function: (rem-test key)
```

### results-status

```lisp
Function: (results-status result-list)
```
Given a list of test results (generated while running a test)
  return true if no results are of type TEST-FAILURE.  Returns second
  and third values, which are the set of failed tests and skipped
  tests respectively.

### run

```lisp
Function: (run test-spec &key (print-names *print-names*))
```
Run the test specified by TEST-SPEC.

TEST-SPEC can be either a symbol naming a test or test suite, or
a testable-object object. This function changes the operations
performed by the !, !! and !!! functions.

### run!

```lisp
Function: (run! &optional (test-spec *suite*)
                &key (print-names *print-names*))
```

Equivalent to (explain! (run TEST-SPEC)).

### run-all-tests

```lisp
Function: (run-all-tests &key (summary :end))
```
Runs all defined test suites, T if all tests passed and NIL otherwise.
SUMMARY can be :END to print a summary at the end, :SUITE to print it
after each suite or NIL to skip explanations.

### signals

```lisp
Macro: (signals condition-spec &body body)
```
Generates a pass if BODY signals a condition of type
CONDITION. BODY is evaluated in a block named NIL, CONDITION is
not evaluated.

### skip

```lisp
Macro: (skip &rest reason)
```
Generates a TEST-SKIPPED result.

### test

```lisp
Macro: (test name &body body)
```
Create a test named NAME. If NAME is a list it must be of the
form:

```lisp
  (name &key depends-on suite fixture compile-at profile)
```

NAME is the symbol which names the test.

DEPENDS-ON is a list of the form:

- (AND . test-names) - This test is run only if all of the tests
 in TEST-NAMES have passed, otherwise a single test-skipped
 result is generated.

- (OR . test-names) - If any of TEST-NAMES has passed this test is
 run, otherwise a test-skipped result is generated.

- (NOT test-name) - This is test is run only if TEST-NAME failed.

AND, OR and NOT can be combined to produce complex dependencies.

If DEPENDS-ON is a symbol it is interpreted as `(AND
,depends-on), this is accomadate the common case of one test
depending on another.

FIXTURE specifies a fixture to wrap the body in.

If PROFILE is T profiling information will be collected as well.

### test-names

```lisp
Function: (test-names)
```

### with-fixture

```lisp
Macro: (with-fixture fixture-name (&rest args) &body body)
```
Insert BODY into the fixture named FIXTURE-NAME.

See Also: [DEF-FIXTURE](#def-fixture).


