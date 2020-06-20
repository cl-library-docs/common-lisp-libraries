# unix-opts - cmd line argument parser

Version: 0.1.7
<br/>
Licence: MIT
<br/>
Nickname: opts

*This documentation was possible due to the excellent [official documentation and
example](https://github.com/libre-man/unix-opts).*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***

## GETTING STARTED

Consider the following command line options defined using [define-opts](#define-opts):

```lisp
(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :level
   :description "the program will run on LEVEL level"
   :short #\l
   :long "level"
   :required t
   :arg-parser #'parse-integer ; <- takes an argument, which we want to parse into integer
   :meta-var "LEVEL")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity ; <- takes an argument, but we keep it as the string
   :meta-var "FILE"))
```

[describe](#describe) gets us the required "help text" (see the documentation for other
options):

```lisp
CL-USER> (opts:describe :prefix "Demonstrating REPL")
Demonstrating REPL

Available options:
  -h, --help               print this help text
  -v, --verbose            verbose output
  -l, --level LEVEL (Required)                           the program will run on LEVEL level
  -o, --output FILE        redirect output to file FILE
NIL
```

Command line arguments can be processed with a call to [get-opts](#get-opts).
For demonstration purposes, we use the REPL. See [this example
directory](https://github.com/libre-man/unix-opts/tree/master/example)
to see how this might be done in a script.

```lisp
CL-USER> (opts:get-opts '())
; Evaluation aborted on #<UNIX-OPTS:MISSING-REQUIRED-OPTION {10084173F3}>.
CL-USER> (opts:get-opts '("--level" "2"))
(:LEVEL 2)
NIL
CL-USER> (opts:get-opts '("--level" "2" "--help"))
(:LEVEL 2 :HELP T)
NIL
```

`get-opts` can throw several conditions:

- [unknown-option](#unknown-option)
- [missing-arg](#missing-arg)
- [arg-parser-failed](#arg-parser-failed)
- [missing-required-option](#missing-required-option)

Each of these has several restarts:

- [use-value](#use-value)
- [skip-option](#skip-option)
- [reparse-arg](#reparse-arg)

(See the documentation for [get-opts](#get-opts) for the details about each of these restarts.
The Cookbook chapter on [Error and Exception
Handling](https://lispcookbook.github.io/cl-cookbook/error_handling.html) should be helpful
if you want to learn about them.)

Thus, a typical way to use `get-opts` would be simply wrap the error in [handler-case]():

```lisp
CL-USER> (handler-case (opts:get-opts '())
           (error (condition)
             (format t "~A" condition)
             (opts:describe)))
missing required options: "--level"
Available options:
  -h, --help               print this help text
  -v, --verbose            verbose output
  -l, --level LEVEL (Required)                           the program will run on LEVEL level
  -o, --output FILE        redirect output to file FILE

NIL
```

You could use [getf](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_getf.htm#getf)
or [destructuring-bind](http://www.lispworks.com/documentation/HyperSpec/Body/m_destru.htm#destructuring-bind)
to obtain the arguments you need:

```lisp
CL-USER> (defun process-options (&rest options)
           (handler-case (opts:get-opts options)
             (error (condition) ; or you could case down on the various conditions above!
               (format t "~A" condition)
               (opts:describe))))
PROCESS-OPTIONS
CL-USER> (destructuring-bind (&key level help) (process-options "--level" "2" "--help")
           (if help
               (opts:describe)
               (format t "I see you've supplied level option, you want ~a level!~%" level)))

Available options:
  -h, --help               print this help text
  -v, --verbose            verbose output
  -l, --level LEVEL (Required)                           the program will run on LEVEL level
  -o, --output FILE        redirect output to file FILE

NIL
CL-USER> (destructuring-bind (&key level help) (process-options "--level" "2")
           (if help
               (opts:describe)
               (format t "I see you've supplied level option, you want ~a level!~%" level)))
I see you've supplied level option, you want 2 level!
NIL
```

You could also [iterate:dsetq](../iterate/#dsetq) to bring the options to global space.

```lisp
CL-USER> (destructuring-bind (&key verbose level help)
             (process-options "--level" "2" "--verbose")
           (if help
               (opts:describe) ; assume we have defined *level* and *verbose* previously
               (dsetq (*verbose* *level*) (list verbose level))))
(T 2)
CL-USER> *verbose*
T
CL-USER> *level*
2
```

## API REFERENCE

### arg-parser-failed


```lisp
Condition
```

This condition is thrown when some option OPTION wants
an argument, it's given but cannot be parsed by argument parser.

<u>**Direct Slots**</u>

**raw-arg**
```lisp
Initargs: :RAW-ARG
Readers: RAW-ARG
```
### argv

```lisp
Function: (argv)
```
Return a list of program's arguments, including command used to execute
the program as first elements of the list. Portable across implementations.

### define-opts

```lisp
Macro: (define-opts &body descriptions)
```
Define command line options. Arguments of this macro must be plists
containing various parameters. Here we enumerate all allowed parameters:

- **:name** - keyword that will be included in list returned by [get-opts](#get-opts) function if
actual option is supplied by user.

- **:description** - description of the option (it will be used in [describe](#describe)
function). This argument is optional, but it's recommended to supply it.

- **:short** - single character, short variant of the option. You may omit this
argument if you supply `:long` variant of option.

- **:long** - string, long variant of option. You may omit this argument if you
supply `:short` variant of option.

- **:arg-parser** - if actual option must take an argument, supply this argument, it
must be a function that takes a string and parses it.

- **:meta-var** - if actual option requires an argument, this is how it will be
printed in option description.

### describe

```lisp
Function: (describe &key prefix suffix usage-of args (stream *standard-output*))
```
Return string describing options of the program that were defined with
`define-opts` macro previously. You can supply `prefix` and `suffix` arguments
that will be printed before and after options respectively. If `usage-of` is
supplied, it should be a string, name of the program for "Usage: "
section. This section is only printed if this name is given. If your program
takes arguments (apart from options), you can specify how to print them in
"Usage: " section with `args` option (should be a string designator). Output
goes to `stream`.

### exit

```lisp
Function: (exit &optional (status 0))
```
Exit the program returning `status`.

### get-opts

```lisp
Function: (get-opts &optional options)
```
Parse command line options. If `options` is given, it should be a list to
parse. If it's not given, the function will use `argv` function to get list
of command line arguments.

Return two values:

* a list that contains keywords associated with command line options with
  `define-opts` macro, and
* a list of free arguments.

If some option requires an argument, you can use `getf` to
test presence of the option and get its argument if the option is present.

The parser may signal various conditions. Let's list them all specifying
which restarts are available for every condition, and what kind of
information the programmer can extract from the conditions.

- [unknown-option](#unknown-option) is thrown when parser encounters unknown (not previously
defined with `define-opts`) option. Use the `option` reader to get name of
the option (string). Available restarts: 
    - [use-value](#use-value): substitute the option and try again, 
    - [skip-option](#skip-option): ignore the option.

- [missing-arg](#missing-arg) is thrown when some option wants an argument, but there is no
such argument given. Use the `option` reader to get name of the
option (string). Available restarts: 
    - [use-value](#use-value): supplied value will be used, 
    - [skip-option](#skip-option): ignore the option.

- [arg-parser-failed](#arg-parser-failed) is thrown when some option wants an argument, it's given
but cannot be parsed by argument parser. Use the `option` reader to get name
of the option (string) and `raw-arg` to get raw string representing the
argument before parsing. Available restarts: 
    - [use-value](#use-value): supplied value will be used, 
    - [skip-option](#skip-option): ignore the option, 
    - [reparse-arg](#reparse-arg): supplied string will be parsed instead.

- [missing-required-option](#missing-required-option) is thrown when some option was required but was
not given. Use the `missing-options` reader to get the list of options that
are missing. Available restarts: 
    - [use-value](#use-value): supplied list of values will be used, 
    - [skip-option](#skip-option): ignore all these options, effectively binding them
to `nil`

### missing-arg


```lisp
Condition
```

This condition is thrown when some option OPTION wants
an argument, but there is no such argument given.

### missing-options

```lisp
Generic Function: (missing-options condition)
```


### missing-required-option


```lisp
Condition
```

This condition is thrown when required options are missing.

<u>**Direct Slots**</u>

**missing-options**
```lisp
Initargs: :MISSING-OPTIONS
Readers: MISSING-OPTIONS
```
### option

```lisp
Generic Function: (option condition)
```



```lisp
Class
```

representation of an option

### raw-arg

```lisp
Generic Function: (raw-arg condition)
```


### reparse-arg

### skip-option

### unknown-option


```lisp
Condition
```

This condition is thrown when parser encounters
unknown (not previously defined with `define-opts`) option.

### use-value

```lisp
Function: (use-value value &optional condition)
```
Transfer control and `value` to a restart named USE-VALUE, or
return NIL if none exists.
