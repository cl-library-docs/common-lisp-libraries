# asdf - Build System

Version: 3.3.1
<br/>
Licence: MIT-style
<br/>
Repository: [asdf/asdf - Gitlab](https://gitlab.common-lisp.net/asdf/asdf/)

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

ASDF, or Another System Definition Facility, is a build system: a tool for specifying how systems of Common Lisp software are made up of components (sub-systems and files), and how to operate on these components in the right order so that they can be compiled, loaded, tested, etc.

ASDF also includes UIOP, currently not documented here.

## Getting Started

All actively maintained lisp implementations these days include a copy of ASDF 3 that can be simply loaded using `(require "asdf")` - in fact, loading [quicklisp](../quicklisp/) takes care of `require`-ing asdf. `(asdf:asdf-version)` will inform you of the version (should be >3). For details, you may want to refer to the section on [Loading ASDF](https://common-lisp.net/project/asdf/asdf.html#Loading-ASDF) in the manual.

ASDF must be configured to find the `.asd` files containing the system definition. If you are using quicklisp - and we recommend you do - for your own projects, you only need to consider [ql:\*local-project-directories\*](../quicklisp/#local-project-directories). For details, see the section on [Configuring ASDF](https://common-lisp.net/project/asdf/asdf.html#Configuring-ASDF) in the manual.

### Trivial System Definition

```lisp
(defsystem "foobar"
  :description "A system definition to demonstrate ASDF"
  :version "0.1.0"
  :serial t
  :license "BSD"
  :pathname "src/"
  :depends-on ("alexandria"
               (:version "some-new-system" "0.7.3"))
  :components ((:file "foo")                         ; .lisp is implicit for :file
               (:file "baz")                         ; this lies in src/
               (:module "bar" 
                        :components ((:file "bar-1") ; this lies in src/bar/
                                      (:file "bar-2")))
               (:static-file "LICENCE" :pathname #P"LICENCE"))
  :in-order-to ((test-op (test-op "foobar/tests")))
  ;; The following is not recommended - for demonstration of :perform
  ;; and operations (see next section) only.
  :perform (load-op (o c) (format t "Loading foobar"))) 

(defsystem "foobar/tests"
  :depends-on ("foobar" "fiveam")
  :components ((:file "foobar-tests"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run! :foobar)))
```

These definitions should go inside a file named `"foobar.asd"`, located somewhere quicklisp/ASDF can find (see previous section). As of ASDF 3.1, the depends-on version translates to 0.7.3+. (TODO: What about ASDF 3.3? Will this ever change in future?) Usually, you'd be basing yourself on a particular version of [quicklisp dist](../quicklisp/#a-note-on-quicklisp-dists). However, if you actually need it, [qlot](https://github.com/fukamachi/qlot) should be helpful.

The `:serial t` states that each item in the `:components` depends on all of the previous items.
When efficiency is a concern, you may want to explicitly specify the list of dependencies a particular component depends on `(:file "baz" :depends-on ("foo"))`.

Thus, the way things are defined, the system "foobar" can simply be tested by calling `(asdf:test-system "foobar")`.

Finally, a note on naming: system names are recommended to be named using lower-cased strings without underscores. There exist what are known as primary systems - with the system name being same as the filename without the .asd extension - and secondary systems. Here, "foobar" is the primary system. "foobar/tests" is a secondary system. The primary system of a secondary system  can be found by noting the `/` as delimiters. A (secondary) system "foo/bar" would be looked up in the file "foo.asd".

### Operations

ASDF has the concept of an *operation* that can act upon a system (or a smaller component thereof).
Typical operations that matter to end-users include:

  * [load-op](#load-op) for loading a system in the current Lisp image, as used by [asdf:load-system](#load-system),
  * [test-op](#test-op) for running tests associated to the system, as used by [asdf:test-system](#test-system),
  * [build-op](#build-op) for doing whatever build operation is associated to the system,
    (which by default is [load-op](#load-op) if the system didn't override it),
    as used by [asdf:make](#make).

Further operations of interest to users include:

  * [compile-op](#compile-op)
    - ensure the system is compiled, without necessarily loading all of it, or any bit of it,
  * [load-source-op](#load-source-op)
    - load the system from source without compiling,
  * [compile-bundle-op](#compile-bundle-op)
    - create a single fasl for the entire system, for delivery,
  * [monolithic-compile-bundle-op](#monolithic-compile-bundle-op)
    - create a single fasl for the entire system *and all its transitive dependencies*, for delivery,
  * [image-op](#image-op)
    - create a development image with this system already loaded, for fast startup,
  * [program-op](#program-op)
    - create a standalone application, which we will see below, etc.

Whichever operation you want, [asdf:operate](#operate) (`(asdf:operate `*operation* *system*`)`)
will ensure this operation is performed on that system
(after all the necessary dependencies of such an action).
A short-hand `asdf:oos` acts as an acronym for `asdf:operate`.

The operation is typically specified as a symbol that names the operation class.
Since ASDF 3, you can also use a keyword to specify an action in the ASDF package.
Thus, `(asdf:oos :load-op :foobar)` is equivalent to `(asdf:load-system :foobar)`.

Whichever operation a system is intended to be used with can be invoked with [make](#make): `(asdf:make :foo)` and specified with [build-op](#build-op). (TODO: Confirm `make` invokes load-op before invoking build-op if both are defined. Why? What's going on under the hood? There is also the concept of dependency between operations.)

TODO: Are there other ways to specify what an operation should do other than `:perform` clause?
TODO: What exactly is the utility of a static file? Perhaps, for bundling data files. Data can then be read from such a file by getting [asdf:component-pathname](#component-pathname). But what happens if you bundle the system into an image?

### A note on Systems and Packages

What one might call packages in other languages are called systems in Common Lisp parlance. Instead, the word `package` in Common Lisp refers to a data structure providing namespacing
for symbols. By contrast, a system is a tool to organize a bunch of files, in accordance with dependencies, and specifying how to perform certain actions on the system. A single system may contain multiple packages. See [this StackOverFlow answer](https://stackoverflow.com/questions/45642330/why-do-many-common-lisp-systems-use-a-single-packages-lisp-file/45643829#45643829) for a detailed discussion.

ASDF also provides [package-inferred-system](https://github.com/fare/asdf/blob/master/doc/best_practices.md#package_inferred) to make the "one file, one package, one system" style easier to use. However, most systems in the wild usually have one main system, several packages, each package spanning several files; each file packing together some relevant functionality. Your IDE (often SLIME) should be the one telling you who calls/sets/macroexpands/etc something. Of use also are `M-.` (slime-edit-definition) and `M-,` (slime-pop-find-definition-stack).


### More Reading

- [ASDF Best Practices](https://github.com/fare/asdf/blob/master/doc/best_practices.md)
- [ASDF Manual](https://common-lisp.net/project/asdf/asdf.html)
- [Building self-contained executables - The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/scripting.html#building-a-self-contained-executable)


## API REFERENCE

### \*central-registry\*

```lisp
Variable
```

A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

```lisp
(setf asdf:*central-registry*
(list '*default-pathname-defaults*
#p"/home/me/cl/systems/"
#p"/usr/share/common-lisp/systems/"))
```
This variable is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.


### \*compile-file-failure-behaviour\*

```lisp
Variable
```

How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file, which includes any non-style-warning warning.
Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.


### \*compile-file-warnings-behaviour\*

```lisp
Variable
```

How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.


### \*default-encoding\*

```lisp
Variable
```

Default encoding for source files.
The default value :utf-8 is the portable thing.
The legacy behavior was :default.
If you (asdf:load-system :asdf-encodings) then
you will have autodetection via *encoding-detection-hook* below,
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.


### \*default-source-registries\*

```lisp
Variable
```

List of default source registries


### \*encoding-detection-hook\*

```lisp
Variable
```

Hook for an extension to define a function to automatically detect a file's encoding


### \*encoding-external-format-hook\*

```lisp
Variable
```

Hook for an extension (e.g. ASDF-ENCODINGS) to define a better mapping
from non-default encodings to and implementation-defined external-format's


### \*resolve-symlinks\*

```lisp
Variable
```

Determine whether or not ASDF resolves symlinks when defining systems.
Defaults to T.


### \*system-definition-search-functions\*

```lisp
Variable
```

A list that controls the ways that ASDF looks for system definitions.
It contains symbols to be funcalled in order, with a requested system name as argument,
until one returns a non-NIL result (if any), which must then be a fully initialized system object
with that name.


### \*user-cache\*

```lisp
Variable
```

A specification as per [resolve-location](#resolve-location) of where the user keeps his FASL cache


### \*utf-8-external-format\*

```lisp
Variable
```

Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.


### \*warnings-file-type\*

```lisp
Variable
```

Pathname type for warnings files, or NIL if disabled


### action-description

```lisp
Generic Function: (action-description operation component)
```

returns a phrase that describes performing this operation
on this component, e.g. "loading /a/b/c".
You can put together sentences using this phrase.

### additional-input-files

```lisp
Generic Function: (additional-input-files operation component)
```

Additional input files for the operation on this
component.  These are files that are inferred, rather than
explicitly specified, and these are typically NOT files that
undergo operations directly.  Instead, they are files that it is
important for ASDF to know about in order to compute operation times,etc.

### already-loaded-systems

```lisp
Function: (already-loaded-systems)
```

return a list of the names of the systems that have been successfully loaded so far

### apply-output-translations

```lisp
Function: (apply-output-translations path)
```



### asdf-message

```lisp
Function: (asdf-message format-string &rest format-args)
```



### asdf-version

```lisp
Function: (asdf-version)
```

Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.: (ASDF:[version-satisfies](#version-satisfies) (ASDF:`asdf-version`) "3.4.5.67").

### bad-system-name

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
Initargs: :NAME
Readers: ASDF/COMPONENT:COMPONENT-NAME
```
**asdf/component:source-file**
```lisp
Initargs: :SOURCE-FILE
Readers: ASDF/SYSTEM:SYSTEM-SOURCE-FILE
```

### basic-compile-bundle-op

```lisp
Class
```

Base class for compiling into a bundle

<u>**Direct Slots**</u>

**asdf/bundle::gather-type**
```lisp
```
**asdf/bundle:bundle-type**
```lisp
```

### build-op

```lisp
Class
```

Since ASDF3, BUILD-OP is the recommended 'master' operation,
to operate by default on a system or component, via the function BUILD.
Its meaning is configurable via the :BUILD-OPERATION option of a component.
which typically specifies the name of a specific operation to which to delegate the build,
as a symbol or as a string later read as a symbol (after loading the defsystem-depends-on);
if NIL is specified (the default), BUILD-OP falls back to LOAD-OP,
that will load the system in the current image.


### bundle-op

```lisp
Class
```

base class for operations that bundle outputs from multiple components

<u>**Direct Slots**</u>

**asdf/bundle:bundle-type**
```lisp
```

### c-source-file

```lisp
Class
```

<u>**Direct Slots**</u>

**type**
```lisp
```

### child-component

```lisp
Class
```

A CHILD-COMPONENT is a COMPONENT that may be part of
a PARENT-COMPONENT.


### circular-dependency

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/action::actions**
```lisp
Initargs: :ACTIONS
Readers: ASDF/ACTION:CIRCULAR-DEPENDENCY-ACTIONS
```

### cl-source-file

```lisp
Class
```

Component class for a Common Lisp source file (using type "lisp")

<u>**Direct Slots**</u>

**type**
```lisp
```

### cl-source-file.cl

```lisp
Class
```

Component class for a Common Lisp source file using type "cl"

<u>**Direct Slots**</u>

**type**
```lisp
```

### cl-source-file.lsp

```lisp
Class
```

Component class for a Common Lisp source file using type "lsp"

<u>**Direct Slots**</u>

**type**
```lisp
```

### clear-configuration

```lisp
Function: (clear-configuration)
```

Call the functions in *CLEAR-CONFIGURATION-HOOK*

### clear-output-translations

```lisp
Function: (clear-output-translations)
```

Undoes any initialization of the output translations.

### clear-source-registry

```lisp
Function: (clear-source-registry)
```

Undoes any initialization of the source registry.

### clear-system

```lisp
Function: (clear-system system)
```

Clear the entry for a `system` in the database of systems previously defined.
However if the system was registered as PRELOADED (which it is if it is IMMUTABLE),
then a new system with the same name will be defined and registered in its place
from which build details will have been cleared.
Note that this does NOT in any way cause any of the code of the system to be unloaded.
Returns T if system was or is now undefined, NIL if a new preloaded system was redefined.

### coerce-name

```lisp
Function: (coerce-name name)
```

Given a designator for a component `name`, return the name as a string.
The designator can be a [component](#component) (designing its name; note that a [system](#system) is a component),
a SYMBOL (designing its name, downcased), or a STRING (designing itself).

### compile-bundle-op

```lisp
Class
```

This operator is an alternative to COMPILE-OP. Build a system
and all of its dependencies, but build only a single ("monolithic") FASL, instead
of one per source file, which may be more resource efficient.  That monolithic
FASL should be loaded with LOAD-BUNDLE-OP, rather than LOAD-OP.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### compile-concatenated-source-op

```lisp
Class
```

Operation to compile the result of concatenate-source-op

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### compile-condition

```lisp
Condition
```

<u>**Direct Slots**</u>

**uiop/lisp-build::context-format**
```lisp
Initargs: :CONTEXT-FORMAT
```
**uiop/lisp-build::context-arguments**
```lisp
Initargs: :CONTEXT-ARGUMENTS
```
**uiop/lisp-build::description**
```lisp
Initargs: :DESCRIPTION
```

### compile-error

```lisp
Condition
```


### compile-failed

```lisp
Condition
```


### compile-failed-error

```lisp
Condition
```


### compile-failed-warning

```lisp
Condition
```


### compile-file\*

```lisp
Function: (compile-file* input-file &rest keys &key
						 (compile-check *compile-check*) output-file warnings-file emit-cfasl
						 &allow-other-keys)
```

This function provides a portable wrapper around COMPILE-FILE.
It ensures that the `output-file` value is only returned and
the file only actually created if the compilation was successful,
even though your implementation may not do that. It also checks an optional
user-provided consistency function `compile-check` to determine success;
it will call this function if not NIL at the end of the compilation
with the arguments sent to COMPILE-FILE*, except with :[output-file](#output-file) TMP-FILE
where TMP-FILE is the name of a temporary output-file.
It also checks two flags (with legacy british spelling from ASDF1),
*COMPILE-FILE-FAILURE-BEHAVIOUR* and *COMPILE-FILE-WARNINGS-BEHAVIOUR*
with appropriate implementation-dependent defaults,
and if a failure (respectively warnings) are reported by COMPILE-FILE,
it will consider that an error unless the respective behaviour flag
is one of :SUCCESS :WARN :IGNORE.
If `warnings-file` is defined, deferred warnings are saved to that file.
On ECL or MKCL, it creates both the linkable object and loadable fasl files.
On implementations that erroneously do not recognize standard keyword arguments,
it will filter them appropriately.

### compile-file-error

```lisp
Condition
```


### compile-file-pathname\*

```lisp
Function: (compile-file-pathname* input-file &rest keys &key output-file
								  &allow-other-keys)
```

Variant of COMPILE-FILE-PATHNAME that works well with COMPILE-FILE*

### compile-op

```lisp
Class
```

Operation for compiling a Lisp file to a FASL

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### compile-system

```lisp
Function: (compile-system system &rest args &key force force-not verbose
						  version &allow-other-keys)
```

Shorthand for `(asdf:operate 'asdf:compile-op system)`. See [operate](#operate) for details.

### compile-warned

```lisp
Condition
```


### compile-warned-error

```lisp
Condition
```


### compile-warned-warning

```lisp
Condition
```


### compiled-file

```lisp
Class
```

Class for a file that is already compiled,
e.g. as part of the implementation, of an outer build system that calls into ASDF,
or of opaque libraries shipped along the source code.

<u>**Direct Slots**</u>

**type**
```lisp
```

### component

```lisp
Class
```

Base class for all components of a build

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
```
**asdf/component:version**
```lisp
Initargs: :VERSION
Readers: ASDF/COMPONENT:COMPONENT-VERSION
Writers: (SETF ASDF/COMPONENT:COMPONENT-VERSION)
```
**asdf/component:description**
```lisp
Initargs: :DESCRIPTION
Readers: ASDF/COMPONENT:COMPONENT-DESCRIPTION
Writers: (SETF ASDF/COMPONENT:COMPONENT-DESCRIPTION)
```
**asdf/component:long-description**
```lisp
Initargs: :LONG-DESCRIPTION
Readers: ASDF/COMPONENT:COMPONENT-LONG-DESCRIPTION
Writers: (SETF ASDF/COMPONENT:COMPONENT-LONG-DESCRIPTION)
```
**asdf/component:sideway-dependencies**
```lisp
```
**asdf/component:if-feature**
```lisp
Initargs: :IF-FEATURE
Readers: ASDF/COMPONENT:COMPONENT-IF-FEATURE
Writers: (SETF ASDF/COMPONENT:COMPONENT-IF-FEATURE)
```
**asdf/component:in-order-to**
```lisp
Initargs: :IN-ORDER-TO
Readers: ASDF/COMPONENT:COMPONENT-IN-ORDER-TO
Writers: (SETF ASDF/COMPONENT:COMPONENT-IN-ORDER-TO)
```
**asdf/component:inline-methods**
```lisp
```
**asdf/component:relative-pathname**
```lisp
Initargs: :PATHNAME
```
**asdf/component:absolute-pathname**
```lisp
```
**asdf/component:operation-times**
```lisp
```
**asdf/component:around-compile**
```lisp
Initargs: :AROUND-COMPILE
```
**asdf/component:properties**
```lisp
Initargs: :PROPERTIES
Readers: ASDF/COMPONENT:COMPONENT-PROPERTIES
Writers: (SETF ASDF/COMPONENT:COMPONENT-PROPERTIES)
```
**asdf/component:%encoding**
```lisp
Initargs: :ENCODING
Readers: ASDF/COMPONENT::%COMPONENT-ENCODING
Writers: (SETF ASDF/COMPONENT::%COMPONENT-ENCODING)
```
**asdf/component:parent**
```lisp
Initargs: :PARENT
Readers: ASDF/COMPONENT:COMPONENT-PARENT
```
**asdf/component::build-operation**
```lisp
Initargs: :BUILD-OPERATION
Readers: ASDF/COMPONENT:COMPONENT-BUILD-OPERATION
```
**asdf/component::additional-input-files**
```lisp
```

### component-children

```lisp
Generic Function: (component-children object)
```



### component-children-by-name

```lisp
Generic Function: (component-children-by-name object)
```



### component-depends-on

```lisp
Generic Function: (component-depends-on operation component)
```

Returns a list of dependencies needed by the component to perform
the operation.  A dependency has one of the following forms:

(<operation> <component>*), where <operation> is an operation designator
with respect to [find-operation](#find-operation) in the context of the `operation` argument,
and each <component> is a component designator with respect to
[find-component](#find-component) in the context of the `component` argument,
and means that the component depends on
<operation> having been performed on each <component>;

[Note: an <operation> is an operation designator -- it can be either an
operation name or an operation object.  Similarly, a <component> may be
a component name or a component object.  Also note that, the degenerate
case of (<operation>) is a no-op.]

Methods specialized on subclasses of existing component types
should usually append the results of CALL-NEXT-METHOD to the list.

### component-encoding

```lisp
Generic Function: (component-encoding component)
```

The encoding of the `component`. By default, only :utf-8 is supported.
Use asdf-encodings to support more encodings.

### component-external-format

```lisp
Generic Function: (component-external-format component)
```

The external-format of the `component`.
By default, deduced from the [component-encoding](#component-encoding).

### component-find-path

```lisp
Function: (component-find-path component)
```

Return a path from a root system to the `component`.
The return value is a list of component NAMES; a list of strings.

### component-load-dependencies

```lisp
Function: (component-load-dependencies component)
```

DEPRECATED. Please use [component-sideway-dependencies](#component-sideway-dependencies) instead; or better,
define your operations with proper use of [sideway-operation](#sideway-operation), [selfward-operation](#selfward-operation),
or define methods on [prepare-op](#prepare-op), etc.

### component-loaded-p

```lisp
Function: (component-loaded-p component)
```

Has the given `component` been successfully loaded in the current image (yet)?
Note that this returns true even if the component is not up to date.

### component-name

```lisp
Generic Function: (component-name component)
```

Name of the `component`, unique relative to its parent

### component-parent

```lisp
Generic Function: (component-parent component)
```

The parent of a child `component`,
or NIL for top-level components (a.k.a. systems)

### component-pathname

```lisp
Generic Function: (component-pathname component)
```

Pathname of the `component` if any, or NIL.

### component-property

```lisp
Generic Function: (component-property component property)
```



### component-relative-pathname

```lisp
Generic Function: (component-relative-pathname component)
```

Specified pathname of the `component`,
intended to be merged with the pathname of that component's parent if any, using merged-pathnames*.
Despite the function's name, the return value can be an absolute pathname, in which case the merge
will leave it unmodified.

### component-sideway-dependencies

```lisp
Generic Function: (component-sideway-dependencies object)
```



### component-system

```lisp
Generic Function: (component-system component)
```

Top-level system containing the `component`

### component-version

```lisp
Generic Function: (component-version component)
```

Return the version of a `component`, which must be a string of dot-separated
natural numbers, or NIL.

### compute-source-registry

```lisp
Function: (compute-source-registry &optional
           (parameter *source-registry-parameter*) (registry *source-registry*))
```



### concatenate-source-op

```lisp
Class
```

Operation to concatenate all sources in a system into a single file


### define-op

```lisp
Class
```

An operation to record dependencies on loading a .asd file.


### defsystem

```lisp
Macro: (defsystem name &body options)
```



### deliver-asd-op

```lisp
Class
```

produce an asd file for delivering the system as a single fasl

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### disable-deferred-warnings-check

```lisp
Function: (disable-deferred-warnings-check)
```

Disable the saving of deferred warnings

### disable-output-translations

```lisp
Function: (disable-output-translations)
```

Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility.

### dll-op

```lisp
Class
```

Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system. Compare with LIB-OP.

<u>**Direct Slots**</u>

**asdf/bundle::gather-type**
```lisp
```
**asdf/bundle:bundle-type**
```lisp
```

### doc-file

```lisp
Class
```


### downward-operation

```lisp
Generic Function: (downward-operation object)
```



```lisp
Class
```

A DOWNWARD-OPERATION's dependencies propagate down the component hierarchy.
I.e., if O is a DOWNWARD-OPERATION and its DOWNWARD-OPERATION slot designates operation D, then
the action (O . M) of O on module M will depends on each of (D . C) for each child C of module M.
The default value for slot DOWNWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a MODULE to be loaded with LOAD-OP (resp. compiled with COMPILE-OP), all the
children of the MODULE must have been loaded with LOAD-OP (resp. compiled with COMPILE-OP.

<u>**Direct Slots**</u>

**asdf/action:downward-operation**
```lisp
```

### duplicate-names

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
Initargs: :NAME
Readers: ASDF/COMPONENT::DUPLICATE-NAMES-NAME
```

### enable-asdf-binary-locations-compatibility

```lisp
Function: (enable-asdf-binary-locations-compatibility &key
           (centralize-lisp-binaries NIL)
           (default-toplevel-directory
            (subpathname (user-homedir-pathname) .fasls/))
           (include-per-user-information NIL) (map-all-source-files (or NIL))
           (source-to-target-mappings NIL)
           (file-types
            (quasiquote
             (,(UIOP/LISP-BUILD:COMPILE-FILE-TYPE) build-report cfasl
              sbcl-warnings))))
```

DEPRECATED. Use asdf-output-translations instead.

### enable-deferred-warnings-check

```lisp
Function: (enable-deferred-warnings-check)
```

Enable the saving of deferred warnings

### ensure-output-translations

```lisp
Function: (ensure-output-translations)
```



### ensure-source-registry

```lisp
Function: (ensure-source-registry &optional parameter)
```



### error-component

```lisp
Generic Function: (error-component condition)
```



### error-name

```lisp
Generic Function: (error-name condition)
```



### error-operation

```lisp
Generic Function: (error-operation condition)
```



### error-pathname

```lisp
Generic Function: (error-pathname condition)
```



### explain

```lisp
Generic Function: (explain operation component)
```

Display a message describing an action.

DEPRECATED. Use ASDF:[action-description](#action-description) and/or ASDF::FORMAT-ACTION instead.

### file-component

```lisp
Class
```

a COMPONENT that represents a file

<u>**Direct Slots**</u>

**type**
```lisp
Initargs: :TYPE
Readers: ASDF/COMPONENT:FILE-TYPE
Writers: (SETF ASDF/COMPONENT:FILE-TYPE)
```

### file-type

```lisp
Generic Function: (file-type object)
```



### find-component

```lisp
Generic Function: (find-component base path &key registered)
```

Find a component by resolving the `path` starting from `base` parent.
If `registered` is true, only search currently registered systems.

### find-operation

```lisp
Generic Function: (find-operation context spec)
```

Find an operation by resolving the `spec` in the `context`

### find-system

```lisp
Generic Function: (find-system system &optional error-p)
```

Given a system designator, find the actual corresponding system object.
If no system is found, then signal an error if `error-p` is true (the default), or else return NIL.
A system designator is usually a string (conventionally all lowercase) or a symbol, designating
the same system as its downcased name; it can also be a system object (designating itself).

### hostname

```lisp
Function: (hostname)
```

return the hostname of the current host

### html-file

```lisp
Class
```

<u>**Direct Slots**</u>

**type**
```lisp
```

### image-op

```lisp
Class
```

create an image file from the system and its dependencies

<u>**Direct Slots**</u>

**asdf/bundle:bundle-type**
```lisp
```
**asdf/bundle::gather-operation**
```lisp
```
**asdf/action:selfward-operation**
```lisp
```

### implementation-identifier

```lisp
Function: (implementation-identifier)
```

Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc.

### implementation-type

```lisp
Function: (implementation-type)
```

The type of Lisp implementation used, as a short UIOP-standardized keyword

### initialize-output-translations

```lisp
Function: (initialize-output-translations &optional
           (parameter *output-translations-parameter*))
```

read the configuration, initialize the internal configuration variable,
return the configuration

### initialize-source-registry

```lisp
Function: (initialize-source-registry &optional
           (parameter *source-registry-parameter*))
```



### input-files

```lisp
Generic Function: (input-files operation component)
```

A list of input files corresponding to this action.

Methods on [perform](#perform) *must* call this function to determine where their inputs are located.
They may rely on the order of the files to discriminate between inputs.


### java-source-file

```lisp
Class
```

<u>**Direct Slots**</u>

**type**
```lisp
```

### lib-op

```lisp
Class
```

Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system. Compare with DLL-OP.

On most implementations, these object files only include extensions to the runtime
written in C or another language with a compiler producing linkable object files.
On CLASP, ECL, MKCL, these object files _also_ include the contents of Lisp files
themselves. In any case, this operation will produce what you need to further build
a static runtime for your system, or a dynamic library to load in an existing runtime.

<u>**Direct Slots**</u>

**asdf/bundle::gather-type**
```lisp
```
**asdf/bundle:bundle-type**
```lisp
```

### load-asd

```lisp
Function: (load-asd pathname &key name)
```

Load system definitions from `pathname`.
`name` if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:`load-asd`.

### load-bundle-op

```lisp
Class
```

This operator is an alternative to LOAD-OP. Build a system
and all of its dependencies, using COMPILE-BUNDLE-OP. The difference with
respect to LOAD-OP is that it builds only a single FASL, which may be
faster and more resource efficient.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### load-compiled-concatenated-source-op

```lisp
Class
```

Operation to load the result of compile-concatenated-source-op

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
```

### load-concatenated-source-op

```lisp
Class
```

Operation to load the result of concatenate-source-op as source

### load-op

```lisp
Class
```

Operation for loading the compiled FASL for a Lisp file

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: '(ASDF/LISP-ACTION:PREPARE-OP
            ASDF/LISP-ACTION:COMPILE-OP)
```

### load-source-op

```lisp
Class
```

Operation for loading a Lisp file as source.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/LISP-ACTION:PREPARE-SOURCE-OP
```

### load-system

```lisp
Function: (load-system system &rest keys &key force force-not verbose version
           &allow-other-keys)
```

Shorthand for `(operate 'asdf:load-op system)`. See [operate](#operate) for details.

### load-system-definition-error

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
Initargs: :NAME
Readers: ASDF/FIND-SYSTEM:ERROR-NAME
```
**pathname**
```lisp
Initargs: :PATHNAME
Readers: ASDF/FIND-SYSTEM:ERROR-PATHNAME
```
**condition**
```lisp
Initargs: :CONDITION
Readers: ASDF/FIND-SYSTEM:ERROR-CONDITION
```

### load-systems

```lisp
Function: (load-systems &rest systems)
```

Loading multiple systems at once.

### load-systems\*

```lisp
Function: (load-systems* systems &rest keys)
```

Loading multiple systems at once.

### locate-system

```lisp
Function: (locate-system name)
```

Given a system `name` designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a [system](#system) object that may be REGISTER-SYSTEM'ed.
PATHNAME when not null is a path from which to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded [system](#system) object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded.

### make

```lisp
Function: (make system &rest keys)
```

The recommended way to interact with ASDF3.1 is via (ASDF:`make` :FOO).
It will build system FOO using the operation [build-op](#build-op),
the meaning of which is configurable by the system, and
defaults to [load-op](#load-op), to load it in current image.

### make-operation

```lisp
Function: (make-operation operation-class)
```

This function creates and memoizes an instance of `operation-class`.
All operation instances MUST be created through this function.

Use of INITARGS is not supported at this time.

### make-plan

```lisp
Generic Function: (make-plan plan-class operation component &key
                   &allow-other-keys)
```

Generate and return a plan for performing `operation` on `component`.

### map-systems

```lisp
Function: (map-systems fn)
```

Apply `fn` to each defined system.

`fn` should be a function of one argument. It will be
called with an object of type asdf:system.

### missing-component

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/find-component::requires**
```lisp
Initargs: :REQUIRES
```
**asdf/component:parent**
```lisp
Initargs: :PARENT
```

### missing-component-of-version

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:version**
```lisp
Initargs: :VERSION
```

### missing-dependency

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/find-component::required-by**
```lisp
Initargs: :REQUIRED-BY
Readers: ASDF/FIND-COMPONENT:MISSING-REQUIRED-BY
```

### missing-dependency-of-version

```lisp
Condition
```


### module

```lisp
Class
```

A module is a intermediate component with both a parent and children,
typically but not necessarily representing the files in a subdirectory of the build source.


### module-components

```lisp
Generic Function: (module-components object)
```



### monolithic-bundle-op

```lisp
Class
```

operations that are both monolithic-op and bundle-op

<u>**Direct Slots**</u>

**asdf/bundle:prologue-code**
```lisp
Readers: ASDF/BUNDLE:PROLOGUE-CODE
Writers: (SETF ASDF/BUNDLE:PROLOGUE-CODE)
```
**asdf/bundle:epilogue-code**
```lisp
Readers: ASDF/BUNDLE:EPILOGUE-CODE
Writers: (SETF ASDF/BUNDLE:EPILOGUE-CODE)
```

### monolithic-compile-bundle-op

```lisp
Class
```

Create a single fasl for the system and its dependencies.


### monolithic-compile-concatenated-source-op

```lisp
Class
```

Operation to compile the result of monolithic-concatenate-source-op

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/CONCATENATE-SOURCE:MONOLITHIC-CONCATENATE-SOURCE-OP
```

### monolithic-concatenate-source-op

```lisp
Class
```

Operation to concatenate all sources in a system and its dependencies
into a single file


### monolithic-deliver-asd-op

```lisp
Class
```

produce fasl and asd files for combined system and dependencies.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: '(ASDF/BUNDLE:MONOLITHIC-COMPILE-BUNDLE-OP)
```

### monolithic-dll-op

```lisp
Class
```

Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system or its dependencies. See LIB-OP

### monolithic-lib-op

```lisp
Class
```

Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system or its dependencies. See LIB-OP.

### monolithic-load-bundle-op

```lisp
Class
```

Load a single fasl for the system and its dependencies.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/BUNDLE:MONOLITHIC-COMPILE-BUNDLE-OP
```

### monolithic-load-compiled-concatenated-source-op

```lisp
Class
```

Operation to load the result of monolithic-compile-concatenated-source-op

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/CONCATENATE-SOURCE:MONOLITHIC-COMPILE-CONCATENATED-SOURCE-OP
```

### monolithic-load-concatenated-source-op

```lisp
Class
```

Operation to load the result of monolithic-concatenate-source-op as source

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/CONCATENATE-SOURCE:MONOLITHIC-CONCATENATE-SOURCE-OP
```

### needed-in-image-p

```lisp
Generic Function: (needed-in-image-p operation component)
```

Is the action of `operation` on `component` needed in the current image
to be meaningful, or could it just as well have been done in another Lisp image?

### non-propagating-operation

```lisp
Class
```

A NON-PROPAGATING-OPERATION is an operation that propagates
no dependencies whatsoever.  It is supplied in order that the programmer be able
to specify that s/he is intentionally specifying an operation which invokes no
dependencies.


### non-system-system

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
Initargs: :NAME
```
**class-name**
```lisp
Initargs: :CLASS-NAME
```

### non-toplevel-system

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:parent**
```lisp
Initargs: :PARENT
```
**asdf/component:name**
```lisp
Initargs: :NAME
```

### oos

```lisp
Function: (oos operation component &rest args &key &allow-other-keys)
```

Short for _operate on system_ and an alias for the [operate](#operate) function.

Operate does mainly four things for the user:

1. Resolves the `operation` designator into an operation object.
   `operation` is typically a symbol denoting an operation class, instantiated with [make-operation](#make-operation).
2. Resolves the `component` designator into a component object.
   `component` is typically a string or symbol naming a system, loaded from disk using [find-system](#find-system).
3. It then calls [make-plan](#make-plan) with the operation and system as arguments.
4. Finally calls [perform-plan](#perform-plan) on the resulting plan to actually build the system.

The entire computation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a [version](#version) argument is supplied, then operate also ensures that the system found satisfies it
using the [version-satisfies](#version-satisfies) method.
If a PLAN-CLASS argument is supplied, that class is used for the plan.
If a PLAN-OPTIONS argument is supplied, the options are passed to the plan.

The :FORCE or :FORCE-NOT argument to [operate](#operate) can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  ([system](#system)1 [system](#system)2 ... SYSTEMN) to force systems named in a given list
:FORCE-NOT has precedence over :FORCE; builtin systems cannot be forced.

For backward compatibility, all keyword arguments are passed to [make-operation](#make-operation)
when instantiating a new operation, that will in turn be inherited by new operations.
But do NOT depend on it, for this is deprecated behavior.

### operate

```lisp
Generic Function: (operate operation component &key plan-class plan-options
                   version verbose on-warnings on-failure &allow-other-keys)
```

Operate does mainly four things for the user:

1. Resolves the `operation` designator into an operation object.
   `operation` is typically a symbol denoting an operation class, instantiated with [make-operation](#make-operation).
2. Resolves the `component` designator into a component object.
   `component` is typically a string or symbol naming a system, loaded from disk using [find-system](#find-system).
3. It then calls [make-plan](#make-plan) with the operation and system as arguments.
4. Finally calls [perform-plan](#perform-plan) on the resulting plan to actually build the system.

The entire computation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a `version` argument is supplied, then operate also ensures that the system found satisfies it
using the [version-satisfies](#version-satisfies) method.
If a `plan-class` argument is supplied, that class is used for the plan.
If a `plan-options` argument is supplied, the options are passed to the plan.

The :FORCE or :FORCE-NOT argument to `operate` can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  ([system](#system)1 [system](#system)2 ... SYSTEMN) to force systems named in a given list
:FORCE-NOT has precedence over :FORCE; builtin systems cannot be forced.

For backward compatibility, all keyword arguments are passed to [make-operation](#make-operation)
when instantiating a new operation, that will in turn be inherited by new operations.
But do NOT depend on it, for this is deprecated behavior.

### operation

```lisp
Class
```

The base class for all ASDF operations.

ASDF does NOT and never did distinguish between multiple operations of the same class.
Therefore, all slots of all operations MUST have :allocation :class and no initargs. No exceptions.



### operation-definition-error

```lisp
Condition
```

Error condition related to definition of incorrect OPERATION objects.


### operation-definition-warning

```lisp
Condition
```

Warning condition related to definition of obsolete OPERATION objects.


### operation-done-p

```lisp
Generic Function: (operation-done-p operation component)
```

Returns a boolean which is NIL if the action must be performed (again).

### operation-error

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/component:component**
```lisp
Initargs: :COMPONENT
Readers: ASDF/BACKWARD-INTERFACE:ERROR-COMPONENT
```
**asdf/operation:operation**
```lisp
Initargs: :OPERATION
Readers: ASDF/BACKWARD-INTERFACE:ERROR-OPERATION
```

### operation-monolithic-p

```lisp
Function: (operation-monolithic-p op)
```



### operation-on-failure

```lisp
Generic Function: (operation-on-failure operation)
```

DEPRECATED. Please use UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead.

### operation-on-warnings

```lisp
Generic Function: (operation-on-warnings operation)
```

DEPRECATED. Please use UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead.

### output-file

```lisp
Function: (output-file operation component)
```

The unique output file of performing `operation` on `component`

### output-files

```lisp
Generic Function: (output-files operation component)
```

Methods for this function return two values: a list of output files
corresponding to this action, and a boolean indicating if they have already been subjected
to relevant output translations and should not be further translated.

Methods on [perform](#perform) *must* call this function to determine where their outputs are to be located.
They may rely on the order of the files to discriminate between outputs.


### package-inferred-system

```lisp
Class
```

Class for primary systems for which secondary systems are automatically
in the one-file, one-file, one-system style: system names are mapped to files under the primary
system's system-source-directory, dependencies are inferred from the first defpackage form in
every such file


### package-inferred-system-missing-package-error

```lisp
Condition
```

<u>**Direct Slots**</u>

**asdf/system:system**
```lisp
Initargs: :SYSTEM
```
**pathname**
```lisp
Initargs: :PATHNAME
```

### package-system

```lisp
Class
```


### parent-component

```lisp
Class
```

A PARENT-COMPONENT is a component that may have children.

<u>**Direct Slots**</u>

**asdf/component:children**
```lisp
Initargs: :COMPONENTS
```
**asdf/component:children-by-name**
```lisp
```
**asdf/component:default-component-class**
```lisp
Initargs: :DEFAULT-COMPONENT-CLASS
Readers: ASDF/COMPONENT:MODULE-DEFAULT-COMPONENT-CLASS
Writers: (SETF ASDF/COMPONENT:MODULE-DEFAULT-COMPONENT-CLASS)
```

### perform

```lisp
Generic Function: (perform operation component)
```

`perform` an action, consuming its input-files and building its output-files

### perform-plan

```lisp
Generic Function: (perform-plan plan &key)
```

Actually perform a plan and build the requested actions

### perform-with-restarts

```lisp
Generic Function: (perform-with-restarts operation component)
```

[perform](#perform) an action in a context where suitable restarts are in place.

### precompiled-system

```lisp
Class
```

Class For a system that is delivered as a precompiled fasl

<u>**Direct Slots**</u>

**asdf/system:build-pathname**
```lisp
Initargs: :FASL, :FASB
```

### prepare-bundle-op

```lisp
Class
```

Operation class for loading the bundles of a system's dependencies

<u>**Direct Slots**</u>

**asdf/action:sideway-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/LISP-ACTION:LOAD-OP
```

### prepare-op

```lisp
Class
```

Load the dependencies for the COMPILE-OP or LOAD-OP of a given COMPONENT.

<u>**Direct Slots**</u>

**asdf/action:sideway-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/LISP-ACTION:LOAD-OP
```

### prepare-source-op

```lisp
Class
```

Operation for loading the dependencies of a Lisp file as source.

<u>**Direct Slots**</u>

**asdf/action:sideway-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/LISP-ACTION:LOAD-SOURCE-OP
```

### primary-system-name

```lisp
Function: (primary-system-name system-designator)
```

Given a system designator NAME, return the name of the corresponding primary system,
after which the .asd file is named. That's the first component when dividing the name
as a string by / slashes. A component designates its system.

### process-source-registry

```lisp
Generic Function: (process-source-registry spec &key inherit register)
```



### program-op

```lisp
Class
```

create an executable file from the system and its dependencies

<u>**Direct Slots**</u>

**asdf/bundle:bundle-type**
```lisp
Allocation: :CLASS
Initform: :PROGRAM
```

### program-system

```lisp
Class
```

<u>**Direct Slots**</u>

**asdf/bundle:prologue-code**
```lisp
Initargs: :PROLOGUE-CODE
Readers: ASDF/BUNDLE:PROLOGUE-CODE
```
**asdf/bundle:epilogue-code**
```lisp
Initargs: :EPILOGUE-CODE
Readers: ASDF/BUNDLE:EPILOGUE-CODE
```
**asdf/bundle::no-uiop**
```lisp
Initargs: :NO-UIOP
```
**asdf/bundle::prefix-lisp-object-files**
```lisp
Initargs: :PREFIX-LISP-OBJECT-FILES
```
**asdf/bundle::postfix-lisp-object-files**
```lisp
Initargs: :POSTFIX-LISP-OBJECT-FILES
```
**asdf/bundle::extra-object-files**
```lisp
Initargs: :EXTRA-OBJECT-FILES
```
**asdf/bundle::extra-build-args**
```lisp
Initargs: :EXTRA-BUILD-ARGS
```

### register-immutable-system

```lisp
Function: (register-immutable-system system-name &rest keys)
```

Register `system-name` as preloaded and immutable.
It will automatically be considered as passed to FORCE-NOT in a plan.

### register-preloaded-system

```lisp
Function: (register-preloaded-system system-name &rest keys &key (version t)
           &allow-other-keys)
```

Register a system as being preloaded. If the system has not been loaded from the filesystem
yet, or if its build information is later cleared with [clear-system](#clear-system), a dummy system will be
registered without backing filesystem information, based on `keys` (e.g. to provide a `version`).
If `version` is the default T, and a system was already loaded, then its version will be preserved.

### register-system-packages

```lisp
Function: (register-system-packages system packages)
```

Register `system` as providing `packages`.

### registered-system

```lisp
Function: (registered-system name)
```

Return a system of given `name` that was registered already,
if such a system exists.  `name` is a system designator, to be
normalized by [coerce-name](#coerce-name). The value returned is a system object,
or NIL if not found.

### registered-systems

```lisp
Function: (registered-systems)
```

Return a list of the names of every registered system.

### require-system

```lisp
Function: (require-system system &rest keys &key &allow-other-keys)
```

Ensure the specified `system` is loaded, passing the `keys` to [operate](#operate), but do not update the
system or its dependencies if it has already been loaded.

```lisp
Class
```

A SYSTEM subclass whose processing is handled by
the implementation's REQUIRE rather than by internal ASDF mechanisms.

<u>**Direct Slots**</u>

**asdf/component:module**
```lisp
Initargs: :MODULE
```

### required-components

```lisp
Function: (required-components system &rest keys &key
           (goal-operation (quote load-op)) &allow-other-keys)
```

Given a `system` and a `goal-operation` (default [load-op](#load-op)), traverse the dependencies and
return a list of the components involved in building the desired action.

### resolve-location

```lisp
Function: (resolve-location x &key ensure-directory wilden directory)
```

Resolve location designator `x` into a PATHNAME

### run-shell-command

```lisp
Function: (run-shell-command control-string &rest args)
```

PLEASE DO NOT USE. This function is not just DEPRECATED, but also dysfunctional.
Please use UIOP:RUN-PROGRAM instead.

### search-for-system-definition

```lisp
Function: (search-for-system-definition system)
```



### selfward-operation

```lisp
Generic Function: (selfward-operation object)
```



```lisp
Class
```

A SELFWARD-OPERATION depends on another operation on the same component.
I.e., if O is a SELFWARD-OPERATION, and its SELFWARD-OPERATION designates a list of operations L,
then the action (O . C) of O on component C depends on each (S . C) for S in L.
E.g. before a component may be loaded by LOAD-OP, it must have been compiled by COMPILE-OP.
A operation-designator designates a singleton list of the designated operation;
a list of operation-designators designates the list of designated operations;
NIL is not a valid operation designator in that context.  Note that any dependency
ordering between the operations in a list of SELFWARD-OPERATION should be specified separately
in the respective operation's COMPONENT-DEPENDS-ON methods so that they be scheduled properly.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Type: (OR ASDF/ACTION::OPERATION-DESIGNATOR LIST)
Allocation: :CLASS
Readers: ASDF/ACTION:SELFWARD-OPERATION
```

### sequential-plan

```lisp
Class
```

Simplest, default plan class, accumulating a sequence of actions


### sideway-operation

```lisp
Generic Function: (sideway-operation object)
```



```lisp
Class
```

A SIDEWAY-OPERATION has dependencies that propagate "sideway" to siblings
that a component depends on. I.e. if O is a SIDEWAY-OPERATION, and its SIDEWAY-OPERATION slot
designates operation S (where NIL designates O itself), then the action (O . C) of O on component C
depends on each of (S . D) where D is a declared dependency of C.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP,
each of its declared dependencies must first be loaded as by LOAD-OP.

<u>**Direct Slots**</u>

**asdf/action:sideway-operation**
```lisp
Type: ASDF/ACTION::OPERATION-DESIGNATOR
Allocation: :CLASS
Readers: ASDF/ACTION:SIDEWAY-OPERATION
```

### source-file

```lisp
Class
```

<u>**Direct Slots**</u>

**type**
```lisp
Readers: ASDF/COMPONENT:SOURCE-FILE-EXPLICIT-TYPE
Writers: (SETF ASDF/COMPONENT:SOURCE-FILE-EXPLICIT-TYPE)
```

### source-file-type

```lisp
Generic Function: (source-file-type component system)
```

DEPRECATED. Use the [file-type](#file-type) of a `component` instead.

### static-file

```lisp
Class
```

Component for a file to be included as is in the build output

<u>**Direct Slots**</u>

**type**
```lisp
```

### sysdef-immutable-system-search

```lisp
Function: (sysdef-immutable-system-search requested)
```



### sysdef-preloaded-system-search

```lisp
Function: (sysdef-preloaded-system-search requested)
```

If `requested` names a system registered as preloaded, return a new system
with its registration information.

### system

```lisp
Class
```

SYSTEM is the base class for top-level components that users may request
ASDF to build.

<u>**Direct Slots**</u>

**asdf/component:description**
```lisp
Readers: ASDF/SYSTEM:SYSTEM-DESCRIPTION
Writers: (SETF ASDF/SYSTEM:SYSTEM-DESCRIPTION)
```
**asdf/component:long-description**
```lisp
Readers: ASDF/SYSTEM:SYSTEM-LONG-DESCRIPTION
Writers: (SETF ASDF/SYSTEM:SYSTEM-LONG-DESCRIPTION)
```
**asdf/component:author**
```lisp
Initargs: :AUTHOR
Readers: ASDF/SYSTEM:SYSTEM-AUTHOR
Writers: (SETF ASDF/SYSTEM:SYSTEM-AUTHOR)
```
**asdf/component:maintainer**
```lisp
Initargs: :MAINTAINER
Readers: ASDF/SYSTEM:SYSTEM-MAINTAINER
Writers: (SETF ASDF/SYSTEM:SYSTEM-MAINTAINER)
```
**asdf/component:licence**
```lisp
Initargs: :LICENSE, :LICENCE
Readers: ASDF/SYSTEM:SYSTEM-LICENSE, ASDF/SYSTEM:SYSTEM-LICENCE
Writers: (SETF ASDF/SYSTEM:SYSTEM-LICENSE), (SETF ASDF/SYSTEM:SYSTEM-LICENCE)
```
**asdf/system:homepage**
```lisp
Initargs: :HOMEPAGE
Readers: ASDF/SYSTEM:SYSTEM-HOMEPAGE
Writers: (SETF ASDF/SYSTEM:SYSTEM-HOMEPAGE)
```
**asdf/system:bug-tracker**
```lisp
Initargs: :BUG-TRACKER
Readers: ASDF/SYSTEM:SYSTEM-BUG-TRACKER
Writers: (SETF ASDF/SYSTEM:SYSTEM-BUG-TRACKER)
```
**asdf/system:mailto**
```lisp
Initargs: :MAILTO
Readers: ASDF/SYSTEM:SYSTEM-MAILTO
Writers: (SETF ASDF/SYSTEM:SYSTEM-MAILTO)
```
**asdf/system:long-name**
```lisp
Initargs: :LONG-NAME
Readers: ASDF/SYSTEM:SYSTEM-LONG-NAME
Writers: (SETF ASDF/SYSTEM:SYSTEM-LONG-NAME)
```
**asdf/system:source-control**
```lisp
Initargs: :SOURCE-CONTROL
Readers: ASDF/SYSTEM:SYSTEM-SOURCE-CONTROL
Writers: (SETF ASDF/SYSTEM:SYSTEM-SOURCE-CONTROL)
```
**asdf/system:builtin-system-p**
```lisp
Initargs: :BUILTIN-SYSTEM-P
Readers: ASDF/SYSTEM:BUILTIN-SYSTEM-P
Writers: (SETF ASDF/SYSTEM:BUILTIN-SYSTEM-P)
```
**asdf/system:build-pathname**
```lisp
Initargs: :BUILD-PATHNAME
Readers: ASDF/SYSTEM:COMPONENT-BUILD-PATHNAME
Writers: (SETF ASDF/SYSTEM:COMPONENT-BUILD-PATHNAME)
```
**asdf/system:entry-point**
```lisp
Initargs: :ENTRY-POINT
Readers: ASDF/SYSTEM:COMPONENT-ENTRY-POINT
Writers: (SETF ASDF/SYSTEM:COMPONENT-ENTRY-POINT)
```
**asdf/component:source-file**
```lisp
Initargs: :SOURCE-FILE
Readers: ASDF/SYSTEM:SYSTEM-SOURCE-FILE
Writers: (SETF ASDF/SYSTEM:SYSTEM-SOURCE-FILE)
```
**asdf/component:defsystem-depends-on**
```lisp
Initargs: :DEFSYSTEM-DEPENDS-ON
Readers: ASDF/SYSTEM:SYSTEM-DEFSYSTEM-DEPENDS-ON
```
**asdf/system::depends-on**
```lisp
Initargs: :DEFSYSTEM-DEPENDS-ON
Readers: ASDF/SYSTEM:SYSTEM-DEFSYSTEM-DEPENDS-ON
```
**asdf/system::weakly-depends-on**
```lisp
Readers: ASDF/SYSTEM:SYSTEM-WEAKLY-DEPENDS-ON
```

### system-author

```lisp
Generic Function: (system-author object)
```



### system-bug-tracker

```lisp
Generic Function: (system-bug-tracker object)
```



### system-definition-error

```lisp
Condition
```


### system-definition-pathname

```lisp
Function: (system-definition-pathname x)
```

DEPRECATED. This function used to expose ASDF internals with subtle
differences with respect to user expectations, that have been refactored
away since. We recommend you use ASDF:[system-source-file](#system-source-file) instead for a
mostly compatible replacement that we're supporting, or even
ASDF:[system-source-directory](#system-source-directory) or ASDF:[system-relative-pathname](#system-relative-pathname)
if that's whay you mean.

### system-defsystem-depends-on

```lisp
Generic Function: (system-defsystem-depends-on object)
```



### system-depends-on

```lisp
Generic Function: (system-depends-on object)
```



### system-description

```lisp
Generic Function: (system-description object)
```



### system-homepage

```lisp
Generic Function: (system-homepage object)
```



### system-licence

```lisp
Generic Function: (system-licence object)
```



### system-license

```lisp
Generic Function: (system-license object)
```



### system-long-description

```lisp
Generic Function: (system-long-description object)
```



### system-long-name

```lisp
Generic Function: (system-long-name object)
```



### system-mailto

```lisp
Generic Function: (system-mailto object)
```



### system-maintainer

```lisp
Generic Function: (system-maintainer object)
```



### system-out-of-date

```lisp
Condition
```

condition signaled when a system is detected as being out of date

<u>**Direct Slots**</u>

**asdf/component:name**
```lisp
Initargs: :NAME
Readers: ASDF/COMPONENT:COMPONENT-NAME
```

### system-output-translations-directory-pathname

```lisp
Function: (system-output-translations-directory-pathname &key
           (direction :input))
```



### system-output-translations-pathname

```lisp
Function: (system-output-translations-pathname &key (direction :input))
```



### system-registered-p

```lisp
Function: (system-registered-p name)
```

DEPRECATED. Return a generalized boolean that is true if a system of given `name` was registered already.
`name` is a system designator, to be normalized by [coerce-name](#coerce-name).
The value returned if true is a pair of a timestamp and a system object.

### system-relative-pathname

```lisp
Function: (system-relative-pathname system name &key type)
```

Given a `system`, and a (Unix-style relative path) `name` of a file (or directory) of given `type`,
return the absolute pathname of a corresponding file under that system's source code pathname.

### system-source-control

```lisp
Generic Function: (system-source-control object)
```



### system-source-directory

```lisp
Function: (system-source-directory system-designator)
```

Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located.

### system-source-file

```lisp
Generic Function: (system-source-file system)
```

Return the source file in which system is defined.

### system-source-registry

```lisp
Function: (system-source-registry &key (direction :input))
```



### system-source-registry-directory

```lisp
Function: (system-source-registry-directory &key (direction :input))
```



### system-weakly-depends-on

```lisp
Generic Function: (system-weakly-depends-on object)
```



### test-op

```lisp
Class
```

Operation for running the tests for system.
If the tests fail, an error will be signaled.

<u>**Direct Slots**</u>

**asdf/action:selfward-operation**
```lisp
Allocation: :CLASS
Initform: 'ASDF/LISP-ACTION:LOAD-OP
```

### test-system

```lisp
Function: (test-system system &rest args &key force force-not verbose version
           &allow-other-keys)
```

Shorthand for `(asdf:operate 'asdf:test-op system)`. See [operate](#operate) for details.

### traverse

```lisp
Generic Function: (traverse operation component &key plan-class
                   &allow-other-keys)
```

DEPRECATED. Use [make-plan](#make-plan) and PLAN-ACTIONS, or [required-components](#required-components),
or some other supported interface instead.

Generate and return a plan for performing `operation` on `component`.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a `component` object. The pairs will be
processed in order by [operate](#operate).

### upgrade-asdf

```lisp
Function: (upgrade-asdf)
```

Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that may possibly depend on ASDF.

### upward-operation

```lisp
Generic Function: (upward-operation object)
```



```lisp
Class
```

An UPWARD-OPERATION has dependencies that propagate up the component hierarchy.
I.e., if O is an instance of UPWARD-OPERATION, and its UPWARD-OPERATION slot designates operation U,
then the action (O . C) of O on a component C that has the parent P will depends on (U . P).
The default value for slot UPWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP, its PARENT
must first be prepared for loading or compiling with PREPARE-OP.

<u>**Direct Slots**</u>

**asdf/action:upward-operation**
```lisp
Allocation: :CLASS
Readers: ASDF/ACTION:UPWARD-OPERATION
```

### user-output-translations-directory-pathname

```lisp
Function: (user-output-translations-directory-pathname &key (direction :input))
```



### user-output-translations-pathname

```lisp
Function: (user-output-translations-pathname &key (direction :input))
```



### user-source-registry

```lisp
Function: (user-source-registry &key (direction :input))
```



### user-source-registry-directory

```lisp
Function: (user-source-registry-directory &key (direction :input))
```



### version-satisfies

```lisp
Generic Function: (version-satisfies component version)
```

Check whether a `component` satisfies the constraint of being at least as recent
as the specified `version`, which must be a string of dot-separated natural numbers, or NIL.
