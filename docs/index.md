# common-lisp-libraries.readthedocs.io

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

*See the [html version of this page](https://common-lisp-libraries.readthedocs.io), rather than the markdown version for the working links below.*

---

## Introduction

Common Lisp documentation - libraries or the HyperSpec - isn't known to be particularly "modern" or "attractive". While those terms are subjective, ease of introduction to a technology does seem to have *some* objective component to it.


## Libraries

### <span id="defacto-installation">Installation</span>

Each of the below libraries (except asdf and quicklisp) can be installed using quicklisp:

```lisp
(ql:quickload "alexandria") ; for example
```
See [quicklisp - Getting Started](./quicklisp/#getting-started) for instructions on
installing quicklisp.

Optionally, you may want to use `trivial-package-local-nicknames` for, well, [adding package local nicknames](https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3#actual-worthwhile-content-starts-here).

```lisp
(ql:quickload :trivial-package-local-nicknames)
(trivial-package-local-nicknames:add-package-local-nickname :a :alexandria)
; OR
(defpackage my-package
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
;;; I'm yet to read up on the naming conventions and the reasons behind those conventions
;;; in the context of systems and packages; some conventions do exist.
```

### Libraries documented so far

The below list of libraries is not even the *complete list of defacto libraries*. Many more awesome Common Lisp libraries are available at [awesome-cl](https://github.com/CodyReichert/awesome-cl).

#### Defacto libraries

- [asdf](./asdf/) - build system for softwares (packages*)
- [alexandria](./alexandria/) - a utility library
- [bordeaux-threads](./bordeaux-threads/) - library for threading
- [cl-ppcre](./cl-ppcre/) - regular expressions
- [cl-who](./cl-who/) - DSL for Markup
- [hunchentoot](./hunchentoot/) - web server
- [fiveam](./fiveam/) - regression testing framework
- [iterate](./iterate/) - a lispy extensible alternative to `loop`
- [postmodern](./postmodern/) - PostgreSQL programming interace
- [quicklisp](./quicklisp/) - library manager
- [utilities](./utilities/) - a collection of utility libraries

#### Not yet defacto

- [numcl](./numcl/) - lispy clone of numpy
- [unix-opts](./unix-opts/) - minimalistic command line options parser


\*What one might call packages in other languages are called systems in Common Lisp parlance. Instead, the word `package` in Common Lisp refers to a data structure providing namespacing
for symbols. By contrast, a system is a tool to organize a bunch of files, in accordance with dependencies, and specifying how to perform certain actions on the system. A single system may contain multiple packages. See [this StackOverFlow answer](https://stackoverflow.com/questions/45642330/why-do-many-common-lisp-systems-use-a-single-packages-lisp-file/45643829#45643829) for a detailed discussion.

## Previous Efforts

Documentation efforts have been made at:

- [Quickdocs](http://quickdocs.org/): I didn't like the theme. I want the API at a glance! Honestly, this can improve! But don't look at me. I'm also not very at ease with full automation without human intervention.

- [Quickref](http://quickref.common-lisp.net/): Frankly, this is just too much. As a user, all I want to know is "What can this library do? And, how do I do it? (What functions, macros or anything is available?)" Therefore, for a user, the only good place I found was the Packages section here. It is, after all, a Reference Manual. Another excuse is, again, that I do want human intervention in documentation.

- [common-lisp.net](http://common-lisp.net/): Ultimately, this is the place for most everything Common Lisp. And indeed, most of the work here is based on the official documentation. An attempt is made to "simplify" wherever need is felt.

- [CLiki](http://cliki.net/): Again, layout and "at a glance"!

- [UltraSpec]: I liked this. The only trouble? It isn't "quick".

## UltraSpec

I liked (the theme of) [UltraSpec]. I also liked [mkdocs](https://www.mkdocs.org/) - I am using mkdocs with the pre-provided [readthedocs](https://readthedocs.org/) theme. What UltraSpec seems to be good for is larger websites than what is currently here, in that it isn't as straightforward as mkdocs for library-documenters to use it.

Additionally, `markdown` files and `dokuwiki` files (the format UltraSpec requires) are interconvertible (but the compatibility is untested) using [pandoc](https://pandoc.org/). (pandoc is indeed useful for a ton of other file formats!)

## mkdocs

As a marketing for mkdocs: [mkdocs](https://www.mkdocs.org/#getting-started) is as simple as

- `pip install mkdocs # or conda`
- create a .yml configuration file (or copy and edit!)
- put your markdown files inside docs/ directory (or as mentioned in the .yml file)
- `mkdocs gh-deploy`# to deploy on github-pages

Done!

*PS: Regardless of the justifications, all I wanted was a documentation site with a "sidebar" that, both, tells the page at glance, and is easy to navigate.*


[ultraspec]: https://phoe.tymoon.eu/clus/doku.php
