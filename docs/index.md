# common-lisp.readthedocs

## Introduction

Common Lisp documentation - libraries or the HyperSpec - isn't known to be particularly "modern" or "attractive". While those terms are subjective, ease of introduction to a technology does seem to have *some* objective component to it.

## Defacto Libraries

### <span id="defacto-installation">Installation</span>

Each of the defacto libraries can be installed using quiclisp:

```lisp
(ql:quickload "alexandria") ; for example
```
See [quicklisp.org](https://www.quicklisp.org/) for instructions on
installing quicklisp.

Optionally, you may want to use `sb-ext:add-package-local-nickname` the packages for, well, [adding package local nicknames](https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3#actual-worthwhile-content-starts-here).

```lisp
(sb-ext:add-package-local-nickname :a :alexandria) ; for SBCL
```

### Libraries documented so far

(See the [html version of this page](https://digikar99.github.io/common-lisp.readthedocs/), rather than the markdown version for the working links below.)

- [alexandria](./alexandria/) - a utility library
- [bordeaux-threads](./bordeaux-threads/) - library for threading
- [hunchentoot](./hunchentoot/) - web server
- [iterate](./iterate/) - a lispy extensible alternative to `loop`


## Previous Efforts

Documentation efforts have been made at:

- [Quickdocs](http://quickdocs.org/): I didn't like the theme. I want the API at a glance! Honestly, this can improve! But don't look at me. I'm also not very at ease with full automation without human intervention.

- [Quickref](http://quickref.common-lisp.net/): Frankly, this is just too much. As a user, all I want to know is "What can this library do? And, how do I do it? (What functions, macros or anything is available?)" Therefore, for a user, the only good place I found was the Packages section here. It is, after all, a Reference Manual. Another excuse is, again, that I do want human intervention in documentation.

- [common-lisp.net](http://common-lisp.net/): Ultimately, this seems to be the place for everything. And indeed, most of the work on the site you are reading is directly based on the official documentation. An attempt is made to "simplify" wherever need is felt.

- [CLiki](http://cliki.net/): Again, layout and "at a glance"!

- [UltraSpec]: I liked this. The only trouble? It isn't "quick".

## UltraSpec

I liked (the theme of) [UltraSpec]. I also liked [mkdocs](https://www.mkdocs.org/) - I am using mkdocs with the pre-provided [readthedocs](https://readthedocs.org/) theme. What UltraSpec seems to be good for is larger websites than what is currently here, in that it isn't as straightforward as mkdocs for library-documenters to use it. 

Additionally, `markdown` files and `dokuwiki` files (the format UltraSpec requires) are interconvertible (but the compatibility is untested) using [pandoc](https://pandoc.org/). (pandoc is indeed useful for a ton of other file formats!)

## mkdocs

As a marketing for mkdocs: `mkdocs` is simple as

- `pip install mkdocs # or conda`
- create a .yml configuration file (or copy and edit!)
- put your markdown files inside docs/ directory (or as mentioned in the .yml file)
- `mkdocs gh-deploy`# to deploy on github-pages

Done!

*PS: Regardless of the justifications, all I wanted was a documentation site with a "sidebar" that, both, tells the page at glance, and is easy to navigate.*


[ultraspec]: https://phoe.tymoon.eu/clus/doku.php
