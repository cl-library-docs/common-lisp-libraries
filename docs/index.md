# common-lisp.readthedocs

## Introduction

Common Lisp documentation - libraries or the HyperSpec - isn't particularly "modern" or "attractive". 

## Previous Efforts

Documentation efforts have been made at:

- Quickdocs: I didn't like the theme. I want the API at a glance! Honestly, this can improve! But don't look at me.

- Quickref: Frankly, this is just too much. As a user, all I want to know is "What can this library do? And, how do I do it? (What functions, macros or anything is available?)" Therefore, for a user, the only good place I found was the Packages section here. It is, after all, a Reference Manual.

- common-lisp.net: Ultimately, this seems to be the place for everything.

- CLiki: Again, layout and "at a glance"!

- UltraSpec: I liked this. The only trouble? It isn't "quick".

## UltraSpec

I liked (the theme of) UltraSpec. I also liked `mkdocs` - that is what I am using here. What UltraSpec seems to be good for is more largish websites than what is here. And anyways, `markdown` files and `dokuwiki` files (which is what UltraSpec uses) are interconvertible (untested - to a fair extent perhaps?) using `pandoc`. (pandoc is also useful for a ton of other files!)

## mkdocs

As a marketing for mkdocs: `mkdocs` is simple as

- `pip install mkdocs # or conda`
- create a .yml configuration file (or copy and edit!)
- `mkdocs gh-deploy`

Done!

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

- [alexandria](./alexandria/)
- [bordeaux-threads](./bordeaux-threads/)
- [hunchentoot](./hunchentoot/)
