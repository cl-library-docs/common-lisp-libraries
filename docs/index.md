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

I liked (the theme of) UltraSpec. I also liked `mkdocs` - that is what I am using here. What UltraSpec seems to be good for is more largish websites than what is here. And anyways, `markdown` files and `dokuwiki` files (which is what UltraSpec uses) are interconvertible (untested - to a fair extent perhaps?) using `pandoc`. (It is also useful for a ton of other files.)

## mkdocs

As a marketing for mkdocs: `mkdocs` is simple as

- `pip install mkdocs # or conda`
- create a .yml configuration file (or copy and edit!)
- `mkdocs gh-deploy`

Done!

## Current Documentation:

(See from the [html version of this page](https://digikar99.github.io/common-lisp.readthedocs/), rather than the markdown version.)

- [alexandria](./alexandria/)
- [bordeaux-threads](./bordeaux-threads/)
