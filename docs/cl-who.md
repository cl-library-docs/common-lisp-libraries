# cl-who - DSL for Markup

Version: 1.1.4
<br/>
Nickname: who
<br/>
Repository: [edicl/cl-who - Github](https://github.com/edicl/cl-who)

*This documentation is possible due to the  [excellent official documentation](https://edicl.github.io/cl-who) as of 5th May 2020.*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***

CL-WHO primarily provides a single macro [with-html-output](#with-html-output) to convert S-expressions intermingled with code into
(X)HTML, XML and the likes.

## GETTING STARTED

[with-html-output](#with-html-output) and [with-html-output-to-string](#with-html-output-to-string)
are the basic macros to get going:

```lisp
CL-USER> (defparameter *site-alist*
           '(("http://zappa.com/" . "Frank Zappa")
             ("http://marcusmiller.com/" . "Marcus Miller")
             ("http://www.milesdavis.com/" . "Miles Davis")))
*SITE-ALIST*

CL-USER> (format t
                 (with-html-output-to-string (s nil :indent t)
                   (loop for (link . title) in *site-alist*
                      do (htm (:a :href link
                                  (:b (str title))) ; <- note the str!
                              :br))))

<a href='http://zappa.com/'>
  <b>Frank Zappa
  </b>
</a>
<br />
<a href='http://marcusmiller.com/'>
  <b>Marcus Miller
  </b>
</a>
<br />
<a href='http://www.milesdavis.com/'>
  <b>Miles Davis
  </b>
</a>
<br />
NIL

CL-USER> (format t
                 (with-output-to-string (s)
                   (with-html-output (s s :indent t)
                     (loop for (link . title) in *site-alist*
                        do (htm (:a :href link
                                    (:b (str title))) ; <- note the str!
                                :br)))))

<a href='http://zappa.com/'>
  <b>Frank Zappa
  </b>
</a>
<br />
<a href='http://marcusmiller.com/'>
  <b>Marcus Miller
  </b>
</a>
<br />
<a href='http://www.milesdavis.com/'>
  <b>Miles Davis
  </b>
</a>
<br />
```

Inside these macros, there exist the lexically scoped macros: 

- **esc** as short-hand for [escape-string](#escape-string)
- **fmt** as short-hand for [cl:format](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_format.htm)
- **htm** as short-hand for (another) `with-html-output`.
- **str** as short-hand for, well, writing to the "inner html" rather than attributes; 
basically the difference between these two:

```lisp
CL-USER> (let ((string "hello")
               (class "world"))
           (with-html-output-to-string (s nil :indent t)
             (htm (:div :class class string))))
"
<div class='world'>
</div>"
CL-USER> (let ((string "hello")
               (class "world"))
           (with-html-output-to-string (s nil :indent t)
             (htm (:div :class class (str string)))))
"
<div class='world'>hello
</div>"
```

In this case, `str` could equivalently be replaced by `fmt` or `esc`.

Also see [html-mode](#html-mode). Detailed transformation rules are available in 
the [official documentation](https://edicl.github.io/cl-who/#syntax).



## CONFIGURATION VARIABLES

### \*attribute-quote-char\*

```lisp
Variable
```

Quote character for attributes.

### \*downcase-tokens-p\*

```lisp
Variable
```

If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  If T, the tag and
attribute name will be converted to lowercase only if it is in the
same case. This is useful when one needs to output case sensitive
XML.

### \*empty-attribute-syntax\*

```lisp
Variable
```

Set this to `t` to enable attribute minimization (also called
"boolean attributes", or "empty attribute syntax" according to the [w3
html standard](https://www.w3.org/TR/2012/WD-html-markup-20120329/syntax.html#syntax-attr-empty)). In XHTML attribute minimization is forbidden, and all
attributes must have a value. Thus in XHTML boolean attributes must be
defined as <input disabled='disabled' /\>. In HTML5 boolean attributes
can be defined as <input disabled\>

If it is NIL the attribute will be left out completely.

```
(:td :nowrap nil) => "<td />"
```

### \*escape-char-p\*

```lisp
Variable
```

Used by [escape-string](#escape-string) to test whether a character should be escaped.

### \*html-empty-tag-aware-p\*

```lisp
Variable
```

Set this to NIL to if you want to use CL-WHO as a strict XML
generator.  Otherwise, CL-WHO will only write empty tags listed
in [\*html-empty-tags\*](#html-empty-tags) as <tag/\> (XHTML mode) or <tag\> (SGML
mode and HTML5 mode).  For all other tags, it will always generate
<tag\></tag\>.

### \*html-empty-tags\*

```lisp
Variable
```

The list of HTML tags that should be output as empty tags.
See [\*html-empty-tag-aware-p\*](#html-empty-tag-aware-p).

### \*html-no-indent-tags\*

```lisp
Variable
```

The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only `:pre` and `:textarea`.

### \*prologue\*

```lisp
Variable
```

This is the first line that'll be printed if the `:prologue` keyword
argument is T

## FUNCTIONS AND MACROS

### conc

```lisp
Function: (conc &rest string-list)
```
Concatenates all arguments which should be string into one string.

### convert-attributes

```lisp
Function: (convert-attributes attr-list)
```
Helper function for [convert-tag-to-string-list](#convert-tag-to-string-list) which converts the
alist `attr-list` of attributes into a list of strings and/or Lisp
forms.

### convert-tag-to-string-list

```lisp
Generic Function: (convert-tag-to-string-list tag attr-list body body-fn)
```
Used by `cl-who::process-tag` to convert `HTML` into a list
of strings.  `tag` is a keyword symbol naming the outer tag, `attr-list`
is an alist of its attributes (the car is the attribute's name as a
keyword, the cdr is its value), `body` is the tag's body, and `body-fn` is
a function which should be applied to `body`.  The function must return
a list of strings or Lisp forms.

### esc

### escape-char

```lisp
Function: (escape-char char &key (test *escape-char-p*))
```
Returns an escaped version of the character `char` if `char` satisfies
the predicate `test`.  Always returns a string.

### escape-char-all

```lisp
Function: (escape-char-all char)
```
Escapes characters which aren't in the 7-bit ASCII character set.

### escape-char-iso-8859-1

```lisp
Function: (escape-char-iso-8859-1 char)
```
Escapes characters that aren't defined in ISO-8859-9.

### escape-char-minimal

```lisp
Function: (escape-char-minimal char)
```
Escapes only #<, #>, and #& characters.

### escape-char-minimal-plus-quotes

```lisp
Function: (escape-char-minimal-plus-quotes char)
```
Like [escape-char-minimal](#escape-char-minimal) but also escapes quotes.

### escape-string

```lisp
Function: (escape-string string &key (test *escape-char-p*))
```
Escape all characters in `string` which pass `test`. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for `string` which'll just be returned.

### escape-string-all

```lisp
Function: (escape-string-all string)
```
Escapes all characters in `string` which aren't in the 7-bit ASCII
character set.

### escape-string-iso-8859-1

```lisp
Function: (escape-string-iso-8859-1 string)
```
Escapes all characters in `string` which aren't defined in ISO-8859-1.

### escape-string-minimal

```lisp
Function: (escape-string-minimal string)
```
Escapes only #<, #>, and #& in `string`.

### escape-string-minimal-plus-quotes

```lisp
Function: (escape-string-minimal-plus-quotes string)
```
Like [escape-string-minimal](#escape-string-minimal) but also escapes quotes.

### fmt

### htm

### html-mode

```lisp
Function: (html-mode)
```
Returns the current HTML mode. :SGML for (SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax).

```lisp
Function: (setf (html-mode) mode)
```

Sets the output mode to XHTML or (SGML-)HTML.  MODE can be
:SGML for HTML, :XML for XHTML or :HTML5 for HTML5 (HTML syntax).


### str

### with-html-output

```lisp
Macro: (with-html-output (var &optional stream &rest rest &key prologue indent) &body body)
```
Transform the enclosed `body` consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings to `var` -
which should either hold a stream or which'll be bound to STREAM if
supplied.

### with-html-output-to-string

```lisp
Macro: (with-html-output-to-string (var &optional string-form &key prologue
                                    indent) &body body)
```
Transform the enclosed `body` consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string.




