(cl:in-package :cl-user)

(defpackage :cl-rtd
  (:use :cl :alexandria :iterate :arrows)
  (:local-nicknames (:mop :closer-mop)))

(in-package :cl-rtd)

(defgeneric format-documentation (slot symbol &optional docstring))

(defvar *samedoc-symbols* ()
  "List of symbols used for hyperlinking on the same page. This is bound in md-file.")
(defvar *blacklist-samedoc-symbols* ()
  "List of symbols users may want to exclude for hyperlinking. This list is subtracted
from *SAMEDOC-SYMBOLS* to obtain the final list.")
(defvar *package-name* "")

(reader+swank:enable-package-local-reader-syntax 'get-val)
