(cl:in-package :cl-user)

(defpackage :cl-rtd
  (:use :cl :alexandria :iterate :simple-arrows)
  (:local-nicknames (:mop :closer-mop)))

(in-package :cl-rtd)

(defgeneric format-documentation (slot symbol &optional docstring))

(defvar *samedoc-symbol-list* ()) ; could be used for hyperlinking
(defvar *package-name* "")
