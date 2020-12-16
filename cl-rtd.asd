(asdf:defsystem "cl-rtd"
  :depends-on ("alexandria"
               "arrows"
               "cl-interpol"
               "closer-mop"
               "introspect-environment"
               "iterate"
               "str"
               "reader+swank"
               "trivial-types"
               #-swank "swank"
               "trivial-package-local-nicknames")
  :pathname #P"src/"
  :components ((:file "package")
               (:file "utilities")
               (:file "variables")
               (:file "functions") ; functions, macros, generic functions
               (:file "types") ; tested on SBCL
               (:file "md-file")))

;;; Why didn't I use docparser or declt?
;;; I tried declt. But found modifying the output format to suit my needs not
;;; worth the time investment, at least for bordeaux-threads.
;;; I found docparser leaving behind some details. I liked it for generating a
;;; output independent documentation-tree, but it seems not very complete.

