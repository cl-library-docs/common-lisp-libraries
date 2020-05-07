(asdf:defsystem "cl-rtd"
  :depends-on (:alexandria
               :str
               :iterate
               :reader
               :simple-arrows
               ;; simple-arrows: https://github.com/digikar99/simple-arrows
               :closer-mop
               ;; we are also using swank/backend; but that leads to some
               ;; not-yet-debugged-by-me errors
               :trivial-package-local-nicknames)
  :pathname #P"src/"
  :components ((:file "package")
               (:file "utilities")
               (:file "variables")
               (:file "functions") ; functions, macros, generic functions
               (:file "types")
               (:file "md-file")))

;;; Why didn't I use docparser or declt?
;;; I tried declt. But found modifying the output format to suit my needs not
;;; worth the time investment, at least for bordeaux-threads.
;;; I found docparser leaving behind some details. I liked it for generating a
;;; output independent documentation-tree, but it seems not very complete.

