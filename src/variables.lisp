(in-package :cl-rtd)

(defmethod format-documentation ((slot (eql 'variable)) symbol
                                 &optional (docstring (documentation symbol slot)))
  (apply #'conc
         (ecase (introspect-environment:variable-information symbol)
           (:constant
            (format nil "~%```lisp~%Constant: ~A~%```~%" (symbol-value symbol)))
           (:special
            (if (boundp symbol)
                (format nil "~%```lisp~%Variable~%Default Value: ~S~%```~%"
                        (symbol-value symbol))
                (format nil "~%```lisp~%Variable~%Default Unbound~%```~%")))
           (:symbol-macro
            (format nil "~%```lisp~%Symbol Macro~%Expansion: ~S~%```~%"
                    (macroexpand-1 symbol)))
           ((nil)))
         (when docstring
           (list #\newline
                 (hyperlink-samedoc-symbols docstring symbol)
                 #\newline))))
