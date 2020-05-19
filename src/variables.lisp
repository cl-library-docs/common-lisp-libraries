(in-package :cl-rtd)

(defmethod format-documentation ((slot (eql 'variable)) symbol
                                 &optional (docstring (documentation symbol slot)))
  (conc (when docstring
          (conc (if (constantp symbol)
                    (format nil "~%```lisp~%Constant: ~A~%```~%~%" (symbol-value symbol))
                    (format nil "~%```lisp~%Variable~%```~%~%"))
                (hyperlink-samedoc-symbols docstring symbol)
                #\newline))
        #\newline))
