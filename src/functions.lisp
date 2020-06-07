(in-package :cl-rtd)

;; This file gets the documentation of Functions, Macros and Generic Functions
(reader:enable-reader-syntax 'lambda 'get-val)

(defun arglist-symbols (arg-list)
  (let ((special-symbols '(&optional &key)))
    (iter 
      (for elt in arg-list)
      (for linear
           initially nil
           then (or linear (member elt special-symbols)))
      (cond ((member elt special-symbols) nil)
            (linear
             (collect (etypecase elt
                        (symbol elt)
                        (list (car elt)))))
            ((listp elt) (appending (arglist-symbols elt)))
            (t (collect elt))))))

(defparameter *classes-are-significant* nil
  "If T, generic functions are not documented separately.")
;;; TODO: This should only disable listing accessors separately

(defmethod format-documentation ((slot (eql 'function)) symbol
                                 &optional (docstring (documentation symbol slot)))
  (when (fboundp symbol)
    (let* ((arg-list (swank/backend:arglist symbol))
           (arg-symbols (arglist-symbols arg-list)))
      (ppcre-flet ((quote-args
                    (let ((symbol-name (subseq target-string [reg-starts 0] [reg-ends 0])))
                      (if (and (string-upcase-p symbol-name)
                               (member (string-upcase symbol-name) arg-symbols
                                       :test 'string=))
                          (conc "`" (string-downcase symbol-name) "`")
                          symbol-name))))
        (conc (format nil "~%```lisp~%~A: ~D~%```~%~%"
                      (cond ((macro-function symbol) "Macro")
                            ((typep (fdefinition symbol)
                                    (find-class 'standard-generic-function))
                             "Generic Function")
                            (t "Function"))
                      (map-tree λ(typecase -
                                   (keyword (string-downcase (format nil "~S" -)))
                                   (string-designator (string-downcase -))
                                   (t (write-to-string -)))
                                (cons symbol arg-list)))
              (when docstring          
                (restart-case
                    (-<> (ppcre:regex-replace-all "([^\\s^\(^\)^\.^\,^\;]*)"
                                                  docstring #'quote-args)
                      ;; Usually, docstring contain symbols in upcase formats. However,
                      ;; quoted-and-downcased symbols "look nicer". 
                      ;; A docstring-ed symbol cannot contain a space and '(', ')' characters.
                      ;; Also assume, full-stops and commas are due to english.
                      (requote-with-backquote <>)
                      ;; Some also contain symbols in `format'. We want them to be in `format`.
                      ;; Ideally they should be hyperlinked elsewhere around the world!
                      (hyperlink-samedoc-symbols <> symbol)
                      ;; Too many links? Control using *blacklist-samedoc-symbols*
                      ;; These functions should be separated 
                      (quote-self <> symbol))
                  (continue-ignoring-errors () (format t "Errors on ~D~%" symbol))))
              #\newline)))))

