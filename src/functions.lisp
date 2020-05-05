(in-package :cl-rtd)

;; This file gets the documentation of Functions, Macros and Generic Functions

(defmethod format-documentation ((slot (eql 'function)) symbol
                                 &optional (docstring (documentation symbol slot)))
  (when (fboundp symbol)
    (let* ((args (swank/backend:arglist symbol))
           (arg-symbols (mapcar λ(etypecase -
                                   (symbol -)
                                   (list (car -)))
                                args)))
      (ppcre-let ((quote-args
                   (let ((symbol (subseq target-string [reg-starts 0] [reg-ends 0])))
                     (if (and (string-upcase-p symbol)
                              (member (string-upcase symbol) arg-symbols
                                      :test 'string=))
                         (conc "`" (string-downcase symbol) "`")
                         symbol)))
                  (hyperlink-samedoc-symbols
                   (let ((symbol (subseq target-string
                                         (1+ [reg-starts 0])
                                         (1- [reg-ends 0]))))
                     (if (member (string-upcase symbol) *samedoc-symbol-list*
                                 :test 'string=)
                         (format nil "[~A](#~A)" symbol symbol)
                         (subseq target-string [reg-starts 0] [reg-ends 0])))))
        (conc (format nil "~%```lisp~%~A: (~{~(~A~)~^ ~})~%```~%"
                      (cond ((macro-function symbol) "Macro")
                            ((typep (fdefinition symbol)
                                    (find-class 'standard-generic-function))
                             "Generic Function")
                            (t "Function"))
                      (cons symbol args))
              (when docstring          
                (-<> (ppcre:regex-replace-all "([^\\s^\(^\)^\.^\,]*)" docstring #'quote-args)
                  ;; Usually, docstring contain symbols in upcase formats. However,
                  ;; quoted-and-downcased symbols "look nicer". 
                  ;; A docstring-ed symbol cannot contain a space and '(', ')' characters.
                  ;; Also assume, full-stops and commas are due to english.
                  (requote-with-backquote <>)
                  ;; Some also contain symbols in `format'. We want them to be in `format`.
                  ;; Ideally they should be hyperlinked elsewhere around the world!
                  ;; (ppcre:regex-replace-all "(\\`[^\\s^\(^\)]*\\`)" <>
                  ;; #'hyperlink-samedoc-symbols) ; too many links!            
                  ))
              #\newline)))))

