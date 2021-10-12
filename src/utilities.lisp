(in-package :cl-rtd)

(reader:enable-reader-syntax 'get-val)

(defun map-tree (function tree)
  (cond ((null tree) ())
        ((listp tree)
         (cons (map-tree function (car tree))
               (map-tree function (cdr tree))))
        (t (funcall function tree))))

(defun string-upcase-p (string)
  (every (lambda (char)
           (or (not (alpha-char-p char))
               (char= (char-upcase char) char)))
         string))

(defmacro ppcre-flet (bindings &body body)
  `(flet (,@(loop for binding in bindings
               collect (destructuring-bind (name &body body) binding
                         `(,name (target-string start end match-start match-end
                                                reg-starts reg-ends)
                                 (declare (ignorable target-string start end match-start match-end
                                                     reg-starts reg-ends))
                                 ,@body))))
     ,@body))

(defun conc (&rest strings/chars)
  (apply #'concatenate
         'string
         (mapcar (lambda (elt)
                   (etypecase elt
                     (string elt)
                     (character (string elt))
                     (null "")))
                 strings/chars)))

(defmacro lm (&rest body-vars)
  `(lambda ,(butlast body-vars)
     ,@(last body-vars)))

(define-constant +lisp-symbol-regex+
    `(:sequence (:register
                 (:greedy-repetition 1 nil
                                     (:alternation (:char-class (:range #\A #\Z))
                                                   #\-
                                                   #\:
                                                   #\*
                                                   (:char-class (:range #\0 #\9))))))
  :test 'equal)

(define-constant +quoted-lisp-symbol-regex+
    `(:sequence #\`
                (:register
                 (:greedy-repetition 1 nil
                                     (:alternation (:char-class (:range #\A #\Z))
                                                   #\-
                                                   #\:
                                                   #\*
                                                   (:char-class (:range #\0 #\9)))))
                #\`)
  :test 'equal)

(defun requote-with-backquote (string)
  "Converts quoted of `format' to `format`."
  (ppcre-flet ((%requote-with-backquote
                (conc (subseq target-string [reg-starts 0] (1- [reg-ends 0])) "`")))
    (ppcre:regex-replace-all "(\\`[^\\s^\(^\)]*')" string #'%requote-with-backquote)))

(defun hyperlink-samedoc-symbols (string current-symbol)
  "Converts SYMBOL or `symbol` to [symbol](#symbol) if SYMBOL is in *SAMEDOC-SYMBOLS*."
  (ppcre-flet ((%hyperlink-samedoc-symbols
                (let* ((potential-symbol-name (subseq target-string [reg-starts 0] [reg-ends 0]))
                       (potential-symbol (ignore-errors (read-from-string
                                                         (string-upcase potential-symbol-name))))
                       (downcased (string-downcase potential-symbol-name)))
                  (if (and (symbolp potential-symbol)
                           (member potential-symbol *samedoc-symbols*
                                   :test #'string=)
                           ;; avoid hyperlinking to the same function
                           (string/= potential-symbol current-symbol))
                      (format nil "[~A](#~A)"
                              (str:replace-all "*" "\\*"
                                               (str:replace-all "\\*" "*" downcased))
                              (string-trim '(#\*)
                                           (string-downcase (symbol-name potential-symbol))))
                      potential-symbol-name))))
    (-<> (ppcre:regex-replace-all +lisp-symbol-regex+ string
                                  #'%hyperlink-samedoc-symbols)
         (ppcre:regex-replace-all +quoted-lisp-symbol-regex+ <>
                                  #'%hyperlink-samedoc-symbols))))

(defun quote-self (string current-symbol)
  (ppcre-flet ((%quote-self
                (let* ((symbol-name (subseq target-string [reg-starts 0] [reg-ends 0]))
                       (downcased (string-downcase symbol-name)))
                  (if (string= (string-upcase symbol-name) current-symbol)
                      (format nil "`~A`" downcased)
                      symbol-name))))
    (ppcre:regex-replace-all +lisp-symbol-regex+ string
                             #'%quote-self)))

