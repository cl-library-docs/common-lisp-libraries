(in-package :cl-rtd)

(defun string-upcase-p (string)
  (every (lambda (char)
           (or (not (alpha-char-p char))
               (char= (char-upcase char) char)))
         string))

(defmacro ppcre-let (bindings &body body)
  `(flet (,@(loop for binding in bindings
               collect (destructuring-bind (name &body body) binding
                         `(,name (target-string start end match-start match-end
                                                reg-starts reg-ends)
                                 (declare (ignorable target-string start end match-start match-end
                                                     reg-starts reg-ends))
                                 ,@body))))
     ,@body))

(defun conc (&rest strings/chars)
  (apply 'concatenate
         'string
         (mapcar (lambda (elt)
                   (etypecase elt
                     (string elt)
                     (character (string elt))
                     (null "")))
                 strings/chars)))

(defun requote-with-backquote (string)
  "Converts quoted of `format' to `format`."
  (ppcre-let ((%requote-with-backquote
               (conc (subseq target-string [reg-starts 0] (1- [reg-ends 0])) "`")))
    (ppcre:regex-replace-all "(\\`[^\\s^\(^\)]*')" string #'%requote-with-backquote)))

