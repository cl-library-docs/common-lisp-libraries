(in-package :cl-rtd)

(reader:enable-reader-syntax 'get-val 'lambda)

(defparameter md-file "~/ram-disk/postmodern.md")

(defparameter file-contents (with-output-to-string (s)
                              (iter (for line in-file md-file using 'read-line)
                                    (write-line line s))))

(defun parse (file-contents)
  (let* ((body-list (cdr
                     (ppcre:split
                      `(:sequence (:flags :single-line-mode-p)
                                  #\newline
                                  (:register (:sequence
                                              (:non-greedy-repetition
                                               0 nil (:inverted-char-class #\newline))))
                                  #\newline
                                  (:greedy-repetition 2 nil #\-)
                                  #\newline
                                  #\newline)
                      file-contents)))
         (symbol-doc-ht (make-hash-table :test 'equal)))
    (ppcre:do-register-groups (title)
        (`(:sequence (:flags :single-line-mode-p)
                     #\newline
                     (:register (:sequence
                                 (:non-greedy-repetition
                                  0 nil (:inverted-char-class #\newline))))
                     #\newline
                     (:greedy-repetition 2 nil #\-)
                     #\newline
                     #\newline)
          file-contents)
      (let ((symbol (str:replace-all "**" "*" [(str:split " " title) 1])))
        ;; (print symbol)
        ;; (print (car body-list))
        ;; (if (= (decf a) 0) (return-from parse nil))
        (if-let (doc [symbol-doc-ht symbol])
          (setf [symbol-doc-ht symbol] (str:concat doc (car body-list)))
          (setf [symbol-doc-ht symbol] (car body-list)))
        (setq body-list (cdr body-list))))
    symbol-doc-ht))



(with-open-file (f "~/ram-disk/postmodern-md.md"
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
  (let ((symbol-doc-ht (parse file-contents)))
    (iter (for (key . val) in
               (sort (hash-table-alist symbol-doc-ht) λ(string< (car -) (car --))))
          (for symbol = (find-symbol (string-upcase key)
                                     :postmodern))
          (unless symbol (format t "~&~D was not found in package~%"
                                 (string-upcase key)))
          (for signature = (ignore-errors (swank/backend:arglist symbol)))
          (write-string
           (conc "### " (str:replace-all "*" "\\*" key)
                 (when (fboundp symbol)
                   (conc #\newline #\newline "```lisp" #\newline
                         (cond ((macro-function symbol) "Macro")
                               ((typep (fdefinition symbol)
                                       (find-class 'standard-generic-function))
                                "Generic Function")
                               (t "Function"))
                         ": " (string-downcase (format nil "~D" (cons key signature)))
                         #\newline "```" #\newline))
                 #\newline
                 val (string #\newline))
           f))))
