(in-package :cl-user)

(use-package :alexandria)
(use-package :iterate)

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
         (symbol-doc-ht (make-hash-table :test 'equal))
         (a 5))
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
      (let ((symbol [(str:split " " title) 1]))
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
  (iter (for (key . val) in
             (sort (hash-table-alist symbol-doc-ht) λ(string< (car -) (car --))))
        (for symbol = (-> key
                          (string-upcase)
                          (find-symbol :postmodern)))
        ;; (unless symbol (next-iteration))
        (for signature = (ignore-errors (swank/backend:arglist symbol)))
        (write-string
         (cl-rtd::conc "### " key #\newline #\newline "```lisp" #\newline
                       (ignore-errors
                         (cond ((macro-function symbol) "Macro")
                               ((typep (fdefinition symbol)
                                       (find-class 'standard-generic-function))
                                "Generic Function")
                               (t "Function")))
                       ": " (string-downcase (format nil "~D" (cons key signature)))
                       #\newline "```" #\newline #\newline
                       val (string #\newline))
         f)))
