(ql:quickload '(:alexandria :str :iterate :reader :arrow-macros :swank/backend))

;;; This file is more of a scratch pad than anything more serious.
;;; Just open the file with SLIME or equivalent, and C-c C-c the
;;; relevant forms for use in the REPL.

;;; Why didn't I use docparser or declt?
;;; I tried declt. But found modifying the output format to suit my needs not
;;; worth the time investment, at least for bordeaux-threads.
;;; I found docparser leaving behind some details. I liked it for generating a
;;; output independent documentation-tree, but it seems not very complete.

(use-package '(:alexandria :iterate :arrow-macros))
(reader:enable-reader-syntax 'lambda)

(defun md-file (package-keyword)
  (with-output-to-file (f (string-downcase (format nil "~A.md" package-keyword))
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format f
            (with-output-to-string (*standard-output*)
              (let ((sorted-symbols (-> (iter (for symbol in-package package-keyword
                                                   external-only t)
                                              (collect symbol))
                                      (sort λ(string< (symbol-name -)
                                                      (symbol-name --))))))
                (iter (initially
                       (format t "# ~(~a~)~%" (-> package-keyword
                                                (find-package)
                                                (package-name)))
                       (terpri)
                       (format t (documentation (find-package package-keyword) t))
                       (terpri)
                       (terpri))
                      (for symbol in sorted-symbols)
                      (format t "### ~(~a~)~%" (let ((sym-name (symbol-name symbol)))
                                                 (if (str:containsp "*" sym-name)
                                                     (str:replace-all "*" "\\\*" sym-name)
                                                     sym-name)))
                      (iter (for slot in '(compiler-macro
                                           function
                                           method-combination
                                           setf
                                           type
                                           variable))
                            (when (and (member slot '(function)) (fboundp symbol))
                              (format t "~%```lisp~%~A: (~{~(~A~)~^ ~})~%```~%"
                                      (if (macro-function 'symbol) "Macro" "Function")
                                      (cons symbol (swank/backend:arglist symbol))))
                            (when-let (docstring (documentation symbol slot))
                              
                              (format t docstring)
                              (terpri)))
                      (terpri)))))))






