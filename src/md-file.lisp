;; (handler-bind ((asdf/find-system:load-system-definition-error
;;                 (lambda (c)
;;                   (declare (ignore c))
;;                   (invoke-restart 'sb-impl::drop-them)))
;;                (sb-int:package-at-variance-error
;;                 (lambda (c)
;;                   (declare (ignore c))
;;                   (invoke-restart 'sb-impl::drop-them))))
;;   (ql:quickload '(:alexandria :str :iterate :reader :arrows :swank :closer-mop)))

;; (ql:quickload '(:alexandria :str :iterate :reader :simple-arrows :closer-mop))

(in-package :cl-rtd)

;;; Our main function is the md-file function below.
;; Typical usage of the below function includes
;; (let ((*blacklist-samedoc-symbols* '(thread lock timeout)))
;;   (md-file :bordeaux-threads "~/ram-disk/bordeaux-threads.md"))

(defparameter *slots* '(function variable type))

(defun documentation-exists-p (symbol)
  (or (some #'identity
            (mapcar (lm doc-slot (documentation symbol doc-slot))
                    *slots*))
      (trivial-types:type-specifier-p symbol)
      (fboundp symbol)
      (boundp symbol)))

(defun generate-package-documentation (&optional prologue-exists-p)
  (apply #'conc
         (unless prologue-exists-p
           (format nil "~%# ~a~%" (string-downcase *package-name*)))
         ;; Refer: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm
         ;; T for documentation merely specifies to get the documentation of the package
         (when-let (doc (documentation (find-package *package-name*) t))
           (format nil "~a~%~%" doc))
         (iter outer
           (for symbol in *samedoc-symbols*)
           ;; (for i below 5)
           (collect (format nil "~%### ~a~%" (let ((sym-name (string-downcase symbol)))
                                                 (if (str:containsp "*" sym-name)
                                                     (str:replace-all "*" "\\\*" sym-name)
                                                     sym-name))))
           (if (not (documentation-exists-p symbol))
               (collect (format nil "~%No documentation found for `~a`~%"
                                (string-downcase symbol)))
               (iter (for slot in *slots*)
                 (for docstring = (documentation symbol slot))
                 (when (and (eq slot 'function)
                            (fboundp symbol)
                            (typep (fdefinition symbol) 'generic-function)
                            *classes-are-significant*)
                   ;; If it's a generic function, we don't even reach the next lines.
                   (next-iteration))
                 ;; (print (list symbol slot))
                 ;; (print (list 'documenting symbol slot));
                 ;; Some classes may have no docstrings, but may have doc.
                 ;; We do want to write the heading for such classes.
                 (when-let (doc (format-documentation slot symbol docstring))
                   (setq doc (if (ppcre:scan "(?s)^\\n*$" doc)
                                 "" doc)) ; remove spurious new lines
                   (in outer (collect doc))))))))

(defun prologue-file-path ()
  (let ((prologue-file-path (merge-pathnames (pathname (conc "../prologue/"
                                                             (string-downcase *package-name*)
                                                             ".md"))
                                             (asdf:component-pathname
                                              (asdf:find-system "cl-rtd")))))
    (if (uiop:file-exists-p prologue-file-path)
        prologue-file-path
        nil)))

(defun md-file (package-keyword &optional (output-file (format nil "~A.md" package-keyword))
                &key classes-are-significant)
  "If CLASSES-ARE-SIGNIFICANT is T, generic functions are not documented separately."

  (let* ((symbols                   (iter (for symbol in-package package-keyword
                                               external-only t)
                                      (collect symbol)))
         (sorted-symbols            (sort symbols #'string<))
         (*samedoc-symbols*         (set-difference sorted-symbols *blacklist-samedoc-symbols*
                                                    :test #'string=))
         (*package-name*            (package-name package-keyword))
         (*classes-are-significant* classes-are-significant)
         (*print-length*            nil)
         (prologue-file-path        (prologue-file-path)))
    (when prologue-file-path
      (uiop:copy-file prologue-file-path output-file))
    (write-string-into-file (generate-package-documentation prologue-file-path)
                            (string-downcase output-file)
                            :if-exists (if prologue-file-path :append :supersede)
                            :if-does-not-exist :create)
    t))
