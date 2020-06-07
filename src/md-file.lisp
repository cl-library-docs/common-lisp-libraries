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

(reader:enable-reader-syntax 'lambda 'get-val)
(in-package :cl-rtd)

;;; Our main function is the md-file function below.
;; Typical usage of the below function includes
;; (let ((*blacklist-samedoc-symbols* '(thread lock timeout)))
;;   (md-file :bordeaux-threads "~/ram-disk/bordeaux-threads.md"))

(defun md-file (package-keyword &optional (output-file (format nil "~A.md" package-keyword))
                &key classes-are-significant)
  "If CLASSES-ARE-SIGNIFICANT is T, generic functions are not documented separately."
  (with-open-file (f (string-downcase output-file)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string 
     (with-output-to-string (s)
       (let* ((sorted-symbols (-> (iter (for symbol in-package package-keyword
                                             external-only t)
                                        (collect symbol))
                                  (sort λ(string< (symbol-name -)
                                                  (symbol-name --)))))
              (*samedoc-symbols* (set-difference sorted-symbols *blacklist-samedoc-symbols*
                                                 :test #'string=))
              (*package-name* (package-name package-keyword))
              (*classes-are-significant* classes-are-significant))
         (iter (initially
                (format s "~&# ~(~a~)~%" (-> package-keyword
                                             (find-package)
                                             (package-name)))
                (terpri s)
                (write-string (or (documentation (find-package package-keyword) t) "") s)
                (terpri s)
                (terpri s))
               ;; (for i below 5)
               (for symbol in sorted-symbols)
               ;; Document the symbol only if some doc exists. Should we?               
               (let ((symbol-is-undocumented t)
                     (wrote-symbol-name nil))
                 (iter (for slot in '(function
                                      variable
                                      type))
                       ;; (unless (eq symbol 'ql-dist:archive-content-sha1) (next-iteration))
                       (for docstring = (documentation symbol slot))
                       (when (and (eq slot 'function)
                                  (fboundp symbol)
                                  (typep (fdefinition symbol)
                                         (find-class 'standard-generic-function))
                                  classes-are-significant)
                         (next-iteration))
                       ;; If it's a generic function, we don't even reach the next lines.
                       ;; (if (eq symbol 'ql-dist:archive-content-sha1) (print 'yes))
                       ;; (print (list symbol slot))
                       ;; (print (list 'documenting symbol slot));
                       ;; Some classes may have no docstrings, but may have doc.
                       ;; We do want to write the heading for such classes.
                       (when-let (doc (format-documentation slot symbol docstring))
                         (setq doc (if (ppcre:scan "(?s)^\\n*$" doc)
                                       "" doc)) ; remove spurious new lines
                         (when (and (string/= doc "") (not wrote-symbol-name)) ; run only once
                           ;; (print (list slot symbol))
                           (setq symbol-is-undocumented nil
                                 wrote-symbol-name t)
                           (format s "~%### ~(~a~)~%" (let ((sym-name (symbol-name symbol)))
                                                        (if (str:containsp "*" sym-name)
                                                            (str:replace-all "*" "\\\*" sym-name)
                                                            sym-name))))
                         (write-string doc s)))))
         (terpri)))
     f))
  t)
