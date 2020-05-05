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

;;; This file is more of a scratch pad than anything more serious.
;;; Just open the file with SLIME or equivalent, and C-c C-c the
;;; relevant forms for use in the REPL.

(reader:enable-reader-syntax 'lambda 'get-val)

;;; Our main function is the md-file function below.

(defun md-file (package-keyword &optional (output-file (format nil "~A.md" package-keyword)))
  (with-open-file (f (string-downcase output-file)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string 
     (with-output-to-string (*standard-output*)
       (let* ((sorted-symbols (-> (iter (for symbol in-package package-keyword
                                             external-only t)
                                        (collect symbol))
                                  (sort λ(string< (symbol-name -)
                                                  (symbol-name --)))))
              (*samedoc-symbol-list* sorted-symbols)
              (*package-name* (package-name package-keyword)))
         (iter (initially
                (format t "~&# ~(~a~)~%" (-> package-keyword
                                             (find-package)
                                             (package-name)))
                (terpri)
                (write-string (or (documentation (find-package package-keyword) t) ""))
                (terpri)
                (terpri))
               ;; (for i below 5)
               (for symbol in sorted-symbols)
               (unless (ignore-errors (typep (fdefinition symbol)
                                             (find-class 'standard-generic-function)))
                 ;;; Excluding generic functions: why?
                 )
               (format t "### ~(~a~)~%" (let ((sym-name (symbol-name symbol)))
                                          (if (str:containsp "*" sym-name)
                                              (str:replace-all "*" "\\\*" sym-name)
                                              sym-name)))
               (iter (for slot in '(function
                                    variable
                                    type))
                     (for docstring = (documentation symbol slot))
                     (when-let (doc (format-documentation slot symbol docstring))
                       (write-string doc))))
         (terpri)))
     f))
  t)
