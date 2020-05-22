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

(defun md-file (package-keyword &optional (output-file (format nil "~A.md" package-keyword)))
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
              (*package-name* (package-name package-keyword)))
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
               (unless (ignore-errors (typep (fdefinition symbol)
                                             (find-class 'standard-generic-function)))
                 ;; Excluding generic functions: why?
                 )
               (format s "### ~(~a~)~%" (let ((sym-name (symbol-name symbol)))
                                          (if (str:containsp "*" sym-name)
                                              (str:replace-all "*" "\\\*" sym-name)
                                              sym-name)))
               (iter (for slot in '(function
                                    variable
                                    type))
                     (for docstring = (documentation symbol slot))
                     (when-let (doc (format-documentation slot symbol docstring))
                       (write-string doc s))))
         (terpri)))
     f))
  t)
