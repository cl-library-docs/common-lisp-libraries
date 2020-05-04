(handler-bind ((asdf/find-system:load-system-definition-error
                (lambda (c)
                  (declare (ignore c))
                  (invoke-restart 'sb-impl::drop-them)))
               (sb-int:package-at-variance-error
                (lambda (c)
                  (declare (ignore c))
                  (invoke-restart 'sb-impl::drop-them))))
  (ql:quickload '(:alexandria :str :iterate :reader :cl-arrows :swank :closer-mop)))

(ql:quickload '(:alexandria :str :iterate :reader :cl-arrows :closer-mop))

;;; This file is more of a scratch pad than anything more serious.
;;; Just open the file with SLIME or equivalent, and C-c C-c the
;;; relevant forms for use in the REPL.

;;; Why didn't I use docparser or declt?
;;; I tried declt. But found modifying the output format to suit my needs not
;;; worth the time investment, at least for bordeaux-threads.
;;; I found docparser leaving behind some details. I liked it for generating a
;;; output independent documentation-tree, but it seems not very complete.

(use-package '(:alexandria :iterate :cl-arrows))
(reader:enable-reader-syntax 'lambda 'get-val)
(add-package-local-nickname :mop :closer-mop)

(defmethod write-documentation ((slot (eql 'function)) symbol docstring)
  (when (fboundp symbol)
    (format t "~%```lisp~%~A: (~{~(~A~)~^ ~})~%```~%"
            (if (macro-function symbol) "Macro" "Function")
            (cons symbol (swank/backend:arglist symbol)))
    (when docstring
      (write-string docstring)
      (terpri))))

(defmethod write-documentation ((slot (eql 'variable)) symbol docstring)  
  (when docstring
    (if (constantp symbol)
        (format t "~%```lisp~%Constant: ~A~%```~%~%" (symbol-value symbol))
        (format t "~%```lisp~%Variable~%```~%~%"))
    (write-string docstring)
    (terpri))
  (terpri))


;;; This functino is incredibly non-lispy!!!
(defun class-documentation (class)
  (let* ((full-doc (with-output-to-string (*standard-output*) (describe class)))
         (start (+ 13 (or (search "Direct slots:" full-doc)
                          0)))
         (end (or (search "Slots with" full-doc) start)))
    (-<> (subseq full-doc start end)
      (ppcre:regex-replace-all ".*(Readers|Writers):.*::.*" <> "")
      (str:replace-all "

" "" <>)
      (ppcre:regex-replace-all ".*::(.*)" <> "
##### \\1

```lisp")
      (ppcre:regex-replace-all "    " <> "")
      (ppcre:do-matches-as-strings (str "#### (.*)" <> <> :sharedp t) (nstring-downcase str))
      (ppcre:do-matches-as-strings (str "Initargs: (.*)" <> <> :sharedp t)
        (nstring-downcase str)
        (setf [str 0] #\I))
      (ppcre:do-matches-as-strings (str "Readers: (.*)" <> <> :sharedp t)
        (nstring-downcase str)
        (setf [str 0] #\R))
      (ppcre:do-matches-as-strings (str "Writers: (.*)" <> <> :sharedp t)
        (nstring-downcase str)
        (setf [str 0] #\W))
      (ppcre:regex-replace-all ".*Documentation:" <> "```
"))))

(defmethod write-documentation ((slot (eql 'type)) symbol docstring)
  (when-let (class (ignore-errors (find-class symbol)))
    (format t "~%```lisp~%~A~%```~%~%"
            (cond ((typep class (find-class 'structure-class))
                   "Structure")
                  ((typep class (find-class 'standard-class))
                   "Class")))
    (when docstring     
      (write-string docstring)
      (terpri)
      (terpri))
    ;; (when (typep class (find-class 'standard-class))
    ;;  (iter (initially (mop:ensure-finalized class))
    ;;        (for slot in (mop:class-slots class))
    ;;        (format t "##### ~(~A~)~%~%" (or (mop:slot-definition-name slot) ""))
    ;;        ;; Works in SBCL. Any portable way?
    ;;        (write-string (or (documentation slot t) ""))
    ;;        (terpri)
    ;;        (terpri)))
    (write-string (class-documentation class))
    ))

(defun md-file (package-keyword)
  (with-open-file (f (string-downcase (format nil "~A.md" package-keyword))
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string 
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
                (write-string (or (documentation (find-package package-keyword) t) ""))
                (terpri)
                (terpri))
               (for symbol in sorted-symbols)
               (unless (ignore-errors (typep (fdefinition symbol)
                                             (find-class 'standard-generic-function)))
                 (format t "#### ~(~a~)~%" (let ((sym-name (symbol-name symbol)))
                                             (if (str:containsp "*" sym-name)
                                                 (str:replace-all "*" "\\\*" sym-name)
                                               sym-name)))
                 (iter (for slot in '(function
                                      variable
                                      type))
                       (for docstring = (documentation symbol slot))
                       (write-documentation slot symbol docstring))))
         (terpri)))
     f))
  t)
