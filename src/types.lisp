(in-package :cl-rtd)

(defun split-and-clean-slots (slots-doc)
  (-<> (str:replace-all "    " "4SPACES" slots-doc)
    (str:split "  " <> :omit-nulls t)
    (mapcar (lm - (str:replace-all "4SPACES" "    " -))
            <>)
    (remove-if (lm - (str:starts-with-p (conc *package-name* "::") -))
               <>)))

(defun class-documentation (class)
  ;; Only slots are documented currently.
  (flet
      ((quote-slots (slots)
         (conc "<u>**Direct Slots**</u>" #\newline #\newline
               (with-output-to-string (s)
                 (ppcre:do-register-groups (slot-name initarg reader writer)
                     (*slot-splitting-regex*
                      (ppcre:regex-replace-all (conc "(?-i)" *package-keyword* #\:)
                                               slots
                                               ""))
                   (when (and slot-name (string/= "" slot-name))
                     (write-string (conc "**" (string-downcase slot-name) "**"
                                         #\newline
                                         "```lisp" #\newline
                                         (or initarg "")
                                         (or reader "")
                                         (or writer "")
                                         "```" #\newline)
                                   s)))))))
    (let ((full-doc (with-output-to-string (*standard-output*) (describe class))))
      (if-let
          (return-value
           (ppcre:register-groups-bind (prologue direct-slots end)
               (`(:sequence (:flags :single-line-mode-p)
                            (:register (:sequence (:greedy-repetition 0 nil :everything)
                                                  "Direct slots:" #\newline))
                            (:register (:sequence (:non-greedy-repetition
                                                   0 nil :everything)
                                                  #\newline))
                            (:register (:sequence #\newline
                                                  (:greedy-repetition 0 nil :everything))))
                 full-doc)
             (declare (ignore prologue end))
             (quote-slots direct-slots)))
        return-value
        ""))))

(defun slot-splitting-regex ()
  `(:sequence
    (:flags :single-line-mode-p)
    (:register (:greedy-repetition 0 nil
                                   :non-whitespace-char-class))
    (:greedy-repetition
     0 1 (:sequence #\newline "    "
                    (:register (:sequence "Initargs: "
                                          (:greedy-repetition
                                           0 nil
                                           :non-whitespace-char-class)
                                          #\newline))))
    (:greedy-repetition
     0 1 (:sequence "    "
                    (:register (:sequence "Readers: "
                                          (:greedy-repetition
                                           0 nil
                                           :non-whitespace-char-class)
                                          #\newline))))
    (:greedy-repetition
     0 1 (:sequence "    "
                    (:register (:sequence
                                "Writers: "
                                (:greedy-repetition
                                 0 nil
                                 (:inverted-char-class #\newline))
                                #\newline))))
    (:greedy-repetition
     0 1 (:sequence "    "
                    (:register (:sequence
                                "Documentation:"
                                (:greedy-repetition
                                 0 nil
                                 :everything)
                                #\newline))))
    (:alternation :void (:sequence "  " ,*package-name*))))

(defun format-slot-documentation (slot-doc-list)
  (if-let (processed-doc-list
           (mapcar (lambda (slot-doc)
                     ;; (write-string slot-doc)
                     (with-output-to-string (s)
                       (ppcre:register-groups-bind (slot-name initarg reader writer)
                           ((slot-splitting-regex)
                            (ppcre:regex-replace-all (conc "(?-i)" *package-name* #\:)
                                                     slot-doc
                                                     ""))
                         (when (and slot-name (string/= "" slot-name))
                           (write-string (conc "**" (string-downcase slot-name) "**"
                                               #\newline
                                               "```lisp" #\newline
                                               (or initarg "")
                                               (or reader "")
                                               (or writer "")
                                               "```" #\newline)
                                         s)))))
                   slot-doc-list))
    (apply 'conc "<u>**Direct Slots**</u>" #\newline #\newline processed-doc-list)
    ""))

(defun direct-slots-documentation (class)
  (declare (type class class))
  (let ((full-doc (with-output-to-string (*standard-output*) (describe class))))
    (ppcre:register-groups-bind (prologue direct-slots end)
        (`(:sequence (:flags :single-line-mode-p)
                     (:register (:sequence (:greedy-repetition 0 nil :everything)
                                           "Direct slots:" #\newline))
                     (:register (:sequence (:non-greedy-repetition
                                            0 nil :everything)
                                           #\newline))
                     (:register (:sequence #\newline
                                           (:greedy-repetition 0 nil :everything))))
          full-doc)
      (declare (ignore prologue end))
      direct-slots)))

(defmethod format-documentation ((slot (eql 'type)) symbol
                                 &optional (docstring (documentation symbol slot)))
  (let ((class (ignore-errors (find-class symbol))))
    (when (trivial-types:type-specifier-p symbol)
      (funcall 'conc
               (format nil "~%```lisp~%~A~%```~%~%"
                       (cond ((null class)
                              "Type")
                             ((typep class (find-class 'structure-class))
                              "Structure")
                             ((typep class (find-class 'standard-class))
                              "Class")
                             ((subtypep class (find-class 'condition))
                              "Condition")
                             ((typep class
                                     (find-class 'closer-mop:funcallable-standard-class))
                              "Function")
                             #+sbcl
                             ((typep class (find-class 'sb-pcl:system-class))
                              "System Class (SBCL)")
                             (t (error "Non-exhaustive cases: ~D" class))))
               (when docstring
                 (conc (requote-with-backquote docstring)
                       #\newline
                       #\newline))
               (when class
                 (-> class
                   direct-slots-documentation
                   split-and-clean-slots
                   format-slot-documentation
                   (hyperlink-samedoc-symbols symbol)))))))
