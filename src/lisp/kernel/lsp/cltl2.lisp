(defpackage #:clasp-cltl2
  (:use #:cl)
  (:documentation "Implementation of CLTL2's environment access functions in section 8.5.")
  (:local-nicknames (#:env #:cleavir-env))
  (:export #:variable-information #:function-information
           #:declaration-information)
  (:export #:augment-environment)
  (:export #:define-declaration)
  (:import-from #:ext #:parse-macro #:parse-compiler-macro)
  (:export #:parse-macro #:parse-compiler-macro)
  (:export #:enclose))

(in-package #:clasp-cltl2)

(defun variable-information (variable &optional env)
  "Retrieve information about the interpretation of a symbolic variable in an environment. See CLTL2 8.5 for more detailed information.
Known bug: Clasp's implementation may not determine whether symbol macros are local correctly."
  (let ((info (env:variable-info clasp-cleavir:*clasp-system*
                                 env variable)))
    (etypecase info
      (null (values nil nil nil))
      (env:constant-variable-info (values :constant nil nil))
      (env:symbol-macro-info
       ;; KLUDGE/FIXME: Cleavir does not report whether a symbol macro binding
       ;; is local or global, so we have to hack it. This is not completely
       ;; correct, as if a binding shadows another global macro binding,
       ;; we'll just report the global.
       (values :symbol-macro (not (nth-value 1 (macroexpand-1 variable env)))
               `((type . ,(env:type info)))))
      (env:special-variable-info
       (values :special
               (not (env:global-p info))
               ;; CLTL2 says the ignore value is T "if the variable has been
               ;; declared ignore", so I guess ignorable isn't reported.
               ;; Cleavir doesn't record dynamic-extent of specials.
               `((ignore . ,(eq (env:ignore info) 'ignore))
                 (type . ,(env:type info)))))
      (env:lexical-variable-info
       (values :lexical t
               `((ignore . ,(eq (env:ignore info) 'ignore))
                 (type . ,(env:type info))
                 (dynamic-extent . ,(env:dynamic-extent info))))))))

(defun function-information (function-name &optional env)
  "Retrieve information about the function name in an environment. See CLTL2 8.5 for more information.
Clasp reports IGNORE declarations on local functions analogously to variables."
  (let ((info (env:function-info clasp-cleavir:*clasp-system*
                                 env function-name)))
    (etypecase info
      (null (values nil nil nil))
      (env:special-operator-info (values :special-form nil nil))
      (env:global-macro-info
       (values :macro nil `((inline . ,(env:inline info)))))
      (env:local-macro-info
       (values :macro t `((inline . ,(env:inline info)))))
      (env:global-function-info
       (values :function nil
               `((inline . ,(env:inline info))
                 (ftype . ,(env:type info)))))
      (env:local-function-info
       (values :function t
               `((inline . ,(env:inline info))
                 (ftype . ,(env:type info))
                 (dynamic-extent . ,(env:dynamic-extent info))
                 ;; CLTL2 doesn't mandate ignore declarations are reported,
                 ;; presumably since in CLTL2 they could not apply to function
                 ;; bindings. But it seems like the nice thing to do.
                 (ignore . ,(eq (env:ignore info) 'ignore))))))))

(defun declaration-information (decl-name &optional env)
  "Retrieve information about a declaration in an environment. See CLTL2 8.5 for more information. Only the OPTIMIZE and DECLARATION declarations are supported."
  (ecase decl-name
    ((optimize) (env:optimize (env:optimize-info clasp-cleavir:*clasp-system* env)))
    ((declaration) (env:declarations clasp-cleavir:*clasp-system* env))))

(defun augment-environment-with-blocks (env blocknames)
  (loop for blockname in blocknames
        do (setf env (env:add-block env blockname)))
  env)

(defun augment-environment-with-tags (env tagnames)
  (loop for tagname in tagnames
        do (setf env (env:add-tag env tagname))))

(defun augment-environment-with-macros (env macrodefs)
  (loop for (name expander) in macrodefs
        do (setf env (env:add-local-macro env name expander)))
  env)

(defun augment-environment-with-symbol-macros (env smdefs)
  (loop for (name expansion) in smdefs
        ;; Make sure we're not doing an illegal shadow.
        for info = (env:variable-info clasp-cleavir:*clasp-system*
                                      env name)
        do (typecase info
             (env:constant-variable-info
              (cerror "Bind it anyway."
                      'cleavir-cst-to-ast::symbol-macro-names-constant
                      :cst (cst:cst-from-expression name)))
             (env:special-variable-info
              (when (env:global-p info)
                (cerror "Bind it anyway."
                        'cleavir-cst-to-ast::symbol-macro-names-global-special
                        :cst (cst:cst-from-expression name)))))
           (setf env (env:add-local-symbol-macro env name expansion)))
  env)

(defun augment-environment-with-functions (env fnames)
  (loop for fname in fnames
        do (setf env (env:add-local-function env fname)))
  env)

;; e.g. turns ((dynamic-extent x) (integer y z)) into
;; ((dynamic-extent x) (type integer y) (type integer z)).
;; ignore-decls are ones defined with proclaim declaration.
;; NOTE: To support define-declaration, a different design would be required.
(defun canonicalize-declaration-specifier (ignore-decls dspec)
  (destructuring-bind (identifier . data) dspec
    (case identifier
      ;; See CLHS Fig. 3-24 (under "Symbol DECLARE") for a complete list of
      ;; standard local declaration specifiers.
      ((dynamic-extent ignore inline ignorable notinline special)
       (loop for datum in data collect (list identifier datum)))
      ((type ftype)
       (destructuring-bind (type . names) data
         (loop for name in names collect (list identifier type name))))
      ((optimize) (list dspec))
      (otherwise
       (if (member identifier ignore-decls :test #'eq)
           (list dspec)
           ;; abbreviated type declaration
           (loop for name in data collect (list 'type identifier name)))))))

(defun canonicalize-declarations (ignore-decls declarations)
  (loop for declaration in declarations
        nconcing (canonicalize-declaration-specifier
                  ignore-decls declaration)))

;; Given a list of varnames and a list of canonicalized declaration specifiers,
;; return two values: A list of lists of declaration specifiers, and a list of
;; declaration specifiers. The first list has one entry for each variable name
;; and is a list of declaration specifiers pertaining to that variable.
;; The second list is the remaining declaration specifiers.
(defun itemize-declaration-specifiers (varnames canonical-dspecs)
  (loop with itemized = (make-list (length varnames))
        with other = nil
        for dspec in canonical-dspecs
        do (case (first dspec)
             ((dynamic-extent ignore ignorable special)
              (let ((pos (position (second dspec) varnames)))
                (if pos
                    (push dspec (nth pos itemized))
                    (push dspec other))))
             ((type)
              (let ((pos (position (third dspec) varnames)))
                (if pos
                    (push dspec (nth pos itemized))
                    (push dspec other))))
             (otherwise (push dspec other)))
        finally (return (values itemized other))))

;; Returns two values: if it's special, and if it's globally special.
(defun variable-is-special-p (env varname idspecs)
  (let* ((existing-info (env:variable-info clasp-cleavir:*clasp-system*
                                           env varname))
         (special-var-p
           (typep existing-info 'env:special-variable-info)))
    (cond ((member 'special idspecs :key #'car)
           (values t (and special-var-p (env:global-p existing-info))))
          ((and special-var-p (env:global-p existing-info))
           (values t t))
          (t (values nil nil)))))

(defun augment-environment-with-one-variable (env varname idspecs)
  (multiple-value-bind (specialp globalp)
      (variable-is-special-p env varname idspecs)
    (cond (globalp) ; no need to "rebind"
          (specialp
           (setf env (env:add-special-variable env varname)))
          (t
           (setf env (env:add-lexical-variable env varname)))))
  ;; Now run through the declarations.
  (loop for (id maybe-type) in idspecs
        do (ecase id
             ((dynamic-extent)
              (setf env (env:add-variable-dynamic-extent env varname)))
             ((ignore ignorable)
              (setf env (env:add-variable-ignore env varname id)))
             ((special)) ; already handled above
             ((type)
              (setf env
                    (env:add-variable-type env varname maybe-type)))))
  env)

(defun function-form-p (form)
  (and (consp form) (consp (cdr form)) (null (cddr form))
       (eq (car form) 'function)
       (symbolp (second form))))

(defun augment-environment-with-one-dspec (env dspec)
  ;; dspec is a free declaration.
  (destructuring-bind (id . data) dspec
    (case id
      ((dynamic-extent)
       (loop for thing in data
             do (setf env
                      (cond ((symbolp thing)
                             (env:add-variable-dynamic-extent
                              env thing))
                            ((function-form-p thing)
                             (env:add-function-dynamic-extent
                              env (second thing)))
                            (t (error "Malformed declaration ~a" dspec))))))
      ((type)
       (destructuring-bind (type . names) data
         (loop for name in names
               do (setf env (env:add-variable-type env name type)))))
      ((ftype)
       (destructuring-bind (type . names) data
         (loop for name in names
               do (setf env (env:add-function-type env name type)))))
      ((ignore ignorable)
       (loop for thing in data
             do (setf env
                      (cond ((symbolp thing)
                             (env:add-variable-ignore env thing id))
                            ((function-form-p thing)
                             (env:add-function-ignore
                              env (second thing) id))
                            (t (error "Malformed declaration ~a" dspec))))))
      ((inline notinline)
       (loop for name in data
             do (setf env (env:add-inline env name id))))
      ((special)
       (loop for name in data
             do (unless (typep (env:variable-info clasp-cleavir:*clasp-system*
                                                  env name)
                               'env:special-variable-info)
                  (setf env (env:add-special-variable env name)))))
      ((optimize)
       (setf env (cleavir-cst-to-ast::augment-environment-with-optimize
                  data env clasp-cleavir:*clasp-system*))))))

(defun augment-environment-with-variables-and-decls
    (env variables declarations)
  (multiple-value-bind (var-dspecs other-dspecs)
      (itemize-declaration-specifiers variables
                                      (canonicalize-declarations
                                       (env:declarations clasp-cleavir:*clasp-system* env)
                                       declarations))
    (loop for var in variables for idspecs in var-dspecs
          do (setf env
                   (augment-environment-with-one-variable env var idspecs)))
    (loop for dspec in other-dspecs
          do (setf env (augment-environment-with-one-dspec env dspec))))
  env)

;;; TAG and BLOCK are provided as extensions to CLTL2.
(defun augment-environment (env &key variable symbol-macro function macro
                                  declare tag block)
  "Given an environment, return environment augmented with the given information. The result is a syntactic environment only, e.g. does not include variable bindings etc. See CLTL2 8.5 for more information.
As an extension, the TAG and BLOCK keyword may be used to provide lists of tag or block names, respectively, to include in the new environment."
  (augment-environment-with-variables-and-decls
   (augment-environment-with-symbol-macros
    (augment-environment-with-functions
     (augment-environment-with-macros
      (augment-environment-with-tags
       (augment-environment-with-blocks env block)
       tag)
      macro)
     function)
    symbol-macro)
   variable declare))

(defmacro define-declaration (decl-name lambda-list &body body)
  "Define a new type of declaration. See CLTL2 8.5 for more information.
Clasp does not implement this functionality yet."
  (declare (ignore decl-name lambda-list body))
  (error "BUG: ~a not implemented yet, sorry!" 'define-declaration))

(defun enclose (lambda-expression &optional env)
  "Given a lambda expression and a syntactic environment, return a function object. See CLTL2 8.5 for more information.
Clasp will discard any non-syntactic information in the environment."
  (cmp:compile-definition lambda-expression (env:compile-time env)))
