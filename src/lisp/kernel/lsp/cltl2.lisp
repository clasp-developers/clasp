(defpackage #:clasp-cltl2
  (:use #:cl)
  (:documentation "Implementation of CLTL2's environment access functions in section 8.5.")
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
  (let ((info (cmp:var-info variable env)))
    (etypecase info
      (null (values nil nil nil))
      (cmp:constant-var-info (values :constant nil nil))
      (cmp:symbol-macro-var-info
       (values :symbol-macro
               ;; KLUDGE/FIXME: globalness is not part of the info.
               (and (typep env 'cmp:lexenv)
                 (let ((g (cmp:var-info variable (cmp:lexenv/global env))))
                   (or (not (typep g 'cmp:symbol-macro-var-info))
                     (not (eq (cmp:symbol-macro-var-info/expander g)
                              (cmp:symbol-macro-var-info/expander info))))))
               nil)) ; FIXME
      (cmp:special-var-info
       (values :special (not (cmp:special-var-info/globalp info))
               nil)) ; FIXME
      (cmp:lexical-var-info
       ;; CLTL2 says the ignore value is T "if the variable has been
       ;; declared ignore", so I guess ignorable isn't reported.
       (values :lexical t nil)))))

(defun function-information (function-name &optional env)
  "Retrieve information about the function name in an environment. See CLTL2 8.5 for more information.
Clasp reports IGNORE declarations on local functions analogously to variables."
  (let ((info (cmp:fun-info function-name env)))
    (etypecase info
      (null (values nil nil nil))
      (cmp:special-operator-info (values :special-form nil nil))
      (cmp:global-macro-info
       (values :macro nil `((inline . ,(cmp:fun-info/inline-status info)))))
      (cmp:local-macro-info
       (values :macro t `((inline . ,(cmp:fun-info/inline-status info)))))
      (cmp:global-fun-info
       (values :function nil
               `((inline . ,(cmp:fun-info/inline-status info)))))
      (cmp:local-fun-info
       (values :function t
               `((inline . ,(cmp:fun-info/inline-status info))))))))

(defun optimize-information (env)
  (typecase env
    (null cmp:*optimize*)
    (cmp:lexenv
     (ncond (loop for (id . data) in (cmp:lexenv/decls env)
                  when (eq id 'optimize)
                    ;; FIXME: error prone
                    nconc (loop for thing in data
                                collect (if (symbolp thing)
                                            `(,thing 3)
                                            thing)))
            (optimize-information (cmp:lexenv/global env))))
    (t (error "BUG: OPTIMIZE information not implemented for FCGEs! Sorry!"))))

(defun declaration-information (decl-name &optional env)
  "Retrieve information about a declaration in an environment. See CLTL2 8.5 for more information. Only the OPTIMIZE and DECLARATION declarations are supported."
  (ecase decl-name
    ((optimize) (optimize-information env))
    ((declaration)
     (if (null env)
         '(core:lambda-name core:lambda-list) ; FIXME: track
         (error "BUG: DECLARATION information not implemented for FCGEs! Sorry!")))))

(defun %augment-environment (env &key vars tags blocks funs)
  (if (typep env 'cmp:lexenv)
      (cmp:lexenv/make (append vars (cmp:lexenv/vars env))
                       (append tags (cmp:lexenv/tags env))
                       (append blocks (cmp:lexenv/blocks env))
                       (append funs (cmp:lexenv/funs env))
                       (cmp:lexenv/decls env)
                       (cmp:lexenv/frame-end env) (cmp:lexenv/global env))
      (cmp:lexenv/make vars tags blocks funs nil 0 env)))

(defun variable-augmentation (var env declares)
  (loop with globally-special-p = (let ((info (cmp:var-info var env)))
                                    (and (typep info 'cmp:special-var-info)
                                      (cmp:special-var-info/globalp info)))
        with specialp = globally-special-p
        for declspec in declares
        if (and (not specialp) ; skip if we figured it out already
             (consp declspec) (eq (car declspec) 'special)
             (loop for (id) on (cdr declspec) ; ignore improper list crap
                     thereis (eq id var)))
          do (setf specialp t)
             ;; look for ignore etc here
        finally (return
                  (cons var
                        (if specialp
                            (cmp:special-var-info/make globally-special-p)
                            (cmp:lexical-var-info/make 0 nil))))))

(defun symbol-macro-augmentation (v env declares)
  (declare (ignore env declares)) ; FIXME
  (destructuring-bind (name expansion) v
    (cons name (cmp:symbol-macro-var-info/make (lambda (form env)
                                                 (declare (ignore form env))
                                                 expansion)))))

(defun declared-inline-status (name declares)
  (loop with inline = nil
        for declspec in declares
        if (and (not inline) ; skip if we figured it out already
             (consp declspec) (eq (car declspec) 'inline)
             (loop for (id) on (cdr declspec) ; ignore improper list crap
                     thereis (eq id name)))
          do (setf inline 'cl:inline)
        else if (and (not inline)
                  (consp declspec) (eq (car declspec) 'notinline)
                  (loop for (id) on (cdr declspec)
                          thereis (eq id name)))
               do (setf inline 'cl:notinline)
             ;; look for ignore etc here
        finally (return inline)))

(defun function-augmentation (name env declares)
  (declare (ignore env))
  (cons name (cmp:local-fun-info/make (declared-inline-status name declares)
                                      0 nil)))

(defun macro-augmentation (f env declares)
  (declare (ignore env))
  (destructuring-bind (name expander) f
    (cons name (cmp:local-macro-info/make (declared-inline-status name declares)
                                          expander))))

(defun function-form-p (form)
  (and (consp form) (consp (cdr form)) (null (cddr form))
       (eq (car form) 'function)
       (symbolp (second form))))

;;; TAG and BLOCK are provided as extensions to CLTL2.
(defun augment-environment (env &key variable symbol-macro function macro
                                  declare tag block)
  "Given an environment, return environment augmented with the given information. The result is a syntactic environment only, e.g. does not include variable bindings etc. See CLTL2 8.5 for more information.
As an extension, the TAG and BLOCK keyword may be used to provide lists of tag or block names, respectively, to include in the new environment."
  ;; except tags and blocks are ignored right now. FIXME
  (declare (ignore tag block))
  (%augment-environment
   env
   :vars (append
          (mapcar (lambda (v) (variable-augmentation v env declare)) variable)
          (mapcar (lambda (v) (symbol-macro-augmentation v env declare))
                  symbol-macro))
   :funs (append
          (mapcar (lambda (f) (function-augmentation f env declare)) function)
          (mapcar (lambda (f) (macro-augmentation f env declare)) macro))))

(defmacro define-declaration (decl-name lambda-list &body body)
  "Define a new type of declaration. See CLTL2 8.5 for more information.
Clasp does not implement this functionality yet."
  (declare (ignore decl-name lambda-list body))
  (error "BUG: ~a not implemented yet, sorry!" 'define-declaration))

(defun enclose (lambda-expression &optional env)
  "Given a lambda expression and a syntactic environment, return a function object. See CLTL2 8.5 for more information.
Clasp will discard any non-syntactic information in the environment."
  (cmp:compile-definition
   lambda-expression
   (if (typep env 'cmp:lexenv)
       (cmp:lexenv/macroexpansion-environment env)
       env)))
