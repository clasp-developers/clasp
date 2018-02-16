(in-package #:compilation-environment)

;;;; What CLHS glossary has as "compilation environment", though we relax the description
;;;; of evaluation environments as having an inheritance relationship. (Instead it's arbitrary.)

;;; Could be in Cleavir or something other than a few parts

(defclass compilation-environment ()
  ((%evaluation-environment :initarg :evaluation-environment :accessor evaluation-environment)
   (%variables :initform (make-hash-table :test #'eq) :accessor variables)
   (%operators :initform (make-hash-table :test #'eq) :accessor operators)
   (%optimize :initform '((compilation-speed 1) (debug 1) (space 1) (speed 1) (safety 1))
              :accessor optimize)
   (%policy :accessor policy)
   (%types :initform (make-hash-table :test #'eq) :accessor types)
   (%declarations :initform nil :accessor declarations)
   (%setfs :initform (make-hash-table :test #'eq) :accessor setfs)))

;;; synchronize the policy with the optimize spec
(defmethod shared-initialize :after ((instance compilation-environment) slot-names &rest initargs)
  (declare (ignore initargs))
  (when (or (eq slot-names t) (member '%optimize slot-names))
    (setf (policy instance) (cleavir-policy:compute-policy (optimize instance) instance))))

(defmethod (setf optimize) :after (new (env compilation-environment))
  (setf (policy env) (cleavir-policy:compute-policy (optimize env) env)))

;;; entry classes
(defclass entry ()
  ((%name :initarg :name :accessor entry-name)))

(defclass variable-entry (entry)
  ((%type :initarg :type :initform 't :accessor variable-type)))
(defclass special-entry (variable-entry) ())
(defclass symbol-macro-entry (variable-entry)
  ((%expansion :initarg :expansion :accessor expansion)))
(defclass constant-entry (variable-entry)
  ((%value :initarg :value :accessor value)))

(defclass operator-entry (entry)
  ;; special operators can't have compiler macros, but there's no harm in giving them the slot.
  ((%compiler-macro :initarg :compiler-macro :initform nil :accessor compiler-macro)))
(defclass special-operator-entry (operator-entry)
  ;; Used for SICL's special operator protocol.
  ((%value :initarg :value :accessor special-operator-entry-value)))
(defclass macro-entry (operator-entry)
  ((%expander :initarg :expander :accessor expander)))
(defclass function-entry (operator-entry)
  ((%type :initarg :type :initform 'function :accessor function-type)
   (%inline :initarg :inline :initform nil :accessor function-inline)
   ;; don't need ignore or dynamic-extent slots, they're always nil globally
   (%ast :initarg :ast :initform nil :accessor function-ast)))

(defclass type-entry (entry) ())
(defclass deftype-entry (type-entry)
  ((%expander :initarg :expander :accessor expander)))
;;; About classes. CLHS is clear that toplevel defclass makes the class available.
;;; The usual way to access that would be find-class, which doesn't matter to the
;;; compilation-environment. But hypothetically the compiler itself can use the class,
;;; e.g. for type check stuff.
;;; And regardless, if there's a class defined it clobbers former deftypes.
(defclass class-entry (type-entry)
  ((%class :initarg :class :accessor type-class)))

(defclass setf-entry (entry)
  ((%expander :initarg :expander :accessor expander)))

;;; entry operations
;;; Though we respond to parts of the sicl-genv protocol, some parts are unimplemented (and unimplementable)
;;; like function-cell. We could define methods that signal errors instead.
(macrolet ((defreader (fname table)
             ;; Returns an entry, or NIL if there isn't one.
             `(defun ,fname (environment name)
                (values (gethash name (,table environment)))))
           (defwriter (fname table)
             `(defun (setf ,fname) (new environment name)
                (setf (gethash name (,table environment)) new)))
           (defaccessor (fname table)
             `(progn (defwriter ,fname ,table) (defreader ,fname ,table)))
           (defremove (fname table)
             `(defun ,fname (environment name)
                (remhash name (,table environment)))))
  (defaccessor variable-entry variables)
  (defaccessor operator-entry operators)
  (defaccessor type-entry types)
  (defaccessor setf-entry setfs)
  (defremove remove-variable-entry variables)
  (defremove remove-operator-entry operators)
  (defremove remove-type-entry types)
  (defremove remove-setf-entry setfs))

(defmethod sicl-genv:special-operator (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'special-operator-entry)
        (special-operator-entry-value entry)
        nil)))
(defmethod (setf sicl-genv:special-operator) (value name (env compilation-environment))
  (if value
      (if (operator-entry env name)
          (error "Tried to define ~a as a special operator when it is already defined" name)
          (setf (operator-entry env name)
                (make-instance 'special-operator-entry :name name :value value)))
      (remove-operator-entry env name)))
(defmethod sicl-genv:macro-function (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'macro-entry)
        (expander entry)
        nil)))
(defmethod (setf sicl-genv:macro-function) (value name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (when entry
      ;; FIXME: style warning? not sure if UB
      (warn "Redefining ~a as macro (was ~a)" name
            (etypecase entry
              (special-operator-entry "special operator")
              (macro-entry "macro")
              (function-entry "function"))))
    (if value
        (setf (operator-entry env name)
              (make-instance 'macro-entry :name name :expander value))
        (remove-operator-entry env name))))
(defmethod sicl-genv:compiler-macro-function (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if entry
        (compiler-macro entry) ; the c macro function, or NIL if none
        nil)))
(defmethod (setf sicl-genv:compiler-macro-function) (new name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry '(or function-entry macro-entry))
        (setf (compiler-macro entry) new)
        (error "Can't define compiler macro for unknown operator ~a" name))))
(defmethod sicl-genv:function-type (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'function-entry)
        (function-type entry)
        nil)))
(defmethod (setf sicl-genv:function-type) (new name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (etypecase entry
      (function-entry
       (unless (eq (function-type entry) 'function) ;; the initform
         ;; FIXME: style warning? not sure if UB
         (warn "Declaring type of ~a as ~a; was previously ~a"
               name new (function-type entry)))
       (setf (function-type entry) new))
      (null
       (setf (operator-entry env name)
             (make-instance 'function-entry :name name :type new))))))
(defmethod sicl-genv:function-inline (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'function-entry)
        (function-inline entry)
        (error "~a's inline value is unknown as it is an unknown operator" name))))
(defmethod (setf sicl-genv:function-inline) (new name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (etypecase entry
      (function-entry (setf (function-inline entry) new))
      ;; another departure from SICL, which says this should err.
      ;; probably incorrectly, since you seem to be able to declare inline on macros.
      (null (setf (operator-entry env name)
                  (make-instance 'function-entry :name name :inline new)))
      (operator-entry))))
;; lambda-list?
(defmethod sicl-genv:function-ast (name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'function-entry)
        (function-ast entry)
        (error "~a has no AST as it is not known as a function" name))))
(defmethod (setf sicl-genv:function-ast) (new name (env compilation-environment))
  (let ((entry (operator-entry env name)))
    (if (typep entry 'function-entry)
        (setf (function-ast entry) new)
        (error "~a's AST cannot be set as it is not known as a function" name))))
;; function-names?
(defmethod sicl-genv:constant-variable (name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (if (typep entry 'constant-entry)
        (values (value entry) t)
        (values nil nil))))
(defmethod (setf sicl-genv:constant-variable) (new name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (etypecase entry
      (null
       (setf (variable-entry env name)
             (make-instance 'constant-entry :name name :value new)))
      (constant-entry
       (if (eql new (value entry))
           (setf (value entry) new) ; not really necessary
           (error "Cannot redefine value ~a of constant ~a to non-EQL value ~a"
                  (value entry) name new)))
      (variable-entry (error "Cannot redefine ~a as a constant" name)))))
;; special-variable is left undefined as we have no value to return.
;; cleavir env accessors should be okay though.
(defmethod (setf sicl-genv:special-variable) (new name (env compilation-environment) initialize-p)
  (declare (ignore new initialize-p))
  (let ((entry (variable-entry env name)))
    (etypecase entry
      (null (setf (variable-entry env name)
                  (make-instance 'special-entry :name name)))
      (special-entry)
      (variable-entry (error "Cannot redefine ~a as special" name)))))
(defmethod sicl-genv:symbol-macro (name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (if (typep entry 'symbol-macro-entry)
        ;; sicl-genv says the function has to be EQ between calls, but i don't care
        (values (let ((expansion (expansion entry)))
                  (lambda (form env) (declare (ignore form env)) expansion))
                t)
        (values nil nil))))
(defmethod (setf sicl-genv:symbol-macro) (new name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (etypecase entry
      (null
       (setf (variable-entry env name)
             (make-instance 'symbol-macro-entry :name name :expansion new)))
      (symbol-macro-entry
       (warn "Redefining symbol macro ~a from ~a to ~a" name (expansion entry) new)
       (setf (expansion entry) new))
      (variable-entry
       (error "Cannot redefine ~a as a symbol macro" name)))))
(defmethod sicl-genv:variable-type (name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (etypecase entry
      (variable-entry
       (variable-type entry))
      (constant-entry
       ;; sicl-genv says to use type-of, but what's the point?
       `(eql ,(value entry)))
      (null 't))))
(defmethod (setf sicl-genv:variable-type) (new name (env compilation-environment))
  (let ((entry (variable-entry env name)))
    (etypecase entry
      (constant-entry
       (error "Cannot proclaim type of constant variable ~a" name))
      (variable-entry
       (unless (eq (variable-type entry) t) ; default
         (warn "Redefining type of ~a from ~a to ~a" name (variable-type entry) new))
       (setf (variable-type entry) new))
      (null ; sicl-genv says proclaiming the type of an unknown variable is allowed,
       ;; but it's pretty inconvenient, so i'm going to see if i can avoid that.
       (error "Cannot proclaim type of unknown variable ~a" name)))))
(defmethod sicl-genv:find-class (name (env compilation-environment))
  (let ((entry (type-entry env name)))
    (if (typep entry 'class-entry)
        (type-class entry)
        nil)))
(defmethod (setf sicl-genv:find-class) (new name (env compilation-environment))
  (let ((entry (type-entry env name)))
    (etypecase entry
      (class-entry
       (warn "Redefining class ~a" name))
      (type-entry
       (warn "Redefining ~a from a type to a class" name))
      (null))
    (if new
        (setf (type-entry env name) (make-instance 'class-entry :name name :class new))
        (remove-type-entry env name))))
(defmethod sicl-genv:setf-expander (name (env compilation-environment))
  (let ((entry (setf-entry env name)))
    (if entry
        (expander entry)
        nil)))
(defmethod (setf sicl-genv:setf-expander) (new name (env compilation-environment))
  (let ((entry (setf-entry env name)))
    ;; sicl-genv errs if the name isn't known as a function etc.
    ;; Also setf expanders can be undefined, but we probably don't need this.
    (when entry
      (warn "Redefining setf expander on ~a" name))
    (setf (setf-entry env name) (make-instance 'setf-entry :name name :expander new))))
(defmethod sicl-genv:default-setf-expander ((env compilation-environment))
  (lambda (place env)
    (declare (ignore env))
    (etypecase place
      (symbol
       (let ((store (gensym)))
         (values nil nil (list store) `(setq ,place ,store) place)))
      (cons
       (let ((temps (loop for arg in (rest place) collect (gensym)))
             (store (gensym)))
         (values temps (rest place) (list store)
                 `(funcall #'(setf ,(first place)) ,store ,@temps)
                 `(,(first place) ,@temps)))))))
(defmethod sicl-genv:type-expander (name (env compilation-environment))
  (let ((entry (type-entry env name)))
    (if (typep entry 'deftype-entry)
        (expander entry)
        nil)))
(defmethod (setf sicl-genv:type-expander) (new name (env compilation-environment))
  (let ((entry (type-entry env name)))
    (etypecase entry
      (deftype-entry
       (warn "Redefining type ~a" name))
      (class-entry
       (warn "Redefining class ~a as type" name))
      (null))
    ;; We could account for undefining types.
    (setf (type-entry env name) (make-instance 'deftype-entry :name name :expander new))))
;;; TODO: packages maybe
(defmethod sicl-genv:declaration (name (env compilation-environment))
  (member name (declarations env)))
(defmethod (setf sicl-genv:declaration) (value name (env compilation-environment))
  (if value
      (pushnew name (declarations env))
      (setf (declarations env) (remove name (declarations env)))))
(defmethod sicl-genv:declarations ((env compilation-environment)) (declarations env))
(defmethod sicl-genv:optimize-quality-values ((env compilation-environment)) (optimize env))
(defmethod (setf sicl-genv:optimize-quality-values) (new (env compilation-environment))
  ;; normalization ought to be done by the caller.
  (setf (optimize env) new))
(defmethod sicl-genv:policy ((env compilation-environment)) (policy env))

;;; cleavir methods
(defmethod cleavir-env:eval (form env (dispatch compilation-environment))
  (let ((e (evaluation-environment dispatch)))
    (cleavir-env:eval form (retarget env e) e)))

(defmethod cleavir-env:variable-info ((environment compilation-environment) symbol)
  ;; FIXME: If the compilation environment has its own packages as well,
  ;; this find-package needs to be altered.
  (when (eq (symbol-package symbol) (load-time-value (find-package "KEYWORD")))
    (return-from cleavir-env:variable-info
      (make-instance 'cleavir-env:constant-variable-info
                     :name symbol :value symbol)))
  (let ((entry (variable-entry environment symbol)))
    (etypecase entry
      (special-entry
       (make-instance 'cleavir-env:special-variable-info
                      :name symbol
                      :global-p t))
      (symbol-macro-entry
       (make-instance 'cleavir-env:symbol-macro-info
                      :name symbol
                      :expansion (expansion entry)))
      (constant-entry
       (make-instance 'cleavir-env:constant-variable-info
                      :name symbol
                      :value (value entry)))
      (null nil))))

(defmethod cleavir-env:function-info ((environment compilation-environment) name)
  (let ((entry (operator-entry environment name)))
    (etypecase entry
      (special-operator-entry
       (make-instance 'cleavir-env:special-operator-info :name name))
      (macro-entry
       (make-instance 'cleavir-env:global-macro-info
                      :name name
                      :expander (expander entry)
                      :compiler-macro (compiler-macro entry)))
      (function-entry
       (make-instance 'cleavir-env:global-function-info
                      :name name
                      :compiler-macro (compiler-macro entry)
                      :type (function-type entry)
                      :inline (function-inline entry)
                      :ast (function-ast entry)))
      (null nil))))

(defmethod cleavir-env:declarations ((environment compilation-environment))
  (declarations environment))

(defmethod cleavir-env:optimize-info ((environment compilation-environment))
  (make-instance 'cleavir-env:optimize-info
                 :optimize (optimize environment)
                 :policy (policy environment)))

(defmethod cleavir-env:macro-function (symbol (environment compilation-environment))
  (let ((entry (operator-entry environment symbol)))
    (if (typep entry 'macro-entry)
        (expander entry)
        nil)))

(defmethod cleavir-env:symbol-macro-expansion (symbol (environment compilation-environment))
  (let ((entry (variable-entry environment symbol)))
    (if (typep entry 'symbol-macro-entry)
        (expansion entry)
        symbol)))

(defun type-expand-1 (compilation-environment type-specifier)
  (etypecase type-specifier
    (symbol (let ((entry (type-entry compilation-environment type-specifier)))
              (typecase entry
                (deftype-entry
                 (values (funcall (expander entry) type-specifier compilation-environment) t))
                (class-entry
                 (values (type-class entry) t))
                (t (values type-specifier nil)))))
    (cons (let ((entry (type-entry compilation-environment (car type-specifier))))
            (typecase entry
              (deftype-entry
               (values (funcall (expander entry) type-specifier compilation-environment) t))
              ;; a class-entry would be invalid. signal an error?
              (t
               (values type-specifier nil)))))
    (class (values type-specifier nil))))

(defmethod cleavir-env:type-expand ((environment compilation-environment) type-specifier)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expanded)
               (type-expand-1 environment type-specifier)
             (if expanded
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded))))))

;;; Any setf macro defined in this environment has to use its setf expansions.
;;; So this does what sicl-genv:setf-expander does for ease's sake.
(declaim (inline get-setf-expander)) ; save definition
(defun get-setf-expander (symbol environment)
  (let ((entry (setf-entry environment symbol)))
    (if entry
        (expander entry)
        nil)))
(declaim (notinline get-setf-expander))
