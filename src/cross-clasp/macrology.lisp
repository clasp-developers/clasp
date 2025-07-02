(in-package #:cross-clasp.clasp.core)

(defmacro with-unique-names (symbols &body body)
  `(let* ,(mapcar (lambda (symbol)
                    (let* ((symbol-name (symbol-name symbol))
                           (stem symbol-name))
                      `(,symbol (gensym ,stem))))
                  symbols)
     ,@body))

(defmacro with-clean-symbols (symbols &body body)
  "Rewrites the given forms replacing the given symbols with uninterned
ones, which is useful for creating hygienic macros."
  `(progn ,@(sublis (mapcar #'(lambda (s) (cons s (make-symbol (symbol-name s))))
			    symbols)
		    body)))

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*

Create a Let* which evaluates each Value-Expression, binding a
temporary variable to the result, and wrapping the Let* around the
result of the evaluation of Body.  Within the body, each Var is bound
to the corresponding temporary variable.

Bare symbols in `specs' are equivalent to:

  (symbol symbol)

Example:

  (defmacro cons1 (x)
    (once-only (x) `(cons ,x ,x)))
  (let ((y 0))
    (cons1 (incf y)))
  ; => (1 . 1)
"
  (labels ((frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   (cond ((atom spec)
                          (setf spec (list spec spec)))
                         ((/= (length spec) 2)
                          (error "Malformed Once-Only binding spec: ~S." spec)))
                   (let ((name (first spec))
                         (exp-temp (gensym)))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "OO-")))
                        (list 'let (list (list ,name ,exp-temp))
                              ,(frob (rest specs) body))
                        #+(or) ; can't use host quasiquote
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))
    (frob specs body)))

(defmacro defconstant-eqx (name form test &rest rest)
  `(defconstant ,name
     (let ((value ,form))
       (cond ((not (boundp ',name)) value)
             ((,test (symbol-value ',name) value)
              (symbol-value ',name))
             (t value))) ; probably error
     ,@rest))

(defmacro defconstant-equal (name form &rest rest)
  `(defconstant-eqx ,name ,form equal ,@rest))

(defun process-declarations (body &optional docstringp)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation docstringp)
    (let* ((decls (mapcan (lambda (L) (copy-list (cdr L))) decls))
           (specials
             (loop for thing in decls
                   when (and (consp thing)
                             (eq (car thing) 'special))
                     append (cdr thing))))
      (values decls body doc specials))))

(defun find-declarations (body &optional (docp t))
  (multiple-value-bind (decls body doc)
      (process-declarations body docp)
    (values (if decls `((declare ,@decls)) nil)
           body doc)))

(defun dm-too-many-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-many))

(defun dm-too-few-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-few))

(defun function-block-name (fname)
  (etypecase fname
    (symbol fname)
    ((cons (eql setf) (cons symbol null)) (second fname))))

;;; So that parsed macros/whatever can be used in the host w/o complaint.
(declaim (declaration lambda-name lambda-list))

(defun cross-clasp.clasp.ext:parse-macro (name lambda-list body &optional env)
  (ecclesia:parse-macro-using-canonicalization name lambda-list body env
                                               `((lambda-name (macro-function ,name))
                                                 (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-compiler-macro
    (name lambda-list body &optional env)
  (ecclesia:parse-compiler-macro-using-canonicalization
   name lambda-list body env
   `((lambda-name (compiler-macro-function ,name))
     (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-deftype (name lambda-list body &optional env)
  (ecclesia:parse-deftype name lambda-list body env
                          `((lambda-name (cross-clasp.clasp.ext:type-expander ,name))
                            (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-define-setf-expander
    (name lambda-list body &optional env)
  (ecclesia:parse-macro name lambda-list body env
                        `((lambda-name (cross-clasp.clasp.ext:setf-expander ,name))
                          (lambda-list ,@lambda-list))))

(defmacro while (condition &body body) `(loop while ,condition do (progn ,@body)))
(defmacro until (condition &body body) `(loop until ,condition do (progn ,@body)))

(defmacro %defun (&whole whole name lambda-list &body body)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation t :whole whole)
    `(progn
       (eval-when (:compile-toplevel)
         (cross-clasp.clasp.cmp::register-global-function-def 'defun ',name))
       (setf (fdefinition ',name)
             (lambda ,lambda-list
               (declare (lambda-name ,name))
               ,@decls
               ,@(when doc (list doc))
               (block ,(function-block-name name) ,@body)))
       ',name)))

;;; We avoid clobbering build macros while building Clasp itself, because that
;;; makes it easier to deal with bootstrapping weirdness.
;;; But when we build libraries we need the full Clasp macros.
;;; So, during build we store macro functions in this variable, and then load
;;; them once Clasp is complete enough.
(defvar *delayed-macros* (make-hash-table))

(defun delay-macro (name expander)
  (setf (gethash name *delayed-macros*) expander))

(defun reset-delayed-macros () (clrhash *delayed-macros*))

(defmacro %defmacro (name lambda-list &body body &environment env)
  (let ((lexpr (cross-clasp.clasp.ext:parse-macro name lambda-list body env)))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((expander #',lexpr))
           (if (macro-function ',name)
               (delay-macro ',name expander)
               (setf (macro-function ',name) expander))))
       (eval-when (:load-toplevel :execute)
         (setf (macro-function ',name) #',lexpr))
       ',name)))

(defmacro %define-compiler-macro (name lambda-list &body body &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compiler-macro-function ',name)
           #',(cross-clasp.clasp.ext:parse-compiler-macro name lambda-list body env))
     ',name))

(defmacro %deftype (name lambda-list &body body &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (cross-clasp.clasp.ext:type-expander ',name)
           #',(cross-clasp.clasp.ext:parse-deftype name lambda-list body env))
     ',name))

(defmacro %define-setf-expander (name lambda-list &body body &environment env)
  (let ((lexpr (cross-clasp.clasp.ext:parse-define-setf-expander
                name lambda-list body env)))
    `(progn
       (eval-when (:compile-toplevel)
         (unless (cross-clasp.clasp.ext:setf-expander ',name)
           (setf (cross-clasp.clasp.ext:setf-expander ',name) #',lexpr)))
       (eval-when (:load-toplevel :execute)
         (setf (cross-clasp.clasp.ext:setf-expander ',name) #',lexpr))
       ',name)))

(defmacro %defvar (name &optional (value nil valuep) doc)
  `(progn
     (declaim (special ,name))
     ,@(when valuep
         `((unless (boundp ',name)
             (setf (symbol-value ',name) ,value))))
     ,@(when doc
         `((cross-clasp.clasp.ext:annotate ',name 'documentation 'variable ,doc)))
     ',name))

(defmacro %defparameter (name value &optional doc)
  `(progn
     (declaim (special ,name))
     (setf (symbol-value ',name) ,value)
     ,@(when doc
         `((cross-clasp.clasp.ext:annotate ',name 'documentation 'variable ,doc)))
     ',name))
