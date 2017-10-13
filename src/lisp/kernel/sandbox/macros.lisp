(defmacro defun (name lambda-list &body body)
  `(progn
     (setf (fdefinition ',name)
           (lambda ,lambda-list ,@body))
     ',name))

(defmacro define-symbol-macro (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:symbol-macro ',symbol) ',expansion)
     ',symbol))

(defmacro defparameter (name initial-value &optional documentation)
  `(progn
     (declaim (special ,name))
     (setf (symbol-value ',name) ,initial-value)
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

(defmacro defvar (name &optional (initial-value nil ivp) documentation)
  `(progn
     (declaim (special ,name))
     ,@(when ivp
         `((unless (boundp ',name)
             (setf (symbol-value ',name) ,initial-value))))
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

(defmacro define-compiler-macro (name lambda-list &body body)
  (multiple-value-bind (function pprint docstring)
      (core::expand-defmacro name lambda-list body 'define-compiler-macro)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name)
             #',function)
       ',name)))

(defmacro defconstant (name value &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:constant-variable ',name) ,value)
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))
