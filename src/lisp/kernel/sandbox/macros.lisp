(defmacro defun (name lambda-list &body body)
  `(progn
     (setf (fdefinition ',name)
           (lambda ,lambda-list
             (declare (core:lambda-name ,name))
             ,@body))
     ',name))

#+(or)
(defmacro define-symbol-macro (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:symbol-macro ',symbol) ',expansion)
     ',symbol))

(defmacro defparameter (name initial-value &optional documentation)
  (declare (ignorable documentation))
  `(progn
     (declaim (special ,name))
     (setf (symbol-value ',name) ,initial-value)
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

(defmacro defvar (name &optional (initial-value nil ivp) documentation)
  (declare (ignorable documentation))
  `(progn
     (declaim (special ,name))
     ,@(when ivp
         `((unless (boundp ',name)
             (setf (symbol-value ',name) ,initial-value))))
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

(defmacro define-compiler-macro (name lambda-list &body body)
  (multiple-value-bind (function pprint docstring)
      (core::expand-defmacro name lambda-list body 'define-compiler-macro)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name)
             #',function)
       #+documentation
       ,@(when docstring
           `((setf (documentation ',name 'compiler-macro) ,docstring)))
       ',name)))

#+(or)
(defmacro defconstant (name value &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:constant-variable ',name) ,value)
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))
