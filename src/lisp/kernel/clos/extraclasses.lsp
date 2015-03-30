;; Finish initializing classes that we defined in C++ that
;; are not in :COMMON-LISP or :SYS package
;; so that we can use them as specializers for generic functions

(in-package "CLOS")
(defun add-extra-classes ()
  (let ((additional-classes (reverse core:*all-cxx-classes*))
	(core-package (find-package :core))
	(cl-package (find-package :cl))
	(ext-package (find-package :ext)))
    (dolist (class-symbol additional-classes)
      (let* ((class (find-class class-symbol))
	     (supers-names (mapcar #'(lambda (x) (class-name x)) (cl:direct-superclasses class))))
	(cond
	  ((or (and (eq (symbol-package class-symbol) core-package)
                    (not (or (eq class-symbol 'core::model)
                             (eq class-symbol 'core::instance))))
	       (eq (symbol-package class-symbol) cl-package)
	       (eq (symbol-package class-symbol) ext-package))
	   ())
	  (t
           (unless supers-names
             (error "The class ~a does not have direct-superclases!!!   Every class needs at least CL:T" class-symbol))
           (format t "clos;extraclasses.lsp -> Initializing class --> class-symbol: ~A  class: ~A  direct-superclasses: ~A~%" class-symbol class supers-names )
           (clos::make-empty-standard-class class-symbol :metaclass 'builtin-class
                                            :direct-superclasses supers-names)
           (clos::finalize-inheritance class)))))))




;;
;; Initialize all extra classes
;;
(add-extra-classes)
