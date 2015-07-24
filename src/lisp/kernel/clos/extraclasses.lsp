;; Finish initializing classes that we defined in C++ that
;; are not in :COMMON-LISP or :SYS package
;; so that we can use them as specializers for generic functions

(in-package "CLOS")

(defun gather-cxx-classes ()
  (let ((additional-classes (reverse core:*all-cxx-classes*))
	(core-package (find-package :core))
	(cl-package (find-package :cl))
	(ext-package (find-package :ext))
	classes)
    (dolist (class-symbol additional-classes)
	(unless (or (and (eq (symbol-package class-symbol) core-package)
			 (not (or (eq class-symbol 'core::model)
				  (eq class-symbol 'core::instance))))
		    (eq (symbol-package class-symbol) cl-package)
		    (eq (symbol-package class-symbol) ext-package))
	  (push class-symbol classes)))
    (nreverse classes)))

(defun add-cxx-class (class-symbol)
    (let* ((class (find-class class-symbol))
	   (supers-names (mapcar #'(lambda (x) (class-name x))
                                 (core:direct-superclasses class))))
      (clos::make-empty-standard-class class-symbol :metaclass 'core:cxx-class ;; was 'builtin-class
				       :direct-superclasses supers-names)
      (clos::finalize-inheritance class)))

(defun add-extra-classes (additional-classes)
  (dolist (class-symbol additional-classes)
    (add-cxx-class class-symbol)))




;;
;; Initialize all extra classes
;;
(add-extra-classes (gather-cxx-classes))
(add-extra-classes '(core:environment
                     core:activation-frame
                     core:value-frame
                     core:lexical-environment
                     core:runtime-visible-environment
                     core:value-environment
                     core:single-dispatch-effective-method-function))
