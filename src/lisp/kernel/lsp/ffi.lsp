(defpackage ffi
  (:use :cl :core)
  (:export "WITH-FOREIGN-OBJECT" "WITH-FOREIGN-OBJECTS")
  )


(in-package :ffi)



;;;----------------------------------------------------------------------
;;; MACROLOGY
;;;

(defmacro with-foreign-object ((var type) &body body)
  `(let ((,var (core:allocate-foreign-object ,type)))
     (unwind-protect
	 (progn ,@body)
       (core:free-foreign-object ,var))))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
    `(with-foreign-object ,(car bindings)
      (with-foreign-objects ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

