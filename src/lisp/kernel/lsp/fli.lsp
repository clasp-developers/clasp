;; ===========================================================================
;;;                  F L I    I M P L E M E N T A T I O N
;;; ===========================================================================
;;; -- IMPLEMEMTATION NOTES ---
;;;
;;; The complete FLI is comprised of the following files:
;;; .../src/core/fli.cc            - corresponding .cc file
;;; .../include/clasp/core/fli.h   - corresponding .h file
;;; .../src/lisp/kernel/fli.lsp    - this file
;;;
;;; --- END OF IMPLEMEMTATION NOTES ---

;;;----------------------------------------------------------------------------
;;; ISSUES / KNOWN PROBLEMS
;;;
;;;  001: ENHANCEMENT: %mem-set not yet implemented.
;;;
;;;----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *echo-repl-tpl-read* t))

(in-package "CLASP-FFI")

;;; ===========================================================================
;;; FLI LISP SIDE IMPLEMENTATION

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; MACROS

(defmacro clasp-ffi::with-foreign-object ((var type) &body body)
  `(let ((,var (clasp-ffi::%allocate-foreign-object ,type)))
     (unwind-protect
          (progn ,@body)
       (clasp-ffi::%free-foreign-object ,var))))

(defmacro clasp-ffi::with-foreign-objects (bindings &rest body)
  (if bindings
      `(clasp-ffi::with-foreign-object ,(car bindings)
         (clasp-ffi::with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; UTILITIES

(defun clasp-ffi::inline (fn)
  (eval-when (:load-toplevel :compile-toplevel :execute)
    (proclaim `(inline ,fn))))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; These code lines were debugged with the help of drneister, stassats and
;;; Shinmera on 2016-09-22 as a joint effort on IRC channel #clasp. Thx again!

;;; === M E M - R E F ===

(defgeneric %mem-ref (ptr type &optional offset))

(defmacro generate-mem-ref-accessor-functions ()
  `(progn
     ,@(loop for spec across *foreign-type-spec-table*
          when spec
	  collect
	    `(defmethod %mem-ref (ptr (type (eql ',(%lisp-symbol spec))) &optional (offset 0))
	       (funcall ,(intern (concatenate 'string "%MEM-REF-" (string (%lisp-symbol spec))))
                        (%offset-address-as-integer ptr offset))))))

;;; === M E M - S E T ===

(defgeneric %mem-set (ptr type value &optional offset))

(defmacro generate-mem-set-accessor-functions ()
  `(progn
     ,@(loop for spec across *foreign-type-spec-table*
          when spec
	  collect
	    `(defmethod %mem-set (ptr (type (eql ',(%lisp-symbol spec))) value &optional (offset 0))
	       (funcall ,(intern (concatenate 'string "%MEM-SET-" (string (%lisp-symbol spec))))
                        (%offset-address-as-integer ptr offset) value)))))

;;;----------------------------------------------------------------------------
;;;
;;; F L I   I N I T I A L I Z A T I O N

(eval-when (:load-toplevel :execute :compile-toplevel)
  (generate-mem-ref-accessor-functions)
  (generate-mem-set-accessor-functions)
  (values))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; EXPORTS

(export '(with-foreign-object
          with-foreign-objects
          %mem-ref
          %mem-set))
