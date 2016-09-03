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

;;; === M E M - R E F ===

(defmacro def-mem-ref-accessor (type-kw)
  `(defmethod %mem-ref (ptr (type (eql ,type-kw)) &optional (offset 0))
     (funcall (intern (string-upcase
                        (concatenate 'string "%MEM-REF-" ,type-kw)))
              (%offset-address-as-integer ptr offset))))


(defgeneric %mem-ref (ptr type &optional (offset 0)))

(defun generate-mem-ref-accessor-functions ()
  (loop for type-spec in core::*foreign-type-spec-table*
     do
       (when type-spec
         (def-mem-ref-accessor (%lisp-symbol type-spec)))))

;;; === M E M - S E T ===

#+memset
(defmacro def-mem-set-accessor (type-kw)
  `(defmethod %mem-ref (ptr (type (eql ,type-kw)) &optional (offset 0))
     (funcall (intern (string-upcase
                        (concatenate 'string "%MEM-REF-" ,type-kw)))
              (%offset-address-as-integer ptr offset))))

#+memset
(defgeneric clasp-ffi::%mem-set (ptr type value &optional (offset 0)))

(defun generate-mem-set-accessor-functions ()
  (loop for type-spec in core::*foreign-type-spec-table*
    do
      (when type-spec
        (def-mem-set-accessor (%lisp-symbol type-spec)))))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;;
;;; F L I   I N I T I A L I Z A T I O N

(eval-when (:load-toplevel :execute :compile-toplevel)
  (generate-mem-ref-accessor-functions)
  (generate-mem-set-accessor-functions)
  (values))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; EXPORTS

(export '(with-foreign-object
          with-foreign-objects
          %mem-ref
          %mem-set))
