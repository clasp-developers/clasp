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

(defgeneric clasp-ffi::%mem-ref (ptr type &optional (offset 0)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :char)) &optional (offset 0))
  (clasp-ffi::%mem-ref-character (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :short)) &optional (offset 0))
  (clasp-ffi::%mem-ref-short (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :unsigned-short)) &optional (offset 0))
  (clasp-ffi::%mem-ref-unsigned-short (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :int)) &optional (offset 0))
  (clasp-ffi::%mem-ref-int (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :unsigned-int)) &optional (offset 0))
  (clasp-ffi::%mem-ref-unsigned-int (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :int8)) &optional (offset 0))
  (clasp-ffi::%mem-ref-int8 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :uint8)) &optional (offset 0))
  (clasp-ffi::%mem-ref-uint8 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :int16)) &optional (offset 0))
  (clasp-ffi::%mem-ref-int16 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :uint16)) &optional (offset 0))
  (clasp-ffi::%mem-ref-uint16 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :int32)) &optional (offset 0))
  (clasp-ffi::%mem-ref-int16 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :uint32)) &optional (offset 0))
  (clasp-ffi::%mem-ref-uint16 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :int64)) &optional (offset 0))
  (clasp-ffi::%mem-ref-int64 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :uint64)) &optional (offset 0))
  (clasp-ffi::%mem-ref-uint64 (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :long)) &optional (offset 0))
  (clasp-ffi::%mem-ref-long (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :unsigned-long)) &optional (offset 0))
  (clasp-ffi::%mem-ref-unsigned-long (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :long-long)) &optional (offset 0))
  (clasp-ffi::%mem-ref-long-long (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :unsigned-long-long)) &optional (offset 0))
  (clasp-ffi::%mem-ref-unsigned-long-long (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :double)) &optional (offset 0))
  (clasp-ffi::%mem-ref-double-float (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :float)) &optional (offset 0))
  (clasp-ffi::%mem-ref-float (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :long-double)) &optional (offset 0))
  (clasp-ffi::%mem-ref-long-double (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :time)) &optional (offset 0))
  (clasp-ffi::%mem-ref-time (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :pointer)) &optional (offset 0))
  (clasp-ffi::%mem-ref-pointer (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :size)) &optional (offset 0))
  (clasp-ffi::%mem-ref-size (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :ssize)) &optional (offset 0))
  (clasp-ffi::%mem-ref-ssize (clasp-ffi::%offset-address-as-integer ptr offset)))

(defmethod clasp-ffi::%mem-ref (ptr (type (eql :ptrdiff)) &optional (offset 0))
  (clasp-ffi::%mem-ref-ptrdiff (clasp-ffi::%offset-address-as-integer ptr offset)))

;;; === M E M - S E T ===

;; (defgeneric clasp-ffi::%mem-set (ptr type value &optional (offset 0)))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; EXPORTS

(export '(with-foreign-object
          with-foreign-objects
          %mem-ref))
