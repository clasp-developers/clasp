(in-package #:cmp)

;;;; Checking the type header of objects.
;;; This code is used by both bclasp and cclasp.

(defparameter *debug-typeq* nil)

(defun tag-check-cond (object-raw mask ctag)
  ;; object at this point is a smart ptr { {}* }
  (if *debug-typeq* (irc-intrinsic "debugPrintI32" (jit-constant-i32 10002)))
  (let ((tag (irc-and (irc-ptr-to-int object-raw %uintptr_t%)
                      (jit-constant-uintptr_t mask) "tag-only")))
    (irc-icmp-eq tag (jit-constant-uintptr_t ctag) "test")))

(defun compile-tag-check (object-raw mask ctag then-br else-br)
  (irc-cond-br (tag-check-cond object-raw mask ctag) then-br else-br))

;;; NOTE: This is only valid if the object is already known to be generalp.
;;; although in practice we just mask out the tag bits.
(defun header-check-cond (header-value-min-max object-raw)
  (if *debug-typeq* (irc-intrinsic "debugPrintI32" (jit-constant-i32 10001)))
  (let* ((byte-ptr           (irc-bit-cast object-raw %i8*%))
         (general            (irc-untag-general byte-ptr))
         (header-addr        (irc-typed-gep %i8% general (list (+ +header-stamp-offset+ (- +header-size+)))))
         (_0 (if *debug-typeq* (irc-intrinsic "debugPointer" header-addr)))
         (header-stamp-ptr-type (cond
                                  ((= 4 +header-stamp-size+) %i32*%)
                                  ((= 8 +header-stamp-size+) %i64*%)
                                  (t (error "illegal +header-stamp-size+ ~a expected 4 or 8" +header-stamp-size+))))
         (header-stamp-type (cond
                              ((= 4 +header-stamp-size+) %i32%)
                              ((= 8 +header-stamp-size+) %i64%)
                              (t (error "illegal +header-stamp-size+ ~a expected 4 or 8" +header-stamp-size+))))
         (header-addr         (irc-bit-cast header-addr header-stamp-ptr-type))
         (value32             (irc-typed-load header-stamp-type header-addr))
         (_1 (if *debug-typeq* (irc-intrinsic "debugPrintI32" value32)))
         (header-value        (irc-zext value32 %uintptr_t%)))
    (declare (ignore _0 _1))
    (if (fixnump header-value-min-max)
        (let ((shifted-header-value (core:shift-stamp-for-compiled-code header-value-min-max)))
          (irc-icmp-eq header-value (jit-constant-uintptr_t shifted-header-value)))
        (progn
          (check-type header-value-min-max cons)
          (let* ((shifted-header-range-min (core:shift-stamp-for-compiled-code (car header-value-min-max)))
                 (shifted-header-range-max (core:shift-stamp-for-compiled-code (cdr header-value-min-max)))
                 (min-match        (irc-icmp-uge header-value (jit-constant-uintptr_t shifted-header-range-min)))
                 (max-match (irc-icmp-ule header-value (jit-constant-uintptr_t shifted-header-range-max))))
            (irc-and min-match max-match))))))
