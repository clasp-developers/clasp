(in-package :literal)

(defvar cmp:*load-time-value-holder-global-var-type* nil
  "Store the current load-time-value data structure type for the compiler")
(defvar cmp:*load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for the compiler")

;;; ------------------------------------------------------------
;;;
;;; Immediate objects don't need to be put into tables
;;;

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (multiple-value-bind (data-or-index in-array literal-name)
      (reference-literal literal t)
    (if in-array
        (values (constants-table-reference data-or-index) literal-name)
        data-or-index)))

;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun constants-table-reference (index &key
                                          (holder cmp:*load-time-value-holder-global-var*)
                                          (holder-type cmp:*load-time-value-holder-global-var-type*)
                                          literal-name)
  (let ((label (if literal-name
                   (core:fmt nil "values-table[{}]/{}" index literal-name)
                   (core:fmt nil "values-table[{}]" index))))
    (cmp:irc-const-gep2-64 holder-type holder 0 index label)))

(defun constants-table-value (index &key (holder cmp:*load-time-value-holder-global-var*)
                                      (holder-type cmp:*load-time-value-holder-global-var-type*)
                                      literal-name)
  (cmp:irc-t*-load (constants-table-reference index
                                              :holder holder
                                              :holder-type holder-type
                                              :literal-name literal-name)))
