(in-package :literal)

;;; ------------------------------------------------------------
;;;
;;; Immediate objects don't need to be put into tables
;;;

;;; Return NIL if the object is not immediate
;;; - if it is an immediate then return an immediate-datum object that
;;; contains the tagged immediate value.
(defun immediate-datum-or-nil (original)
  (let ((immediate (core:create-tagged-immediate-value-or-nil original)))
    (if immediate
        (make-immediate-datum :value immediate)
        nil)))

;;; Helper function: we write a few things out as base strings.
;;; FIXME: Use a more efficient representation.
(defun prin1-to-base-string (object)
  (with-output-to-string (s nil :element-type 'base-char)
    (prin1 object s)))

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
