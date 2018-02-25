;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2017 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package #:esrap)

;;; RULE REPRESENTATION AND STORAGE
;;;
;;; For each rule, there is a RULE-CELL in *RULES*, whose %INFO slot has the
;;; function that implements the rule in car, and the rule object in CDR. A
;;; RULE object can be attached to only one non-terminal at a time, which is
;;; accessible via RULE-SYMBOL.

(defvar *rules* (make-hash-table))

(defun clear-rules ()
  (clrhash *rules*)
  nil)

(defstruct (rule-cell
             (:conc-name cell-)
             (:constructor
              make-rule-cell
              (symbol &aux (%info (cons (undefined-rule-function symbol) nil))))
             (:copier nil)
             (:predicate nil))
  ;; A cons
  ;;
  ;;   (FUNCTION . RULE)
  ;;
  ;; where
  ;;
  ;; FUNCTION is a function with lambda-list (text position end) which
  ;; is called to do the actual parsing work (or immediately signal an
  ;; error in case of referenced but undefined rules).
  ;;
  ;; RULE is a RULE instance associated to the cell or nil for
  ;; referenced but undefined rules.
  (%info (required-argument :%info) :type (cons function t))
  ;; Either NIL if the corresponding rule is not currently traced or a
  ;; list
  ;;
  ;;   (INFO BREAK CONDITION)
  ;;
  ;; where
  ;;
  ;; INFO is the original value (i.e. before the rule was traced) of
  ;; the %INFO slot of the cell.
  ;;
  ;; BREAK is a Boolean indicating whether to CL:BREAK when the traced
  ;; rule is executed.
  ;;
  ;; CONDITION is NIL or a function that is called when the traced
  ;; rule is executed to determine whether the trace action should be
  ;; performed.
  (trace-info nil)
  (referents nil :type list))

(declaim (inline cell-function))
(defun cell-function (cell)
  (car (cell-%info cell)))

(defun cell-rule (cell)
  (cdr (cell-%info cell)))

(defun set-cell-info (cell function rule)
  ;; Atomic update
  (setf (cell-%info cell) (cons function rule))
  cell)

(defun undefined-rule-function (symbol)
  (lambda (&rest args)
    (declare (ignore args))
    (undefined-rule symbol)))

(defun ensure-rule-cell (symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock *RULES*.
  (ensure-gethash symbol *rules* (make-rule-cell symbol)))

(defun delete-rule-cell (symbol)
  (remhash symbol *rules*))

(defun reference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (when referent
      (pushnew referent (cell-referents cell)))
    cell))

(defun dereference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (symbol)
  (check-type symbol nonterminal)
  (gethash symbol *rules*))

(defclass rule ()
  ((%symbol :initform nil
            :reader rule-symbol)
   (%expression :initarg :expression
                :initform (required-argument :expression))
   ;; Only for DESCRIBE-GRAMMAR. The %CONDITION slot stores the actual
   ;; condition.
   (%guard-expression :initarg :guard-expression
                      :initform t
                      :reader rule-guard-expression)
   ;; Either T for rules that are always active (the common case),
   ;; NIL for rules that are never active, or a function to call
   ;; to find out if the rule is active or not.
   (%condition :initarg :condition
               :initform t
               :reader rule-condition)
   (%transform :initarg :transform
               :initform nil
               :reader rule-transform)
   (%around :initarg :around
            :initform nil
            :reader rule-around)
   ;; Describes in which parts of an error report this rule, its
   ;; children and the input (transitively) expected by the rule may
   ;; be mentioned. This allows preventing "utility" rules from
   ;; cluttering up error reports.
   (%error-report :initarg :error-report
                  :type rule-error-report
                  :reader rule-error-report
                  :initform t)))

(setf (documentation 'rule-symbol 'function)
      "Returns the nonterminal associated with the RULE, or NIL if the
rule is not attached to any nonterminal.")

(declaim (ftype (function (symbol rule-error-report-pattern)
                          (values boolean &optional))
                rule-suitable-for-report-part-p))
(defun rule-suitable-for-report-part-p (symbol part-or-parts)
  (when-let ((rule (find-rule symbol)))
    (error-report-behavior-suitable-for-report-part-p
     (rule-error-report rule) part-or-parts)))

(defun detach-rule (rule)
  (dolist (dep (%rule-direct-dependencies rule))
    (dereference-rule-cell dep (rule-symbol rule)))
  (setf (slot-value rule '%symbol) nil))

(defmethod shared-initialize :after ((rule rule) slots &key)
  (declare (ignore slots))
  (check-expression (rule-expression rule)))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type t :identity nil)
    (format stream "~:[(detached)~;~:*~S <- ~S~]"
            (rule-symbol rule) (rule-expression rule))))

(defun sort-dependencies (symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies
   (rule-symbol rule) (%expression-dependencies (rule-expression rule))))

(defun rule-direct-dependencies (rule)
  (sort-dependencies
   (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule))))

(defun %rule-direct-dependencies (rule)
  (delete (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule))))
