(in-package #:clasp-cleavir-translate-bir)

;;;; Variables and accessors used in the translation process.
;;;; In a separate file because translate-bir and landing-pad-bir both need
;;;; these and circular dependencies are bad.

(defvar *tags*)
(defvar *datum-values*)
(defvar *constant-values*)
(defvar *dynenv-storage*)
(defvar *unwind-ids*)
(defvar *function-info*)
(defvar *enclose-initializers*)

(defun delay-initializer (initializer-thunk)
  (push initializer-thunk *enclose-initializers*))

(defun force-initializers ()
  (loop (unless *enclose-initializers*
          (return))
        (funcall (pop *enclose-initializers*))))

(defun iblock-tag (iblock)
  (or (gethash iblock *tags*)
      (error "BUG: No tag for iblock: ~a" iblock)))

(defun bind-variable (var)
  (if (cleavir-bir:immutablep var)
      ;; This should get initialized eventually.
      nil
      (setf (gethash var *datum-values*)
            (ecase (cleavir-bir:extent var)
              ((:local :dynamic)
               ;; just an alloca
               (cmp:alloca-t*))
              ((:indefinite)
               ;; make a cell
               (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
                "cc_makeCell" nil ""))))))

;; Return either the value or cell of a closed over variable depending
;; on whether it is immutable so we can close over the memory location
;; and implement the the cell indirection properly when the variable
;; is mutable and closed over.
(defun variable-as-argument (variable)
  (let ((value/cell (or (gethash variable *datum-values*)
                        (error "BUG: Variable or cell missing: ~a" variable))))
    (if (or (typep variable 'cleavir-bir:catch)
            (cleavir-bir:immutablep variable))
        value/cell
        (ecase (cleavir-bir:extent variable)
          (:indefinite value/cell)
          (:dynamic (cmp:irc-bit-cast value/cell cmp:%t*%))
          (:local
           (error "Should not be passing the local variable ~a as an environment argument." variable))))))

(defun in (datum)
  (check-type datum (or cleavir-bir:phi cleavir-bir:ssa))
  (or (gethash datum *datum-values*)
      (error "BUG: No variable for datum: ~a" datum)))

(defun variable-in (variable)
  (check-type variable cleavir-bir:variable)
  (if (cleavir-bir:immutablep variable)
      (or (gethash variable *datum-values*)
          (error "BUG: Variable missing: ~a" variable))
      (ecase (cleavir-bir:extent variable)
        (:local
         (let ((alloca (or (gethash variable *datum-values*)
                           (error "BUG: Variable missing: ~a" variable))))
           (cmp:irc-load alloca)))
        (:dynamic
         (let ((alloca (or (gethash variable *datum-values*)
                           (error "BUG: DX cell missing: ~a" variable))))
           (cmp:irc-load (cmp:irc-bit-cast alloca cmp:%t**%))))
        (:indefinite
         (let ((cell (or (gethash variable *datum-values*)
                         (error "BUG: Cell missing: ~a" variable)))
               (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
           (cmp:irc-load-atomic (cmp::gen-memref-address cell offset)))))))

(defun out (value datum)
  (check-type datum cleavir-bir:ssa)
  (assert (not (gethash datum *datum-values*))
          ()
          "Double OUT for ~a: Old value ~a, new value ~a"
          datum (gethash datum *datum-values*) value)
  (setf (gethash datum *datum-values*) value))

(defun phi-out (value datum llvm-block)
  (check-type datum cleavir-bir:phi)
  (unless (eq (cleavir-bir:rtype datum) :multiple-values)
    (llvm-sys:add-incoming (in datum) value llvm-block)))

(defun variable-out (value variable)
  (check-type variable cleavir-bir:variable)
  (if (cleavir-bir:immutablep variable)
      (setf (gethash variable *datum-values*) value)
      (ecase (cleavir-bir:extent variable)
        (:local
         (let ((alloca (or (gethash variable *datum-values*)
                           (error "BUG: Variable missing: ~a" variable))))
           (cmp:irc-store value alloca)))
        (:dynamic
         (let ((alloca (or (gethash variable *datum-values*)
                           (error "BUG: DX cell missing: ~a" variable))))
           (cmp:irc-store value (cmp:irc-bit-cast alloca cmp:%t**%))))
        (:indefinite
         (let ((cell (or (gethash variable *datum-values*)
                         (error "BUG: Cell missing: ~a" variable)))
               (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
           (cmp:irc-store-atomic
            value
            (cmp::gen-memref-address cell offset)))))))

(defun dynenv-storage (dynenv)
  (check-type dynenv cleavir-bir:dynamic-environment)
  (or (gethash dynenv *dynenv-storage*)
      (error "BUG: Missing dynenv storage for ~a" dynenv)))

(defun (setf dynenv-storage) (new dynenv)
  (setf (gethash dynenv *dynenv-storage*) new))

(defun get-destination-id (iblock)
  (or (gethash iblock *unwind-ids*)
      (error "Missing unwind ID for ~a" iblock)))

(defun find-llvm-function-info (function)
  (or (gethash function *function-info*)
      (error "Missing llvm function info for BIR function ~a." function)))
