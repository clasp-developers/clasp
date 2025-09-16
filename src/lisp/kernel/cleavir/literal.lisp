(in-package #:clasp-cleavir)

;; A vector of constants, load time values, etc. A constant's position in
;; the vector matches its position in the runtime constants vector.
(defvar *constant-indices*)

(defvar *similarity*)

(defstruct similarity-table
  (identity (make-hash-table :test #'eq))
  ;; A few special tables for particular types we expect to see a lot of,
  ;; or which can be meaningfully coalesced.
  ;; Everything else gets the identity.
  (number (make-hash-table :test #'eql))
  (cons (make-hash-table :test #'eq))
  (array (make-hash-table :test #'eq))
  (symbol (make-hash-table :test #'eq))
  (base-string (make-hash-table :test #'equal))
  (pathname (make-hash-table :test #'equal))
  (hash-table (make-hash-table :test #'eq))
  (fungen (make-hash-table :test #'eq))
  ;; The below tables are only used for file compilation, because they refer
  ;; to objects that don't necessarily exist yet at compile time.
  ;; For runtime compilation (COMPILE) all constants necessarily exist already.
  ;; Table for LTVs; the nature of the keys is defined by the caller
  ;; but have to work with an EQL table.
  (ltv (make-hash-table :test #'eql))
  ;; Keyed by variable name
  (vcell (make-hash-table :test #'eq))
  ;; Keyed by function name
  (fcell (make-hash-table :test #'equal)))

(defun similarity-table-for (object read-only-p &optional (simtable *similarity*))
  (if read-only-p
      (typecase object
        (number (similarity-table-number simtable))
        (cons (similarity-table-cons simtable))
        (array (similarity-table-array simtable))
        (symbol (similarity-table-symbol simtable))
        (base-string (similarity-table-base-string simtable))
        (pathname (similarity-table-pathname simtable))
        (t (similarity-table-identity simtable)))
      (similarity-table-identity simtable)))

(defun similar (object read-only-p &optional (simtable *similarity*))
  (gethash object (similarity-table-for object read-only-p simtable)))

(defun (setf similar) (new object read-only-p &optional (simtable *similarity*))
  (setf (gethash object (similarity-table-for object read-only-p simtable)) new))

(defmacro ensure-similar (object read-only-p new-form
                          &optional (simtable '*similarity*))
  (let ((o (gensym "OBJECT")) (table (gensym "TABLE")))
    `(let* ((,o ,object)
            (,table (similarity-table-for ,o ,read-only-p ,simtable)))
       (or (gethash ,o ,table)
           (setf (gethash ,o ,table) ,new-form)))))

(defun ensure-constant (constant &optional (simtable *similarity*))
  (ensure-similar constant t
                  (let ((info (cmp:constant-info/make constant)))
                    (vector-push-extend info *constant-indices*))
                  simtable))

(defgeneric ensure-literal-info (info))

(defmethod ensure-literal-info ((constant-info cmp:constant-info))
  (ensure-similar (cmp:constant-info/value constant-info) t
                  (vector-push-extend constant-info *constant-indices*)
                  *similarity*))
;; We adapt BIR info objects into our compiler's.
;; FIXME: Stupid
(defmethod ensure-literal-info ((constant-info bir:constant))
  (let ((value (bir:constant-value constant-info)))
    (ensure-similar value t
                    (vector-push-extend
                     (cmp:constant-info/make value)
                     *constant-indices*)
                    *similarity*)))

(defmethod ensure-literal-info ((ltv-info cmp:load-time-value-info))
  (let ((table (similarity-table-ltv *similarity*)))
    (or (gethash ltv-info table)
        (setf (gethash ltv-info table)
              (vector-push-extend ltv-info *constant-indices*)))))
(defmethod ensure-literal-info ((ltv-info bir:load-time-value))
  (let ((table (similarity-table-ltv *similarity*)))
    (or (gethash ltv-info table)
        (setf (gethash ltv-info table)
              (vector-push-extend
               (cmp:load-time-value-info/make
                (bir:form ltv-info) (bir:read-only-p ltv-info))
               *constant-indices*)))))

(defmethod ensure-literal-info ((vinfo cmp:variable-cell-info))
  (let ((table (similarity-table-vcell *similarity*))
        (vname (cmp:variable-cell-info/vname vinfo)))
    (or (gethash vname table)
        (setf (gethash vname table)
              (vector-push-extend vinfo *constant-indices*)))))
(defmethod ensure-literal-info ((vinfo bir:variable-cell))
  (let ((table (similarity-table-vcell *similarity*))
        (vname (bir:variable-name vinfo)))
    (or (gethash vname table)
        (setf (gethash vname table)
              (vector-push-extend
               (cmp:variable-cell-info/make vname)
               *constant-indices*)))))

(defmethod ensure-literal-info ((finfo cmp:function-cell-info))
  (let ((table (similarity-table-fcell *similarity*))
        (fname (cmp:function-cell-info/fname finfo)))
    (or (gethash fname table)
        (setf (gethash fname table)
              (vector-push-extend finfo *constant-indices*)))))
(defmethod ensure-literal-info ((finfo bir:function-cell))
  (let ((table (similarity-table-fcell *similarity*))
        (fname (bir:function-name finfo)))
    (or (gethash fname table)
        (setf (gethash fname table)
              (vector-push-extend
               (cmp:function-cell-info/make fname)
               *constant-indices*)))))

(defmethod ensure-literal-info ((info cmp:env-info))
  ;; FIXME: Make Cleavir aware of an explicit runtime environment?
  (ensure-constant nil))

(defmethod ensure-literal-info ((info core:simple-core-fun-generator))
  (let ((table (similarity-table-fungen *similarity*)))
    (or (gethash info table)
        (setf (gethash info table)
              (vector-push-extend info *constant-indices*)))))

;;; codegen

(defvar *constants-vector-ir*)
(defun %constants-vector-type () cmp:%t*[0]%)

(defun %constants-table-reference (index label)
  (cmp:irc-const-gep2-64 (%constants-vector-type)
                         *constants-vector-ir*
                         0 index
                         (or label "const_ref")))

(defun %load-constant (index label)
  (cmp:irc-t*-load (%constants-table-reference index label)
                   (or label "const")))

(defun literal (constant &optional label)
  (let ((immediate (core:create-tagged-immediate-value-or-nil constant)))
    (if immediate
        (llvm-sys:constant-expr/get-int-to-ptr (%i64 immediate) cmp:%t*% nil)
        (%load-constant (ensure-constant constant) label))))

(defun info-literal (info &optional label)
  (%load-constant
   (typecase info
     (bir:constant
      (let ((imm (core:create-tagged-immediate-value-or-nil
                  (bir:constant-value info))))
        (if imm
            (return-from info-literal
              (llvm-sys:constant-expr/get-int-to-ptr (%i64 imm) cmp:%t*% nil))
            (ensure-literal-info info))))
     (cmp:constant-info
      (let ((imm (core:create-tagged-immediate-value-or-nil
                  (cmp:constant-info/value info))))
        (if imm
            (return-from info-literal
              (llvm-sys:constant-expr/get-int-to-ptr (%i64 imm) cmp:%t*% nil))
            (ensure-literal-info info))))
     (t (ensure-literal-info info))) ; never immediate
   label))

(defun do-constants (thunk constants constants-table-name)
  (let ((*constant-indices* constants)
        (*similarity* (make-similarity-table))
        ;; This is a dummy global variable that we will replace in the end.
        (*constants-vector-ir*
          (llvm-sys:make-global-variable cmp:*the-module* cmp:%t*[0]%
                                         nil
                                         'llvm-sys:internal-linkage
                                         nil
                                         (concatenate 'string
                                                      constants-table-name
                                                      "-dummy"))))
    (multiple-value-prog1 (multiple-value-call #'values
                            (funcall thunk) *constant-indices*)
      ;; Replace the dummy with an array of the ultimate length.
      (let* ((array-type
               (llvm-sys:array-type-get cmp:%t*% (length *constant-indices*)))
             (actual-table
              (llvm-sys:make-global-variable
               cmp:*the-module* array-type
               nil 'llvm-sys:external-linkage
               (llvm-sys:undef-value-get array-type)
               constants-table-name)))
        (llvm-sys:replace-all-uses-with *constants-vector-ir* actual-table)
        (llvm-sys:erase-from-parent *constants-vector-ir*)))))

(defmacro with-constants ((constants constants-table-name) &body body)
  `(do-constants (lambda () (progn ,@body))
     ,constants ,constants-table-name))

(defun gen-function-vector (functions fvector-name)
  (let ((type (llvm-sys:array-type-get cmp:%opaque-fn-prototype*%
                                       (length functions))))
    (llvm-sys:make-global-variable cmp:*the-module*
                                   type
                                   nil 'llvm-sys:external-linkage
                                   (llvm-sys:constant-array-get
                                    type
                                    (coerce functions 'list))
                                   fvector-name)))
