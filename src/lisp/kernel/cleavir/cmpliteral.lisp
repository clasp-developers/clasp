(in-package :literal)

#+threads(defvar *value-table-id-lock* (mp:make-lock :name '*value-table-id-lock*))
(defvar *value-table-id* 0)
(defun incf-value-table-id-value ()
  #+threads(mp:with-lock (*value-table-id-lock*) (incf *value-table-id*))
  #-threads(incf *value-table-id*))

(defun next-value-table-holder-name (module-id &optional suffix)
  (if suffix
      (core:fmt nil "{}-{}{}" suffix core:+literals-name+ module-id)
      (core:fmt nil "{}{}" core:+literals-name+ module-id)))

(defstruct literal-node-toplevel-funcall arguments)
(defstruct literal-node-call function source-pos-info holder)
(defstruct literal-node-side-effect name arguments)
(defstruct literal-dnode datum)
(defstruct (literal-node-creator (:include literal-dnode))
  name literal-name object arguments)
(defstruct (literal-node-runtime (:include literal-dnode)) object)

(defstruct function-datum index)
#+short-float
(defstruct short-float-datum value)
(defstruct single-float-datum value)
(defstruct double-float-datum value)
#+long-float
(defstruct long-float-datum value)
(defstruct immediate-datum value)
(defstruct datum kind index literal-node-creator)

(defun literal-datum-p (datum)
  (eq (datum-kind datum) :literal))

(defun transient-datum-p (datum)
  (eq (datum-kind datum) :transient))

(defun make-literal-datum (&key index)
  (make-datum :kind :literal :index index))

(defun make-transient-datum ()
  (make-datum :kind :transient :index nil))

(defun upgrade-transient-datum-to-literal (datum)
  (unless (transient-datum-p datum)
    (error "The datum ~s must be a transient" datum))
  (setf (datum-kind datum) :literal)
  (setf (datum-index datum) (new-table-index)))
  
(defun literal-datum-index (datum)
  (unless (literal-datum-p datum)
    (error "The datum ~s must be a literal" datum))
  (datum-index datum))


(defun datum-tag (datum)
  (cond
    ((literal-datum-p datum) #\l)
    ((transient-datum-p datum) #\t)
    (t (error "No tag for datum ~a" datum))))

(defun datum-index-tag-kind (datum)
  (let ((index (datum-index datum))
        (tag (datum-tag datum))
        (kind (datum-kind datum)))
    (values index tag kind)))


(defun literal-node-index (node)
  (let ((datum (literal-dnode-datum node)))
    (unless (literal-datum-p datum)
      (error "The node ~a has a non-literal datum ~a" node datum))
    (datum-index datum)))

(defparameter *literal-machine* nil)

(defun run-all-add-node (node)
  (vector-push-extend node (literal-machine-run-all-objects *literal-machine*))
  node)

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



(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object datum table)
  (setf (gethash object table) datum))


(defstruct literal-machine
  (run-all-objects (make-array 64 :fill-pointer 0 :adjustable t))
  (table-index 0)
  (function-vector (make-array 16 :fill-pointer 0 :adjustable t))
  (constant-data '())
  (identity-coalesce (make-similarity-table #'eq))
  (ratio-coalesce (make-similarity-table #'eql))
  (cons-coalesce (make-similarity-table #'eq))
  (complex-coalesce (make-similarity-table #'eql))
  (array-coalesce (make-similarity-table #'eq))
  (hash-table-coalesce (make-similarity-table #'eq))
  (bignum-coalesce (make-similarity-table #'eql))
  (symbol-coalesce (make-similarity-table #'eq))
  (base-string-coalesce (make-similarity-table #'equal))
  (pathname-coalesce (make-similarity-table #'equal))
  (function-description-coalesce (make-similarity-table #'equal))
  (entry-point-coalesce (make-similarity-table #'eq))
  (package-coalesce (make-similarity-table #'eq))
  (double-float-coalesce (make-similarity-table #'eql))
  #+long-float
  (long-float-coalesce (make-similarity-table #'eql))
  (fcell-coalesce (make-similarity-table #'equal))
  (vcell-coalesce (make-similarity-table #'eq))
)


;;; ------------------------------------------------------------
;;;
;;;

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 (literal-machine-table-index *literal-machine*)
    (incf (literal-machine-table-index *literal-machine*))))

(defun new-datum (toplevelp)
  (if toplevelp
      (make-literal-datum :index (new-table-index))
      (make-transient-datum)))

;;; Helper function: we write a few things out as base strings.
;;; FIXME: Use a more efficient representation.
(defun prin1-to-base-string (object)
  (with-output-to-string (s nil :element-type 'base-char)
    (prin1 object s)))

(defun call-with-constant-arguments-p (form &optional env)
  (and (consp form)
       (core:proper-list-p (rest form))
       (symbolp (first form))
       (when (fboundp (first form))
         (and (not (macro-function (first form)))
              (not (special-operator-p (first form)))))
       (every (lambda (f) (constantp f env)) (rest form))))

(defvar *run-time-coalesce*)

(defun pretty-load-time-name (object ltv-idx)
  (cond
    ((symbolp object) (core:fmt nil "SYMBOL->{}" object))
    ((consp object) "CONS")
    ((arrayp object) "ARRAY")
    ((numberp object) (format nil "NUMBER->~a" object))
    (t (subseq (core:fmt nil "ltv-idx_{}_val->{}" ltv-idx object) 0 30))))

;;;---------------------------------------------------------------------
;;;
;;; run time values (i.e., cl:compile)
;;;

(declaim (ftype (function (t boolean) (values (or immediate-datum literal-node) boolean)) run-time-reference-literal))
(defun run-time-reference-literal (object read-only-p)
  "If the object is an immediate object return (values immediate nil nil).
   Otherwise return (values creator T index)."
  (declare (ignore read-only-p))
  (let ((immediate-datum (immediate-datum-or-nil object)))
    (if immediate-datum
        (values immediate-datum NIL)
        (let* ((similarity *run-time-coalesce*)
               (existing (find-similar object similarity)))
          (if existing
              (values existing T)
              (values (let* ((datum (new-datum t))
                             (new-obj (make-literal-node-runtime :datum datum :object object)))
                        (add-similar object new-obj similarity)
                        (run-all-add-node new-obj)
                        new-obj)
                      T))))))

;;; ------------------------------------------------------------
;;;
;;; compile-form
;;;
;;; Compile the form and return a 0-arity function that
;;; returns a result.
;;;

(defun compile-form (form)
  (funcall (find-symbol "COMPILE-FORM" "CLASP-CLEAVIR")
           form))

;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;;
;;; reference-literal
;;;
;;; Returns an index for the object for both COMPILE-FILE and COMPILE
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------

(defun reference-literal (object &optional read-only-p)
  "Return (values index T) for the literal object in a constants-table.
   Returns (values :poison-value-from-reference-literal nil) if the object is an immediate and doesn't have a place in the constants-table."
  (let ((cmp:*compile-file-debug-dump-module* nil)
        (cmp:*compile-debug-dump-module* nil))
    (multiple-value-bind (immediate-datum?literal-node-runtime in-array)
        (run-time-reference-literal object read-only-p)
      (if in-array
          (let* ((literal-node-runtime immediate-datum?literal-node-runtime)
                 (index (literal-node-index literal-node-runtime)))
            (values index T))
          (let ((immediate-datum immediate-datum?literal-node-runtime))
            (values (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value immediate-datum))
                    nil))))))

;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-literal (literal
                                     &optional (read-only-p t))
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (multiple-value-bind (data-or-index in-array literal-name)
      (reference-literal literal read-only-p)
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

