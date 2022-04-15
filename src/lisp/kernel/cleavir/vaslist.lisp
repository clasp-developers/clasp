(in-package #:cc-vaslist)

;;;; The code in this file is supposed to perform a transform on functions
;;;; with a &rest parameter such that no list actually needs to be consed.
;;;; Essentially, if a &rest parameter is only used in certain limited ways,
;;;; such as the last argument to APPLY, or for iteration, it can be
;;;; represented more cheaply as a simple "vector" of values.

;;;; The following operations are available; that is, if a &rest parameter is
;;;; only ever passed as the list argument to these functions, the compiler
;;;; can perform the transformation and avoid actual consing.

;;;; (if x y z) -> (if (vaslist-endp x) z y)
;;;; (typeq x cons) -> (if (vaslist-endp x) nil t)
;;;; nth -> (vaslist-nth n vaslist) ; with (car x) = (nth 0 x)
;;;; nthcdr -> (vaslist-nthcdr n vaslist) ; with (cdr x) = (nthcdr 0 x)
;;;; last -> (vaslist-last vaslist n)
;;;; butlast -> (vaslist-butlast vaslist n)
;;;; values-list -> (vaslist-values-list vaslist)

;;;; Note that this last is how APPLY works, since (apply x y z) is compiler-
;;;; macroexpanded into (mv-call x (values y) (values-list z)).

;;;; At the moment, the transform will not take place if a vaslist would be
;;;; stored in a variable with multiple assignments. Working on that.
;;;; Also at the moment, car and cdr actually inhibit the transformation since
;;;; they inline into complicated stuff. Working on that too.
;;;; Other TODOs include expanding to other sources of lists,
;;;; and allowing passing vaslists to/returning them from functions as long as
;;;; they do not escape the dynamic extent.

(defparameter *vaslistable* '(values-list))

(defun vaslistablep (fname) (member fname *vaslistable*))

(defun origin-source (origin)
  (loop for org = origin then (cst:source org)
        while (typep org 'cst:cst)
        finally (return org)))

(defgeneric format-reason (reason))
(defmethod format-reason (reason)
  (when (typep reason 'bir:values-collect) (break "~a" reason))
  (format nil "used in unhandled instruction ~a" reason))
(defmethod format-reason ((reason bir:variable))
  (format nil "stored in variable ~a, which is mutated" (bir:name reason)))
(defmethod format-reason ((reason bir:thei))
  (format nil "type checked against ~s"
          (ctype:primary (bir:asserted-type reason)
                         clasp-cleavir:*clasp-system*)))
(defmethod format-reason ((reason bir:returni))
  (format nil "returned from a function"))
(defmethod format-reason ((reason bir:abstract-call))
  (let* ((attr (bir:attributes reason))
         (name (first (attributes:identities attr))))
    (format nil "passed to ~:[a function~;~:*~a~]~@[: ~a~]"
            name (bir:name (bir:callee reason)))))

(defun format-reasons (reasons)
  (mapcar #'format-reason (remove-duplicates reasons)))

(define-condition consing-&rest (ext:compiler-note)
  ((%parameter :initarg :parameter :reader parameter)
   (%reasons :initarg :reasons :reader reasons))
  (:report
   (lambda (condition stream)
     (format stream "Unable to avoid consing &rest parameter ~a, because it is:
~{* ~a~%~}"
             (bir:name (parameter condition))
             (format-reasons (reasons condition))))))

(defvar *record-failures* nil)
(defvar *failure-reasons*)

(defgeneric datum-ok-p (datum)
  (:method ((datum bir:datum))
    (when *record-failures* (push datum *failure-reasons*))
    nil))

(defgeneric use-ok-p (user datum)
  (:method ((user bir:instruction) (datum bir:datum))
    (when *record-failures* (push user *failure-reasons*))
    nil)
  (:method ((user null) (datum bir:datum)) t))

(defmethod datum-ok-p ((datum bir:linear-datum))
  (use-ok-p (bir:use datum) datum))

(defmethod datum-ok-p ((datum bir:variable))
  ;; TODO: loosen
  (let ((imm (bir:immutablep datum))
        (record *record-failures*))
    (when (and (not imm) record)
      (push datum *failure-reasons*))
    (and (or imm record)
         (let* ((def (bir:binder datum))
                (fun (bir:function def))
                (success imm))
           (set:doset (reader (bir:readers datum) success)
             (unless (and (eq (bir:function reader) fun)
                          (use-ok-p reader datum))
               (if record
                   (setf success nil)
                   ;; we're not recording all failures, so quit immediately
                   (return nil))))))))

(defmethod use-ok-p ((inst bir:writevar) (datum bir:datum))
  (datum-ok-p (bir:output inst)))

(defmethod use-ok-p ((inst bir:readvar) (datum bir:datum))
  (datum-ok-p (bir:output inst)))

(defmethod use-ok-p ((inst bir:fixed-to-multiple) (datum bir:datum))
  (cond ((= (length (bir:inputs inst)) 1) (datum-ok-p (bir:output inst)))
        (*record-failures* (push inst *failure-reasons*) nil)
        (t nil)))

(defmethod use-ok-p ((inst bir:thei) (datum bir:datum))
  ;; type checks should be gone at this point, so this is paranoia
  (cond ((symbolp (bir:type-check-function inst))
         (datum-ok-p (bir:output inst)))
        ((let ((type (ctype:primary (bir:asserted-type inst)
                                    clasp-cleavir:*clasp-system*)))
           ;; This can happen when we've passed through a cdr/nthcdr-
           ;; because we do not have a proper list type, the type of the
           ;; result of cdr cannot be derived as LIST, even though for a
           ;; proper list like &rest parameters it necessarily is.
           ;; FIXME: Add a proper list type instead of doing this?
           ;; FIXME: Not sure this is correct with values types, e.g.
           ;; if subsequent types to be checked would fail.
           (member type '(list (or cons null) (or null cons)
                          (or (cons t t) (member nil))
                          (or (member nil) (cons t t)))
                   :test #'equal))
         (datum-ok-p (bir:output inst)))
        (*record-failures* (push inst *failure-reasons*) nil)
        (t nil)))

(defmethod use-ok-p ((inst bir:typeq-test) (datum bir:datum))
  (let ((type (bir:test-ctype inst)))
    (or (eq type 'cons) (equal type '(cons t t)))))

(defmethod use-ok-p ((inst cc-bmir:consp) (datum bir:datum)) t)

;; This arises e.g. from (null a-&rest-list).
(defmethod use-ok-p ((inst bir:ifi) (datum bir:datum)) t)

(defmethod use-ok-p ((inst bir:primop) (datum bir:datum))
  (case (cleavir-primop-info:name (bir:info inst))
    ((cleavir-primop:car) t)
    ((cleavir-primop:cdr) (datum-ok-p (first (bir:outputs inst))))
    (otherwise nil)))

;;; FIXME: This function only looks for existing derivations, rather than
;;; prompting any new ones. More reason this whole file should be part of
;;; the data flow analysis proper.
(defun nonnegative-fixnum-p (arg)
  (let ((sys clasp-cleavir:*clasp-system*))
    (values
     (ctype:subtypep (ctype:primary (bir:ctype arg) sys)
                     (ctype:range 'integer 0 most-positive-fixnum sys)
                     sys))))

(defmethod use-ok-p ((inst bir:call) (datum bir:datum))
  (let* ((name
           ;; KLUDGE
           (first (attributes:identities (bir:attributes inst))))
         (args (rest (bir:inputs inst)))
         (out (bir:output inst))
         (result
           (case name
             ((cl:car) (and (= (length args) 1)
                            (eq datum (first args))))
             ((cl:cdr) (and (= (length args) 1)
                            (eq datum (first args))
                            (datum-ok-p out)))
             ((cl:nth) (and (= (length args) 2)
                            (eq datum (second args))
                            (nonnegative-fixnum-p (first args))))
             ((cl:nthcdr) (and (= (length args) 2)
                               (eq datum (second args))
                               (nonnegative-fixnum-p (first args))
                               (datum-ok-p out)))
             ((cl:elt) (and (= (length args) 2)
                            (eq datum (first args))
                            (nonnegative-fixnum-p (second args))))
             #+(or)
             ((endp) (and (= (length args) 1)
                          (eq datum (first args))))
             ((cl:last) (and (<= 1 (length args) 2)
                             (eq datum (first args))
                             (if (second args)
                                 (nonnegative-fixnum-p (second args))
                                 t)
                             (datum-ok-p out)))
             ;; the description of nbutlast kind of makes it sound like it
             ;; _must_ modify the list which would rule out this transformation,
             ;; since no modification will take place and any modification could
             ;; be noticed elsewhere.
             ;; but we consider it to mean that it _may_ modify the list.
             ((cl:butlast cl:nbutlast)
              (and (<= 1 (length args) 2)
                   (eq datum (first args))
                   (if (second args)
                       (nonnegative-fixnum-p (second args))
                       t)
                   (datum-ok-p out)))
             ((cl:values-list) (and (= (length args) 1)
                                    (eq datum (first args))))
             (otherwise nil))))
    (when (and *record-failures* (not result))
      (push inst *failure-reasons*))
    result))

(defgeneric rewrite-use (use))

(defmethod rewrite-use ((use null)))

(defmethod rewrite-use ((use bir:writevar))
  (let ((var (bir:output use)))
    (assert (bir:immutablep var))
    (set:mapset nil #'rewrite-use (bir:readers var))))

(defmethod rewrite-use ((use bir:readvar))
  (rewrite-use (bir:use (bir:output use))))

(defmethod rewrite-use ((use bir:fixed-to-multiple))
  (let ((input (first (bir:inputs use)))
        (out (bir:output use)))
    (setf (bir:inputs use) nil)
    (bir:replace-uses input out)
    (bir:delete-instruction use)
    (rewrite-use (bir:use input))))

(defmethod rewrite-use ((use bir:thei))
  (let ((input (bir:input use)))
    (bir:delete-thei use)
    (rewrite-use (bir:use input))))

(defmethod rewrite-use ((use cc-bmir:consp))
  (change-class use 'nendp))
(defmethod rewrite-use ((use bir:typeq-test))
  (change-class use 'nendp))

(defmethod rewrite-use ((use bir:ifi))
  ;; Insert a nendp test.
  (let* ((vaslist (bir:input use))
         (new-out (make-instance 'bir:output
                    :name '#:endp :derived-type (bir:ctype vaslist)))
         (nendp (make-instance 'nendp
                  :inputs (list vaslist) :outputs (list new-out)
                  :origin (bir:origin use) :policy (bir:policy use))))
    (bir:insert-instruction-before nendp use)
    (setf (bir:inputs use) (list new-out))))

(defun insert-constant-before (constant inst)
  (let* ((const
           (bir:constant-in-module constant (bir:module (bir:function inst))))
         (sys clasp-cleavir:*clasp-system*)
         (type (ctype:member sys constant))
         (vtype (ctype:single-value type sys))
         (cout (make-instance 'bir:output
                 :name '#:index :derived-type vtype))
         (cref (make-instance 'bir:constant-reference
                 :inputs (list const) :outputs (list cout))))
    (bir:insert-instruction-before cref inst)
    cout))

(defun rewrite-nth (inst index arg)
  (change-class inst 'nth :inputs (list index arg)))
(defun rewrite-nthcdr (inst index arg)
  (change-class inst 'nthcdr :inputs (list index arg))
  (rewrite-use (bir:use (bir:output inst))))

(defmethod rewrite-use ((use bir:primop))
  (let ((name (cleavir-primop-info:name (bir:info use)))
        (args (bir:inputs use)))
    (ecase name
      ((cleavir-primop:car)
       (let ((index (insert-constant-before 0 use)))
         (rewrite-nth use index (first args))))
      ((cleavir-primop:cdr)
       (let ((index (insert-constant-before 1 use)))
         (rewrite-nthcdr use index (first args)))))))

;;; Delete the now unused constant ref->fdefinition
;;; FIXME: again, with flow analysis, remove-unused-instruction would handle
;;; this properly
(defun delete-callee (callee)
  (when (bir:unused-p callee)
    (let* ((fdef (and (typep callee 'bir:output) (bir:definition callee)))
           (pname (and (typep fdef 'bir:primop)
                       (cleavir-primop-info:name (bir:info fdef)))))
      (when (member pname '(fdefinition cc-bir::setf-fdefinition))
        (let ((sym (first (bir:inputs fdef))))
          (bir:delete-instruction fdef)
          (when (bir:unused-p sym)
            (let ((cref (and (typep sym 'bir:output) (bir:definition sym))))
              (when cref (bir:delete-instruction cref))))))))
  (values))

(defmethod rewrite-use ((use bir:call))
  (let ((name
          (first (attributes:identities (bir:attributes use))))
        (callee (bir:callee use))
        (args (rest (bir:inputs use))))
    (ecase name
      ((cl:car)
       (let ((index (insert-constant-before 0 use)))
         (rewrite-nth use index (first args))))
      ((cl:cdr)
       (let ((index (insert-constant-before 1 use)))
         (rewrite-nthcdr use index (first args))))
      ((cl:nth)
       (rewrite-nth use (first args) (second args)))
      ((cl:nthcdr)
       (rewrite-nthcdr use (first args) (second args)))
      ((cl:elt)
       (rewrite-nth use (second args) (first args)))
      ((cl:last)
       (let ((index (or (second args) (insert-constant-before 1 use))))
         (change-class use 'last :inputs (list index (first args)))
         (rewrite-use (bir:use (bir:output use)))))
      ((cl:butlast cl:nbutlast)
       (let ((index (or (second args) (insert-constant-before 1 use))))
         (change-class use 'butlast :inputs (list index (first args)))
         (rewrite-use (bir:use (bir:output use)))))
      ((cl:values-list)
       ;; FIXME: Flush fdefinition of values-list if possible
       (change-class use 'values-list :inputs args)))
    (delete-callee callee)))

(defun maybe-transform (argument)
  (check-type argument bir:argument)
  (cond ((bir:unused-p argument)) ; don't bother
        ((datum-ok-p argument)
         #+(or)
         (setf (cc-bmir:rtype argument) :vaslist)
         (rewrite-use (bir:use argument))
         t)
        (t nil)))

(defun maybe-transform-function (function)
  (let* ((llrest (member '&rest (bir:lambda-list function)))
         (rest (second llrest)))
    (if rest
        (let ((*record-failures*
                (policy:policy-value
                 (bir:policy function)
                 'clasp-cleavir::note-consing-&rest))
              (*failure-reasons* nil))
          (cond ((maybe-transform rest)
                 (setf (first llrest) 'core:&va-rest)
                 t)
                (*record-failures*
                 (cmp:note 'consing-&rest
                           :origin (origin-source (bir:origin function))
                           :parameter rest
                           :reasons *failure-reasons*))
                (t nil)))
        nil)))

(defun maybe-transform-module (module)
  (set:mapset nil #'maybe-transform-function (bir:functions module)))
