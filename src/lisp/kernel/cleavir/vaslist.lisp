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

(defgeneric datum-ok-p (datum)
  (:method ((datum bir:datum)) nil))

(defgeneric use-ok-p (user datum)
  (:method ((user bir:instruction) (datum bir:datum)) nil)
  (:method ((user null) (datum bir:datum)) t))

(defmethod datum-ok-p ((datum bir:linear-datum))
  (use-ok-p (bir:use datum) datum))

(defmethod datum-ok-p ((datum bir:variable))
  ;; TODO: loosen
  (and (bir:immutablep datum)
       (let* ((def (bir:binder datum))
              (fun (bir:function def)))
         (set:doset (reader (bir:readers datum) t)
           (unless (and (eq (bir:function reader) fun)
                        (use-ok-p reader datum))
             (return nil))))))

(defmethod use-ok-p ((inst bir:writevar) (datum bir:datum))
  (datum-ok-p (bir:output inst)))

(defmethod use-ok-p ((inst bir:readvar) (datum bir:datum))
  (datum-ok-p (bir:output inst)))

(defmethod use-ok-p ((inst bir:fixed-to-multiple) (datum bir:datum))
  (and (= (length (bir:inputs inst)) 1)
       (datum-ok-p (bir:output inst))))

(defmethod use-ok-p ((inst bir:thei) (datum bir:datum))
  (datum-ok-p (bir:output inst)))

(defmethod use-ok-p ((inst cc-bmir:consp) (datum bir:datum)) t)

;; This arises e.g. from (null a-&rest-list).
(defmethod use-ok-p ((inst bir:ifi) (datum bir:datum)) t)

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
  (let ((name
          ;; KLUDGE
          (first (attributes:identities (bir:attributes inst))))
        (args (rest (bir:inputs inst)))
        (out (bir:output inst)))
    (declare (ignorable out))
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
      ;; _must_ modify the list, which would rule out this transformation,
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
         (change-class argument 'cc-bmir:argument :rtype :vaslist)
         (rewrite-use (bir:use argument))
         t)
        (t nil)))

(defun maybe-transform-function (function)
  (let* ((llrest (member '&rest (bir:lambda-list function)))
         (rest (second llrest)))
    (cond ((and rest (maybe-transform rest))
           (setf (first llrest) 'core:&va-rest)
           t)
          (t nil))))

(defun maybe-transform-module (module)
  (set:mapset nil #'maybe-transform-function (bir:functions module)))
