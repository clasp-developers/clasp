(in-package #:clos)

(defgeneric compute-applicable-methods (generic-function arguments))
(defmethod compute-applicable-methods
    ((gf standard-generic-function) args)
  (sort-applicable-methods gf (applicable-method-list gf args)
                           (mapcar #'class-of args)))

(defun applicable-method-list (gf args)
  (flet ((applicable-method-p (method)
           (loop for spec in (method-specializers method)
                 for arg in args
                 always (specializer-accepts-p spec arg))))
    ;; no remove-if-not yet. Could make it work via compiler macro
    ;; or inlining i guess?
    (loop for method in (generic-function-methods gf)
          when (applicable-method-p method)
            collect method)))

;;; TODO?: Generalizers (arxiv 1403.2765), export
(defgeneric specializer-accepts-p (specializer object))
(defmethod specializer-accepts-p ((spec class) object)
  (core::of-class-p object spec))
(defmethod specializer-accepts-p ((spec eql-specializer) object)
  (eql object (eql-specializer-object spec)))

;;; we don't have typep yet
(defgeneric eql-specializer-p (specializer))
(defmethod eql-specializer-p ((spec eql-specializer)) t)
(defmethod eql-specializer-p ((spec class)) nil)

(defgeneric compute-applicable-methods-using-classes
    (generic-function classes))
(defmethod compute-applicable-methods-using-classes
    ((gf standard-generic-function) classes)
  (flet ((applicable-method-p (method)
           (loop for spec in (method-specializers method)
                 for class in classes
                 always (if (eql-specializer-p spec)
                            (if (core::of-class-p
                                 (eql-specializer-object spec)
                                 class)
                                (return-from compute-applicable-methods-using-classes
                                  (values nil nil))
                                nil)
                            (core::subclassp class spec)))))
    (values (sort-applicable-methods
             gf
             (loop for method in (generic-function-methods gf)
                   when (applicable-method-p method)
                     collect method)
             classes)
            t)))

;;; used in miss.lisp
(defun compute-applicable-methods-using-specializers (generic-function specializers)
  (sort-applicable-methods
   generic-function
   (applicable-method-list-using-specializers generic-function specializers)
   specializers))

(defun method-applicable-to-specializers-p (method argspecs)
  (loop for spec in (method-specializers method)
        for argspec in argspecs
        always (cond ((eql-specializer-p argspec)
                      (specializer-accepts-p spec (eql-specializer-object argspec)))
                     ;; if the method has an eql specializer and we don't
                     ;; the method isn't applicable.
                     ((eql-specializer-p spec) nil)
                     (t (core:subclassp argspec spec)))))

(defun applicable-method-list-using-specializers (gf argspecs)
  (loop for method in (generic-function-methods gf)
        when (method-applicable-to-specializers-p method argspecs)
          collect method))

(defun sort-applicable-methods (gf methods args-specializers)
  ;; Reorder args-specializers to match APO.
  (let ((f (generic-function-a-p-o-function gf)))
    (when f
      (setf args-specializers (funcall f args-specializers)))
    ;; then order the list. Simple selection sort. FIXME?
    ;; note that this mutates the list, so be sure methods
    ;; is fresh.
    (loop for to-sort on methods
          do (loop for comparees on (rest to-sort)
                   for comparee = (first comparees)
                   for most-specific = (first to-sort)
                   when (eql (compare-methods most-specific comparee
                                              args-specializers f)
                             2)
                     do (rotatef (first comparees) (first to-sort))))
    methods))

(defun compare-methods (method-1 method-2 args-specializers f)
  (let* ((specializers-list-1 (method-specializers method-1))
         (specializers-list-2 (method-specializers method-2)))
    (compare-specializers-lists (if f (funcall f specializers-list-1) specializers-list-1)
                                (if f (funcall f specializers-list-2) specializers-list-2)
                                args-specializers)))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (loop for spec1 in spec-list-1 for spec2 in spec-list-2
        for arg-specializer in args-specializers
        for c = (compare-specializers spec1 spec2 arg-specializer)
        do (case c
             ;; if =, just keep going
             ((1) (return 1))
             ((2) (return 2))
             ((nil)
              (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	             (or spec1 t) (or spec2 t) arg-specializer)))))

(defun fast-subtypep (spec1 spec2)
  ;; Specialized version of subtypep which uses the fact that spec1
  ;; and spec2 are either classes or eql specializers (basically member types)
  (if (eql-specializer-p spec1)
      (if (eql-specializer-p spec2)
          (eq spec1 spec2) ; take advantage of internment
	  (si::of-class-p (eql-specializer-object spec1) spec2))
      (if (eql-specializer-p spec2)
	  ;; There is only one class with a single element, which
	  ;; is NULL = (MEMBER NIL).
	  (and (null (eql-specializer-object spec2))
	    (eq spec1 (load-time-value (find-class 'null) t)))
	  (si::subclassp spec1 spec2))))

(defun compare-specializers (spec-1 spec-2 arg-spec)
  (let ((cpl (class-precedence-list (if (eql-specializer-p arg-spec)
                                        (class-of (eql-specializer-object
                                                   arg-spec))
                                        arg-spec))))
    (cond ((eq spec-1 spec-2) '=)
          ((fast-subtypep spec-1 spec-2) '1)
          ((fast-subtypep spec-2 spec-1) '2)
          ;; Per CLHS 7.6.6.1.2, an eql specializer is considered
          ;; more specific than a class. Also, for an eql specializer
          ;; to be compared to a class here, they must both be
          ;; applicable, and as such the eql is a "sub specializer".
          ((eql-specializer-p spec-1) '1)
          ((eql-specializer-p spec-2) '2)
          ((member spec-1 (rest (member spec-2 cpl))) '2)
          ((member spec-2 (rest (member spec-1 cpl))) '1)
	  ;; This will force an error in the caller
	  (t nil))))
