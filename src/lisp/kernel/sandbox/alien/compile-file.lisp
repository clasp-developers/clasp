(defpackage #:sicl-system-construction
  (:use #:cl)
  (:shadow #:with-compilation-unit #:compile-file)
  (:export #:with-compilation-unit)
  (:export #:compile-file #:load))

(in-package #:sicl-system-construction)

;;;; COMPILE-FILE

(defun required-argument ()
  (error "required &key argument not supplied"))

(defun compile-file (input-file &key (output-file nil output-file-p)
				  (verbose *compile-verbose*) (print *compile-print*)
				  (environment (required-argument))
				  (system (required-argument)))
  (with-compilation-unit ()
    (let ((output-file (if output-file-p
                           (compile-file-pathname input-file :output-file output-file)
                           (compile-file-pathname input-file)))
          (*macroexpand-hook* (macroexpand-hook))
          (*compile-file-truename* (truename input-file))
          (*compile-file-pathname* (pathname (merge-pathnames input-file)))
          (*package* *package*)
          (*readtable* *readtable*))
      (when verbose
        (format t "~&;;;; Compiling ~a~%" input-file))
      (let (warnings-and-errors style-warnings (eof (gensym "EOF")))
        (with-open-file (in input-file)
          (with-open-file (out output-file :direction :output :if-exists :supersede)
            (loop
               (let ((form (read in nil eof)))
                 (cond ((eq form eof)
                        (return (values (truename output-file) warnings-and-errors style-warnings)))
                       (t
                        (when print
                          (let ((*print-length* 2) (*print-level* 2))
                            ;; this will get us e.g. (defun foo ...) and ((lambda (x) ...) ...)
                            (format t "~&;;; ~a~%" form)))
                        ;; If we hit an error while compiling a form, skip it in the FASL.
                        (block compile-error
                          (output-compile-result
                           out
                           (handler-bind ((error
                                           (lambda (err)
                                             (push err warnings-and-errors)
                                             ;; Resignal for compilation-unit and anyone else.
                                             (signal err)
                                             (return-from compile-error)))
                                          ((and warning (not style-warning))
                                           (lambda (w)
                                             (push w warnings-and-errors)))
                                          (style-warning
                                           (lambda (w)
                                             (push w style-warnings))))
                             (compile-file-form form environment system))))))))))))))

(defun output-compile-result (stream compiled-thing)
  (fresh-line stream)
  (let ((*package* (find-package "KEYWORD"))) ; force package prefix output
    (write compiled-thing :stream stream :readably t :circle t)))

;;;; LOAD TIME VALUE

(defvar *ltv-position*)
(defvar *ltv-forms*)
(defvar *coalesce*)
(defvar *ltv-array-sym*)

(defun compile-file-form (form environment system)
  (allowing-missing-definitions
    (let ((*ltv-position* 0)
          (*ltv-forms* nil)
          ;; coalescence can be done with less similar objects,
          ;; but i think EQ is a minimum to fix circularity
          (*coalesce* (make-hash-table :test 'eq))
          (*ltv-array-sym* (gensym "LTV")))
      (let ((result (%compile-file-form form environment system)))
        `(let ((,*ltv-array-sym* (make-array ,*ltv-position*)))
           ,@(nreverse *ltv-forms*)
           ,result)))))

;;; Excepting LTV, this is all of compile-file-form.
(defun %compile-file-form (form environment system)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file) ; could be moved outward
	 (ast (cleavir-generate-ast:generate-ast form environment system))
	 (hoisted (cleavir-ast-transformations:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel-unhoisted hoisted)))
    `(,(cleavir-hir-interpreter:translate hir)
       ,@(mapcar (lambda (f) (ensure-ltv f environment system)) (cleavir-ast:forms hoisted)))))

(defun ensure-ltv (form environment system)
  (if (constantp form)
      (ensure-object (eval form) environment system)
      ;; arbitrary load-time-value
      (%compile-file-form form environment system)))

(defun ensure-object (object environment system)
  (typecase object
    ;; some objects can't be circular, so just dump em.
    (number object)
    (character object)
    (random-state object)
    ;; we want to catch some common array types to avoid e.g. string reconstruction.
    ;; It would be nice to just check (subtypep array-element-type '(or number character ...))
    ;; but we can't put that in a typecase.
    (string object)
    ((array bit) object)
    ((or hash-table readtable package stream function)
     (error "Don't know how to dump ~a" object))
    (symbol `',object)
    ;; otherwise, do it the hard way.
    (t (multiple-value-bind (index present)
	   (gethash object *coalesce*)
	 (if present
	     `(aref ,*ltv-array-sym* ,index)
	     (prog1 `(aref ,*ltv-array-sym* ,*ltv-position*)
	       (setf (gethash object *coalesce*) *ltv-position*)
	       ;; note *ltv-position* needs to be incremented before calling register-load-form,
	       ;; because that ensure-object's recursively.
	       (register-load-form object (prog1 *ltv-position* (incf *ltv-position*))
				   environment system)))))))

(defun register-load-form (object index environment system)
  (multiple-value-bind (create init)
      (my-make-load-form object environment)
    (push `(setf (aref ,*ltv-array-sym* ,index) ,(%compile-file-form create environment system))
	  *ltv-forms*)
    (push (%compile-file-form init environment system) *ltv-forms*)))

;;; like make-load-form, but handles conses and arrays as well.
(defun my-make-load-form (object &optional environment)
  (typecase object
    (cons (values `(cons nil nil) `(progn (rplaca ',object ',(car object))
					  (rplacd ',object ',(cdr object)))))
    (array (values `(make-array ',(array-dimensions object)
		      :element-type ',(array-element-type object))
		   `(progn
		      ,@(loop for i from 0 below (array-total-size object)
			      collect `(setf (row-major-aref ,object ,i)
					     ',(row-major-aref object i))))))
    (t (make-load-form object environment))))
