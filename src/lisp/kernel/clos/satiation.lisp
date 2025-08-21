(in-package "CLOS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERAL SATIATION INTERFACE

;;; Main entry point
;;; Actually quite simple.
;;; Possible improvement: Put in some handler-case stuff to reduce any errors to
;;; warnings, given that this is just for optimization.
(defun satiate (generic-function &rest lists-of-specializer-designators)
  "Prepare a generic function so that its discriminating function will not have
to be recompiled very much when called with pre-specified specializers.
GENERIC-FUNCTION is a generic function. LISTS-OF-SPECIALIZER-DESIGNATORS is a
list of lists of specializer designator. Each inner list should have as many
elements as the generic function has specializable (i.e. required) arguments.
A specializer designator is either a specializer, or a symbol naming a class, or
a list (EQL object) - just like DEFMETHOD."
  (loop with method-combination = (generic-function-method-combination generic-function)
        with old-history = (generic-function-call-history generic-function)
        for list in lists-of-specializer-designators
        for specializers = (mapcar #'coerce-specializer-designator list)
        for applicable-methods
          = (compute-applicable-methods-using-specializers generic-function specializers)
        for outcome = (or (find-existing-outcome history applicable-methods)
                          (outcome generic-function old-history method-combination
                                   applicable-methods specializers))
        collect (cons (coerce specializers 'simple-vector) outcome) into history
        finally (append-generic-function-call-history generic-function history)
                (compile-discriminating-function generic-function)))

(defun coerce-specializer-designator (specializer-designator)
  (etypecase specializer-designator
    (specializer specializer-designator)
    (symbol (find-class specializer-designator))
    ((cons (eql eql) (cons t null)) ; (eql thing)
     (intern-eql-specializer (second specializer-designator)))))

;;; Add a portion of call history into a gf's existing call history.
;;; If any entry to be added duplicates an existing entry, the new entry prevails.
(defun append-generic-function-call-history (generic-function new-entries)
  (mp:atomic-update (generic-function-call-history generic-function)
                    (lambda (history new-entries)
                      (append new-entries
                              (remove-if (lambda (entry)
                                           (call-history-find-key new-entries
                                                                  (car entry)))
                                         history)))
                    new-entries))

;;; Compile time discrimination is on the backburner until I decide how to make it
;;; work in cross-clasp.
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISCRIMINATING FUNCTIONS IN COMPILE-FILE
;;;

;; Given a list of specializer names or specializers, returns a list of specializers.
(defun coerce-to-list-of-specializers (list-of-specializer-names)
  (mapcar (lambda (sname)
            (etypecase sname
              (specializer sname)
              ;; (eql 'something)
              ((cons (eql eql)
                     (cons t null))
               (intern-eql-specializer (second sname)))
              (symbol (find-class sname))))
          list-of-specializer-names))

(defun compile-time-call-history (generic-function &rest lists-of-specializer-names)
  (loop with mc = (generic-function-method-combination generic-function)
        for list-of-specializer-names in lists-of-specializer-names
        for list-of-specializers
          = (coerce-to-list-of-specializers list-of-specializer-names)
        for am = (compute-applicable-methods-using-specializers
                  generic-function list-of-specializers)
        for cached-outcome = (find am outcome-cache :test #'equal
                                   :key #'outcome-methods)
        for outcome = (or cached-outcome
                          (compute-outcome generic-function mc am list-of-specializers))
        when (not cached-outcome)
          collect outcome into outcome-cache
        collect (cons (coerce list-of-specializers 'vector) outcome)
          into entries
        nconc (remove-if-not (lambda (sp) (typep sp 'class))
                             list-of-specializers)
          into all-classes
        finally (return (values entries all-classes))))

;;; Given an outcome, return a form that, when evaluated, returns an outcome
;;; similar to it.
(defun outcome-producer (outcome &optional (arg-info '(t)))
  ;; Note that this relies on methods being dumpable, which we
  ;; establish with their MAKE-LOAD-FORM.
  (cond ((optimized-slot-reader-p outcome)
         `(make-optimized-slot-reader
           ;; FIXME: Probably not correct for :allocation :class.
           :index ',(optimized-slot-reader-index outcome)
           :slot-name ',(optimized-slot-reader-slot-name outcome)
           :methods ',(outcome-methods outcome)
           :class ,(optimized-slot-reader-class outcome)))
        ((optimized-slot-writer-p outcome)
         `(make-optimized-slot-writer
           ;; FIXME: Probably not correct for :allocation :class.
           :index ',(optimized-slot-writer-index outcome)
           :slot-name ',(optimized-slot-writer-slot-name outcome)
           :methods ',(outcome-methods outcome)
           :class ,(optimized-slot-writer-class outcome)))
        ((effective-method-outcome-p outcome)
         ;; The handling of :function is basically the only reason
         ;; we don't just dump the call history and let the usual
         ;; literal mechanisms handle it.
         (let ((form (effective-method-outcome-form outcome))
               (required (rest arg-info))
               (rest (if (car arg-info) 'satiated-more nil)))
           `(make-effective-method-outcome
             :methods ',(outcome-methods outcome)
             :form ',form
             :function (lambda (,@required
                                ,@(when rest `(core:&va-rest ,rest)))
                         (with-effective-method-parameters
                             ((,@required) ,rest)
                           ,form)))))
        (t (error "BUG: Don't know how to reconstruct outcome: ~a"
                  outcome))))

;;; Given a call history, return a form that, when evaluated,
;;; returns a call history similar to the
;;; provided one, and furthermore is dumpable.
(defun call-history-producer (call-history &optional (arg-info '(t)))
  (loop for (key . outcome) in call-history
        for cached-outcome-info = (assoc outcome outcome-cache)
        for name = (or (second cached-outcome-info) (gensym "OUTCOME"))
        for outcome-form
          = (or (third cached-outcome-info)
                (outcome-producer outcome arg-info))
        unless cached-outcome-info
          collect (list outcome name outcome-form) into outcome-cache
        ;; Keys are vectors of classes and lists (representing eql specializers)
        ;; and should therefore be dumpable.
        collect `(cons ,key ,name) into new-ch
        finally
           (return
             `(let (,@(loop for (_ name form) in outcome-cache
                            collect `(,name ,form)))
                (list ,@new-ch)))))

(defun compile-time-discriminator (generic-function call-history)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (let ((name (generic-function-name generic-function)))
      (values
       (generate-discriminator-from-data
        call-history (safe-gf-specializer-profile generic-function)
        `(load-time-value (fdefinition ',name) t) min max
        :inline-effective-methods 'cl:require
        :generic-function-name name)))))

;;; The less simple part is doing things at compile-time.
;;; We can't put discriminating functions into FASLs because they include the class
;;; stamps, which are hard to synchronize between compile- and load-time.
;;; But we can dump (invented) call histories, including ones with functions in them.
;;; It's somewhat convoluted however.
;;; Anywho, we do that for SATIATE calls provided
;;; 1) the GENERIC-FUNCTION form is #'foo
;;; 2) all the lists of specializer designators are constant.
;;; At the moment, it also requires that the generic function and all relevant methods
;;; are defined at compile time. If we store information about DEFMETHODs in the
;;; environment at compile time, though, that shouldn't be required.
;;; (EQL specializers will be a bit weird, though.)
;;; NOTE: As of now we dump with class stamps anyway. This is okay for system code
;;; where the stamps work out identically, but for library code it would be bad.
;;; Might want to make a separate macro or something.

(define-compiler-macro satiate
    (&whole form generic-function &rest lists &environment env)
  (if (and (consp generic-function)
           (eq (car generic-function) 'function)
           (consp (cdr generic-function))
           (null (cddr generic-function))
           (fboundp (second generic-function))
           (loop for list in lists always (constantp list env)))
      `(%satiate ,(second generic-function)
                 ,@(mapcar (lambda (form) (ext:constant-form-value form env)) lists))
      form))

;;; This function checks that stamps at satiation time match stamps at load
;;; time. If this is not true, objects will end up in methods for other
;;; classes, which can cause very strange problems; verifying will check for
;;; this ahead of time, when the precompiled discriminating function is loaded.
(defun verify-stamp-consistency (alist)
  (let ((inconsistent
          (loop for (class . compile-stamp) in alist
                unless (= (core:class-stamp-for-instances class)
                          compile-stamp)
                  collect class)))
    (unless (null inconsistent)
      (error "Stamps for the following classes changed between build and load time: ~a"
             inconsistent))))

;;; Macro version of SATIATE, that the exported function sometimes expands into.
(defmacro %satiate (generic-function-name &rest lists-of-specializer-names)
  (let ((generic-function (fdefinition generic-function-name)))
    (multiple-value-bind (call-history classes)
        (apply #'compile-time-call-history
               generic-function lists-of-specializer-names)
      `(let* ((gf (fdefinition ',generic-function-name)))
         (verify-stamp-consistency
          '(,@(loop for class in classes
                    collect (cons class
                                  (core:class-stamp-for-instances class)))))
         (append-generic-function-call-history
          gf
          ,(call-history-producer call-history (gf-arg-info generic-function)))
         (set-funcallable-instance-function
          gf ,(if (eq cmp:*default-output-type* :bytecode)
                  `(calculate-fastgf-dispatch-function gf)
                  (compile-time-discriminator generic-function call-history)))))))
|#
