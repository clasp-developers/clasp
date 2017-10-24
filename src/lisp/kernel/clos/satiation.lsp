(in-package "CLOS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satiation of generic functions to start fastgf
;;;
;;; Ideas copied from Sicl/Code/CLOS/satiation.lisp
;;;

;;; Essentially, for some gfs we need in a consistent state for the system to work,
;;; during boot we fake a call history so that they can be called without invoking
;;; gfs such as themselves that haven't yet been placed in a working state.

;;; Satiation should occur before any generic function calls are performed, and
;;; involve no generic function calls, therefore.

;;; Note that the accessors for call-history and specializer-profile are not accessors,
;;; they are C++ functions. So they're okay to use without early-accessors.

(defun add-satiation-entries (generic-function lists-of-specializers)
  (with-early-accessors (+standard-generic-function-slots+) ; for -method-combination
    (let* ((new-entries
             (loop for specific-specializers in lists-of-specializers
                   for methods = (std-compute-applicable-methods-using-classes
                                  generic-function specific-specializers)
                   ;; Everything should use standard method combination during satiation.
                   ;; FIXME: Once compute-effective-method is changed, we should do the
                   ;; reader/writer optimizations here as well.
                   for effective-method-function = (std-compute-effective-method
                                                    generic-function
                                                    (generic-function-method-combination generic-function)
                                                    methods)
                   collect (cons (coerce specific-specializers 'vector) effective-method-function))))
      (loop for call-history = (generic-function-call-history generic-function)
            for new-call-history = (append new-entries call-history)
            for exchange = (generic-function-call-history-compare-exchange
                            generic-function call-history new-call-history)
            until (eq exchange new-call-history)))))

(defun satiate-generic-function (generic-function lists-of-specializers)
  ;; Many generic functions at startup will be missing specializer-profile at startup
  ;;    so we compute one here using the number of required arguments in the lambda-list.
  ;; The call-history may be incorrect because of improper initialization as
  ;;    clos starts up - so lets wipe it out and then satiate it.
  (gf-log "Starting satiate-generic-function\n")
  ;; Wipe out the call-history and satiate it using methods
  (gf-log "About to set call history\n")
  (erase-generic-function-call-history generic-function)
  (add-satiation-entries generic-function lists-of-specializers)
  ;; Now when the function is called the discriminating-function will be invalidated-dispatch-function
  ;; This well set up the real discriminating function. This shouldn't involve a dispatch miss, and
  ;; no generic-function calls (other than the one for the actual call, of course).
  (set-funcallable-instance-function generic-function 'invalidated-dispatch-function))

(defun satiate-standard-generic-functions ()
  (macrolet ((satiate-one (gf-name &body lists-of-class-names)
               `(prog2
                  (gf-log ,(concatenate 'string "Satiating " (string gf-name) "\n"))
                  (satiate-generic-function
                   (fdefinition ',gf-name)
                   (list ,@(loop for list in lists-of-class-names
                                 collect `(list ,@(loop for name in list
                                                        collect `(find-class ',name))))))
                  (gf-log ,(concatenate 'string "Done satiating " (string gf-name) "\n")))))
    ;; I think what we need to satiate are just what dispatch-miss can call.
    ;; With the actual classes. Abstract classes aren't relevant.
    (satiate-one class-slots
                 (standard-class)
                 (funcallable-standard-class))
    ;; instance updates we shouldn't need to satiate...
    (satiate-one compute-applicable-methods-using-classes
                 (standard-generic-function cons)
                 (standard-generic-function null)) ; nulls may not be necessary, but no big.
    (satiate-one compute-applicable-methods
                 (standard-generic-function cons)
                 (standard-generic-function null))
    (satiate-one compute-effective-method
                 (standard-generic-function method-combination cons)
                 (standard-generic-function method-combination null))
    ;; We should satiate the method combination accessors, but we actually
    ;; just use early accessors at the moment... which is probably wrong (FIXME?)
    (satiate-one method-qualifiers ; called by method combinations
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one method-specializers
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one accessor-method-slot-definition
                 (standard-reader-method) (standard-writer-method))
    (satiate-one slot-definition-allocation
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-name
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-location
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one generic-function-name
                 (standard-generic-function))
    (satiate-one generic-function-method-combination
                 (standard-generic-function))
    ;; This one is needed for the initial specializer profile computation in fixup.
    ;; (i.e., it's called by initialize-generic-function-specializer-profile)
    (satiate-one generic-function-lambda-list
                 (standard-generic-function))))

;;; This function sets up an initial specializer profile for a gf that doesn't have one.
;;; It can only not have one if it was defined unnaturally, i.e. during boot.
;;; We have to call this on all generic functions so defined, so more than the ones that
;;; we satiate.
;;; Furthermore, we have to do so before any generic functions are called. That's why
;;; we use this separate function rather than compute-and-set-specializer-profile.
;;; FIXME: Since this is only called during boot, we probably only need one compare-exchange.
(defun satiation-setup-specializer-profile (proto-gf)
  ;; proto-gf in that it's unnaturally defined. it's still a standard-generic-function object.
  (with-early-accessors (+standard-generic-function-slots+
                         +standard-method-slots+)
    (unless (slot-boundp proto-gf 'lambda-list)
      (error "In satiation-setup-specializer-profile - ~s has no lambda list!"
             (core:low-level-standard-generic-function-name proto-gf)))
    (let* ((ll (generic-function-lambda-list proto-gf))
           ;; FIXME: l-l-r-a is defined in generic.lsp, which is after this, so we get a style warning.
           ;; (this function is not actually called until fixup)
           (new-profile (make-array (length (lambda-list-required-arguments ll))
                                    :initial-element nil)))
      (loop for old-profile = (generic-function-specializer-profile proto-gf)
            for exchange = (generic-function-specializer-profile-compare-exchange proto-gf old-profile new-profile)
            until (eq exchange new-profile)))
    (let ((methods (generic-function-methods proto-gf)))
      (if methods
          (loop for method in methods
                for specializers = (method-specializers method)
                ;; in closfastgf. we rely on this function not calling gfs.
                do (update-specializer-profile proto-gf specializers))
          (error "In satiation-setup-specializer-profile - ~s has no methods!"
                 (core:low-level-standard-generic-function-name proto-gf))))
    (gf-log "Set initial specializer profile for satiated function %s to %s\n"
            (core:low-level-standard-generic-function-name proto-gf)
            (generic-function-specializer-profile proto-gf))))

(defun cache-status ()
  (format t "                method-cache: ~a~%" (multiple-value-list (core:method-cache-status)))
  (format t "single-dispatch-method-cache: ~a~%" (multiple-value-list (core:single-dispatch-method-cache-status)))
  (format t "                  slot-cache: ~a~%" (multiple-value-list (core:slot-cache-status))))

(export '(cache-status satiate-standard-generic-functions))
