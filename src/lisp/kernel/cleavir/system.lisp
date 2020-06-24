(in-package :clasp-cleavir)

;;; Define the CLASP system
(defclass clasp () ())
(defclass clasp-64bit (clasp) ())

;; singleton- don't bother with constructor
(defvar *clasp-system*
  (locally (declare (notinline make-instance)) (make-instance 'clasp-64bit)))

(defclass clasp-global-environment () () )
(defvar *clasp-env*
  (locally (declare (notinline make-instance)) (make-instance 'clasp-global-environment)))

;;
;; Define the ABI for x86-64
(defclass abi-x86-64 () ())
(defclass abi-x86-32 () ())

(defvar *abi-x86-64*
  (locally (declare (notinline make-instance)) (make-instance 'abi-x86-64)))

(export '(clasp-global-environment
          *clasp-env*
          clasp
          *clasp-system*
          *abi-x86-64*))


#+(or)(defmacro cc-dbg-when (cond &rest body) nil)
(defmacro cc-dbg-when (cond &rest body)
  `(when (and ,cond *debug-log-on*)
     ,@body))

(defvar *debug-log* nil)
(defvar *debug-log-on* nil)
(defvar *debug-instruction-function* nil)
(defvar *debug-log-index* 0)
(defvar *debug-ownerships*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stealth mixins
;;;
;;;
#+stealth-gids
(progn
  (defvar *stealth-mixins* (make-hash-table))

  (defmacro class-stealth-mixins (class)
    `(gethash ,class *stealth-mixins*))

  (defun class-equalp (c1 c2)
    (when (symbolp c1) (setf c1 (find-class c1)))
    (when (symbolp c2) (setf c2 (find-class c2)))
    (eq c1 c2))

  (defmacro define-stealth-mixin (name super-classes victim-class &rest for-defclass)
    `(progn
       ;; First define the class we talk about
       (defclass ,name ,super-classes ,@for-defclass)
       (clos::ensure-class
        ',victim-class
        :direct-superclasses
        (adjoin ',name
                (and (find-class ',victim-class nil) (clos::class-direct-superclasses
                                                      (find-class ',victim-class)))
                :test #'class-equalp))
       ;; Register it as a new mixin for the victim class
       (pushnew ',name (class-stealth-mixins ',victim-class))
       (defmethod clos::ensure-class-using-class :around 
         (class (name (eql ',victim-class))
                &rest arguments
                &key (direct-superclasses nil direct-superclasses-p) &allow-other-keys)
         (dolist (k (class-stealth-mixins name)) 
           (pushnew k direct-superclasses
                    :test #'class-equalp)) 
         (apply #'call-next-method class name
                :direct-superclasses direct-superclasses
                arguments))
       ',name))



  (define-stealth-mixin secret-instruction-id () cleavir-ir:instruction
                        ((%gid :initform 0 :accessor instruction-gid)))

  (define-stealth-mixin secret-datum-id () cleavir-ir:datum
                        ((%gid :initform 0 :accessor datum-gid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add labels to graphviz drawings
;;;

  (defmethod cleavir-ir-graphviz:label :around (instr)
             (with-output-to-string (s)
               (format s "~a:~a" (call-next-method) (instruction-gid instr))))

  (defmethod cleavir-ir-graphviz::name :around (datum)
             (with-output-to-string (s)
               (format s "~a:~a" (call-next-method) (datum-gid datum))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Macro to time compiler stages
;;;


(defvar *ct-previous*)
(defvar *ct-start*)
(defvar *ct-end*)
(defvar *ct-generate-ast*)
(defvar *ct-hoist-ast*)
(defvar *ct-generate-hir*)
(defvar *ct-process-captured-variables*)
(defvar *ct-liveness*)
(defvar *ct-optimize-stack-enclose*)
(defvar *ct-mark-dynamic-extent*)
(defvar *ct-infer-types*)
(defvar *ct-prune-typeqs*)
(defvar *ct-delete-the*)
(defvar *ct-eliminate-typeq*)
(defvar *ct-eliminate-load-time-value-inputs*)
(defvar *ct-translate*)

(defun ct-total ()
  (+ *ct-start*
     *ct-end*
     *ct-generate-ast*
     *ct-hoist-ast*
     *ct-generate-hir*
     *ct-process-captured-variables*
     *ct-liveness*
     *ct-optimize-stack-enclose*
     *ct-mark-dynamic-extent*
     *ct-infer-types*
     *ct-prune-typeqs*
     *ct-delete-the*
     *ct-eliminate-typeq*
     *ct-eliminate-load-time-value-inputs*
     *ct-translate*))


(defun compiler-timer-start ()
  (setf *ct-previous* (get-internal-run-time)))

(defun compiler-timer-elapsed ()
  (unless (boundp '*ct-previous*)
    (setq *ct-previous* (get-internal-run-time)))
  (prog1 (- (get-internal-run-time) *ct-previous*)
    (setf *ct-previous* (get-internal-run-time))))

(defmacro compiler-time (form)
  `(let ((*ct-previous* 0)
         (*ct-start* 0)
         (*ct-end* 0)
         (*ct-generate-ast* 0)
         (*ct-hoist-ast* 0)
         (*ct-generate-hir* 0)
         (*ct-process-captured-variables* 0)
         (*ct-liveness* 0)
         (*ct-optimize-stack-enclose* 0)
         (*ct-mark-dynamic-extent* 0)
         (*ct-infer-types* 0)
         (*ct-prune-typeqs* 0)
         (*ct-delete-the* 0)
         (*ct-eliminate-typeq* 0)
         (*ct-eliminate-load-time-value-inputs* 0)
         (*ct-translate* 0))
     (compiler-timer-start)
     (multiple-value-prog1 ,form
       (setf *ct-end* (compiler-timer-elapsed))
       (format *trace-output*
               "~{~,5F secs : ~,30a~%~}~
~,5F secs : Total   cf~a ~%"
               (list
                (float (/ *ct-start* internal-time-units-per-second)) "*ct-start*"
                (float (/ *ct-generate-ast* internal-time-units-per-second)) "*ct-generate-ast*"
                (float (/ *ct-hoist-ast* internal-time-units-per-second)) "*ct-hoist-ast*"
                (float (/ *ct-generate-hir* internal-time-units-per-second)) "*ct-generate-hir*"
                (float (/ *ct-process-captured-variables* internal-time-units-per-second)) "*ct-process-captured-variables*"
                (float (/ *ct-liveness* internal-time-units-per-second)) "*ct-liveness*"
                (float (/ *ct-optimize-stack-enclose* internal-time-units-per-second)) "*ct-optimize-stack-enclose*"
                (float (/ *ct-mark-dynamic-extent* internal-time-units-per-second)) "*ct-mark-dynamic-extent*"
                (float (/ *ct-infer-types* internal-time-units-per-second)) "*ct-infer-types*"
                (float (/ *ct-prune-typeqs* internal-time-units-per-second)) "*ct-prune-typeqs*"
                (float (/ *ct-delete-the* internal-time-units-per-second)) "*ct-delete-the*"
                (float (/ *ct-eliminate-typeq* internal-time-units-per-second)) "*ct-eliminate-typeq*"
                (float (/ *ct-eliminate-load-time-value-inputs* internal-time-units-per-second)) "*ct-eliminate-load-time-value-inputs*"
                (float (/ *ct-translate* internal-time-units-per-second)) "*ct-translate*"
                (float (/ *ct-end* internal-time-units-per-second)) "*ct-end*"
                )
               (float (/ (ct-total) internal-time-units-per-second))
               cmp:*debug-compile-file-counter*))))


  
