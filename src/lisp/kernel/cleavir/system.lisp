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

(defvar *cst-client*)
(defvar *additional-clasp-character-names*)

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
  )
