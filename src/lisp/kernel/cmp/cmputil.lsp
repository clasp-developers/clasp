;;;
;;;    File: cmputil.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

(in-package :cmp)

(defvar *global-function-defs*)
(defvar *global-function-refs*)
(defvar *compilation-messages*)

(core:defconstant-equal +note-format+        "Note:          %s")
(core:defconstant-equal +style-warn-format+  "Style warning: %s")
(core:defconstant-equal +warn-format+        "Warning:       %s")
(core:defconstant-equal +error-format+       "Error:         %s")
(core:defconstant-equal +fatal-format+       "Fatal-error:   %s")

(defstruct (compiler-message (:type vector))
  (prefix "Note")
  (format +note-format+)
  message
  source-pos-info
  top-level-form
  form)

(defstruct (compiler-note (:include compiler-message)
                          (:type vector)))

(defstruct (compiler-warning
             (:type vector)
             (:include compiler-message
                       (prefix "Warning")
                       (format +warn-format+))))

(defstruct (compiler-error
             (:type vector)
             (:include compiler-message
                       (prefix "Error")
                       (format +error-format+))))

(defstruct (compiler-fatal-error
             (:type vector)
             (:include compiler-error
                       (prefix "Fatal Error")
                       (format +fatal-format+))))

(defstruct (compiler-style-warning
             (:type vector)
             (:include compiler-warning
                       (prefix "Style Warning")
                       (format +style-warn-format+))))

(defun compiler-error (form message &rest args)
  (let* ((cspi (ext:current-source-location)))
    (let ((err (make-compiler-error :message (apply #'core:bformat nil message args)
                                    :source-pos-info cspi
                                    :form form)))
      (push err *compilation-messages*))))

(defun compiler-warning (form message &rest args)
  (let* ((cspi (ext:current-source-location)))
    (let ((err (make-compiler-warning :message (apply #'core:bformat nil message args)
                                      :source-pos-info cspi
                                      :form form)))
      (push err *compilation-messages*))))

(defun compiler-style-warning (form message &rest args)
  (let* ((cspi (ext:current-source-location)))
    (let ((err (make-compiler-style-warning :message (apply #'core:bformat nil message args)
                                            :source-pos-info cspi
                                            :form form)))
      (push err *compilation-messages*))))


#||
(defstruct (
    ((prefix :initform "Warning")
     (format :initform +warn-format+)))

  (define-condition compiler-error (compiler-message)
    ((prefix :initform "Error")
     (format :initform +error-format+)))



  (define-condition compiler-message (simple-condition)
    ((prefix :initform "Note" :accessor compiler-message-prefix)
     (format :initform +note-format+ :accessor compiler-message-format)
     (file :initarg :file :initform *compile-file-pathname*
           :accessor compiler-message-file)
     (position :initarg :file :initform *compile-file-position*
               :accessor compiler-message-file-position)
     (toplevel-form :initarg :form :initform *current-toplevel-form*
                    :accessor compiler-message-toplevel-form)
     (form :initarg :form :initform *current-form*
           :accessor compiler-message-form))
    (:report (lambda (c stream)
               (apply #'compiler-message-report stream c
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c)))))

  (define-condition compiler-note (compiler-message) ())

  (define-condition compiler-debug-note (compiler-note) ())

  (define-condition compiler-warning (compiler-message style-warning)
    ((prefix :initform "Warning")
     (format :initform +warn-format+)))

  (define-condition compiler-macro-expansion-failed (compiler-warning)
    ())

  (define-condition compiler-error (compiler-message)
    ((prefix :initform "Error")
     (format :initform +error-format+)))

  (define-condition compiler-fatal-error (compiler-error)
    ((format :initform +fatal-format+)))

  (define-condition compiler-internal-error (compiler-fatal-error)
    ((prefix :initform "Internal error")))

  (define-condition compiler-style-warning (compiler-message style-warning)
    ((prefix :initform "Style warning")
     (format :initform +warn-format+)))

  (define-condition compiler-undefined-variable (compiler-style-warning)
    ((variable :initarg :name :initform nil))
    (:report
     (lambda (c stream)
       (compiler-message-report stream c
                                "Variable ~A was undefined. ~
                               Compiler assumes it is a global."
                                (slot-value c 'variable)))))
  )

||#

(defun describe-source-location (cspi)
  (let* ((lineno (source-pos-info-lineno cspi))
         (filepos (source-pos-info-filepos cspi))
         (source-file-info (source-file-info cspi))
         (pathname (source-file-info-pathname source-file-info)))
  (core:bformat nil "%s filepos: %d  lineno: %d" (namestring pathname) filepos lineno)))

(defun print-compiler-message (c stream)
  (let ((msg (core:bformat nil (compiler-message-format c) (compiler-message-message c))))
    (bformat stream ";;; %s%N" msg)
    (bformat stream ";;;     at %s %N" (describe-source-location (compiler-message-source-pos-info c)))))

(defmacro with-compiler-env ( (conditions &rest options) &rest body )
  "Initialize the environment to protect nested compilations from each other"
  `(let ((*the-module* nil)
	 (*irbuilder-ltv-function-alloca* nil)
	 (*irbuilder-ltv-function-body* nil)
	 (*ltv-function-landing-pad-block* nil)
	 (*irbuilder-function-alloca* nil)
	 (*irbuilder-function-body* nil)
	 (*generate-compile-file-load-time-values* nil)
	 (*table-index* nil)
	 (*load-time-value-holder-global-var* nil)
	 (*load-time-value-coalesce* nil)
	 (*load-time-initializer-environment* nil)
	 (*the-module-dibuilder* nil)
	 (*readtable* *readtable*)
	 (*package* *package*))
     (with-compilation-unit ()
       (unwind-protect (progn ,@body)
         (setq ,conditions *compilation-messages*)))))


(defstruct (global-function-def (:type vector) :named)
  type
  name
  source-pos-info)


(defstruct (global-function-ref (:type vector) :named)
  name
  source-pos-info)

(defun known-function-p (name)
  (and (boundp '*global-function-defs*)
       (gethash name *global-function-defs*)))

(export '(known-function-p)) ; FIXME MOVE

(defun register-global-function-def (type name)
  (when (boundp '*global-function-defs*)
    (let ((existing (gethash name *global-function-defs*)))
      (if (and existing
               ;; defmethod can define a gf, so we still want to note it-
               ;; but multiple defmethods, or a defmethod after a defgeneric,
               ;; shouldn't cause a warning.
               (not (and (eq type 'defmethod)
                         (or (eq (global-function-def-type existing) 'defgeneric)
                             (eq (global-function-def-type existing) 'defmethod)))))
          (compiler-warning name "The %s %s was previously defined as a %s at %s%N"
                            type name (global-function-def-type existing)
                            (describe-source-location (global-function-def-source-pos-info existing)))
          (setf (gethash name *global-function-defs*)
                (make-global-function-def :type type
                                       :name name
                                       :source-pos-info (ext:current-source-location)))))))

(defun register-global-function-ref (name)
  (let ((refs (gethash name *global-function-refs*)))
    (push (make-global-function-ref :name name
                                    :source-pos-info (ext:current-source-location)) refs)
    (setf (gethash name *global-function-refs*) refs)))

(defun compiler-warning-undefined-global-variable (var)
  (compiler-warning var "Undefined variable %s" var))

(defun function-info (env func)
  (let ((info (classify-function-lookup env func)))
    (when (eq (car info) 'core::global-function)
      (when (not *code-walking*)
        (register-global-function-ref func)))
    info))

(defun variable-info (env var)
  "Lookup the variable in the lexical environment - if not found then check if it is a special"
  (let (#+(or)(core:*environment-debug* (null (symbol-package var))))
    (let ((info (classify-variable env var)))
      (unless info
        ;; We treat constants pretty much identically to specials in bclasp.
        ;; It's not the best way to compile constants.
        (setq info (cons 'ext:special-var var))
        (unless (or (ext:specialp var) (core:symbol-constantp var))
          (when (not *code-walking*)
            (compiler-warning-undefined-global-variable var))))
      info)))

(defun compilation-unit-finished (messages)
  ;; Add messages for global function references that were never satisfied
  (maphash (lambda (name references)
             (unless (or (fboundp name)
                         (gethash name *global-function-defs*))
               (dolist (ref references)
                 (pushnew (make-compiler-style-warning
                           :message (core:bformat nil "Undefined function %s" name)
                           :source-pos-info (global-function-ref-source-pos-info ref)
                           :form name)
                          messages :test #'equalp))))
           *global-function-refs*)
  (dolist (m (reverse messages))
    (print-compiler-message m *debug-io*)))
