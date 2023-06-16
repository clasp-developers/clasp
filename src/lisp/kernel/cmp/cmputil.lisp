;;;
;;;    File: cmputil.lisp
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

;;;; This is the compiler signaling system as used by bclasp, i.e. before
;;;; the CLOS condition system exists. Most of this file will be redefined
;;;; in compiler-conditions.lisp.


(in-package :cmp)

(defvar *global-function-defs*)
(defvar *global-function-refs*)

(defvar *warnings-p*)
(defvar *failure-p*)

;;; Global OPTIMIZE declaration and derived policy.
;;; This can be changed later by OPTIMIZE proclamations.
;;; The policy is computed later in cleavir/setup.lisp.
(defvar *optimize*
  '((compilation-speed 1)
    (debug 1) (space 1) (speed 1) (safety 1)))
(defvar *policy* ())

;;; (setq core::*echo-repl-read* t)

(defmacro with-compiler-env ((&rest options) &rest body)
  "Initialize the environment to protect nested compilations from each other"
  (declare (ignore options)) ; FIXME: Find a use or remove
  `(let ((*the-module* nil)
	 (*irbuilder-function-alloca* nil)
	 (*irbuilder-function-body* nil)
	 (*generate-compile-file-load-time-values* nil)
	 (*load-time-value-holder-global-var-type* nil)
	 (*load-time-value-holder-global-var* nil)
	 (*the-module-dibuilder* nil))
     ,@body))

(defmacro with-compilation-unit ((&rest options) &body body)
  `(do-compilation-unit #'(lambda () ,@body) ,@options))

(defun make-global-function-defs-table ()
  (make-hash-table :test #'equal))

(defun make-global-function-refs-table ()
  (make-hash-table :test #'equal :thread-safe t))

(defvar *active-protection* nil)

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
    (let ((existing (gethash name *global-function-defs*))
          (cspi (ext:current-source-location)))
      (if (and existing
               ;; defmethod can define a gf, so we still want to note it-
               ;; but multiple defmethods, or a defmethod after a defgeneric,
               ;; shouldn't cause a warning.
               (not (and (eq type 'defmethod)
                         (or (eq (global-function-def-type existing) 'defgeneric)
                             (eq (global-function-def-type existing) 'defmethod)))))
          (warn-redefined-function
           name type cspi
           (global-function-def-type existing)
           (global-function-def-source-pos-info existing))
          (setf (gethash name *global-function-defs*)
                (make-global-function-def :type type
                                          :name name
                                          :source-pos-info cspi))))))

(defun register-global-function-ref (name &optional (origin (ext:current-source-location)))
  (when (boundp '*global-function-refs*)
    ;; Can't do (push ... (gethash ...)) because we're too early.
    (let ((existing-refs (gethash name *global-function-refs*))
          (new-ref
            (make-global-function-ref :name name
                                      :source-pos-info origin)))
      (setf (gethash name *global-function-refs*)
            (cons new-ref existing-refs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff for taking care of the second & third return values
;;; of COMPILE, COMPILE-FILE, etc.

(defmacro with-compilation-results ((&rest options) &body body)
  `(call-with-compilation-results (lambda () ,@body) ,@options))

(export 'with-atomic-file-rename)
(defmacro with-atomic-file-rename ((temp-pathname final-pathname) &body body)
  `(let ((,temp-pathname (core:mkstemp (namestring ,final-pathname))))
     #+(or)(format t "Writing to ~s~%" ,temp-pathname)
     (unwind-protect
          (progn
            ,@body)
       (progn
         #+(or)(format t "Renaming ~s to ~s~%" ,temp-pathname ,final-pathname)
         (rename-file ,temp-pathname ,final-pathname :if-exists t)))))


(defun write-bitcode (module output-path &key output-type)
  ;; Write bitcode as either .bc files or .ll files
  (cond
    ((eq output-type :object)
     (if *use-human-readable-bitcode*
         (let* ((filename (make-pathname :type "ll" :defaults (pathname output-path))))
           (with-atomic-file-rename (temp-pathname filename)
             (with-open-file (fout temp-pathname :direction :output
                                   :if-does-not-exist :create)
               (llvm-sys:dump-module module fout))))
         (with-atomic-file-rename (temp-pathname output-path)
           (llvm-sys:write-bitcode-to-file module (namestring temp-pathname)))))
    ((eq output-type :faspll)
     (with-atomic-file-rename (temp-pathname output-path)
       (with-open-file (fout temp-pathname :direction :output
                             :if-does-not-exist :create)
         (llvm-sys:dump-module module fout))))
    ((eq output-type :faspbc)
     (with-atomic-file-rename (temp-pathname output-path)
       (llvm-sys:write-bitcode-to-file module (namestring temp-pathname)))))
  (let ((file-length 0))
    (with-open-file (fin output-path :direction :input)
      (setf file-length (file-length fin)))
    (if (= file-length 0)
        (error "A zero length ~a file was written" output-type))))

;;;(setq core::*echo-repl-read* t)
