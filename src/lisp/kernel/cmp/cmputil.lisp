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


(in-package #:cmp)

(defvar *global-function-defs*)
(defvar *global-function-refs*)

;;; The policy is computed later in cleavir/setup.lisp.
(defvar *policy* ())

(defmacro with-compilation-unit ((&rest options) &body body)
  `(do-compilation-unit #'(lambda () ,@body) ,@options))

(defun make-global-function-defs-table ()
  (make-hash-table :test #'equal))

(defun make-global-function-refs-table ()
  (make-hash-table :test #'equal :thread-safe t))

(defvar *active-protection* nil)

(defstruct global-function-def
  type
  name
  source-pos-info)

(defstruct global-function-ref
  name
  source-pos-info)

(defun known-function-p (name)
  (and (boundp '*global-function-defs*)
       (gethash name *global-function-defs*)))

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

(defmacro with-atomic-file-rename ((temp-pathname final-pathname) &body body)
  `(let ((,temp-pathname (core:mkstemp (namestring ,final-pathname))))
     (unwind-protect
          (progn ,@body)
       (when (core:file-kind ,temp-pathname t)
         (rename-file ,temp-pathname ,final-pathname :if-exists t)))))
