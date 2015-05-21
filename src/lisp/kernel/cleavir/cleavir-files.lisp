;; Copyright (c) 2011, Mark Cox
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage "LISP-EXECUTABLE.CREATION"
  (:use :common-lisp)
  )


(in-package "LISP-EXECUTABLE.CREATION")

(defvar *lisp-machine-output-stream* *standard-output*)

(defun remove-from-plist-unless-keys-are (plist valid-keys &key (test #'eql))
  "Utility function that helps with implementing
START-NEW-LISP-MACHINE and SAVE-EXECUTABLE-USING-FUNCTION-AND-DIE."
  (loop
     :for (key value) :on plist :by #'cddr
     :append
     (if (member key valid-keys :test test)
	 (list key value)
	 nil)))

(defgeneric start-new-lisp-machine (&rest args &key &allow-other-keys))
(defgeneric lisp-machine-input (lisp-machine))
(defgeneric wait-for-lisp-machine (lisp-machine))
(defgeneric kill-lisp-machine (lisp-machine))
(defgeneric lisp-machine-exit (exit-status))
(defgeneric save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys))
(defgeneric command-line-arguments ())
(defgeneric executable-files (output-file))
(defgeneric do-with-control-c-handled (function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-control-c-handled (&body body)
    `(do-with-control-c-handled #'(lambda ()
				    ,@body))))

(defvar *all-systems* nil)
(defvar *all-source-files* nil)
(defclass sticky-beak-op (#-asdf3 asdf:operation
				  #+asdf3 asdf:downward-operation)
  ())

(defmethod asdf:component-depends-on ((op sticky-beak-op) component)
  (append (list (cons 'sticky-beak-op (asdf::component-load-dependencies component)))
	  (call-next-method)))

(defmethod asdf:perform ((op sticky-beak-op) (component t))
  (declare (ignore op component)))

(defmethod asdf:perform ((op sticky-beak-op) (component asdf:system))
  (declare (ignore op))
  (pushnew component *all-systems*))




(defmethod asdf:perform ((op sticky-beak-op) (c asdf/component:source-file))
  (pushnew c *all-source-files*))


(defun determine-complete-set-of-asdf-systems (systems)
  (let ((*all-systems* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    *all-systems*))

(defun determine-complete-set-of-asdf-source-files (systems)
  (let ((*all-source-files* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    (let ((source (mapcar (lambda (x)
			    (let* ((part-name (enough-namestring (asdf/component:component-pathname x)
								 (translate-logical-pathname #P"SYS:")))
				   (no-type (make-pathname :directory (pathname-directory part-name)
							   :name (pathname-name part-name))))
			      (intern (namestring no-type) "CLASP-CLEAVIR")))
			  *all-source-files*)))
      (nreverse source))))



