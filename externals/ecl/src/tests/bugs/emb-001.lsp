;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Embedding regression tests

(in-package :cl-test)

(defun test-C-program (c-code &key capture-output)
  (with-open-file (s "tmp/aux.c" :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (princ c-code s))
  (c::compiler-cc "tmp/aux.c" "tmp/aux.o")
  (c::linker-cc "tmp/aux.exe" "tmp/aux.o")
  (case capture-output
    (nil
     (return-from test-C-program (zerop (si::system "tmp/aux.exe"))))
    (STRING
     (with-output-to-string (s)
       (let ((in (si::run-program "tmp/aux.exe" '() :output :stream))
	     line)
	 (loop
	  (setf line (read-line in nil))
	  (unless line (return))
	  (write-line line s)))))
    (T
     (do* ((all '())
	   (x t)
	   (in (si::run-program "tmp/aux.exe" '() :output :stream)))
       ((null in) all)
       (setf x (read in nil nil))
       (unless x (return all))
       (push x all)))))

;;; Date: 21/06/2006 (goffioul)
;;; Fixed: 23/06/2006 (juanjo)
;;; Description:
;;;
;;;	Multiple invocations of cl_shutdown() can hang ECL. Also,
;;;	cl_shutdown() is still invoked at exit (registered with
;;;	atexit()) even if cl_shutdown was previously invoked.
;;;
;;; Fixed: 03/2006 (juanjo)
;;;
(deftest emb-0001-shutdown
  (let* ((skeleton "
#include <ecl/ecl.h>
int main (int argc, char **argv) {
  cl_object x;
  cl_boot(argc, argv);
  si_safe_eval(3, x = c_string_to_object(~S), Cnil, Cnil);
  cl_shutdown();
  exit(0);
}")
	 (form '(push (lambda () (print :shutdown)) ext::*exit-hooks*))
	 (c-code (format nil skeleton (format nil "~S" form)))
	 (data (test-C-program (print c-code) :capture-output t)))
    data)
  '(:shutdown))

