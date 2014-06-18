(defpackage :ecl-quicklisp
  (:use :cl))

(in-package :ecl-quicklisp)

(require :ecl-curl)

(defparameter *quicklisp-url* "http://beta.quicklisp.org/quicklisp.lisp")

(defparameter *quicklisp-directory*
  #+windows
  (translate-logical-pathname "SYS:QUICKLISP;")
  #-windows
  (translate-logical-pathname "HOME:QUICKLISP;")
  )

(defparameter *quicklisp-setup*
  #+windows
  "SYS:QUICKLISP;SETUP.LISP"
  #-windows
  "HOME:QUICKLISP;SETUP.LISP"
)

(defun safe-download (url filename)
  (ensure-directories-exist filename)
  (handler-case
      (ecl-curl:download-url-to-file url filename)
    (ecl-curl:download-error (c)
      (format t "~&;;;~%;;; Unable to download quicklisp. Aborting. ~%;;;")
      (ext:quit 1)))
  filename)

(defun install-quicklisp (target-directory)
  (let ((file (merge-pathnames "_installer.lisp" target-directory)))
    (ensure-directories-exist file)
    (ecl-curl:download-url-to-file *quicklisp-url* file)
    (load file)
    (eval (read-from-string
	   (format nil "(quicklisp-quickstart:install :path ~S)"
		   (namestring (truename target-directory))))
	  )))

(handler-case
    (progn
      (unless (probe-file *quicklisp-setup*)
	(install-quicklisp *quicklisp-directory*))
      (unless (find-package "QL")
	(load *quicklisp-setup*))
      (eval (read-from-string "
       (pushnew #'(ext:lambda-block quicklisp-require (module)
				    (let* ((module (string-downcase module)))
				      (when (find module (ql:provided-systems t)
						  :test #'string-equal
						  :key #'ql-dist:name)
					(and (ql:quickload module)
                                             (provide module)))))
		sys::*module-provider-functions*)
")))
  (error (c)
    (format t "~%;;; Unable to load / install quicklisp. Error message follows:~%~A"
	    c)))

(provide "ecl-quicklisp")


