;; DONT USE


(in-package "SYSTEM")


(defun find-easy-module (name)
  (let ((dir (make-pathname :host "src" :directory (list :absolute "lisp" (symbol-name name)))))
    (if (probe-file dir)
	(let ((module-path 
    (format t "Looking for module ~a~%" name)
    (format t "Pathname ~a~%" dir)
    (format t "Exists -->  ~a~%" (probe-file dir))
    nil))


(defun setup-easy-module ()
  (push #'find-easy-module core:*module-provider-functions*))

(setup-easy-module)


