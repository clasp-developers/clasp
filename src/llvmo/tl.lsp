
(defparameter *load-file* (let* ((original-file "./_module.bc")
				 (edited-file "./_module_edited.bc")
				 (load-file (let ((orig-path (make-path original-file))
						  (edited-path (make-path edited-file)))
					      (if (> (last-write-time orig-path)
						     (if (exists edited-path)
							 (last-write-time edited-path)
							 0))
						  original-file
						  edited-file))))
			    load-file))
(bformat t "Loading module from file: %s\n" *load-file*)
(defparameter *mod* (llvm-sys:parse-bitcode-file *load-file* co:*llvm-context* co:*the-execution-engine*))
(bformat t "loaded module is valid = %d\n" (llvm-sys:module-valid *mod*))
(llvm-sys:link-external-globals-in-module *mod* co:*the-execution-engine*)
(defparameter *main-fn* (llvm-sys:get-function *mod* "+++main+++"))
(llvm-sys:run-function co:*the-execution-engine* *main-fn* nil)

