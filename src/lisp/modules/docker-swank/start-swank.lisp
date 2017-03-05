(format t "Starting swank~%")
(load (format nil "~a/swank-loader.lisp" core::*swank-home*))
(swank-loader:init
 :delete nil
 :reload nil
 :load-contribs nil)
(let ((swank::*loopback-interface* "0.0.0.0"))
  (swank:create-server :port 4005
		       :dont-close nil))
(format t "You should never reach here~%")
