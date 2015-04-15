(provide 'system)

#+(or)(setf (logical-pathname-translations "brcl")
	    (list (list "**;*.*" (concatenate 'string (core:getenv "CLASP_HOME") "/**/*.*"))))


#+(or)(require 'serialize)
