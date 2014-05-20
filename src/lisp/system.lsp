(provide 'system)

(setf (logical-pathname-translations "brcl")
      (list (list "**;*.*" (concatenate 'string (core:getenv "CLASP_HOME") "/**/*.*"))))


(require 'serialize)
