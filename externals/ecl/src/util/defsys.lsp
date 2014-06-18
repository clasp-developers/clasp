;;;
;;; **********************************************************************
;;; (c) Copyright G. Attardi, 1990.  All rights reserved.
;;; **********************************************************************

(defparameter *util-directory*	"")

(sbt:defsystem util

     :modules
     '((system	t	t	())	; a system building tool
       )

     :directory *util-directory*

     :pathname-types  '("lsp" . "o"))
