
(defpackage "SYSTEM")
(defpackage "SI")
(defpackage "GOOF"
    (:shadowing-import-from "CL" "DEFUN" "LOAD" "IN-PACKAGE"
			    "MAKE-VECTOR"
			    )
  )
(in-package :goof)
(load "predlib.lsp")
