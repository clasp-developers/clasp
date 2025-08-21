(in-package #:cmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup dynamic variables
;;;

(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-file-pathname* nil "The pathname of the currently compiled file")
(defvar *compile-file-truename* nil "The truename of the currently compiled file")
;;; These variables are used to let compile-file insert debug information that does not
;;; correspond to the actual file being compiled. This is useful for editors (SLIME) that
;;; may present Clasp with a temporary file containing a portion of some other file; we want
;;; the debug data in the compilation of this file to reflect the other file, not the temp.
(defvar *compile-file-source-debug-pathname*) ; Pathname for source info
(defvar *compile-file-file-scope*) ; File scope bound by compile-file etc for source file info
(defvar *compile-file-source-debug-offset*) ; Offset bound by compile-file etc for SFIs
(defvar *compile-file-source-debug-lineno*) ; ditto

(defvar *compile-file-parallel* nil)
