(defpackage #:encoding-test (:use :cl))

(in-package :encoding-test)

(defvar *boole-array*)

(defun boole$ (op i1 i2)
  (declare (type (integer 0 15) op)
           (type integer i1 i2))
  (boole (aref *boole-array* op) i1 i2))
  
;                        PRINTING and READING

; Without the setting of custom:*default-file-encoding* for clisp in
; acl2.lisp, the build breaks with the following string (note the accented "i"
; in Martin, below):
;   Francisco J. Martín Mateos
; With that setting, we do not need an explicit :external-format argument for
; the call of with-open-file in acl2-check.lisp that opens a stream for
; "acl2-characters".

; Because of the comment above, save an Emacs buffer connected to this file
; after setting the necessary buffer-local variable as follows.

; (setq save-buffer-coding-system 'iso-8859-1)


