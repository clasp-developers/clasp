;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;

  
(in-package "SI")

;;; The guts of print-unreadable-object, inspired by SBCL. This is
;;; a redefinition of the function in iolib.lisp which add support
;;; for pprint-logical-block.
(defun %print-unreadable-object (object stream type identity body)
  (cond (*print-readably*
         (error 'print-not-readable :object object))
        ((and *print-pretty* (inravina-shim:pretty-stream-p stream))
         (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
           (print-unreadable-object-contents object stream type identity body)))
        (t
         (let ((stream (cond ((null stream)
                              *standard-output*)
                             ((eq t stream)
                              *terminal-io*)
                             (t
                              stream))))
           (write-string "#<" stream)
           (print-unreadable-object-contents object stream type identity body)
           (write-char #\> stream))))
  nil)

(inravina-shim:initialize-inravina)
