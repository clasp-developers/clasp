
(in-package :cmp)


(defvar *debug-attach-debugging-info-to-value-frames* t)



;; Turn on all sorts of debug printing within the compiler
;;
(defvar *debug-compiler* nil)

;;
;; Insert low-level tracing calls within the generated code
;;
(defvar *low-level-trace* nil)


;;
;; Keep track of with-try basic blocks
;;
(defparameter *next-try-id* 0)


;; 
;; Name basic-blocks according to what with-try block they are in
;;
(defparameter *block-name-prefix* "")


