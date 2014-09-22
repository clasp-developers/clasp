
(in-package :cmp)

(defvar *debug-attach-debugging-info-to-value-frames* t)


;; Use the DebugIRPass to add self referencing debugging informato to IR
(defvar *debug-ir* nil)


;; Turn on all sorts of debug printing within the compiler
;;
(defvar *debug-compiler* nil)
(export '*debug-compiler*)


;; Generate C++ destructors for reference-counting otherwise don't
;;

(defvar *compiler-suppress-dtors* #+use-refcount nil #-use-refcount t)
(export '*compiler-suppress-dtors*)


;;
;;
;; Insert low-level tracing calls within the generated code at specific points
;;
;; 
;;
(defvar *low-level-trace* nil
  "Insert tracing calls within the generated code at specific points.
Set its value to a list of symbols that indicate where the tracing calls will be inserted.
Options are :tagbody :go :all :eh-landing-pads
" )

(defvar *low-level-trace-print* nil
  "Controls whether low-level trace id's are printed(T) or not(nil) as they are encountered.")


;;
;; Define the first environment id
;;
(defvar *next-environment-id* 2000000)


;;
;; Keep track of with-try basic blocks
;;
(defparameter *next-try-id* 0)


;; 
;; Name basic-blocks according to what with-try block they are in
;;
(defparameter *block-name-prefix* "")



;;
;; Calculate the data layout for the system
;;

(defvar *data-layout* nil)
(let* ((module (llvm-create-module (next-run-time-module-name)))
       (engine-builder (llvm-sys:make-engine-builder module)))
       ;; module is invalid after make-engine-builder call
  (llvm-sys:set-target-options engine-builder '(llvm-sys:jitemit-debug-info t
						llvm-sys:jitemit-debug-info-to-disk t))
;;  (llvm-sys:set-use-mcjit engine-builder t)
  (let* ((execution-engine (llvm-sys:create engine-builder)))
    (setq *data-layout* (llvm-sys:get-data-layout execution-engine))))




#-debug-compiler
(progn
  (defmacro debug-print-i32 (num) nil)
  (defmacro cmp-log-dump (fn) nil)
  (defmacro cmp-log (fmt &rest args ) nil)
  )


#+debug-compiler
(progn
  (defun is-debug-compiler-on ()
    *debug-compiler*)

  (defmacro debug-print-i32 (num)
    `(if (is-debug-compiler-on)
	 (irc-intrinsic "debugPrintI32" (jit-constant-i32 ,num))
	 nil))

  (defmacro cmp-log (fmt &rest args)
    `(if (is-debug-compiler-on)
	 (progn
	   (bformat t "%s:%s " (source-file-name) (source-line-column))
	   (bformat t ,fmt ,@args))
	 nil))

  (defmacro cmp-log-dump (fn-or-module)
    `(if (is-debug-compiler-on)
	 (llvm-sys:dump ,fn-or-module)
	 nil))
  )


