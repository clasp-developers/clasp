(defpackage #:clasp-debug2
  (:use #:cl)
  ;; low level
  (:export #:code-source-line
           #:code-source-line-pathname
           #:code-source-line-line-number
           #:code-source-line-column)
  (:export #:frame)
  (:export #:frame-up #:frame-down)
  (:export #:frame-function #:frame-arguments
           #:frame-locals #:frame-source-position
           #:frame-language)
  (:export #:frame-function-name
           #:frame-function-lambda-list
           #:frame-function-source-position
           #:frame-function-form
           #:frame-function-documentation)
  (:export #:disassemble-frame)
  ;; frame selection
  (:export #:with-truncated-stack #:truncation-frame-p
           #:with-capped-stack #:cap-frame-p)
  (:export #:*frame-filters*)
  ;; mid level
  (:export #:call-with-stack #:with-stack)
  (:export #:up #:down #:visible)
  (:export #:map-stack #:list-stack)
  ;; defined later in conditions.lsp
  (:export #:safe-prin1 #:prin1-frame-call
           #:princ-code-source-line
           #:print-stack)
  ;; high level
  (:export #:map-indexed-stack #:goto)
  (:export #:print-backtrace ; in conditions.lsp
           #:map-backtrace
           #:map-indexed-backtrace)
  (:export #:hide-package #:unhide-package
           #:hide #:unhide #:unhide-all)
  ;; misc
  (:export #:function-name-package))

(in-package #:clasp-debug2)

;;; Low level interface

;;; FIXME: Unify source position stuff somehow.
;;; This one could be a core:source-pos-info, except
;;; we don't have an offset into the file.
(defstruct (code-source-line (:type vector) :named)
  pathname line-number column)

(defclass frame ()
  ((%up :initarg :up :accessor %frame-up :reader frame-up)
   (%down :initarg :down :accessor %frame-down :reader frame-down)
   (%fname :initarg :fname :reader frame-function-name)
   (%source-position :initarg :sp :reader frame-source-position)
   (%language :initarg :lang :reader frame-language)))

(defun frame-function (frame) (declare (ignore frame)) nil)
(defun frame-arguments (frame) (declare (ignore frame)) nil)
(defun frame-locals (frame) (declare (ignore frame)) nil)
(defun frame-function-lambda-list (frame)
  "Return the lambda list of the function being called in this frame, and a second value indicating success. This function may fail, in which case the first value is undefined and the second is NIL. In success the first value is the lambda list and the second value is true."
  ;; FIXME: Get from function description
  (let ((f (frame-function frame)))
    (if f
        (ext:function-lambda-list f)
        (values nil nil))))
(defun frame-function-source-position (frame)
  "Return a CODE-SOURCE-LINE object representing the source file position for this frame's function."
  ;; TODO: Get from function description
  (declare (ignore frame))
  nil)
(defun frame-function-form (frame)
  "Return a lambda expression for this frame's function if it's available, or else NIL."
  ;; TODO
  (declare (ignore frame))
  nil)
(defun frame-function-documentation (frame)
  "Return the docstring for this frame's function if it exists and is available, or else NIL."
  (let ((f (frame-function frame)))
    (if f
        (documentation f 'function)
        nil)))
(defun disassemble-frame (frame)
  "Disassemble this frame's function to *standard-output*."
  (declare (ignore frame)))

(defun frame-from-symbol (symbol)
  ;; make a frame from backtrace_symbols information - which is quite bare.
  (destructuring-bind (idx exec addr name plus offset) (core:split symbol " ")
    (declare (ignore idx exec addr plus offset))
    (make-instance 'frame :sp nil :fname (or (core:maybe-demangle name) name)
                   :lang :c++)))

(defun decode-lisp-fname (fname)
  (let* ((parts (cmp:unescape-and-split-jit-name fname))
         (end-part-pos (position "" parts :test #'equal))
         (package-name (second parts))
         (package (find-package package-name))
         (symbol-name (first parts))
         (symbol (if package (find-symbol symbol-name package) nil))
         (type-name (if (integerp end-part-pos)
                        (elt parts (1- end-part-pos))
                        (car (last parts)))))
    ;; todo: method
    (cond ((not symbol) fname)
          ((string= type-name "SETF") `(setf ,symbol))
          (t symbol))))

(defun frame-from-dwarf (sectioned-address object-file)
  (let ((dc (llvm-sys:create-dwarf-context object-file)))
    (multiple-value-bind (filename fname source line column startline disc)
        (llvm-sys:get-line-info-for-address dc sectioned-address)
      (declare (ignore source startline disc))
      (make-instance 'frame
        :fname (decode-lisp-fname fname) :lang :lisp
        :sp (if filename
                (make-code-source-line
                 :pathname filename :line-number line :column column)
                nil)))))

(defun frame-from-os-info (pointer symbol)
  (multiple-value-bind (sectioned-address object-file)
      (llvm-sys:object-file-for-instruction-pointer pointer nil)
    (if sectioned-address
        (frame-from-dwarf sectioned-address object-file)
        (frame-from-symbol symbol))))

(defun frames-from-os-backtrace (pointers symbols)
  (loop with bot
        for pointer in pointers for symbol in symbols
        for prev-frame = nil then frame
        for frame = (frame-from-os-info pointer symbol)
        do (setf (%frame-down frame) prev-frame)
        if bot
          do (setf (%frame-up prev-frame) frame)
        else
          do (setf bot frame)
        finally (setf (%frame-up frame) nil)
                (return bot)))

;;; Frame selection

;;; This is not the most inspired way to do it, but should work.
;;; More efficient would be to keep frame pointers around and compare
;;; them. I think the current code does this but only kinda?

(declaim (notinline call-with-truncated-stack))
(defun call-with-truncated-stack (function) (funcall function))

(defmacro with-truncated-stack ((&key) &body body)
  "Execute the body such that WITH-STACK and derived tools will not see frames below this form's continuation, unless they are passed :delimited nil.
Only the innermost WITH-TRUNCATED-STACK matters for this purpose."
  ;; progn to avoid declarations
  `(call-with-truncated-stack (lambda () (progn ,@body))))

(defun truncation-frame-p (frame)
  "Return true iff this frame represents a use of WITH-TRUNCATED-STACK."
  (eq (frame-function-name frame) 'call-with-truncated-stack))

(declaim (notinline call-with-capped-stack))
(defun call-with-capped-stack (function) (funcall function))

(defmacro with-capped-stack ((&key) &body body)
  "Execute the body such that WITH-STACK and derived tools will not see frames above this form's continuation, unless they are passed :delimited nil.
Only the outermost WITH-CAPPED-STACK matters for this purpose."
  `(call-with-capped-stack (lambda () (progn ,@body))))

(defun cap-frame-p (frame)
  "Return true iff this frame represents a use of WITH-CAPPED-STACK."
  (eq (frame-function-name frame) 'call-with-capped-stack))


;;; Mid level interface: Nativate frames nicely

(defvar *stack-top*)
(defvar *stack-bot*)

(defun find-bottom-frame (start) start)
(defun find-top-frame (start)
  (loop for f = start then (%frame-up f)
        until (null (%frame-up f))
        finally (return f)))

(defun call-with-stack (function &key (delimited t))
  "Functional form of WITH-STACK."
  (core:call-with-operating-system-backtrace
   (lambda (pointers symbols bps)
     (declare (ignore bps))
     (let* ((floor (frames-from-os-backtrace pointers symbols))
            (*stack-bot* (if delimited (find-bottom-frame floor) floor))
            (*stack-top* (if delimited (find-top-frame *stack-bot*) nil)))
       (funcall function *stack-bot*)))))

(defmacro with-stack ((stack &rest kwargs &key (delimited t))
                      &body body)
  "Execute the body in an environment in which FRAME is bound to a CLASP-DEBUG:FRAME object representing the current continuation. This frame object has dynamic extent.
You can use UP and DOWN to navigate frames. Frames above and below WITH-CAPPED-STACK and WITH-TRUNCATED-STACK are inaccessible with these operators, unless DELIMITED is false (default true).
Frames hidden by *FRAME-FILTERS* will be skipped.
The delimiters and visibility may be ignored by using the lower level FRAME-UP, FRAME-DOWN."
  (declare (ignore delimited))
  `(call-with-stack (lambda (,stack) (declare (core:lambda-name with-stack-lambda)) ,@body)
                    ,@kwargs))

(defparameter *frame-filters* (list 'non-lisp-frame-p
                                    'package-hider
                                    'fname-hider)
  "A list of function designators. Any CLASP-DEBUG:FRAME for which any of the functions returns true will be considered invisible by the mid level CLASP-DEBUG interface (e.g. UP, DOWN)")


(defun frame-visible-p (frame)
  (notany (lambda (f) (funcall f frame)) *frame-filters*))

(defun up1 (frame &optional limit)
  (do* ((prev frame next)
        (next (frame-up prev) (frame-up prev)))
       (nil)
    ;; limit is exclusive
    (when (eq next limit) (return prev))
    (when (frame-visible-p next) (return next))))
(defun down1 (frame &optional limit)
  (do* ((prev frame next)
        (next (frame-down prev) (frame-down prev)))
       (nil)
    ;; limit is inclusive
    (when (eq next limit) (return next))
    (when (frame-visible-p next) (return next))))

(defun up (frame &optional (n 1))
  "Return the nth visible frame above the given frame, or if there are not that many visible frames above, return the topmost frame."
  (loop repeat n do (setf frame (up1 frame *stack-top*)))
  frame)

(defun down (frame &optional (n 1))
  "Return the nth visible frame below the given frame, or if there are not that many visible frames above, return the bottommost frame."
  (loop repeat n do (setf frame (down1 frame *stack-bot*)))
  frame)

;; Return the frame if it's visible, or else the next
;; visible frame up if it's not.
(defun visible (frame)
  "If FRAME is visible, return it. Otherwise return the nearest visible frame above."
  (if (frame-visible-p frame)
      frame
      (up frame)))

(defun map-stack (function frame &key count)
  "Call FUNCTION exactly once on each visible frame in the stack, starting at FRAME and then proceeding upward.
If COUNT is provided, at most that many visible frames are called on."
  (loop for f = (visible frame) then (up f)
        for i from 0
        when (or (and count (= i count))
                 ;; This is a bit inefficient, but interactive
                 ;; debuggers aren't usually fast path
                 (eq (up f) f))
          return (values)
        do (funcall function f)))

(defun list-stack (frame &key count)
  "Return a list of visible frames starting at FRAME and moving upward.
If COUNT is provided, at most that many frames are returned."
  (let ((l nil))
    (map-stack
     (lambda (frame)
       (declare (core:lambda-name list-stack-lambda))
       (push frame l))
     frame
     :count count)
    (nreverse l)))

;;; Navigating a coherent backtrace

;;; Call the function on each visible frame.
;;; The function receives two arguments: the base frame,
;;; and an ID number. These numbers are assigned from
;;; zero, increase monotonically, and are not affected by
;;; visibility. If all frames are visible these IDs will
;;; be contiguous as well. If COUNT is provided, at most
;;; that many visible frames have the function called on them.
(defun map-indexed-stack (function base &key count)
  "Like MAP-STACK, except the function is called with two arguments: the frame, and an index number.
These index numbers start with base = 0 and increase by 1 for each frame, visible or not. This means that frames from the same WITH-STACK will have consistent indices even if visibility rules are changed between MAP-INDEXED-STACK calls.
Note that as with MAP-STACK, only visible frames are used with respect to the COUNT."
  (loop for f = base then (frame-up f)
        for i from 0
        with c = 0
        when (frame-visible-p f)
          do (funcall function f i)
             (incf c)
        until (eq (frame-up f) *stack-top*)
        until (and count (= c count))))

(defun goto (base i)
  "Return the frame I up from the base frame, where I is an index as in MAP-INDEXED-STACK.
Note that as such, the frame returned may not be visible."
  (loop repeat i
        if (eq (frame-up base) *stack-top*) return base
        else do (setf base (frame-up base)))
  base)

(defun map-backtrace (function &key count (delimited t))
  "Like MAP-STACK, but implicitly uses WITH-STACK to obtain and use the current stack."
  (with-stack (stack :delimited delimited)
    (map-stack function stack :count count)))

(defun map-indexed-backtrace (function &key count (delimited t))
  "Like MAP-INDEXED-STACK, but implicitly uses WITH-STACK to obtain and use the current stack."
  (with-stack (stack :delimited delimited)
    (map-indexed-stack function stack :count count)))

;;; Some filters

(defun non-lisp-frame-p (frame)
  (not (eq (frame-language frame) :lisp)))

(defparameter *hidden-packages* nil)

(defun hide-package (package-designator)
  "Mark frames whose functions are in the given package as invisible. See FUNCTION-NAME-PACKAGE."
  (pushnew (find-package package-designator) *hidden-packages*
           :test #'eq)
  *hidden-packages*)

(defun unhide-package (package-designator)
  "Undo HIDE-PACKAGE for the given package."
  (setq *hidden-packages*
        (delete (find-package package-designator) *hidden-packages*
                :test #'eq))
  *hidden-packages*)

(defun package-hider (frame)
  (member (function-name-package (frame-function-name frame))
          *hidden-packages*))

(defparameter *hidden-fnames*
  '(error cerror apply funcall invoke-debugger
    core:universal-error-handler
    core:apply0 core:apply1 core:apply2 core:apply3 core:apply4
    core::catch-lambda core::throw-lambda
    core::unwind-protected-lambda core::unwind-cleanup-lambda
    core::mvc-argument-lambda core::progv-lambda
    clos::interpret-dtree-program clos::dispatch-miss-va
    clos::perform-outcome
    clos::dispatch-miss clos::invalidated-dispatch-function
    clos::invalidated-discriminating-function
    clos::combine-method-functions.lambda
    clos::interpreted-discriminating-function))

(defun hide (function-name)
  "Mark frames whose functions have the given name as invisible."
  (pushnew function-name *hidden-fnames* :test #'equal)
  *hidden-fnames*)

(defun unhide (function-name)
  "Undo a HIDE on the given function name."
  (setq *hidden-fnames*
        (delete function-name *hidden-fnames* :test #'equal))
  *hidden-fnames*)

(defun fname-hider (frame)
  (member (frame-function-name frame) *hidden-fnames*
          :test #'equal))

(defun unhide-all ()
  "Unhide all hidden packages and function names."
  (setq *hidden-packages* nil *hidden-fnames* nil))

;;; Miscellaneous.

;;; 
;;; Used above and by SLDB. FIXME: Robustness
(defun function-name-package (function-name)
  "Return the package a function name conceptually belongs to, or NIL if there is none.
For example, for a function-name that is a symbol, returns that symbol's package, and for a function-name that is a list (SETF symbol), returns that symbol's package."
  (cond ((null function-name) nil) ; information is lacking
        ((stringp function-name) nil) ; C/C++ frame, inapplicable
        ((eq function-name 'cl:lambda) nil) ; anonymous
        ((symbolp function-name) (symbol-package function-name))
        ((and (consp function-name)
              (consp (cdr function-name))
              (symbolp (second function-name))
              (null (cddr function-name))
              ;; Standard SETF name, or one of our FLET or LABELS names.
              (member (second function-name) '(setf flet labels)))
         (symbol-package (second function-name)))
        ((and (consp function-name)
              (eq (car function-name) 'cl:lambda))
         nil)
        ;; shrug.
        (t nil)))

;;; FIXME use conditions.lsp defs

(defun safe-prin1 (object &optional output-stream-designator)
  "PRIN1 the OBJECT to the given stream (default *STANDARD-OUTPUT*).
Extra care is taken to ensure no errors are signaled. If the object cannot be printed, an unreadable representation is returned instead."
  (let ((string
          (handler-case
              ;; First just try it.
              (prin1-to-string object)
            (serious-condition ()
              (handler-case
                  ;; OK, print type.
                  ;; FIXME: Should print a pointer too but I don't actually
                  ;; know how to get that from Lisp.
                  (let ((type (type-of object)))
                    (concatenate 'string
                                 "#<error printing "
                                 (prin1-to-string type)
                                 ">"))
                (serious-condition ()
                  ;; Couldn't print the type. Give up entirely.
                  "#<error printing object>"))))))
    (write-string string output-stream-designator)))

(defun display-fname (fname &optional output-stream-designator)
  (if (stringp fname) ; C/C++ frame
      (write-string fname output-stream-designator)
      (safe-prin1 fname output-stream-designator)))

(defun prin1-frame-call (frame &optional output-stream-designator)
  "PRIN1 a representation of the given frame's call to the stream (default *STANDARD-OUTPUT*).
Extra care is taken to ensure no errors are signaled, using SAFE-PRIN1."
  (let ((fname (frame-function-name frame))
        (args (frame-arguments frame)))
    (if (null args)
        (display-fname fname output-stream-designator)
        (progn (write-char #\( output-stream-designator)
               (display-fname fname output-stream-designator)
               (loop for arg in args
                     do (write-char #\Space output-stream-designator)
                        (safe-prin1 arg output-stream-designator))
               (write-char #\) output-stream-designator))))
  frame)

(defun princ-code-source-line (code-source-line &optional output-stream-designator)
  "Write a human-readable representation of the CODE-SOURCE-LINE to the stream."
  (let ((string
          (handler-case
              (format nil "~a:~d"
                      (code-source-line-pathname code-source-line)
                      (code-source-line-line-number code-source-line))
            (serious-condition () "error while printing code-source-line"))))
    (write-string string output-stream-designator)))

(defun print-stack (base &key (stream *standard-output*) count source-positions)
  "Write a representation of the stack beginning at BASE to STREAM.
If COUNT is provided and not NIL, at most COUNT frames are printed.
If SOURCE-POSITIONS is true, a description of the source position of each frame's call will be printed."
  (map-indexed-stack
   (lambda (frame i)
     (format stream "~&~4d: " i)
     (prin1-frame-call frame stream)
     (when source-positions
       (let ((fsp (frame-source-position frame)))
         (when fsp
           (fresh-line stream)
           (write-string "    |---> " stream)
           (princ-code-source-line fsp stream)))))
   base
   :count count)
  (fresh-line stream)
  (values))

(defun print-backtrace (&key (stream *standard-output*) count source-positions
                          (delimited t))
  "Write a current backtrace to STREAM.
If COUNT is provided and not NIL, at most COUNT frames are printed.
If SOURCE-POSITIONS is true, a description of the source position of each frame's call will be printed.
Other keyword arguments are passed to WITH-STACK."
  (with-stack (stack :delimited delimited)
    (print-stack stack :stream stream :count count
                 :source-positions source-positions)))
