(defpackage #:clasp-debug
  (:use #:cl)
  ;; low level
  (:export #:code-source-line
           #:code-source-line-pathname
           #:code-source-line-line-number
           #:code-source-line-column)
  (:import-from #:core #:frame)
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

(in-package #:clasp-debug)

;;; Low level interface

;;; FIXME: Unify source position stuff somehow.
;;; This one could be a core:source-pos-info, except
;;; we don't have an offset into the file.
(defstruct (code-source-line (:type vector) :named)
  pathname line-number column)

(defun frame-up (frame)
  "Return the frame directly above the current frame, or NIL if there are no more frames.

This notion of direction is arbitrary, and unrelated to any machine stack growth directions. The frame for the caller is 'above' the frame for what it calls.

This function ignores visibility and may not halt at delimiters. UP is the higher level interface."
  (core:backtrace-frame-up frame))

(defun frame-down (frame)
  "Return the frame directly below the current frame, or NIL if there are no more frames.

This notion of direction is arbitrary, and unrelated to any machine stack growth directions. The frame for the callee is 'below' the frame for what called it.

This function ignores visibility and may not halt at delimiters. DOWN is the lower level interface."
  (core:backtrace-frame-down frame))

(defun frame-function (frame)
  "Return the function being called in this frame, or NIL if it is not available.
The FRAME-FUNCTION-xxx functions may return meaningful information even when this function fails."
  (core:backtrace-frame-closure frame))

(defun frame-arguments (frame)
  "Return the list of arguments to the call for this frame."
  (coerce (core:backtrace-frame-arguments frame) 'list))

(defun frame-locals (frame)
  "Return an alist of local lexical variables and their values at the continuation the frame represents. The CARs are variable names and CDRs their values.
Multiple bindings with the same name may be returned, as there is no notion of lexical scope in this interface."
  ;; TODO: This is not a real solution, at all.
  (let* ((fname (clasp-debug:frame-function-name frame))
         (args (clasp-debug:frame-arguments frame)))
    ;; KLUDGE (on top of a kludge) to do better with method arguments
    ;; We have two kinds of method functions - fast and not - but we
    ;; name them the same. The former we can treat normally, but the
    ;; latter have a vaslist of the real arguments as their first, and
    ;; the next method list as the second.
    (if (and (consp fname)
             (eq (first fname) 'cl:method)
             (= (length args) 2)
             (core:vaslistp (first args)))
        (let ((method-args (core:list-from-va-list (first args)))
              (next-methods (second args)))
          (append
           (loop for arg in method-args for i from 0
                 collect (cons (intern (format nil "ARG~d" i) :cl-user)
                               arg))
           (list (cons 'cl-user::next-methods next-methods))))
        (loop for arg in args for i from 0
              collect (cons (intern (format nil "ARG~d" i) :cl-user)
                            arg)))))

(defun frame-source-position (frame)
  "Return a CODE-SOURCE-LINE object representing the source file position for this frame's call, or NIL if no information is available."
  (multiple-value-bind (pathname line-number column)
      (core::code-source-position
       (core:backtrace-frame-return-address frame))
    (if (null pathname)
        nil
        (make-code-source-line
         :pathname pathname
         :line-number line-number
         :column column))))

(defun frame-language (frame)
  "Return a marker of the programming language of the code for this frame. May be :LISP or :C++."
  (core:backtrace-frame-type frame))

(defun frame-function-name (frame)
  "Return the name of the function being called in this frame. This will be one of:
* A symbol.
* A list (SETF symbol).
* A list that is one of Clasp's function names, such as (FLET ...), (LABELS ...), or (METHOD ...).
* A string, representing a C or C++ function."
  (or (core:backtrace-frame-function-name frame)
      ;; _RUN-ALL names don't have proper function names for some reason, so fall back.
      (core:backtrace-frame-raw-name frame)))

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
  ;; TODO: Use function-start-address, function-end-address
  (let ((f (frame-function frame)))
    (when f (disassemble f))))

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

;;; Mid level interface: Navigate frames nicely

;; The frame one beyond the selected stack (i.e. exclusive limit)
(defvar *stack-top*)
;; The frame beginning the selected stack (i.e. inclusive limit)
(defvar *stack-bot*)

(defun find-bottom-frame (start)
  ;; Look for a frame truncation marker.
  ;; If none is found, use the start.
  ;; If a cap is found, don't look for a bottom beyond that.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) start)
    (when (truncation-frame-p f)
      ;; A truncation frame is a call to call-with-truncated-stack,
      ;; and we don't really want to keep that around, so.
      ;; If truncated-frame-p does something else later we should
      ;; maybe do something else here too.
      (return (frame-up f)))))

(defun find-top-frame (start)
  ;; Look for a frame cap.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) f)))

(defun call-with-stack (function &key (delimited t))
  "Functional form of WITH-STACK."
  ;; FIXME: Consing up the list and then discarding it is silly.
  (core:call-with-frame
   (lambda (frame)
     (declare (core:lambda-name call-with-stack-lambda))
     (let* ((*stack-bot* (if delimited (find-bottom-frame frame) frame))
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
    clos::perform-outcome clos::do-dispatch-miss
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

;;; Called by SIGINFO handler, see gctools/interrupt.cc
(defun information-interrupt (&rest args)
  (core:safe-backtrace))
