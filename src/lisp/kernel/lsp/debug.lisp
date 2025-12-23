(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLASP-DEBUG")
    (make-package "CLASP-DEBUG" :use '("CL")))
  (in-package #:clasp-debug)
  ;; we intern several symbols below
  (ext:add-implementation-package "CLASP-DEBUG" "CLOS")
  (flet ((%export (names)
           (export (mapcar (lambda (s) (intern (symbol-name s))) names))))
    (%export '(#:code-source-line-pathname
               #:code-source-line-line-number
               #:code-source-line-column))
    ;; TODO: Rename core:debugger-frame to frame, reexport that
    (%export '(#:frame))
    (%export '(#:frame-up #:frame-down))
    (%export '(#:frame-function #:frame-arguments
               #:frame-locals #:frame-source-position
               #:frame-language))
    (%export '(#:frame-function-name
               #:frame-function-lambda-list
               #:frame-function-source-position
               #:frame-function-form
               #:frame-function-documentation))
    (%export '(#:disassemble-frame))
    ;; frame selection
    (%export '(#:with-truncated-stack #:truncation-frame-p
               #:with-capped-stack #:cap-frame-p))
    (%export '(#:*frame-filters*))
    ;; mid level
    (%export '(#:call-with-stack #:with-stack))
    (%export '(#:up #:down #:visible))
    (%export '(#:map-stack #:list-stack))
    ;; defined later in conditions.lisp
    (%export '(#:safe-prin1 #:prin1-frame-call
               #:princ-code-source-line
               #:print-stack))
    ;; high level
    (%export '(#:map-indexed-stack #:goto))
    (%export '(#:print-backtrace ; in conditions.lisp
               #:map-backtrace
               #:map-indexed-backtrace))
    (%export '(#:hide-package #:unhide-package
               #:hide #:unhide #:unhide-all))
    ;; misc
    (%export '(#:function-name-package))
    ;; stepper
    (%export '(#:step-condition #:step-form #:step-call
               #:step-into #:step-over))
    (import '(core:set-breakstep core:unset-breakstep core:breakstepping-p))
    (export '(core:set-breakstep core:unset-breakstep core:breakstepping-p))))

;;; Low level interface

;;; these should maybe be deprecated, since they just go through source
;;; position info accessors.
(defun code-source-line-pathname (spi)
  (core:file-scope-pathname
   (core:file-scope (core:source-pos-info-file-handle spi))))
(defun code-source-line-line-number (spi) (core:source-pos-info-lineno spi))
(defun code-source-line-column (spi) (core:source-pos-info-column spi))

(defun frame-up (frame)
   "Return the frame directly above the current frame, or NIL if there are no more frames.
This notion of direction is arbitrary, and unrelated to any machine stack growth directions. The frame for the caller is 'above' the frame for what it calls.
This function ignores visibility and may not halt at delimiters. UP is the higher level interface."
  (core:debugger-frame-up frame))
(defun frame-down (frame)
  "Return the frame directly below the current frame, or NIL if there are no more frames.
This notion of direction is arbitrary, and unrelated to any machine stack growth directions. The frame for the callee is 'below' the frame for what called it.
This function ignores visibility and may not halt at delimiters. DOWN is the lower level interface."
  (core:debugger-frame-down frame))
(defun frame-function-name (frame)
    "Return the name of the function being called in this frame. This will be one of:
* A symbol.
* A list (SETF symbol).
* A list that is one of Clasp's function names, such as (FLET ...), (LABELS ...), or (METHOD ...).
* A string, representing a C or C++ function."
  (core:debugger-frame-fname frame))
(defun frame-source-position (frame)
  "Return a CODE-SOURCE-LINE object representing the source file position for this frame's call, or NIL if no information is available."
  (or (core:debugger-frame-source-position frame)
      ;; better than nothing
      (frame-function-source-position frame)))
(defun frame-function-description (frame)
  (core:debugger-frame-function-description frame))
(defun frame-language (frame)
  "Return a marker of the programming language of the code for this frame. May be :LISP or :C++."
  (core:debugger-frame-lang frame))
(defun frame-function (frame)
  "Return the function being called in this frame, or NIL if it is not available.
Note that this function may be less reliable than the FRAME-FUNCTION-etc functions."
  (core:debugger-frame-closure frame))
(defun frame-arguments (frame)
  "Return the list of arguments to the call for this frame, and T.
If the arguments are not available, returns NIL NIL."
  (cond ((core:debugger-frame-args-available-p frame)
         ;; If there are args, use em
         (values (core:debugger-frame-args frame) t))
        ((and (eq (core:debugger-frame-lang frame) :lisp)
              (not (core:debugger-frame-xep-p frame))
              (core:debugger-frame-xep-p (core:debugger-frame-up frame))
              (equal (core:debugger-frame-fname frame)
                     (core:debugger-frame-fname
                      (core:debugger-frame-up frame))))
         ;; If this is a local function called from its XEP, grab the XEP's
         ;; arguments
         (let ((xep (core:debugger-frame-up frame)))
           (values (core:debugger-frame-args xep)
                   (core:debugger-frame-args-available-p xep))))
        (t ; nothin
         (values nil nil))))

(defun lambda-list-alist (lambda-list arguments eval &aux bindings)
  (flet ((bind-var (var init-form &optional supplied-p test-form value)
           (cond
             (test-form ; the value is valid so use it
               (push (cons var value) bindings))
             (eval ; no value passed but we have an eval function
               (push (cons var (funcall eval init-form bindings)) bindings))
             ((constantp init-form) ; no eval function but init-form is constant
               (push (cons var (ext:constant-form-value init-form)) bindings)))
           (when supplied-p ; bind supplied-p if requested
             (push (cons supplied-p (and test-form t)) bindings))))
    (multiple-value-bind (required optional rest key-flag keys allow-other-keys aux)
        (core:process-lambda-list lambda-list 'function)
      (declare (ignore key-flag allow-other-keys))
      (dolist (var (cdr required))
        (push (cons var (pop arguments)) bindings))
      (do* ((opts (cdr optional) (cdddr opts)))
           ((null opts))
        (let ((var (first opts)) (init-form (second opts))
              (supplied-p (third opts)))
          (bind-var var init-form supplied-p arguments (pop arguments))))
      (when rest
        (push (cons rest (copy-list arguments)) bindings))
      (do* ((keys (cdr keys) (cddddr keys)))
           ((null keys))
        (let ((keyk (first keys)) (var (second keys))
              (init-form (third keys)) (supplied-p (fourth keys)))
          (multiple-value-bind (indicator value)
              (get-properties arguments (list keyk))
            (bind-var var init-form supplied-p indicator value))))
      (do* ((auxs aux (cddr auxs)))
           ((null auxs))
        (bind-var (first auxs) (second auxs)))
      (nreverse bindings))))

(defun locals-from-arguments (frame eval &aux (fname (frame-function-name frame)))
  (multiple-value-bind (args args-available) (frame-arguments frame)
    (multiple-value-bind (lambda-list lambda-list-available)
        (frame-function-lambda-list frame)
      (cond
        ((not args-available) nil)
        (lambda-list-available
         (lambda-list-alist lambda-list args eval))
        ;; The frame is missing a lambda list so fallback to just naming the arguments sequentially.
        ((and (consp fname)
           (eq (first fname) 'cl:method)
           (= (length args) 2)
           (core:vaslistp (first args)))
         ;; This is non-fast method. The real arguments are a vaslist in
         ;; the first element and the method list is in the second element.
         (let ((method-args (core:list-from-vaslist (first args)))
               (next-methods (second args)))
           (append
            (let ((result ()))
              (do ((args method-args (cdr method-args))
                   (i 0 (1+ i)))
                  ((null args) (nreverse result))
                (push (cons (intern (format nil "ARG~d" i) :cl-user)
                            (first args))
                      result)))
            (list (cons 'cl-user::next-methods next-methods)))))
        (t
         ;; This is a fast method. Just treat it normally.
         (let ((result ()))
           (do ((args args (cdr args))
                (i 0 (1+ i)))
               ((null args) (nreverse result))
             (push (cons (intern (format nil "ARG~d" i) :cl-user)
                         (first args))
                   result))))))))

(defun frame-locals (frame &key eval)
  "Return an alist of local lexical/special variables and their values at the continuation the frame
  represents. The CARs are variable names and CDRs their values. Multiple bindings with the same
  name may be returned, as there is no notion of lexical scope in this interface. By default
  init-forms in the frame's lambda list that require evaluation and are not constant will not be
  returned. Passing a function (lambda (form locals) ...) via the eval key will result in init-forms
  being evaluated with this function along with the current locals passed as the second argument."
  (append
   ;; This only gives anything for bytecode functions right now.
   (core:debugger-frame-locals frame)
   (if (eq (core:debugger-frame-lang frame) :bytecode)
       ;; bytecode frames already have good locals, so don't bother w/arguments
       nil
       (locals-from-arguments frame eval))))

(defun frame-function-lambda-list (frame)
  "Return the lambda list of the function being called in this frame, and a second value indicating success. This function may fail, in which case the first value is undefined and the second is NIL. In success the first value is the lambda list and the second value is true."
  (let ((fd (frame-function-description frame)))
    (if fd
        (values (core:function-description-lambda-list fd) t)
        (values nil nil))))
(defun frame-function-source-position (frame)
  "Return a CODE-SOURCE-LINE object representing the source file position for this frame's function."
  (let ((fd (frame-function-description frame)))
    (when fd
      (let ((path (core:function-description-source-pathname fd)))
        (when path
          (core:make-source-pos-info
           :filename (namestring path)
           :lineno (core:function-description-lineno fd)
           :column (core:function-description-column fd)))))))
(defun frame-function-form (frame)
  "Return a lambda expression for this frame's function if it's available, or else NIL."
  ;; TODO
  (declare (ignore frame))
  nil)
(defun frame-function-documentation (frame)
  "Return the docstring for this frame's function if it exists and is available, or else NIL."
  (let ((fd (frame-function-description frame)))
    (when fd
      (core:function-description-docstring fd))))
(defun disassemble-frame (frame)
  "Disassemble this frame's function to *standard-output*."
  (declare (ignore frame)))

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

(defun find-bottom-frame (start)
  ;; Look for a frame truncation marker.
  ;; If none is found, use the start.
  ;; If a cap is found, don't look for a bottom beyond that.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) start)
    (when (truncation-frame-p f)
      ;; A truncation frame is a call to call-with-truncated-stack, which we
      ;; don't want to keep in the backtrace, so we go up one more.
      (return (frame-up f)))))
(defun find-top-frame (start)
  ;; Look for a call to call-with-capped-stack.
  (do ((f start (frame-up f)))
      ((or (null f) (cap-frame-p f)) f)))

(defun call-with-stack (function &key (delimited t))
  "Functional form of WITH-STACK."
  (core:call-with-frame
   (lambda (floor)
     (let* ((*stack-bot* (if delimited (find-bottom-frame floor) floor))
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

(defparameter *frame-filters* (list 'c++-frame-p
                                    'redundant-xep-p
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
  (dotimes (i n frame)
    (declare (ignorable i))
    (setf frame (up1 frame *stack-top*))))

(defun down (frame &optional (n 1))
  "Return the nth visible frame below the given frame, or if there are not that many visible frames above, return the bottommost frame."
  (dotimes (i n frame)
    (declare (ignorable i))
    (setf frame (down1 frame *stack-bot*))))

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
  (do ((f (visible frame) (up f))
       (i 0 (1+ i)))
      ((or (and count (= i count))
           ;; This is a bit inefficient, but interactive
           ;; debuggers aren't usually fast path
           (eq (up f) f))
       (values))
    (funcall function f)))

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
  (do ((f base (frame-up f))
       (i 0 (1+ i))
       (c 0))
      (nil)
    (when (frame-visible-p f)
      (funcall function f i)
      (incf c))
    (when (or (eq (frame-up f) *stack-top*)
              (and count (= c count)))
      (return))))

(defun goto (base i)
  "Return the frame I up from the base frame, where I is an index as in MAP-INDEXED-STACK.
Note that as such, the frame returned may not be visible."
  (dotimes (n i base)
    (declare (ignorable n))
    (if (eq (frame-up base) *stack-top*)
        (return base)
        (setf base (frame-up base)))))

(defun map-backtrace (function &key count (delimited t))
  "Like MAP-STACK, but implicitly uses WITH-STACK to obtain and use the current stack."
  (with-stack (stack :delimited delimited)
    (map-stack function stack :count count)))

(defun map-indexed-backtrace (function &key count (delimited t))
  "Like MAP-INDEXED-STACK, but implicitly uses WITH-STACK to obtain and use the current stack."
  (with-stack (stack :delimited delimited)
    (map-indexed-stack function stack :count count)))

;;; Some filters

(defun c++-frame-p (frame)
  (eq (frame-language frame) :c++))

;;; With how Clasp works at this time, any given Lisp function has two "real"
;;; functions - an eXternal Entry Point (XEP), and a "local function". The XEP
;;; takes a standard calling convention and does nothing but parse arguments and
;;; call the local function, which has the actual body of code.
;;; The XEP should do a tail call, but usually doesn't for whatever reason,
;;; so we have a XEP and local function on the stack for essentially every Lisp
;;; function call.
;;; We hide XEPs that have a local function call with the same name in the next
;;; frame down, to avoid redundancy; but the local inherits the arguments from
;;; the XEP - see frame-arguments above.
(defun redundant-xep-p (frame)
  (and (eq (frame-language frame) :lisp)
       (core:debugger-frame-xep-p frame)
       (let ((down (core:debugger-frame-down frame)))
         (and down
              (eq (frame-language down) :lisp)
              (not (core:debugger-frame-xep-p down))
              (equal (core:debugger-frame-fname down)
                     (core:debugger-frame-fname frame))))))

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
  '(apply funcall invoke-debugger
    core:universal-error-handler
    core:apply0 core:apply1 core:apply2 core:apply3 core:apply4
    core::catch-lambda core::throw-lambda
    core::unwind-protected-lambda core::unwind-cleanup-lambda
    core::mvc-argument-lambda core::progv-lambda
    clos::dispatch-miss-va
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

;;;

(defun primitive-display-fname (fname &optional output-stream-designator)
  (if (stringp fname) ; C/C++ frame
      (write-string fname output-stream-designator)
      (prin1 fname output-stream-designator)))

(defun primitive-prin1-frame-call (frame &optional os-designator)
  (let ((fname (frame-function-name frame)))
    (multiple-value-bind (args availablep)
        (frame-arguments frame)
      (cond (availablep
             (write-char #\( os-designator)
             (primitive-display-fname fname os-designator)
             (dolist (arg args)
               (write-char #\Space os-designator)
               (prin1 arg os-designator))
             (write-char #\) os-designator))
            (t (primitive-display-fname fname os-designator)))))
  frame)

(defun primitive-princ-code-source-line (code-source-line
                                         &optional os-designator)
  (format os-designator "~a:~d"
          (code-source-line-pathname code-source-line)
          (code-source-line-line-number code-source-line)))

(defun primitive-print-stack (base &key (stream *standard-output*)
                                     count source-positions)
  (map-indexed-stack
   (lambda (frame i)
     (fresh-line stream)
     (format stream "~d: " i)
     (primitive-prin1-frame-call frame stream)
     (when source-positions
       (let ((fsp (frame-source-position frame)))
         (when fsp
           (fresh-line stream)
           (write-string "    |---> " stream)
           (primitive-princ-code-source-line fsp stream)))))
   base :count count)
  (fresh-line stream)
  (values))

(defun primitive-print-backtrace (&key (stream ext:+process-standard-output+)
                                    count source-positions (delimited t))
  (with-stack (stack :delimited delimited)
    (primitive-print-stack stack :stream stream :count count
                                 :source-positions source-positions)))

(defun sys:primitive-print-backtrace ()
  (clasp-debug::primitive-print-backtrace))

