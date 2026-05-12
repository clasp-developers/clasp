
;;; Stack bounds sanity checks.
;;; Verify that the stack pointer used for GC lies within the thread's
;;; registered stack bounds.

(defun stack-bounds-test ()
  (multiple-value-bind (low sp high)
      (gctools:stw-stack-bounds)
    (if (<= low sp high)
        (values nil nil nil)
        (values low sp high))))

(test stw-stack-bounds-main-thread
      (stack-bounds-test)
      (nil nil nil))

(test stw-stack-bounds-new-thread
      (mp:process-join
       (mp:process-run-function nil #'stack-bounds-test))
      (nil nil nil))

(test stw-stack-bounds-multiple-threads
      (let ((procs (loop repeat 4
                         collect (mp:process-run-function
                                  nil #'stack-bounds-test))))
        (mapcar (lambda (p) (multiple-value-list (mp:process-join p))) procs))
      (((nil nil nil) (nil nil nil) (nil nil nil) (nil nil nil))))

;;; ----------------------------------------------------------------------
;;;
;;; This is a separate function in a perhaps-futile effort to prevent
;;; compiler optimizations from keeping the cons "reachable" when it's
;;; not in the source.
;;; Note that while we can use weak pointers to see if an object is accessible,
;;; boehm won't actually run finalizers until some point after weak pointers
;;; are splatted (according to gc.h) and other garbage collectors really
;;; don't make many guarantees at all about if or when finalizers run.
;;; So these tests are inherently dicey.

(defun finalized-objects (maker n)
  (let (;; We store the count in a cons so we can use atomic-incf.
        ;; FIXME: Better would be supporting atomic ops on lexicals.
        (countc (list 0)))
    (flet ((inc (a)
             (declare (ignore a))
             (mp:atomic-incf (car countc))))
      (values (loop repeat n
                    for object = (funcall maker)
                    do (gctools:finalize object #'inc)
                    collect (ext:make-weak-pointer object))
              (lambda () (mp:atomic (car countc)))))))
(declaim (notinline finalized-objects))

(defun test-finalizers (maker n)
  (multiple-value-bind (wps counter)
      (finalized-objects maker n)
    ;; Try to GC until the objects become unreachable, and then
    ;; try to force finalizers to be invoked for good measure.
    ;; Max 10 iterations so we don't hang if something goes wrong.
    (loop repeat 10
          do (gctools:garbage-collect))
    (gctools:invoke-finalizers)
    ;; Since finalizers are inherently a little unreliable, we just check
    ;; that the count of ran finalizers and uncollected objects sums to
    ;; at most n (i.e. no accessible object was finalized), and that
    ;; at least 95% of the finalizers ran. This idea is cribbed from SBCL.
    ;; SBCL also displays traces for any unsplatted objects, which might be
    ;; nice to do if we ever grow that capability.
    (let* ((count (funcall counter))
           (success-fraction (/ count n))
           (sum (+ count (count-if #'ext:weak-pointer-valid wps))))
      (values (or (> success-fraction (/ 95 100)) success-fraction)
              (or (>= n sum) sum)))))

(test finalizers-cons
      (test-finalizers (lambda () (make-list 5)) 100)
      (t t)
      :description "Check if list of cons finalizers were executed")

(test finalizers-cons-remove
      (let ((count 0))
        (let ((s (make-list 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc)))
          (gctools:definalize s))
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (0)
      :description "Check if list of cons finalizers were discarded")

(test finalizers-general
      (test-finalizers (lambda () (make-array 5)) 100)
      (t t)
      :description "Check if list of general finalizers were executed")

(test finalizers-general-remove
      (let ((count 0))
        (let ((s (make-array 5)))
          (flet ((inc (a) (declare (ignore a)) (incf count)))
            (loop repeat 5 do (gctools:finalize s #'inc)))
          (gctools:definalize s))
        ;; S is now unreachable
        (loop repeat 10 do (gctools:garbage-collect))
        count)
      (0)
      :description "Check if list of general finalizers were discarded")

;;; ----------------------------------------------------------------------
;;; gctools:traceablep tests
;;;
;;; gctools:traceablep stops the world and checks which of a set of objects
;;; are reachable via the tracer. It takes a list of weak pointers and
;;; returns a list of booleans indicating traceability.
;;;
;;; These tests are inherently a little dodgy since the tracer is free to
;;; overestimate what is alive, so tests may succeed when they should fail.
;;; In particular the global accessibility tests try to remove stack
;;; references by the time the test is done.
;;; And more perversely, the compiler could optimize out stack referenced
;;; objects before they are tested, resulting in false negatives.

;;; Object held in a special variable is reachable via global roots.
(defvar *traceablep-anchor* nil)

(test traceablep-special-variable
      (flet ((setup ()
               (let ((obj (list 'in-special)))
                 (setf *traceablep-anchor* obj)
                 (core:make-weak-pointer obj))))
        (prog1 (first (gctools:traceablep (list (setup))))
          (setf *traceablep-anchor* nil)))
      (t)
      :description "Object in a special variable is traceable via global roots")

;;; Object reachable from a special variable through a heap chain.
(test traceablep-heap-reachable
      (flet ((setup ()
               (let* ((inner (list 'inner))
                      (outer (list inner)))
                 (setf *traceablep-anchor* outer)
                 (core:make-weak-pointer outer))))
        (prog1 (first (gctools:traceablep (list (setup))))
          (setf *traceablep-anchor* nil)))
      (t)
      :description "Object reachable from a special variable through the heap is traceable")

;;; Object stored in a closure that is itself in a special variable.
(test traceablep-closure-captured
      (flet ((setup ()
               (let* ((obj (list 'in-closure))
                      (closure (lambda () obj)))
                 (setf *traceablep-anchor* closure)
                 (core:make-weak-pointer obj))))
        (prog1 (first (gctools:traceablep (list (setup))))
          (setf *traceablep-anchor* nil)))
      (t)
      :description "Object captured in a closure held in a special variable is traceable")

;;; Various global objects.
(test traceablep-globals
      (values-list
       (gctools:traceablep
        (mapcar #'core:make-weak-pointer
                (list (find-package "COMMON-LISP")
                      (pprint-dispatch '(let ((x 7)) x))
                      (find-class 'standard-class)
                      #'cons
                      (get-dispatch-macro-character #\# #\()
                      *random-state*
                      *standard-output*
                      *debugger-hook*))))
      (t t t t t t t t))

;;; FAILS WITHOUT STACK SCANNING.
;;; Object referenced only by a local variable; not reachable from any
;;; global root. Requires scanning the calling thread's stack.
;;; NOTE above comment about false negatives.
(test traceablep-stack-local
      (let ((obj (list 'stack-only)))
        (first (gctools:traceablep (list (core:make-weak-pointer obj)))))
      (t)
      :description "Object referenced only from a local variable is traceable (requires stack scanning)")

;;; Object with no live strong references should not be traceable.
;;; We allocate it in a notinline function so the reference is gone on return.
(defun make-unreachable-weak-pointer ()
  (ext:make-weak-pointer (list 'unreachable)))
(declaim (notinline make-unreachable-weak-pointer))

(test traceablep-unreachable
      (let ((wp (make-unreachable-weak-pointer)))
        (gctools:garbage-collect)
        (first (gctools:traceablep (list wp))))
      (nil)
      :description "Object with no live strong references is not traceable")
