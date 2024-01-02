(defpackage "FINALIZE-TEST"
  (:use :cl :clasp-tests))

(defvar *a*)
(defparameter *count* 0)

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
