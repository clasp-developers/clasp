;;; ===========================================================================
;;;                Q U E U E     I M P L E M E N T A T I O N
;;; ===========================================================================
;;; -- IMPLEMEMTATION NOTES ---
;;;
;;; The complete QUEUE comprised of the following files:
;;; .../src/core/queue.cc            - corresponding .cc file
;;; .../include/clasp/core/queue.h   - corresponding .h file
;;; .../src/lisp/kernel/queue.lsp    - this file
;;;
;;; --- END OF IMPLEMEMTATION NOTES ---
;;;
;;; *** THIS CODE IS TAKEN FROM SBCL IN LARGE PARTS. THE RESPECTIVE
;;;     COPYRIGHT
;;;
;;;; Written by James M. Lawrence for SBCL.
;;;; API and docstrings by Nikodemus Siivola.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.
;;;
;;; APPLIES ***.

(in-package "MP")

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Debugging MACROS

;;; (pushnew :clasp-mp.debug cl:*features*)

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; MACROS

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; FUNCTIONS

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
(defun make-queue (&key name initial-contents)
  "Returns a new QUEUE with NAME and contents of the INITIAL-CONTENTS
sequence enqueued."
  (let ((queue (%make-queue :name name)))
    (flet ((enc-1 (x)
             (enqueue x queue)))
      (declare (dynamic-extent #'enc-1))
      (map nil #'enc-1 initial-contents))
    queue))

(defun enqueue (value queue)
  "Adds VALUE to the end of QUEUE. Returns ( VALUE T ) upon success, ( NIL NIL ) otherwise."
  (declare (optimize speed))
  (%enqueue queue value))

(defun dequeue (queue)
  "Retrieves the oldest value in QUEUE and returns it as the primary value,
and T as secondary value. If the queue is empty, returns NIL as both primary
and secondary value."
  (declare (optimize speed))
  (%dequeue queue))

(defun queue-count (queue)
  "Returns the number of objects in QUEUE. Mainly useful for manual
examination of queue state, and in PRINT-OBJECT methods: inefficient as it
must walk the entire queue."
  (declare (optimize speed))
  (%queue-count queue))

(defun queue-empty-p (queue)
  "Returns T if QUEUE is empty, NIL otherwise."
  (declare (optimize speed))
  (zerop (queue-count queue)))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; E X P O R T S

(eval-when (:load-toplevel :execute :compile-toplevel)
  (export '(make-queue
            enqueue
            dequeue
            queue-count
            queue-empty-p)))
