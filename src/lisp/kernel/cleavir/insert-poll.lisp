(in-package #:clasp-cleavir)

;;; This file inserts interrupt polls into loops. Interrupt polls on entering a
;;; function are inserted in translate.lisp/layout-main-function*.
;;; The concept of "loop" here is very simplistic, including all CFG cycles.
;;; TODO: It would be nice to distinguish demonstratably finite loops from not,
;;; and have an option to only insert polls into the latter, since if polls
;;; aren't inserted into a finite loop it just means interrupts will be delayed,
;;; not entirely ignored.
;;; We could also skip poll insertion for loops that a function immediately
;;; enters.

(defun maybe-insert-poll-into-iblock (iblock)
  (let* ((start (cleavir-bir:start iblock))
         (policy (cleavir-bir:policy start)))
    (unless (< (cleavir-policy:policy-value policy 'insert-polls) 2)
      (cleavir-bir:insert-instruction-before
       (make-instance 'cleavir-bir:nvprimop
         :info (cleavir-primop-info:info 'core::%check-pending-interrupts)
         :policy policy :origin (cleavir-bir:origin start)
         :outputs () :inputs ())
       start))))

(defun insert-polls-into-function (function)
  (let ((seen nil))
    (cleavir-bir:do-iblocks (ib function)
      (when (cleavir-set:doset (pred (cleavir-bir:predecessors ib) nil)
              (when (not (member pred seen))
                (return t)))
        (maybe-insert-poll-into-iblock ib))
      (push ib seen))))

(defun insert-polls-into-module (module)
  (cleavir-bir:map-functions #'insert-polls-into-function module))
