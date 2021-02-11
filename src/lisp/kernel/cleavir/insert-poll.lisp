(in-package #:clasp-cleavir)

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
