(in-package #:clasp-cleavir)

(defclass function-info ()
  ((%enter-instruction :initarg :enter-instruction :accessor enter-instruction)
   ;; a list of WIND-TO-INSTRUCTIONs, i.e. nonlocal entrances, in the function.
   (%wtos :initform nil :accessor wtos)
   ;; The LLVM Value for the frame marker.
   (%frame-value :accessor frame-value)
   ;; Whether the function has high enough optimize debug to save arguments.
   (%debug-on :initform nil :accessor debug-on)
   ;; Used in several places, unfortunately miscellaneously.
   (%calling-convention :accessor calling-convention)
   ;; Used for source tracking.
   (%metadata :accessor metadata)))

(defun make-function-info-map (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    ;; Do the basic construction: collect catches.
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       ;; map-instructions-with-owner guarantees we hit the enter before anything it owns,
       ;; so this should create the info before setting any properties.
       (typecase instruction
         (cleavir-ir:enter-instruction
          (setf (gethash instruction result)
                (make-instance 'function-info :enter-instruction instruction)))
         (cleavir-ir:wind-to-instruction
          (push instruction (wtos (gethash owner result)))))
       ;; If an instruction is marked as wanting good debug info, the whole function gets it.
       (when (has-policy-p instruction 'maintain-shadow-stack)
         (setf (debug-on (gethash owner result)) t)))
     initial-instruction)
    result))

(defun instruction-go-index (instruction)
  (cc-mir:go-index instruction))

;;; This inserts save-frame instructions when necessary, and assigns unwind targets IDs.
(defun lower-enter-catches (enter info)
  (let ((wtos (wtos info)))
    ;; If there are no entrances, we don't need a save-frame or the rest.
    (unless (null wtos)
      ;; Insert the save-frame.
      (cleavir-ir:insert-instruction-after
       (let ((cleavir-ir:*policy* (cleavir-ir:policy enter)))
         (cc-mir:make-save-frame-instruction))
       enter)
      ;; Assign a go-index to every WIND-TO entrance.
      ;; FIXME: maybe check for pathological case of overflowing i?
      (loop for wto in wtos
            for i from 0
            do (change-class wto 'cc-mir:numbered-wind-to-instruction
                             :go-index i)))))

(defun lower-catches (function-info-map)
  (maphash #'lower-enter-catches function-info-map))
