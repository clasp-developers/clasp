(in-package #:clasp-cleavir)

(defclass function-info ()
  ((%enter-instruction :initarg :enter-instruction :accessor enter-instruction)
   ;; a list of CATCH-INSTRUCTIONs, i.e. nonlocal entrances, in the function.
   (%catches :initform nil :accessor catches)
   ;; The lexical location that stores the frame marker.
   (%frame-marker :accessor frame-marker)
   ;; Whether the function has high enough optimize debug to save arguments.
   (%debug-on :initform nil :accessor debug-on)
   ;; Used in several places, unfortunately miscellaneously.
   (%calling-convention :accessor calling-convention)
   ;; Used for source tracking.
   (%metadata :accessor metadata)))

;;; Abstraction for elsewhere
(defun catches-p (function-info)
  (not (null (catches function-info))))

(defun make-catch-map (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (typep instruction 'cleavir-ir:catch-instruction)
         (push instruction (gethash owner result)))))
    result))

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
         (cleavir-ir:catch-instruction
          (push instruction (catches (gethash owner result)))))
       ;; If an instruction is marked as wanting good debug info, the whole function gets it.
       (when (has-policy-p instruction 'maintain-shadow-stack)
         (setf (debug-on (gethash owner result)) t)))
     initial-instruction)
    result))

;;; This reduces HIR catches to MIR catches.
;;; This means it converts catches to mere assignments as described above,
;;; and inserts one instruction right after the ENTER to make the actual continuation.
(defun lower-enter-catches (enter info)
  (let ((catches (catches info)))
    (unless (null catches)
      (let ((frame (cleavir-ir:new-temporary "FRAME")))
        (cleavir-ir:insert-instruction-after
         (let ((cleavir-ir:*policy* (cleavir-ir:policy enter)))
           (cc-mir:make-save-frame-instruction frame))
         enter)
        (loop for catchn from 0 for catch in catches
              do (change-class catch 'cc-mir:assign-catch-instruction
                               :inputs (list frame) :go-index catchn))
        (setf (frame-marker info) frame)))))

(defun lower-catches (function-info-map)
  (maphash #'lower-enter-catches function-info-map))
