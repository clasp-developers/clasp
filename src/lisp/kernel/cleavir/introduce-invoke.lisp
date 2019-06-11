(in-package #:clasp-cleavir)

(defclass function-info ()
  ((%enter-instruction :initarg :enter-instruction :accessor enter-instruction)
   ;; a list of CATCH-INSTRUCTIONs, i.e. nonlocal entrances, in the function.
   (%catches :initform nil :accessor catches)
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
         (cleavir-ir:catch-instruction
          (push instruction (catches (gethash owner result)))))
       ;; If an instruction is marked as wanting good debug info, the whole function gets it.
       (when (has-policy-p instruction 'save-register-args)
         (setf (debug-on (gethash owner result)) t)))
     initial-instruction)
    result))

;; HT from instructions to their go-indices, for unwinding
(defvar *instruction-go-indices*)

(defun make-go-indices () (make-hash-table :test #'eq))

(defun instruction-go-index (instruction)
  (or (gethash instruction *instruction-go-indices*) ; bound by translate
      (error "BUG: Instruction not a valid unwind target: ~a" instruction)))

(defun (setf instruction-go-index) (index instruction)
  (setf (gethash instruction *instruction-go-indices*) index))

;;; This inserts save-frame instructions when necessary, and assigns unwind targets IDs.
(defun lower-enter-catches (enter info)
  (let ((catches (catches info)))
    ;; If there are no catches, we don't need a save-frame or the rest.
    (unless (null catches)
      ;; Insert the same-frame.
      (cleavir-ir:insert-instruction-after
       (let ((cleavir-ir:*policy* (cleavir-ir:policy enter))
             (cleavir-ir:*dynamic-environment*
               (cleavir-ir:dynamic-environment enter)))
         (cc-mir:make-save-frame-instruction))
       enter)
      ;; Assign a go-index to every instruction that's an abnormal successor to a catch.
      ;; FIXME: maybe check for pathological case of overflowing i?
      (loop with i = 0
            for catch in catches
            for abnormal = (rest (cleavir-ir:successors catch))
            do (loop for succ in abnormal
                     do (setf (instruction-go-index succ) i)
                        (incf i))))))

(defun lower-catches (function-info-map)
  (maphash #'lower-enter-catches function-info-map))
