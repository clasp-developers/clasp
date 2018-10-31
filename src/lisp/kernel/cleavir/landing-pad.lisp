(in-package :clasp-cleavir)

;;; Generates code that checks whether the unwind is to this frame.
;;; If it is, it returns the control point index, i.e. which catch to jump to.
;;; Otherwise it rethrows, going to the provided landing pad.
;;; An alternate strategy would be to test whether it's an unwind, and if not, cleanup and/or resume
;;; and if so, extract the go-index and jump table and so on in a catch block.
;;; Saves a landing pad and my comprehension.
(defun generate-match-unwind (return-value abi frame landing-pad-for-unwind-rethrow exn.slot)
  ;; We do this weird thing to get the index while still generating the other code around it.
  ;; FIXME: with-begin-end-catch should maybe abstract away so it can return an actual thing.
  (let (go-index)
    (cmp:with-begin-end-catch ((%load exn.slot "exn") exception-ptr nil)
      ;; Check if frame is correct against tagbody and jump to jumpid
      (cmp:with-landing-pad landing-pad-for-unwind-rethrow
        (setq go-index
              (%intrinsic-invoke-if-landing-pad-or-call
               "cc_landingpadUnwindMatchFrameElseRethrow" 
               (list exception-ptr frame)
               "go-index"))
        ;; Restore multiple values before going to whichever block.
        (with-return-values (return-vals return-value abi)
          (%intrinsic-call "cc_restoreMultipleValue0" (list return-value)))))
    go-index))

;;; Generates a landing pad and code to deal with unwinds to this function.
(defun generate-catch-landing-pad (maybe-cleanup-landing-pad not-unwind-block exn.slot ehselector.slot
                                   return-value abi tags catches frame)
  (assert (not (null catches)))
  ;; Bind a fresh IRBuilder so we don't fuck with whatever our caller's been doing.
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder cmp:*llvm-context*))
    (let ((landing-pad-for-unwind-rethrow
            (cmp::generate-rethrow-landing-pad cmp:*current-function* not-unwind-block
                                               nil ; '(cmp::typeid-core-unwind)
                                               exn.slot ehselector.slot))
          (landing-pad-block (cmp:irc-basic-block-create "catch-landing-pad")))
      (cmp:irc-begin-block landing-pad-block)
      (let* ((lpad (cmp:irc-create-landing-pad 1 "lp"))
             (exception-structure (cmp:irc-extract-value lpad (list 0) "exception-structure"))
             (exception-selector (cmp:irc-extract-value lpad (list 1) "exception-selector")))
        (%store exception-structure exn.slot)
        (%store exception-selector ehselector.slot)
        (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))
        ;; we have to go to the cleanup later, so we can't ignore any exceptions.
        (when maybe-cleanup-landing-pad (llvm-sys:set-cleanup lpad t))
        (let* ((is-unwind-block (cmp:irc-basic-block-create "match-unwind"))
               (typeid (%intrinsic-call "llvm.eh.typeid.for"
                                        (list (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))))
               (matches-type (cmp:irc-icmp-eq exception-selector typeid)))
          ;; If the exception is Clasp's UnwindException, we handle it.
          ;; Otherwise we go to the cleanup, or perhaps directly to the resume.
          (cmp:irc-cond-br matches-type is-unwind-block not-unwind-block)
          (cmp:irc-begin-block is-unwind-block)
          (let* ((go-index (generate-match-unwind
                            return-value abi frame landing-pad-for-unwind-rethrow exn.slot))
                 (default-block (cmp:irc-basic-block-create "switch-default"))
                 (sw (cmp:irc-switch go-index default-block (length catches))))
            (loop for catch in catches
                  for jump-id = (cc-mir:go-index catch)
                  do (let* ((target (second (cleavir-ir:successors catch)))
                            (tag-block (gethash target tags)))
                       (assert (not (null tag-block)))
                       (llvm-sys:add-case sw (%size_t jump-id) tag-block)))
            (cmp:irc-begin-block default-block)
            (%intrinsic-invoke-if-landing-pad-or-call "throwIllegalSwitchValue"
                                                      (list go-index (%size_t (length catches)))
                                                      ""
                                                      maybe-cleanup-landing-pad)
            (cmp:irc-unreachable))))
      landing-pad-block)))

(defun generate-cleanup-invocation-history (calling-convention)
  (%intrinsic-call "cc_pop_InvocationHistoryFrame"
                   (list (cmp:calling-convention-closure calling-convention)
                         (cmp:calling-convention-invocation-history-frame* calling-convention))))

(defun maybe-gen-cleanup-invocation-history (function-info)
  (when (debug-on function-info)
    (let ((cc (calling-convention function-info)))
      (generate-cleanup-invocation-history cc))))

(defun generate-cleanup-landing-pad (exn.slot ehselector.slot block)
  (let ((cleanup-landing-pad       (cmp:irc-basic-block-create "cleanup-lpad"))
        (ehbuilder                 (llvm-sys:make-irbuilder cmp:*llvm-context*)))
    (cmp:irc-set-insert-point-basic-block cleanup-landing-pad ehbuilder)
    (cmp:with-irbuilder (ehbuilder)
      (let ((landpad (cmp:irc-create-landing-pad 1)))
        (llvm-sys:set-cleanup landpad t)
        (cmp:preserve-exception-info landpad exn.slot ehselector.slot)
        (cmp:irc-br block)))
    cleanup-landing-pad))

(defun generate-cleanup-block (calling-convention ehresume-block)
  (let ((cleanup-block             (cmp:irc-basic-block-create "cleanup"))
        (ehbuilder                 (llvm-sys:make-irbuilder cmp:*llvm-context*)))
    (cmp:irc-set-insert-point-basic-block cleanup-block ehbuilder)
    (cmp:with-irbuilder (ehbuilder)
      (generate-cleanup-invocation-history calling-convention)
      (cmp:irc-br ehresume-block))
    cleanup-block))

(defun generate-resume-block (exn.slot ehselector.slot)
  (let* ((ehbuilder       (llvm-sys:make-irbuilder cmp:*llvm-context*))
         (ehresume        (cmp:irc-basic-block-create "ehresume"))
         (_               (cmp:irc-set-insert-point-basic-block ehresume ehbuilder))
         (exn7            (llvm-sys:create-load-value-twine ehbuilder exn.slot "exn7"))
         (sel             (llvm-sys:create-load-value-twine ehbuilder ehselector.slot "sel"))
         (undef           (llvm-sys:undef-value-get cmp:%exception-struct% ))
         (lpad.val        (llvm-sys:create-insert-value ehbuilder undef exn7 '(0) "lpad.val"))
         (lpad.val8       (llvm-sys:create-insert-value ehbuilder lpad.val sel '(1) "lpad.val8"))
         (_               (llvm-sys:create-resume ehbuilder lpad.val8)))
    (declare (ignore _))
    ehresume))

(defun alloca-exn.slot ()
  (alloca-i8* "exn.slot"))
(defun alloca-ehselector.slot ()
  (alloca-i32 "ehselector.slot"))

;;; Returns four values. If debug-on is false, they're all NIL. Otherwise,
;;; 1) a block with a cleanup landing pad for a function
;;; 2) a block that does the cleanup (successor of 1)
;;; 3,4): exn.slot and ehselector.slot.
(defun maybe-generate-landing-pad (info)
  ;; If we don't need to worry about unwinds, don't generate any code, and return NIL immediately.
  (let* ((debug-on (debug-on info))
         (calling-convention (calling-convention info)))
    (if debug-on
        (let* ((exn.slot (alloca-exn.slot))
               (ehselector.slot (alloca-ehselector.slot))
               (resume-block (generate-resume-block exn.slot ehselector.slot))
               (cleanup-block (generate-cleanup-block calling-convention resume-block))
               (cleanup-landing-pad (generate-cleanup-landing-pad
                                     exn.slot ehselector.slot cleanup-block)))
          (values cleanup-landing-pad cleanup-block exn.slot ehselector.slot))
        (values nil nil nil nil))))

;;; "unreal" catches are catch instructions produced in HIR that lie around in invoke-instructions
;;; despite being deleted as useless. Most catches probably turn out to be unreal.
(defun real-catches (catches)
  (remove-if-not (lambda (catch) (typep catch 'cc-mir:assign-catch-instruction)) catches))

;;; Interface:
;;; WITH-CLEANUP around compilation of the whole function.
;;; WITH-CATCH-PAD-PREP around whole translation, or for each function-
;;;  latter means consing a few more hash tables is all.
;;; CATCH-PAD in invokes.
(defvar *cleanup-pad*)
(defvar *cleanup-block*)
(defvar *exn.slot*)
(defvar *ehselector.slot*)
(defvar *catch-pads-table*)

(defmacro with-cleanup ((info) &body body)
  `(multiple-value-bind (*cleanup-pad* *cleanup-block* *exn.slot* *ehselector.slot*)
       (maybe-generate-landing-pad ,info)
     (cmp:with-landing-pad *cleanup-pad*
       ,@body)))

(defmacro with-catch-pad-prep (&body body)
  `(let ((*catch-pads-table* (make-hash-table :test #'equal)))
     ,@body))

;;; Note: NOT like LLVM "catchpad" instruction.
(defun catch-pad (catches return-value abi tags function-info)
  (or (gethash catches *catch-pads-table*)
      (setf (gethash catches *catch-pads-table*)
            (let ((real (real-catches catches)))
              (if (null real)
                  ;; no real catches - just invoke to the cleanup
                  *cleanup-pad*
                  ;; hard part
                  ;; Now, if debug-on is false but there are still catches to go to,
                  ;; WITH-CLEANUP has bound all this stuff to NIL, and we need actual values.
                  ;; In this case having the slot is pointless since it'll always be the
                  ;; output from our one landingpad, but whatever.
                  (let* ((exn.slot (or *exn.slot* (alloca-exn.slot)))
                         (ehselector.slot (or *ehselector.slot* (alloca-ehselector.slot)))
                         (cleanup-block (or *cleanup-block*
                                            (generate-resume-block exn.slot ehselector.slot)))
                         (semiframe (translate-datum (frame-marker function-info))))
                    ;; KLUDGE TIME
                    ;; semiframe will have been assigned by a save-frame-instruction.
                    ;; Nothing else writes to it, so it's an SSA variable.
                    ;; (As indicated by being a cons in *vars*.)
                    ;; We kind of magic our way into that here.
                    (assert (consp semiframe))
                    (generate-catch-landing-pad
                     *cleanup-pad* cleanup-block exn.slot ehselector.slot
                     return-value abi tags real (car semiframe))))))))
