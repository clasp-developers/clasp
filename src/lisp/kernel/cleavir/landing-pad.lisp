(in-package :clasp-cleavir)

;;; Generates code that checks whether the unwind is to this frame.
;;; If it is, it returns the control point index, i.e. which catch to jump to.
;;; Otherwise it rethrows, going to the provided landing pad.
;;; An alternate strategy would be to test whether it's an unwind, and if not, cleanup and/or resume
;;; and if so, extract the go-index and jump table and so on in a catch block.
;;; Saves a landing pad and my comprehension.
(defun generate-match-unwind (return-value abi frame-holder landing-pad-for-unwind-rethrow exn.slot)
  ;; We do this weird thing to get the index while still generating the other code around it.
  ;; FIXME: with-begin-end-catch should maybe abstract away so it can return an actual thing.
  (let (go-index)
    (cmp:with-begin-end-catch ((%load exn.slot "exn") exception-ptr nil)
      ;; Check if frame is correct against tagbody and jump to jumpid
      (cmp:with-landing-pad landing-pad-for-unwind-rethrow
        (setq go-index
              (%intrinsic-invoke-if-landing-pad-or-call
               "cc_landingpadUnwindMatchFrameElseRethrow" 
               (list exception-ptr (cmp:irc-load frame-holder))
               "go-index"))
        ;; Restore multiple values before going to whichever block.
        (with-return-values (return-vals return-value abi)
          (%intrinsic-call "cc_restoreMultipleValue0" (list return-value)))))
    go-index))

;;; Generates a landing pad and code to deal with unwinds to this function.
(defun generate-catch-landing-pad (maybe-cleanup-landing-pad ehselector.slot exn.slot not-unwind-block
                                   return-value abi frame-holder tags catches)
  (let ((landing-pad-for-unwind-rethrow
          (cmp::generate-rethrow-landing-pad cmp:*current-function* not-unwind-block
                                             nil ; '(cmp::typeid-core-unwind)
                                             exn.slot ehselector.slot))
        (landing-pad-block (cmp:irc-basic-block-create "landing-pad")))
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
                         return-value abi frame-holder landing-pad-for-unwind-rethrow exn.slot))
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
                                                    maybe-cleanup-landing-pad)))
        (cmp:irc-unreachable))
      landing-pad-block))

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

;;; Returns either NIL or a block to serve as a landing pad.
(defun maybe-generate-landing-pad (info tags return-value abi)
  ;; If we don't need to worry about unwinds, don't generate any code, and return NIL immediately.
  (let* ((debug-on (debug-on info)) (catches (catches info))
         (calling-convention (calling-convention info))
         (catches-p (not (null catches))))
    (if (or debug-on catches-p)
        (let* ((exn.slot (alloca-i8* "exn.slot"))
               (ehselector.slot (alloca-i32 "ehselector.slot"))
               (resume-block (generate-resume-block exn.slot ehselector.slot))
               (cleanup-block (if (debug-on info)
                                  (generate-cleanup-block calling-convention resume-block)
                                  resume-block))
               (cleanup-landing-pad (if (debug-on info)
                                        (generate-cleanup-landing-pad
                                         exn.slot ehselector.slot cleanup-block)
                                        nil)))
          (if catches-p
              (generate-catch-landing-pad
               cleanup-landing-pad ehselector.slot exn.slot cleanup-block
               return-value abi (translate-datum (frame-marker info)) tags catches)
              cleanup-landing-pad))
        nil)))
