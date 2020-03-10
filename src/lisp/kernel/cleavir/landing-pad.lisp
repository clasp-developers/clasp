(in-package :clasp-cleavir)

;;; When we perform an invocation, we can guess whether there is any possibility that we
;;; will actually handle any exception, or if we just need to clean up (if there is any cleanup).
;;; This is because the only exceptions we need to handle are the ones for BLOCK/TAGBODY.
;;; For example, if we call an intrinsic, we know it's not return-from-ing, so we will not handle
;;; any exceptions. (This is also true of cc_unwind, since we don't return to the frame from which
;;; we are unwinding.)
;;; So we have two kinds of landing pads - one that checks the exception, and one that ignores it
;;; and cleans up and finally resumes.
;;; The second kind is easy to organize. The landing pad stores the exn.slot and ehselector.slot
;;; and branches to an appropriate cleanup block. That block then branches to other cleanup blocks,
;;; etc., until the resume block is reached.
;;; For the first kind, we organize as follows. Each landing pad checks if the exception is our
;;; exception and if the frame is correct. If it isn't, we divert to the cleanup path. If it is,
;;; we follow a separate path. If the landing-pad is for a catch-instruction, it switches on the
;;; ID, transfers control if one matches, otherwise goes to the processor for the next landing
;;; pad. If the landing-pad is for a bind-instruction, it cleans up and then proceeds. At the end,
;;; rather than resuming, we signal an error (bug).

;; HT from dynenv locations to maybe-entry landing pad blocks (or NIL for no cleanup)
(defvar *maybe-entry-landing-pads*)
;; HT from instructions to maybe-entry processing code blocks
(defvar *maybe-entry-processors*)
;; HT from dynenv locations to never-entry landing pad blocks (or NIL for no cleanup)
(defvar *never-entry-landing-pads*)
;; HT from instructions to never-entry processing code blocks
(defvar *never-entry-processors*)
;; i8** Value to store the exception in
(defvar *exn.slot*)
;; i32* Value to store the exception selector in
(defvar *ehselector.slot*)
;; size_t* Value to store the go index in
(defvar *go-index.slot*)

;;; shared between both

;;; Generates code that checks whether the unwind is to this frame.
;;; If it is, it returns the control point index, i.e. which catch to jump to.
;;; Otherwise it rethrows, going to the provided landing pad.
;;; An alternate strategy would be to test whether it's an unwind, and if not, cleanup and/or resume
;;; and if so, extract the go-index and jump table and so on in a catch block.
;;; Saves a landing pad and my comprehension.
(defun generate-match-unwind (return-value frame landing-pad-for-unwind-rethrow exn.slot)
  (cmp:with-begin-end-catch ((cmp:irc-load exn.slot "exn") exception-ptr nil)
    ;; Check if frame is correct against tagbody and jump to jumpid
    (cmp:with-landing-pad landing-pad-for-unwind-rethrow
      (%intrinsic-invoke-if-landing-pad-or-call
       "cc_landingpadUnwindMatchFrameElseRethrow" 
       (list exception-ptr frame)
       "go-index"))))

(defun generate-resume-block (exn.slot ehselector.slot)
  (let* ((ehbuilder       (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
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
  (cmp:alloca-i8* "exn.slot"))
(defun alloca-ehselector.slot ()
  (cmp:alloca-i32 "ehselector.slot"))
(defun alloca-go-index.slot ()
  (cmp:alloca-size_t "go-index.slot"))

(defun dynenv-definer (location)
  (let ((definers (cleavir-ir:defining-instructions location)))
    (unless (= (length definers) 1)
      (error "BUG: Dynamic-environment ~a def-use chain is messed up - definers: ~a"
             location definers))
    (first definers)))

;;; There's also gen-unbind. Don't look at me like that.
(defun generate-unbind (bind-instruction next)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((bb (cmp:irc-basic-block-create "unbind-special-variable")))
      (cmp:irc-begin-block bb)
      (let ((symbol (in (first (cleavir-ir:inputs bind-instruction))))
            (old-value (in (first (cleavir-ir:outputs bind-instruction)))))
        ;; This function cannot throw, so no landing pad needed
        (%intrinsic-call "cc_setTLSymbolValue" (list symbol old-value))
        (cmp:irc-br next))
      bb)))

(defun lp-generate-protect (u-p-instruction next return-value function-info)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((bb (cmp:irc-basic-block-create "execute-protection")))
      (cmp:irc-begin-block bb)
      (let ((thunk (in (first (cleavir-ir:inputs u-p-instruction))))
            (protection-dynenv (cleavir-ir:dynamic-environment u-p-instruction)))
        ;; There is a subtle point here with regard to unwinding out of a cleanup
        ;; form. CLHS 5.2 specifies that when unwinding begins, exit points between
        ;; the unwind point and the destination are "abandoned" and can no longer be
        ;; exited to - doing so is undefined behavior. For example, the code
        ;; (block nil (unwind-protect (throw something) (return)))
        ;; has undefined consequences - the (return) effectively quits the throw
        ;; before it can finish.
        ;; An alternate X3J13 proposal, EXIT-EXTENT:MEDIUM, would have allowed this
        ;; behavior. And we do too - no reason not to really. We do not abandon
        ;; intervening exit points. And we indicate that by using for the call
        ;; to the protected thunk the same dynamic-environment that was in place
        ;; upon entry to the unwind-protect.
        (gen-protect thunk protection-dynenv return-value function-info))
      (cmp:irc-br next)
      bb)))

;;; maybe-entry landing pads, for when we may be nonlocally entering this function.

(defun generate-maybe-entry-landing-pad (next cleanup-block cleanup-p return-value frame)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((lp-block (cmp:irc-basic-block-create "never-entry-landing-pad"))
          (is-unwind-block (cmp:irc-basic-block-create "is-unwind")))
      (cmp:irc-begin-block lp-block)
      (let* ((lpad (cmp:irc-create-landing-pad 1 "lp"))
             (exception-structure (cmp:irc-extract-value lpad (list 0) "exception-structure"))
             (exception-selector (cmp:irc-extract-value lpad (list 1) "exception-selector")))
        (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))
        (when cleanup-p (llvm-sys:set-cleanup lpad t))
        (cmp:irc-store exception-structure *exn.slot*)
        (cmp:irc-store exception-selector *ehselector.slot*)
        (let* ((typeid (%intrinsic-call "llvm.eh.typeid.for"
                                        (list (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))))
               (matches-type (cmp:irc-icmp-eq exception-selector typeid)))
          ;; If the exception is Clasp's Unwind exception, we handle it.
          ;; Otherwise we go to the cleanup, or perhaps directly to the resume.
          (cmp:irc-cond-br matches-type is-unwind-block cleanup-block)))
      ;; Now that we know it's the right type of exception, see if we're in the right frame,
      ;; and get the go index. Also restore multiple values.
      (cmp:irc-begin-block is-unwind-block)
      (let ((go-index (generate-match-unwind
                       return-value frame (generate-end-catch-landing-pad cleanup-block)
                       *exn.slot*)))
        (cmp:irc-store go-index *go-index.slot*)
        (restore-multiple-value-0 return-value)
        (cmp:irc-br next))
      lp-block)))

(defun generate-end-catch-landing-pad (cleanup-block)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((lp-block (cmp:irc-basic-block-create "end-catch-landing-pad")))
      (cmp:irc-begin-block lp-block)
      (let* ((lpad (cmp:irc-create-landing-pad 0 "lp"))
             (exception-structure (cmp:irc-extract-value lpad (list 0) "exception-structure"))
             (exception-selector (cmp:irc-extract-value lpad (list 1) "exception-selector")))
        (llvm-sys:set-cleanup lpad t)
        (cmp:irc-store exception-structure *exn.slot*)
        (cmp:irc-store exception-selector *ehselector.slot*)
        ;; FIXME: Check if end_catch ever actually unwinds. I think no.
        (cmp:with-landing-pad nil
          (%intrinsic-invoke-if-landing-pad-or-call "__cxa_end_catch" nil))
        (cmp:irc-br cleanup-block))
      lp-block)))

(defun maybe-entry-processor (instruction return-value tags function-info)
  (or (gethash instruction *maybe-entry-processors*)
      (setf (gethash instruction *maybe-entry-processors*)
            (compute-maybe-entry-processor instruction return-value tags function-info))))

(defgeneric compute-maybe-entry-processor (instruction return-value tags function-info))

(defmethod compute-maybe-entry-processor ((instruction cleavir-ir:assignment-instruction)
                                          return-value tags function-info)
  ;; Proxy - go up
  (maybe-entry-processor (dynenv-definer (first (cleavir-ir:inputs instruction)))
                         return-value tags function-info))

(defmethod compute-maybe-entry-processor ((instruction cleavir-ir:catch-instruction)
                                          return-value tags function-info)
  ;; Jump into this function based on the go index.
  ;; If the index doesn't match anything in this catch, go onto the next block.
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let* ((destinations (rest (cleavir-ir:successors instruction)))
           (next (maybe-entry-processor
                  (dynenv-definer (cleavir-ir:dynamic-environment instruction))
                  return-value tags function-info))
           (bb (cmp:irc-basic-block-create "catch"))
           (_ (cmp:irc-begin-block bb))
           (go-index (cmp:irc-load *go-index.slot*))
           (sw (cmp:irc-switch go-index next (length destinations))))
      (declare (ignore _))
      (loop for dest in destinations
            for jump-id = (instruction-go-index dest)
            #| Assertion
            ;; For making sure no instructions are multiply present.
            ;; (If they are, you get a switch with duplicate entries,
            ;;  which kills the LLVM.)
            do (when (member jump-id used-ids)
                 (error "Duplicated ID!"))
            collect jump-id into used-ids
            |#
            do (let ((tag-block (gethash dest tags)))
                 (assert (not (null tag-block)))
                 (llvm-sys:add-case sw (%size_t jump-id) tag-block)))
      bb)))

(defmethod compute-maybe-entry-processor ((instruction clasp-cleavir-hir:bind-instruction)
                                          return-value tags function-info)
  ;; Unbind.
  (generate-unbind instruction
                   (maybe-entry-processor
                    (dynenv-definer (cleavir-ir:dynamic-environment instruction))
                    return-value tags function-info)))

(defmethod compute-maybe-entry-processor
    ((instruction clasp-cleavir-hir:unwind-protect-instruction)
     return-value tags function-info)
  (lp-generate-protect instruction
                       (maybe-entry-processor
                        (dynenv-definer (cleavir-ir:dynamic-environment instruction))
                        return-value tags function-info)
                       return-value function-info))

(defmethod compute-maybe-entry-processor ((instruction cleavir-ir:enter-instruction)
                                          return-value tags function-info)
  (declare (ignore return-value function-info))
  ;; We found in the landing pad that we were supposed to jump into this frame.
  ;; However, no relevant catch has transferred control.
  ;; This is a bug in the compiler.
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((err (cmp:irc-basic-block-create "bug-in-catch")))
      (cmp:irc-begin-block err)
      (cmp:with-landing-pad nil
        (%intrinsic-invoke-if-landing-pad-or-call
         "cc_error_bugged_catch" (list (cmp:irc-load *go-index.slot*))))
      (cmp:irc-unreachable)
      err)))

;;; Returns T iff calls in this dynamic environment may nonlocal return to this
;;; function, i.e. there is a catch instruction up the way somewhere.
(defun location-may-enter-p (location)
  (let ((definer (dynenv-definer location)))
    (etypecase definer
      (cleavir-ir:enter-instruction nil)
      (cleavir-ir:assignment-instruction
       (location-may-enter-p (first (cleavir-ir:inputs definer))))
      ((or clasp-cleavir-hir:bind-instruction
           clasp-cleavir-hir:unwind-protect-instruction)
       (location-may-enter-p (cleavir-ir:dynamic-environment definer)))
      (cleavir-ir:catch-instruction t))))

(defun compute-maybe-entry-landing-pad (location return-value tags function-info)
  ;; KLUDGE: If there are no catches we just use the never-entry pad.
  ;; This is bad in that, if there's some weird generation bug or coincidental
  ;; out of extent return, we could hypothetically end up unwinding to a frame
  ;; with no catches, and in this case we should signal an error rather than
  ;; do whatever weird thing.
  (if (location-may-enter-p location)
      (let ((definer (dynenv-definer location)))
        (etypecase definer
          (cleavir-ir:assignment-instruction
           ;; Proxy: Just check higher
           (maybe-entry-landing-pad (first (cleavir-ir:inputs definer))
                                    return-value tags function-info))
          ((or cleavir-ir:catch-instruction
               clasp-cleavir-hir:bind-instruction
               clasp-cleavir-hir:unwind-protect-instruction
               cleavir-ir:enter-instruction)
           (generate-maybe-entry-landing-pad
            (maybe-entry-processor definer return-value tags function-info)
            (never-entry-processor definer return-value function-info)
            (definer-needs-cleanup-p definer)
            return-value (frame-value function-info)))))
      (never-entry-landing-pad location return-value function-info)))

;;; never-entry landing pads, for when we always end with a resume.

(defun generate-never-entry-landing-pad (next)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((lp-block (cmp:irc-basic-block-create "never-entry-landing-pad")))
      (cmp:irc-begin-block lp-block)
      (let* ((lpad (cmp:irc-create-landing-pad 0 "lp"))
             (exception-structure (cmp:irc-extract-value lpad (list 0) "exception-structure"))
             (exception-selector (cmp:irc-extract-value lpad (list 1) "exception-selector")))
        (llvm-sys:set-cleanup lpad t)
        (cmp:irc-store exception-structure *exn.slot*)
        (cmp:irc-store exception-selector *ehselector.slot*)
        (cmp:irc-br next))
      lp-block)))

(defun never-entry-processor (instruction return-value function-info)
  (or (gethash instruction *never-entry-processors*)
      (setf (gethash instruction *never-entry-processors*)
            (compute-never-entry-processor instruction return-value function-info))))

(defgeneric compute-never-entry-processor (instruction return-value function-info))

(defun c-n-e-p-next (next-dynenv return-value function-info)
  (never-entry-processor (dynenv-definer next-dynenv) return-value function-info))

(defmethod compute-never-entry-processor
    ((instruction cleavir-ir:enter-instruction) return-value function-info)
  (declare (ignore return-value function-info))
  (generate-resume-block *exn.slot* *ehselector.slot*))

(defmethod compute-never-entry-processor
    ((instruction cleavir-ir:assignment-instruction) return-value function-info)
  (c-n-e-p-next (first (cleavir-ir:inputs instruction)) return-value function-info))

(defmethod compute-never-entry-processor
    ((instruction cleavir-ir:catch-instruction) return-value function-info)
  (c-n-e-p-next (cleavir-ir:dynamic-environment instruction) return-value function-info))

(defmethod compute-never-entry-processor
    ((instruction clasp-cleavir-hir:bind-instruction) return-value function-info)
  (generate-unbind instruction
                   (c-n-e-p-next (cleavir-ir:dynamic-environment instruction)
                                 return-value function-info)))

(defmethod compute-never-entry-processor
    ((instruction clasp-cleavir-hir:unwind-protect-instruction) return-value function-info)
  (lp-generate-protect instruction
                       (c-n-e-p-next (cleavir-ir:dynamic-environment instruction)
                                     return-value function-info)
                       return-value function-info))

;;; Used above. Should match compute-never-entry-landing-pad
(defun definer-needs-cleanup-p (definer)
  (etypecase definer
    ;; Proxy
    (cleavir-ir:assignment-instruction
     (definer-needs-cleanup-p
      (dynenv-definer (first (cleavir-ir:inputs definer)))))
    ;; Next might need a cleanup
    (cleavir-ir:catch-instruction
     (definer-needs-cleanup-p
      (dynenv-definer (cleavir-ir:dynamic-environment definer))))
    ;; Definitive answers
    (cleavir-ir:enter-instruction nil)
    ((or clasp-cleavir-hir:bind-instruction clasp-cleavir-hir:unwind-protect-instruction)
     t)))

(defun compute-never-entry-landing-pad (location return-value function-info)
  (let ((definer (dynenv-definer location)))
    (etypecase definer
      (cleavir-ir:assignment-instruction
       ;; Proxy: Just check higher
       (never-entry-landing-pad (first (cleavir-ir:inputs definer)) return-value function-info))
      (cleavir-ir:catch-instruction
       ;; We never catch, so just keep going up.
       (never-entry-landing-pad (cleavir-ir:dynamic-environment definer)
                                return-value function-info))
      ((or clasp-cleavir-hir:bind-instruction clasp-cleavir-hir:unwind-protect-instruction)
       (generate-never-entry-landing-pad
        (never-entry-processor definer return-value function-info)))
      (cleavir-ir:enter-instruction
       ;; Nothing to do
       nil))))

;;; INTERFACE below

(defmacro with-catch-pad-prep (&body body)
  `(let ((*maybe-entry-landing-pads* (make-hash-table :test #'eq))
         (*maybe-entry-processors* (make-hash-table :test #'eq))
         (*never-entry-landing-pads* (make-hash-table :test #'eq))
         (*never-entry-processors* (make-hash-table :test #'eq))
         (*exn.slot* (alloca-exn.slot))
         (*ehselector.slot* (alloca-ehselector.slot))
         (*go-index.slot* (alloca-go-index.slot)))
     ,@body))

(defun maybe-entry-landing-pad (location return-value tags function-info)
  (or (gethash location *maybe-entry-landing-pads*)
      (setf (gethash location *maybe-entry-landing-pads*)
            (compute-maybe-entry-landing-pad
             location return-value tags function-info))))

(defun never-entry-landing-pad (location return-value function-info)
  (or (gethash location *never-entry-landing-pads*)
      (setf (gethash location *never-entry-landing-pads*)
            (compute-never-entry-landing-pad location return-value function-info))))
