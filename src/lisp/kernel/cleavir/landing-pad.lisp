(in-package :clasp-cleavir)

;;; When we perform an invocation, we can guess whether there is any possibility that we
;;; will actually handle any exception, or if we just need to clean up (if there is any cleanup).
;;; This is because the only exceptions we need to handle are the ones for BLOCK/TAGBODY.
;;; So we have two kinds of landing pads - one that checks the exception, and one that ignores it
;;; and cleans up and finally resumes.
;;; The second kind of pad is easy to organize. The landing pad stores the exn.slot and
;;; ehselector.slot and branches to an appropriate cleanup block. That block then branches to
;;; other cleanup blocks, etc., until the resume block is reached.
;;; For the first kind, we organize as follows. Each landing pad checks if the exception is our
;;; exception and if the frame is correct. If it isn't, we divert to the cleanup path. If it is,
;;; we follow a separate path. If the landing-pad is for a come-from-instruction it switches on the
;;; ID, transfers control if one matches, otherwise goes to the processor for the next landing
;;; pad. If the landing-pad is for a bind-instruction, it cleans up and then proceeds. At the end,
;;; rather than resuming, we signal an error (bug).
;;; We can use the ignorant, second type pads in several cases:
;;; 1) There are no nonlocal entrances (BLOCK/TAGBODYs) around the call in the function.
;;; 2) We have already caught an exception and determined it's not ours, or not for this frame.
;;; 3) The compiler knows the function being invoked will never return to this frame.
;;; The third is kind of hard - intrinsics can call ERROR and get a handler, for example - so
;;; we underutilize (FIXME) this optimization at the moment.

;;;; See unwind-notes.txt for more background on this system.

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

(defmacro with-begin-end-catch ((exn exception-ptr rethrow-bb) &rest body)
  (let ((exn-gs (gensym)))
    `(let ((,exn-gs ,exn))
       (multiple-value-prog1
           (cmp:with-landing-pad ,rethrow-bb
             (let ((,exception-ptr (cmp:irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
               ,@body))
         (cmp:with-landing-pad nil
           (cmp:irc-intrinsic "__cxa_end_catch"))))))

;;; Generates code that checks whether the unwind is to this frame.
;;; If it is, it returns the control point index, i.e. which come-from to jump to.
;;; Otherwise it rethrows, going to the provided landing pad.
;;; An alternate strategy would be to test whether it's an unwind, and if not, cleanup and/or resume
;;; and if so, extract the go-index and jump table and so on in a catch block.
;;; Saves a landing pad and my comprehension.
(defun generate-match-unwind (frame landing-pad-for-unwind-rethrow exn.slot)
  (with-begin-end-catch ((cmp:irc-typed-load cmp:%exn% exn.slot "exn") exception-ptr nil)
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
         (exn7            (llvm-sys:create-load-type-value-twine ehbuilder cmp:%exn% exn.slot "exn7"))
         (sel             (llvm-sys:create-load-type-value-twine ehbuilder cmp:%ehselector% ehselector.slot "sel"))
         (undef           (llvm-sys:undef-value-get cmp:%exception-struct% ))
         (lpad.val        (llvm-sys:create-insert-value ehbuilder undef exn7 '(0) "lpad.val"))
         (lpad.val8       (llvm-sys:create-insert-value ehbuilder lpad.val sel '(1) "lpad.val8"))
         (_1              (llvm-sys:create-resume ehbuilder lpad.val8)))
    (declare (ignore _ _1))
    ehresume))

(defun alloca-exn.slot ()
  (cmp:alloca-i8* "exn.slot"))
(defun alloca-ehselector.slot ()
  (cmp:alloca-i32 "ehselector.slot"))
(defun alloca-go-index.slot ()
  (cmp:alloca-size_t "go-index.slot"))

(defun generate-unbind (index old-value de-stack next)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((bb (cmp:irc-basic-block-create "unbind-special-variable")))
      (cmp:irc-begin-block bb)
      ;; These calls cannot throw, so no landing pad needed
      (unbind-special index old-value de-stack)
      (cmp:irc-br next)
      bb)))

(defun lp-generate-protect (u-p-instruction next)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((bb (cmp:irc-basic-block-create "execute-protection")))
      (cmp:irc-begin-block bb)
      ;; pop the dynenv.
      (let ((de-stack (dynenv-storage u-p-instruction)))
        (%intrinsic-call "cc_set_dynenv_stack" (list de-stack)))
      (let ((thunk (in (first (cleavir-bir:inputs u-p-instruction)))))
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
        (let* ((nvals (%intrinsic-call "cc_nvalues" nil "nvals"))
               ;; NOTE that this is kind of really dumb. We save the values, i.e. alloca
               ;; a VLA, for every unwind protect executed. We could at least merge unwind
               ;; protects in the same frame - but what would be really smart would be
               ;; just having the exception object carry the values, so we can fuck with the
               ;; global (thread-local) values with impunity while unwinding.
               ;; Probably challenging to arrange in C++, though.
               (mv-temp (cmp:alloca-temp-values nvals)))
          (%intrinsic-call "cc_save_all_values" (list nvals mv-temp))
          (gen-call-cleanup u-p-instruction)
          (%intrinsic-call "cc_load_all_values" (list nvals mv-temp))))
      (cmp:irc-br next)
      bb)))

;;; Get the frame pointer for dynenv matching.
;;; This is called late (i.e. while generating code) because it
;;; may entail code generation itself.
(defgeneric dynenv-frame (dynenv))

(defmethod dynenv-frame ((dynenv bir:dynamic-environment))
  ;; default: just grab the frame pointer
  (%intrinsic-call "llvm.frameaddress.p0" (list (%i32 0))
                   "frame"))

(defmethod dynenv-frame ((dynenv bir:come-from))
  ;; come-from: get it from the instruction
  (%intrinsic-call "cc_dynenv_frame" (list (in dynenv))))

;;; maybe-entry landing pads, for when we may be nonlocally entering this function.

(defun generate-maybe-entry-landing-pad (next cleanup-block cleanup-p dynenv)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((lp-block (cmp:irc-basic-block-create "maybe-entry-landing-pad"))
          (is-unwind-block (cmp:irc-basic-block-create "is-unwind")))
      (cmp:irc-begin-block lp-block)
      (let* ((lpad (cmp:irc-create-landing-pad 1 "lp"))
             (exception-structure (cmp:irc-extract-value lpad (list 0) "exception-structure"))
             (exception-selector (cmp:irc-extract-value lpad (list 1) "exception-selector")))
        (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))
        (when cleanup-p (llvm-sys:set-cleanup lpad t))
        (cmp:irc-store exception-structure *exn.slot*)
        (cmp:irc-store exception-selector *ehselector.slot*)
        (let* ((typeid (%intrinsic-call cmp:+intrinsic/llvm.eh.typeid.for.p0+
                                        (list (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))))
               (matches-type (cmp:irc-icmp-eq exception-selector typeid)))
          ;; If the exception is Clasp's Unwind exception, we handle it.
          ;; Otherwise we go to the cleanup, or perhaps directly to the resume.
          (cmp:irc-cond-br matches-type is-unwind-block cleanup-block)))
      ;; Now that we know it's the right type of exception, see if we're in the right frame,
      ;; and get the go index.
      (cmp:irc-begin-block is-unwind-block)
      (let* ((frame (dynenv-frame dynenv))
             (go-index (generate-match-unwind
                        frame (generate-end-catch-landing-pad cleanup-block)
                        *exn.slot*)))
        (cmp:irc-store go-index *go-index.slot*)
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

(defun maybe-entry-processor (dynenv tags)
  (or (gethash dynenv *maybe-entry-processors*)
      (setf (gethash dynenv *maybe-entry-processors*)
            (compute-maybe-entry-processor dynenv tags))))

(defgeneric compute-maybe-entry-processor (dynenv tags))

;; Is this iblock a place unknown values are nonlocally returned to?
(defun nonlocal-valued-p (iblock)
  (and (= (length (cleavir-bir:inputs iblock)) 1)
       (not (null (cc-bmir:rtype (first (cleavir-bir:inputs iblock)))))))

(defmethod compute-maybe-entry-processor ((dynenv cleavir-bir:come-from) tags)
  (if (cleavir-set:empty-set-p (cleavir-bir:unwinds dynenv))
      ;; Nothing unwinds here, so generate nothing
      (maybe-entry-processor (cleavir-bir:parent dynenv) tags)
      ;; Jump into this function based on the go index.
      ;; If the index doesn't match anything in this come-from
      ;; go onto the next block.
      (cmp:with-irbuilder ((llvm-sys:make-irbuilder
                            (cmp:thread-local-llvm-context)))
        (let* ((destinations (rest (cleavir-bir:next dynenv)))
               (ndestinations (count-if #'has-entrances-p destinations))
               (next (maybe-entry-processor (cleavir-bir:parent dynenv) tags))
               (bb (cmp:irc-basic-block-create "come-from"))
               (_0 (cmp:irc-begin-block bb))
               (llde (dynenv-storage dynenv))
               ;; Restore the dynenv, if there is one.
               (_1 (when llde
                     (%intrinsic-call "cc_set_dynenv_stack"
                                      (list (second llde)))))
               ;; Restore multiple values.
               ;; Note that we do this late, after any unwind-protect cleanups,
               ;; so that we get the correct values.
               (tmv (when (some #'nonlocal-valued-p destinations)
                      (restore-multiple-value-0)))
               (tv (when tmv
                     (let ((rt (cc-bmir:rtype
                                (first (bir:inputs (first destinations))))))
                       (cond ((eq rt :multiple-values) tmv)
                             ((equal rt '(:object))
                              (cmp:irc-tmv-primary tmv))
                             ((null rt) nil) ; redundant with nonlocal-valued-p
                             ((every (lambda (x) (eq x :object)) rt)
                              (list* (cmp:irc-tmv-primary tmv)
                                     (loop for i from 1 below (length rt)
                                           collect (cmp:irc-t*-load (return-value-elt i)))))
                             (t (error "BUG: Bad rtype ~a" rt))))))
               (go-index (cmp:irc-typed-load cmp:%go-index% *go-index.slot*))
               (sw (cmp:irc-switch go-index next ndestinations)))
          (declare (ignore _0 _1))
          (loop for dest in destinations
                for has-entrances-p = (has-entrances-p dest)
                for jump-id = (when has-entrances-p (get-destination-id dest))
                for tag-block = (when has-entrances-p (gethash dest tags))
                when has-entrances-p
                  do (assert (not (null tag-block)))
                     ;; KLUDGE time. See bug #990.
                     ;; Basically, we sometimes have multiple tags ending up at
                     ;; the same basic-block, e.g. in (tagbody a b ...)
                     ;; In this case we get duplicate pairs and we need to avoid
                     ;; that.
                     ;; It might be better to avoid this at a higher level but
                     ;; i'm not completely sure.
                     (let* ((jump-id (get-destination-id dest))
                            (tag-block (gethash dest tags))
                            (existing (assoc jump-id used-ids)))
                       (if (null existing)
                           (llvm-sys:add-case sw (%size_t
                                                  jump-id)
                                              tag-block)
                           (unless (eq tag-block (cdr existing))
                             (error "BUG: Duplicated ID in landing-pad.lisp"))))
                  and do (when tv
                           (phi-out
                            tv (first (cleavir-bir:inputs dest)) bb))
                  and collect (cons jump-id tag-block) into used-ids)
          bb))))

(defmethod compute-maybe-entry-processor ((instruction bir:constant-bind) tags)
  (destructuring-bind (index old-value de-stack) (dynenv-storage instruction)
    (generate-unbind index old-value de-stack
                     (maybe-entry-processor
                      (cleavir-bir:parent instruction) tags))))

(defmethod compute-maybe-entry-processor ((instruction bir:unwind-protect)
                                          tags)
  (lp-generate-protect instruction (maybe-entry-processor
                                    (cleavir-bir:parent instruction) tags)))

(defmethod compute-maybe-entry-processor ((instruction cleavir-bir:values-save)
                                          tags)
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder
                        (cmp:thread-local-llvm-context)))
    (let ((stackpos (dynenv-storage instruction))
          (bb (cmp:irc-basic-block-create "escape-m-v-prog1")))
      (cmp:irc-begin-block bb)
      ;; Lose the saved values alloca.
      (%intrinsic-call cmp:+intrinsic/llvm.stackrestore.p0+
                       (list stackpos))
      ;; Continue
      (cmp:irc-br
       (maybe-entry-processor (cleavir-bir:parent instruction) tags))
      bb)))
(defmethod compute-maybe-entry-processor ((inst bir:values-collect) tags)
  (let ((stackpos (dynenv-storage inst)))
    (if stackpos
        (cmp:with-irbuilder ((llvm-sys:make-irbuilder
                              (cmp:thread-local-llvm-context)))
          (let ((bb (cmp:irc-basic-block-create "escape-m-v-prog1")))
            (cmp:irc-begin-block bb)
            ;; Lose the saved values alloca.
            (%intrinsic-call cmp:+intrinsic/llvm.stackrestore.p0+
                             (list stackpos))
            ;; Continue
            (cmp:irc-br
             (maybe-entry-processor (cleavir-bir:parent inst) tags))
            bb))
        (maybe-entry-processor (cleavir-bir:parent inst) tags))))

(defmethod compute-maybe-entry-processor ((instruction cleavir-bir:function)
                                          tags)
  (declare (ignore tags))
  ;; We found in the landing pad that we were supposed to jump into this frame.
  ;; However, no relevant come-from has transferred control.
  ;; This is a bug in the compiler.
  (cmp:with-irbuilder ((llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
    (let ((err (cmp:irc-basic-block-create "bug-in-come-from")))
      (cmp:irc-begin-block err)
      (cmp:with-landing-pad nil
        (%intrinsic-invoke-if-landing-pad-or-call
         "cc_error_bugged_come_from" (list (cmp:irc-typed-load cmp:%go-index% *go-index.slot*))))
      (cmp:irc-unreachable)
      err)))

;;; Returns T iff calls in this dynamic environment may nonlocal return to this
;;; function, i.e. there is a come-from instruction up the way somewhere.
(defun dynenv-may-enter-p (dynenv)
  (etypecase dynenv
    (cleavir-bir:function nil)
    ((or cleavir-bir:leti cleavir-bir:values-save bir:values-collect
         bir:unwind-protect bir:constant-bind)
     (dynenv-may-enter-p (cleavir-bir:parent dynenv)))
    (bir:come-from
     (if (or (cleavir-set:empty-set-p (cleavir-bir:unwinds dynenv))
             (cleavir-bir-transformations:simple-unwinding-p
              dynenv *clasp-system*))
         ;; simple unwinds are orthogonal to landing pads
         (dynenv-may-enter-p (cleavir-bir:parent dynenv))
         t))))

(defun compute-maybe-entry-landing-pad (dynenv tags)
  ;; KLUDGE: If there are no come-froms we just use the never-entry pad.
  ;; This is bad in that, if there's some weird generation bug or coincidental
  ;; out of extent return, we could hypothetically end up unwinding to a frame
  ;; with no come-froms, and in this case we should signal an error rather than
  ;; do whatever weird thing.
  (if (dynenv-may-enter-p dynenv)
      (etypecase dynenv
        (bir:come-from
         (if (or (cleavir-set:empty-set-p (cleavir-bir:unwinds dynenv))
                 (cleavir-bir-transformations:simple-unwinding-p
                  dynenv *clasp-system*))
             ;; simple SJLJ is orthogonal to landing pads,
             ;; i.e. a simple unwind never goes through or to a
             ;; landing pad.
             (maybe-entry-landing-pad (cleavir-bir:parent dynenv) tags)
             (generate-maybe-entry-landing-pad
              (maybe-entry-processor dynenv tags)
              (never-entry-processor dynenv)
              (dynenv-needs-cleanup-p dynenv)
              dynenv)))
        ((or bir:constant-bind cleavir-bir:values-save bir:values-collect
             cleavir-bir:leti bir:unwind-protect
             cleavir-bir:function)
         (generate-maybe-entry-landing-pad
          (maybe-entry-processor dynenv tags)
          (never-entry-processor dynenv)
          (dynenv-needs-cleanup-p dynenv)
          dynenv)))
      (never-entry-landing-pad dynenv)))

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

(defun never-entry-processor (instruction)
  (or (gethash instruction *never-entry-processors*)
      (setf (gethash instruction *never-entry-processors*)
            (compute-never-entry-processor instruction))))

(defgeneric compute-never-entry-processor (dynenv))

(defmethod compute-never-entry-processor ((dynenv cleavir-bir:function))
  (generate-resume-block *exn.slot* *ehselector.slot*))

(defmethod compute-never-entry-processor ((dynenv cleavir-bir:values-save))
  ;; This whole frame is being discarded,
  ;; so no smaller stack unwinding is necessary.
  (never-entry-processor (cleavir-bir:parent dynenv)))
(defmethod compute-never-entry-processor ((dynenv bir:values-collect))
  ;; ditto
  (never-entry-processor (bir:parent dynenv)))

(defmethod compute-never-entry-processor ((dynenv bir:come-from))
  (never-entry-processor (cleavir-bir:parent dynenv)))

(defmethod compute-never-entry-processor ((dynenv cleavir-bir:leti))
  (never-entry-processor (cleavir-bir:parent dynenv)))

(defmethod compute-never-entry-processor ((instruction bir:constant-bind))
  (destructuring-bind (index old-value de-stack) (dynenv-storage instruction)
    (generate-unbind index old-value de-stack
                     (compute-never-entry-processor
                      (cleavir-bir:parent instruction)))))

(defmethod compute-never-entry-processor ((instruction bir:unwind-protect))
  (lp-generate-protect instruction
                       (never-entry-processor
                        (cleavir-bir:parent instruction))))

;;; Used above. Should match compute-never-entry-landing-pad
(defun dynenv-needs-cleanup-p (dynenv)
  (etypecase dynenv
    ;; Next might need a cleanup
    ((or bir:come-from
         ;; Cleanup only required for local exit.
         cleavir-bir:leti cleavir-bir:values-save bir:values-collect)
     (dynenv-needs-cleanup-p (cleavir-bir:parent dynenv)))
    ;; Definitive answers
    (cleavir-bir:function nil)
    ((or bir:constant-bind bir:unwind-protect)
     t)))

(defun compute-never-entry-landing-pad (dynenv)
  (etypecase dynenv
    ((or bir:come-from cleavir-bir:leti cleavir-bir:values-save
         bir:values-collect)
     ;; We never come-from, so just keep going up.
     (never-entry-landing-pad (cleavir-bir:parent dynenv)))
    ((or bir:constant-bind bir:unwind-protect)
     (generate-never-entry-landing-pad (never-entry-processor dynenv)))
    (cleavir-bir:function
     ;; Nothing to do
     nil)))

;;; INTERFACE below

(defmacro with-catch-pad-prep (&body body)
  `(let ((*maybe-entry-landing-pads* (make-hash-table :test #'eq))
         (*maybe-entry-processors* (make-hash-table :test #'eq))
         (*never-entry-landing-pads* (make-hash-table :test #'eq))
         (*never-entry-processors* (make-hash-table :test #'eq))
         (*exn.slot* (cmp:alloca-exn))
         (*ehselector.slot* (cmp:alloca-ehselector))
         (*go-index.slot* (cmp:alloca-go-index)))
     ,@body))

(defun maybe-entry-landing-pad (location tags)
  (or (gethash location *maybe-entry-landing-pads*)
      (setf (gethash location *maybe-entry-landing-pads*)
            (compute-maybe-entry-landing-pad location tags))))

(defun never-entry-landing-pad (location)
  (or (gethash location *never-entry-landing-pads*)
      (setf (gethash location *never-entry-landing-pads*)
            (compute-never-entry-landing-pad location))))
