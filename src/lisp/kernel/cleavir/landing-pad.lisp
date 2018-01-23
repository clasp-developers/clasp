(in-package :clasp-cleavir)

(export 'create-unwind-landing-pad)
(defun generate-landing-pad-for-unwind (function-info enter-instruction return-value tags abi maybe-unwind-landing-pad landing-pad-for-unwind-rethrow not-unwind-exception-bb &key is-cleanup)
  (let ( ;;;(entry-cont-block (cmp:irc-basic-block-create "entry-cont"))
	(landing-pad-block (cmp:irc-basic-block-create "landing-pad")))
;;;    (cmp:irc-br entry-cont-block)
    (cmp:irc-begin-block landing-pad-block)
    (let* ((lpad                      (cmp:irc-create-landing-pad 1 "lp"))
           (_                         (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind)))
           (_                         (and is-cleanup (llvm-sys:set-cleanup lpad t)))
           (exception-structure (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 0) "exception-structure"))
           (exception-selector (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 1) "exception-selector")))
      (llvm-sys:create-store cmp:*irbuilder* exception-structure (exn.slot function-info) nil)
      (llvm-sys:create-store cmp:*irbuilder* exception-selector (ehselector.slot function-info) nil)
      (let* ( ;;(not-unwind-exception-bb cmp::*exception-handler-cleanup-block*) ;; (cmp:irc-basic-block-create "not-unwind-exception"))
             (unwind-exception-bb (cmp:irc-basic-block-create "match-unwind"))
             (typeid (%intrinsic-call "llvm.eh.typeid.for"
                                      (list (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))))
             (matches-type (cmp:irc-icmp-eq exception-selector typeid)))
        (cmp:irc-cond-br matches-type unwind-exception-bb not-unwind-exception-bb)
        (cmp:irc-begin-block unwind-exception-bb)
        (let (go-index)
          (cmp:with-begin-end-catch ((exn.slot function-info) exception-ptr nil)
            ;; Check if frame is correct against tagbody and jump to jumpid
            (cmp:with-landing-pad landing-pad-for-unwind-rethrow ;; cmp::*current-function-terminate-landing-pad*
              (cmp:irc-low-level-trace :cclasp-eh)
              (setq go-index
                    (%intrinsic-invoke-if-landing-pad-or-call
                     "cc_landingpadUnwindMatchFrameElseRethrow" 
                     (list exception-ptr 
                           (cmp:irc-load (clasp-cleavir::translate-datum
                                          (clasp-cleavir-hir:frame-holder enter-instruction))))
                     "go-index"
                     #+(or)maybe-unwind-landing-pad))
              (with-return-values (return-vals return-value abi)
                (%intrinsic-call "cc_restoreMultipleValue0" (list return-value)))))
          (let* ((default-block (cmp:irc-basic-block-create "switch-default"))
                 (unwinds (unwinds function-info))
                 (sw (cmp:irc-switch go-index default-block (length unwinds)))
                 (jump-id 0))
            (mapc #'(lambda (one-unwind)
                      (let* ((target (first (cleavir-ir:successors one-unwind)))
                             (tag-block (gethash target tags)))
                        (llvm-sys:add-case sw (%size_t jump-id) tag-block))
                      (incf jump-id))
                  unwinds)
            (cmp:irc-begin-block default-block)
            (%intrinsic-invoke-if-landing-pad-or-call "throwIllegalSwitchValue"
                                                      (list go-index (%size_t (length unwinds)))
                                                      ""
                                                      maybe-unwind-landing-pad)))
        (cmp:irc-unreachable))
;;;        (cmp:irc-begin-block entry-cont-block)
      landing-pad-block)))


(defun generate-ehresume-code (function function-info)
  (let* ((ehbuilder       (llvm-sys:make-irbuilder cmp:*llvm-context*))
         (ehresume        (cmp:irc-basic-block-create "ehresume" function))
         (_               (cmp:irc-set-insert-point-basic-block ehresume ehbuilder))
         (exn7            (llvm-sys:create-load-value-twine ehbuilder (exn.slot function-info) "exn7"))
         (sel             (llvm-sys:create-load-value-twine ehbuilder (ehselector.slot function-info) "sel"))
         (undef           (llvm-sys:undef-value-get cmp:%exception-struct% ))
         (lpad.val        (llvm-sys:create-insert-value ehbuilder undef exn7 '(0) "lpad.val"))
         (lpad.val8       (llvm-sys:create-insert-value ehbuilder lpad.val sel '(1) "lpad.val8"))
         (_               (llvm-sys:create-resume ehbuilder lpad.val8)))
    ehresume))

(defun generate-cleanup-block (function function-info ehresume-block)
  (let* ((cleanup-block             (cmp:irc-basic-block-create "cleanup" function))
         (ehbuilder                 (llvm-sys:make-irbuilder cmp:*llvm-context*))
         (_                         (cmp:irc-set-insert-point-basic-block cleanup-block ehbuilder)))
    (cmp:with-irbuilder (ehbuilder)
      (let* ((_                         (funcall (on-exit-for-cleanup function-info) function-info))
             (_                         (cmp:irc-br ehresume-block)))))
    cleanup-block))


(defun generate-landing-pad-for-cleanup (function function-info resume-block)
  (let* ((cleanup-landing-pad       (cmp:irc-basic-block-create "cleanup-lpad" function))
         (ehbuilder                 (llvm-sys:make-irbuilder cmp:*llvm-context*))
         (_                         (cmp:irc-set-insert-point-basic-block cleanup-landing-pad ehbuilder)))
    (cmp:with-irbuilder (ehbuilder)
      (let* ((landpad                   (cmp:irc-create-landing-pad 1))
             (_                         (llvm-sys:set-cleanup landpad t))
             (_                         (cmp:preserve-exception-info landpad (exn.slot function-info) (ehselector.slot function-info)))
             (_                         (cmp:irc-br resume-block)))))
    cleanup-landing-pad))



#++
(defmacro with-cleavir-landing-pad ((function irbuilder-entry) cleanup-lambda &body body)
  `(let* ((cmp::*current-function-exn.slot* (alloca-i8* "exn.slot"))
          (cmp::*current-function-ehselector.slot* (alloca-i32 "ehselector.slot"))
          (cmp::*exception-handler-cleanup-block* (cmp::generate-ehcleanup-and-resume-code ,function cmp::*current-function-exn.slot* cmp::*current-function-ehselector.slot* ,cleanup-lambda))
          (*cleanup-lambda* ,cleanup-lambda)
          #++(*current-function-terminate-landing-pad* (generate-terminate-code ,function))
          (cmp::*current-unwind-landing-pad-dest* (generate-cleanup-landing-pad ,function cmp::*exception-handler-cleanup-block*)))
     ,@body))


(defun generate-needed-landing-pads (function function-info return-value tags abi)
  ;; Setup the landing pads
  (when (or (debug-on function-info) (unwind-target function-info))
    (let* ((ehresume-block  (generate-ehresume-code function function-info))
           (cleanup-block (when (debug-on function-info)
                            (generate-cleanup-block function function-info ehresume-block))))
      (when cleanup-block
        (setf (landing-pad-for-cleanup function-info)
              (generate-landing-pad-for-cleanup function function-info cleanup-block)))
      ;; If (landing-pad-for-cleanup function-info) is NIL then if the UNWIND landing-pad causes
      ;; a rethrow of the exception then it will be in the context of a CALL and be passed higher up
      ;; if it isn't NIL then the rethrown exception will be caught, cleanup will be performed and then
      ;; it will be rethrown
      (when (unwind-target function-info)
        (let ((landing-pad-for-unwind-rethrow
                (cmp::generate-rethrow-landing-pad cmp::*current-function*
                                                   (if cleanup-block
                                                       cleanup-block
                                                       ehresume-block)
                                                   '(cmp::typeid-core-unwind)
                                                   (exn.slot function-info)
                                                   (ehselector.slot function-info))))
          (setf (landing-pad-for-unwind function-info)
                (generate-landing-pad-for-unwind function-info (enter-instruction function-info)
                                                 return-value tags abi
                                                 (landing-pad-for-cleanup function-info)
                                                 landing-pad-for-unwind-rethrow
                                                 (if cleanup-block
                                                     cleanup-block
                                                     ehresume-block)
                                                 :is-cleanup (not (null cleanup-block)))))))))
                                                                                           

(defun evaluate-cleanup-code (function-info)
  (and (on-exit-for-unwind function-info) (funcall (on-exit-for-unwind function-info) function-info))
  (and (on-exit-for-cleanup function-info) (funcall (on-exit-for-cleanup function-info) function-info)))
