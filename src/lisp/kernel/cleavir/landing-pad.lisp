(in-package :clasp-cleavir)


(defun create-landing-pad (exn.slot-alloca ehselector.slot-alloca enter-instruction landing-pad-object tags abi)
  (let ((entry-cont-block (cmp:irc-basic-block-create "entry-cont"))
	(landing-pad-block (cmp:irc-basic-block-create "landing-pad"))
	(terminate-block (cmp:irc-basic-block-create "terminate")))
    (cmp:irc-br entry-cont-block)
    (cmp:irc-begin-block landing-pad-block)
    (let ((lpad (cmp:irc-create-landing-pad 1 "lp")))
      (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind))
      (let* ((exception-structure (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 0) "exception-structure"))
	     (exception-selector (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 1) "exception-selector")))
	(llvm-sys:create-store cmp:*irbuilder* exception-structure exn.slot-alloca nil)
	(llvm-sys:create-store cmp:*irbuilder* exception-selector ehselector.slot-alloca nil)
	(let* ((not-unwind-exception-bb (cmp:irc-basic-block-create "not-unwind-exception"))
	       (unwind-exception-bb (cmp:irc-basic-block-create "match-unwind"))
	       (typeid (cmp:irc-intrinsic "llvm.eh.typeid.for"
					  (cmp:irc-exception-typeid* 'cmp:typeid-core-unwind)))
	       (matches-type (cmp:irc-icmp-eq exception-selector typeid)))
	  (cmp:irc-cond-br matches-type unwind-exception-bb not-unwind-exception-bb)
	  (cmp:irc-begin-block unwind-exception-bb)
          (let (go-index)
            (cmp:with-catch (exn.slot-alloca exception-ptr nil)
              ;; Check if frame is correct against tagbody and jump to jumpid
              (cmp:with-landing-pad terminate-block
                (cmp:irc-low-level-trace :cclasp-eh)
                (setq go-index
                      (cmp:irc-create-call
                       "cc_landingpadUnwindMatchFrameElseRethrow" 
                       (list exception-ptr 
                             (cmp:irc-load (clasp-cleavir::translate-datum
                                            (clasp-cleavir-hir:frame-holder enter-instruction))))))
                (with-return-values (return-vals abi)
                  (cmp:irc-intrinsic "cc_restoreMultipleValue0" (sret-arg return-vals)))))
            (let* ((default-block (cmp:irc-basic-block-create "switch-default"))
                   (unwinds (unwinds landing-pad-object))
                   (sw (cmp:irc-switch go-index default-block (length unwinds)))
                   (jump-id 0))
              (mapc #'(lambda (one-unwind)
                        (let* ((target (first (cleavir-ir:successors one-unwind)))
                               (tag-block (gethash target tags)))
                          (llvm-sys:add-case sw (%size_t jump-id) tag-block))
                        (incf jump-id))
                    unwinds)
              (cmp:irc-begin-block default-block)
              (cmp:irc-intrinsic "throwIllegalSwitchValue"
                                 go-index (%size_t (length unwinds)))))
	  (cmp:irc-unreachable)
	  (cmp:irc-begin-block not-unwind-exception-bb)
	  (let* ((exn7 (llvm-sys:create-load-value-twine cmp:*irbuilder* exn.slot-alloca "exn7"))
		 (sel (cmp:irc-load ehselector.slot-alloca))
		 (undef (llvm-sys:undef-value-get cmp:+exception-struct+ ))
		 (lpad.val (llvm-sys:create-insert-value cmp:*irbuilder*
							 undef exn7 '(0) "lpad.val"))
		 (lpad.val8 (llvm-sys:create-insert-value cmp:*irbuilder*
							  lpad.val sel '(1) "lpad.val8")))
	    (llvm-sys:create-resume cmp:*irbuilder* lpad.val8)))))
    ;;(cmp:irc-intrinsic "exceptionStackUnwind" (cmp:irc-load landing-pad-id-alloca)) ;; What is this for?
    (cmp:irc-begin-block terminate-block)
    (cmp:irc-generate-terminate-code)
    (cmp:irc-begin-block entry-cont-block)
    landing-pad-block))

#+(or)(progn (progn (progn
	       (cmp:irc-preserve-exception-info ,env ,landpad-gs)
	       (irc-branch-to-and-begin-block ,dispatch-header-gs)
	       ,@(when cleanup-clause-body
		       cleanup-clause-body)
	       ,(if first-dispatcher-block-gs
		    `(irc-br ,first-dispatcher-block-gs)
		    `(irc-br ,parent-cleanup-block-gs))
	       ,@(maplist #'(lambda (cur-disp-block-gs cur-clause-gs)
			      (try.one-dispatcher-and-handler (car cur-disp-block-gs)
							      (if (cadr cur-disp-block-gs)
								  (cadr cur-disp-block-gs)
								  parent-cleanup-block-gs)
							      (car cur-clause-gs)
							      cont-block-gs
							      exn.slot-gs ehselector.slot-gs env)
			      )
			  dispatcher-block-gensyms
			  exception-clauses)
	       ))
	     (irc-branch-if-no-terminator-inst ,cont-block-gs)
	     (irc-begin-block ,cont-block-gs)
	     )
