(in-package :clasp-cleavir)


(defun create-landing-pad (exn.slot-alloca ehselector.slot-alloca landing-pad-info tags)
  (let ((entry-cont-block (cmp:irc-basic-block-create "entry-cont"))
	(landing-pad-block (cmp:irc-basic-block-create "landing-pad"))
	(terminate-block (cmp:irc-basic-block-create "terminate"))
	(landing-pad-id (clasp-cleavir:landing-pad-id landing-pad-info)))
    (cmp:irc-br entry-cont-block)
    (cmp:irc-begin-block landing-pad-block)
    (let ((lpad (cmp:irc-create-landing-pad 1 "")))
      (cmp:irc-add-clause lpad (cmp:irc-exception-typeid* 'cmp:typeid-core-dynamic-go))
      (let ((exception-structure (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 0) "")))
	(llvm-sys:create-store cmp:*irbuilder* exception-structure exn.slot-alloca nil))
      (let ((exception-selector (llvm-sys:create-extract-value cmp:*irbuilder* lpad (list 1) "")))
	(llvm-sys:create-store cmp:*irbuilder* exception-selector ehselector.slot-alloca nil))
      ;;(cmp:irc-branch-to-and-begin-block dispatch-header
      )
    (cmp:with-catch (exn.slot-alloca exception-ptr nil)
      ;; Check if frame is correct against tagbody and jump to jumpid
      (cmp:with-landing-pad terminate-block
	(let* ((go-index (cmp:irc-intrinsic "tagbodyDynamicGoIndexElseRethrow" 
					    exception-ptr 
					    (%i32 landing-pad-id)))
	       (default-block (cmp:irc-basic-block-create "switch-default"))
	       (targets (clasp-cleavir:targets landing-pad-info))
	       (sw (cmp:irc-switch go-index default-block (length targets)))
	       (jump-id 0))
	  (mapc #'(lambda (one)
		    (let ((tag-block (gethash one tags)))
		      (llvm-sys:add-case sw (%i32 jump-id) tag-block)))
		targets)
	  (cmp:irc-begin-block default-block)
	  (cmp:irc-intrinsic "throwIllegalSwitchValue"
			     go-index (%i32 (length targets))))))
    (cmp:irc-intrinsic "exceptionStackUnwind" (%i32 landing-pad-id))
    (cmp:irc-unreachable)
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
