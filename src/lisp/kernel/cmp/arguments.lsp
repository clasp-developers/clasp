(in-package :cmp)



(defun translate-datum (datum)
  (cond
    ((consp datum)
     (irc-alloca-tsp))
    (t
     (break "What do I do with datum"))))

(defun gather-required-arguments (reqargs
				  args ;;  calling-convention arguments
				  arg-idx-alloca ;; this is now in a register
				  )
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-required-arguments"))
  (compile-error-if-not-enough-arguments (calling-convention-nargs args)
					     (%size_t (car reqargs)))
  (dbg-set-current-debug-location-here)
  (irc-low-level-trace :arguments)
  ;; Now get the required arguments
  (let (reqs)
    (do* ((cur-req (cdr reqargs) (cdr cur-req))
	  (target (car cur-req) (car cur-req))
	  (arg-idx (irc-load arg-idx-alloca)
		   (irc-add arg-idx (%size_t 1) "arg-idx")))
	 ((endp cur-req) (%store arg-idx arg-idx-alloca))
      (let* ((target-alloca (translate-datum target))
	     (val-ref (calling-convention-args.gep args arg-idx)))
	(%store (irc-load val-ref) target-alloca)
	(push target-alloca reqs)))
    (nreverse reqs)))

(defun gather-optional-arguments (optargs
				  args ;; nargs va-list
				  arg-idx-alloca
				  true-val)
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-optional-arguments"))
  ;; Copy argument values or evaluate init forms
  (let (opts)
    (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	  (target (car cur-opt) (car cur-opt))
	  (flag (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
	  (arg-idx (irc-load arg-idx-alloca) (irc-add arg-idx (%size_t 1) "arg-idx")))
	 ((endp cur-opt) (%store arg-idx arg-idx-alloca))
      (let ((arg-block (irc-basic-block-create "opt-arg"))
	    (init-block (irc-basic-block-create "opt-init"))
	    (cont-block (irc-basic-block-create "opt-cont"))
	    (cmp (irc-icmp-slt arg-idx (calling-convention-nargs args) "enough-given-args")))
	(irc-cond-br cmp arg-block init-block)
	(irc-begin-block arg-block)
	(let ((target-alloca (translate-datum target))
	      (flag-alloca (translate-datum flag))
	      (val-ref (calling-convention-args.gep args arg-idx)))
	  (irc-low-level-trace :arguments)
	  (%store (irc-load val-ref) target-alloca)
	  (%store true-val flag-alloca)
	  (irc-br cont-block)
	  (irc-begin-block init-block)
	  (irc-low-level-trace :arguments)
	  (%store (%nil) target-alloca)
	  (%store (%nil) flag-alloca)
	  (irc-br cont-block)
	  (irc-begin-block cont-block)
	  (irc-low-level-trace :arguments)
	  (push target-alloca opts)
	  (push flag-alloca opts))))
    (nreverse opts)))

(defun compile-rest-argument (rest-var
			      args ;; nargs va-list
			      arg-idx-alloca)
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-rest-argument"))
  (when rest-var
  ;; Copy argument values or evaluate init forms
    (let* ((arg-idx (irc-load arg-idx-alloca))
	   (ptr (llvm-sys:create-geparray *irbuilder* 
					  (calling-convention-args args)
					  (list (%i32 0) (%i32 0)) "multiple-values"))
	   (rest (irc-intrinsic "cc_gatherRestArguments" 
				    (calling-convention-nargs args)
				    ptr
				    arg-idx *gv-current-function-name*))
	   (rest-alloca (translate-datum rest-var)))
      (%store rest rest-alloca)
      rest-alloca)))

(defun compile-key-arguments (keyargs
			      lambda-list-allow-other-keys
			      args	; nargs va-list
			      arg-idx-alloca
			      true-val)
  "saw-aok keeps track if &allow-other-keys was defined or if :allow-other-keys t/nil was seen.
   saw-aok can have the values (2[&a-o-k or :a-o-k t], 1[:a-o-k nil] or 0 [no &a-o-k or :a-o-k]) "
  ;; Process the keyword arguments
  (let ((process-kw-args-block (irc-basic-block-create "process-kw-args")))
    (irc-branch-to-and-begin-block process-kw-args-block)
    ;; Nil the supplied-p sensors
    (do* ((cur-key (cdr keyargs) (cddddr cur-key))
	  (supplied (cadddr cur-key) (cadddr cur-key))
	  (idx 0 (1+ idx)))
	 ((null cur-key) nil)
      (let ((supplied-alloca (translate-datum supplied)))
	(%store (%nil) supplied-alloca)))
    (let* ((entry-saw-aok (%size_t (if lambda-list-allow-other-keys 2 0)))
	   (entry-bad-kw-idx (%size_t 65536))
	   (aok-val (%literal :allow-other-keys "aok"))
	   (loop-kw-args-block (irc-basic-block-create "loop-kw-args"))
	   (kw-exit-block (irc-basic-block-create "kw-exit-block"))
	   (loop-cont-block (irc-basic-block-create "loop-cont"))
	   (kw-start-block (irc-basic-block-create "kw-begin-block"))
	   (entry-arg-idx (irc-load arg-idx-alloca "arg-idx")))
      (irc-branch-to-and-begin-block kw-start-block)
      (let ((entry-arg-idx_lt_nargs (irc-icmp-slt entry-arg-idx (calling-convention-nargs args))) )
	(irc-cond-br entry-arg-idx_lt_nargs loop-kw-args-block kw-exit-block))
      (irc-begin-block loop-kw-args-block)
      (let* ((phi-saw-aok (irc-phi +size_t+ 2 "phi-saw-aok"))
	     (phi-arg-idx (irc-phi +size_t+ 2 "phi-reg-arg-idx"))
	     (phi-bad-kw-idx (irc-phi +size_t+ 2 "phi-bad-kw-idx")) )
	(irc-phi-add-incoming phi-saw-aok entry-saw-aok kw-start-block)
	(irc-phi-add-incoming phi-arg-idx entry-arg-idx kw-start-block)
	(irc-phi-add-incoming phi-bad-kw-idx entry-bad-kw-idx kw-start-block)
	(irc-low-level-trace :arguments)
	(let* ((arg-val (irc-load (calling-convention-args.gep args phi-arg-idx)))) ;; (irc-gep va-list (list phi-arg-idx))))
	  (irc-intrinsic "cc_ifNotKeywordException" arg-val)
	  (let* ((eq-aok-val-and-arg-val (irc-trunc (irc-icmp-eq aok-val arg-val) +i1+)) ; compare arg-val to a-o-k
		 (aok-block (irc-basic-block-create "aok-block"))
		 (possible-kw-block (irc-basic-block-create "possible-kw-block"))
		 (advance-arg-idx-block (irc-basic-block-create "advance-arg-idx-block"))
		 (bad-kw-block (irc-basic-block-create "bad-kw-block"))
		 (use-kw-block (irc-basic-block-create "use-kw-block"))
		 (good-kw-block (irc-basic-block-create "good-kw-block")))
	    (irc-cond-br eq-aok-val-and-arg-val aok-block possible-kw-block)
	    (irc-begin-block aok-block)
	    (let* ((loop-saw-aok (irc-intrinsic "cc_allowOtherKeywords"
						    phi-saw-aok
						    (calling-convention-nargs args)
						    (calling-convention-args args)
						    phi-arg-idx)) )
	      (irc-br advance-arg-idx-block)
	      (irc-begin-block possible-kw-block)
	      ;; Generate a test for each keyword
	      (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
		    (key (car cur-key-arg) (car cur-key-arg))
		    (target (caddr cur-key-arg) (caddr cur-key-arg))
		    (supplied (cadddr cur-key-arg) (cadddr cur-key-arg))
		    (idx 0 (1+ idx))
		    (next-kw-test-block (irc-basic-block-create "next-kw-block")
					(irc-basic-block-create "next-kw-block")) )
		   ((endp cur-key-arg))
		(irc-branch-to-and-begin-block (irc-basic-block-create (core:bformat nil "kw-%s-test" key)))
		(irc-low-level-trace :arguments)
		(let* ((kw-val (%literal key (string key)))
		       (target-ref (translate-datum target))
		       (supplied-ref (translate-datum supplied))
		       (test-kw-and-arg (irc-intrinsic "cc_matchKeywordOnce" kw-val arg-val (irc-load supplied-ref)))
		       (no-kw-match (irc-icmp-eq test-kw-and-arg (%size_t 0)))
		       (matched-kw-block (irc-basic-block-create "matched-kw-block"))
		       (not-seen-before-kw-block (irc-basic-block-create "not-seen-before-kw-block")))
		  (irc-cond-br no-kw-match next-kw-test-block matched-kw-block)
		  (irc-begin-block matched-kw-block)
		  (let ((arg-idx+1 (irc-add phi-arg-idx (%size_t 1)))
			(kw-seen-already (irc-icmp-eq test-kw-and-arg (%size_t 2))))
		    (irc-cond-br kw-seen-already good-kw-block not-seen-before-kw-block)
		    (irc-begin-block not-seen-before-kw-block)
		    (let ((kw-arg-val (irc-load (calling-convention-args.gep args arg-idx+1))))
		      (%store kw-arg-val target-ref)
		      ;; Set the boolean flag to indicate that we saw this key
		      (%store true-val supplied-ref)
		      (irc-br good-kw-block)
		      (irc-begin-block next-kw-test-block)
		      ))))
	      ;; We fell through all the keyword tests - this might be a unparameterized keyword
	      (irc-branch-to-and-begin-block bad-kw-block) ; fall through to here if no kw recognized
	      (let ((loop-bad-kw-idx (irc-intrinsic "kw_trackFirstUnexpectedKeyword"
							phi-bad-kw-idx phi-arg-idx)))
		(irc-low-level-trace :arguments)
		(irc-br advance-arg-idx-block)
		(irc-begin-block good-kw-block) ; jump to here if kw was recognized
		(irc-low-level-trace :arguments)
		(irc-br advance-arg-idx-block)
		;; Now advance the arg-idx, finish up the phi-nodes
		;; and if we ran out of arguments branch out of the loop else branch to the top of the loop
		(irc-begin-block advance-arg-idx-block)
		(let* ((phi-arg-bad-good-aok (irc-phi +size_t+ 3 "phi-this-was-aok"))
		       (phi.aok-bad-good.bad-kw-idx (irc-phi +size_t+ 3 "phi.aok-bad-good.bad-kw-idx")))
		  (irc-phi-add-incoming phi-arg-bad-good-aok loop-saw-aok aok-block)
		  (irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok bad-kw-block)
		  (irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok good-kw-block)
		  (irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx aok-block)
		  (irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx loop-bad-kw-idx bad-kw-block)
		  (irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx good-kw-block)
		  (irc-low-level-trace :arguments)
		  (let* ((loop-arg-idx (irc-add phi-arg-idx (%size_t 2)))
			 (loop-arg-idx_lt_nargs (irc-icmp-slt loop-arg-idx (calling-convention-nargs args))))
		    (irc-phi-add-incoming phi-saw-aok phi-arg-bad-good-aok advance-arg-idx-block)
		    (irc-phi-add-incoming phi-bad-kw-idx phi.aok-bad-good.bad-kw-idx advance-arg-idx-block)
		    (irc-phi-add-incoming phi-arg-idx loop-arg-idx advance-arg-idx-block)
		    (irc-cond-br loop-arg-idx_lt_nargs loop-kw-args-block loop-cont-block)
		    (irc-begin-block loop-cont-block)
		    (irc-intrinsic "cc_ifBadKeywordArgumentException" phi-arg-bad-good-aok phi.aok-bad-good.bad-kw-idx (calling-convention-nargs args) (calling-convention-args args))
		    (let ((kw-done-block (irc-basic-block-create "kw-done-block")))
		      (irc-branch-to-and-begin-block kw-done-block)
		      (irc-branch-to-and-begin-block kw-exit-block)
		      (let ((phi-arg-idx-final (irc-phi +size_t+ 2 "phi-arg-idx-final")))
			(irc-phi-add-incoming phi-arg-idx-final entry-arg-idx kw-start-block)
			(irc-phi-add-incoming phi-arg-idx-final loop-arg-idx kw-done-block)
			(irc-low-level-trace :arguments)
			phi-arg-idx-final))))))))))))

(defun compile-general-lambda-list-code (reqargs 
					 optargs 
					 rest-var 
					 key-flag 
					 keyargs 
					 allow-other-keys
					 outputs
					 calling-conv )
  (calling-convention-write-passed-arguments-to-multiple-values calling-conv INSERT-ENVIRONMENT!!!!)
  (let* ((arg-idx-alloca (alloca-size_t "arg-idx-alloca"))
	 true-val)
    (%store (%size_t 0) arg-idx-alloca)
    (gather-required-arguments reqargs calling-conv arg-idx-alloca)
    (dolist (req (cdr reqargs))
      (let ((req-alloca (translate-datum req))
	    (out-alloca (translate-datum (pop outputs))))
	(%store (irc-load req-alloca) out-alloca)))
    (when (or (> (first optargs) 0) key-flag)
      (setq true-val (%literal t "T")))
    (when (> (first optargs) 0)
      (gather-optional-arguments optargs calling-conv arg-idx-alloca true-val)
      (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	    (target (car cur-opt) (car cur-opt))
	    (supplied-p (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
	    (arg-idx (irc-load arg-idx-alloca) (irc-add arg-idx (%size_t 1) "arg-idx")))
	   ((endp cur-opt))
	(let ((target-alloca (translate-datum target))
	      (supplied-alloca (translate-datum supplied-p))
	      (target-out-alloca (translate-datum (pop outputs)))
	      (supplied-out-alloca (translate-datum (pop outputs))))
	  (%store (irc-load target-alloca) target-out-alloca)
	  (%store (irc-load supplied-alloca) supplied-out-alloca))))
    (when rest-var
      (compile-rest-argument rest-var calling-conv arg-idx-alloca)
      (%store (irc-load (translate-datum rest-var)) (translate-datum (pop outputs))))
    (when key-flag
      (compile-key-arguments keyargs allow-other-keys calling-conv arg-idx-alloca true-val)
      (do* ((cur-key (cdr keyargs) (cddddr cur-key))
	    (target (caddr cur-key) (caddr cur-key))
	    (supplied (cadddr cur-key) (cadddr cur-key)))
	   ((null cur-key))
	(let ((target-output-ref (translate-datum (pop outputs)))
	      (supplied-output-ref (translate-datum (pop outputs))))
	  (%store (irc-load (translate-datum target)) target-output-ref)
	  (%store (irc-load (translate-datum supplied)) supplied-output-ref)))
      )))

(defun compile-<=3-required-arguments (reqargs outputs cc)
;;  (compile-error-if-wrong-number-of-arguments (calling-convention-nargs cc) (car reqargs))
  (let ((fixed-args (calling-convention-register-args cc)))
    (do* ((cur-target (cdr reqargs) (cdr cur-target))
	  (cur-fixed-args fixed-args (cdr cur-fixed-args))
	  (cur-output outputs (cdr cur-output))
	  (target (car cur-output) (car cur-output))
	  (arg (car cur-fixed-args) (car cur-fixed-args)))
	 ((null cur-target))
      (let ((dest (translate-datum target)))
	#+(or)(format t "compile-<=3-required-arguments store: ~a to ~a  target: ~a~%" arg dest target)
	(llvm-sys:create-store *irbuilder* arg dest nil)))))

(defun compile-lambda-list-code (lambda-list outputs calling-conv)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys)
      (core:process-lambda-list lambda-list 'core::function)
    (let ((req-opt-only (and (not rest-var)
                             (not key-flag)
                             (eql 0 (car keyargs))
                             (not allow-other-keys)))
          (num-req (car reqargs))
          (num-opt (car optargs)))
      #+(or)(progn
	(format t "lambda-list: ~a~%" lambda-list)
	(format t "reqs: ~a~%" reqargs)
	(format t "opts: ~a~%" optargs)
	(format t "rest: ~a~%" rest-var)
	(format t "key-flag: ~a~%" key-flag)
	(format t "keys: ~a~%" keyargs)
	(format t "allow-other-keys: ~a~%" allow-other-keys))
      (cond
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
	#+(or)((and req-opt-only (<= num-req 3) (eql 0 num-opt) )
	 (compile-<=3-required-arguments reqargs outputs calling-conv))
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (t
         (compile-general-lambda-list-code reqargs 
					   optargs 
					   rest-var 
					   key-flag 
					   keyargs 
					   allow-other-keys
					   outputs
					   calling-conv ))))))


;;; Process arguments for bclasp
(defun bclasp-compile-lambda-list-code (lambda-list-handler
                                        old-env
                                        args
                                        new-env)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let (outputs)
      (dolist (req reqargs)
        (push (irc-alloca-tsp) outputs))
      (dolist (opt optargs)
        (push (irc-alloca-tsp) outputs)   ;; opt target
        (push (irc-alloca-tsp) outputs)   ;; opt-p target
        )
      (if rest-var
          (push (irc-alloca-tsp) outputs) ;; rest target
          )
      (dolist (key keyargs)
        (push (irc-alloca-tsp) outputs)   ;; key target
        (push (irc-alloca-tsp) outputs)   ;; key-p target
        )
      (setq outputs (nreverse outputs))
      (let ((req-opt-only (and (not rest-var)
                               (not key-flag)
                               (eql 0 (car keyargs))
                               (eql 0 (car auxargs))
                               (not allow-other-keys)))
            (num-req (car reqargs))
            (num-opt (car optargs)))
        (cond
          ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
          ((and req-opt-only (<= num-req 3) (eql 0 num-opt) )
           (compile-<=3-required-arguments reqargs outputs args))
          ;; Test for
          ;; (x &optional y)
          ;; (x y &optional z)
          (t
           (compile-general-lambda-list-code lambda-list-handler old-env args new-env))))
      ;; Now copy outputs into the targets and generate code for initializers if opt-p or key-p is not defined
      )))

        
