(in-package :clasp-cleavir)


(defun gather-required-arguments (reqargs
				  args ;;  calling-convention arguments
				  arg-idx-alloca ;; this is now in a register
				  )
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-required-arguments"))
  (cmp:compile-error-if-not-enough-arguments (cmp:calling-convention-nargs args)
					     (%size_t (car reqargs)))
  (cmp:irc-low-level-trace :arguments)
  ;; Now get the required arguments
  (let (reqs)
    (do* ((cur-req (cdr reqargs) (cdr cur-req))
	  (target (car cur-req) (car cur-req))
	  (arg-idx (cmp:irc-load arg-idx-alloca)
		   (cmp:irc-add arg-idx (%size_t 1) "arg-idx")))
	 ((endp cur-req) (%store arg-idx arg-idx-alloca))
      (let* ((target-alloca (translate-datum target))
	     (val (cmp:calling-convention-args.va-arg args)))
	(%store val target-alloca)
	(push target-alloca reqs)))
    (nreverse reqs)))



(defun gather-optional-arguments (optargs
				  args ;; nargs va-list
				  arg-idx-alloca
				  true-val)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-optional-arguments"))
  ;; Copy argument values or evaluate init forms
  (let (opts)
    (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	  (target (car cur-opt) (car cur-opt))
	  (flag (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
	  (arg-idx (cmp:irc-load arg-idx-alloca) (cmp:irc-add arg-idx (%size_t 1) "arg-idx")))
	 ((endp cur-opt) (%store arg-idx arg-idx-alloca))
      (let ((arg-block (cmp:irc-basic-block-create "opt-arg"))
	    (init-block (cmp:irc-basic-block-create "opt-init"))
	    (cont-block (cmp:irc-basic-block-create "opt-cont"))
	    (cmp (cmp:irc-icmp-slt arg-idx (cmp:calling-convention-nargs args) "enough-given-args")))
	(cmp:irc-cond-br cmp arg-block init-block)
	(cmp:irc-begin-block arg-block)
	(let ((target-alloca (translate-datum target))
	      (flag-alloca (translate-datum flag))
	      (val (cmp:calling-convention-args.va-arg args)))
	  (cmp:irc-low-level-trace :arguments)
	  (%store val target-alloca)
	  (%store true-val flag-alloca)
	  (cmp:irc-br cont-block)
	  (cmp:irc-begin-block init-block)
	  (cmp:irc-low-level-trace :arguments)
	  (%store (%nil) target-alloca)
	  (%store (%nil) flag-alloca)
	  (cmp:irc-br cont-block)
	  (cmp:irc-begin-block cont-block)
	  (cmp:irc-low-level-trace :arguments)
	  (push target-alloca opts)
	  (push flag-alloca opts))))
    (nreverse opts)))



(defun compile-rest-argument (rest-var
                              varest-p
			      args ;; nargs va-list
			      arg-idx-alloca)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (when rest-var
    ;; Copy argument values or evaluate init forms
    (let* ((arg-idx (cmp:irc-load arg-idx-alloca))
	   (rest (if varest-p
                     (let ((temp-valist (alloca-vaslist)))
                       (%intrinsic-call "cc_gatherVaRestArguments" 
                                            (list (cmp:calling-convention-va-list* args)
                                                  (cmp:calling-convention-remaining-nargs* args)
                                                  temp-valist)))
                     (%intrinsic-call "cc_gatherRestArguments" 
                                          (list (cmp:calling-convention-va-list* args)
                                                (cmp:calling-convention-remaining-nargs* args)))))
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
  (let ((process-kw-args-block (cmp:irc-basic-block-create "process-kw-args")))
    (cmp:irc-branch-to-and-begin-block process-kw-args-block)
    ;; Nil the supplied-p sensors
    (do* ((cur-key (cdr keyargs) (cddddr cur-key))
	  (supplied (cadddr cur-key) (cadddr cur-key))
	  (idx 0 (1+ idx)))
	 ((null cur-key) nil)
      (let ((supplied-alloca (translate-datum supplied)))
	(%store (%nil) supplied-alloca)))
    (let* ((entry-saw-aok (%size_t (if lambda-list-allow-other-keys 2 0)))
	   (entry-bad-kw-idx (%size_t 65536))
	   (aok-val (%literal-value :allow-other-keys "aok"))
	   (loop-kw-args-block (cmp:irc-basic-block-create "loop-kw-args"))
	   (kw-exit-block (cmp:irc-basic-block-create "kw-exit-block"))
	   (loop-cont-block (cmp:irc-basic-block-create "loop-cont"))
	   (kw-start-block (cmp:irc-basic-block-create "kw-begin-block"))
	   (entry-arg-idx (cmp:irc-load arg-idx-alloca "arg-idx")))
      (cmp:irc-branch-to-and-begin-block kw-start-block)
      (let ((entry-arg-idx_lt_nargs (cmp:irc-icmp-slt entry-arg-idx (cmp:calling-convention-nargs args))) )
	(cmp:irc-cond-br entry-arg-idx_lt_nargs loop-kw-args-block kw-exit-block))
      (cmp:irc-begin-block loop-kw-args-block)
      (let* ((phi-saw-aok (cmp:irc-phi cmp:%size_t% 2 "phi-saw-aok"))
	     (phi-arg-idx (cmp:irc-phi cmp:%size_t% 2 "phi-reg-arg-idx"))
	     (phi-bad-kw-idx (cmp:irc-phi cmp:%size_t% 2 "phi-bad-kw-idx")) )
	(cmp:irc-phi-add-incoming phi-saw-aok entry-saw-aok kw-start-block)
	(cmp:irc-phi-add-incoming phi-arg-idx entry-arg-idx kw-start-block)
	(cmp:irc-phi-add-incoming phi-bad-kw-idx entry-bad-kw-idx kw-start-block)
	(cmp:irc-low-level-trace :arguments)
	(let* ((arg-val (cmp:calling-convention-args.va-arg args))
               (arg-idx+1 (cmp:irc-add phi-arg-idx (cmp:jit-constant-size_t 1)))
               (kw-arg-val (cmp:calling-convention-args.va-arg args)))
          ;;; FIXME: This must INVOKE if the function has cleanup forms.
	  (%intrinsic-invoke-if-landing-pad-or-call "cc_ifNotKeywordException" (list arg-val phi-arg-idx (cmp:calling-convention-va-list* args)))
	  (let* ((eq-aok-val-and-arg-val (cmp:irc-trunc (cmp:irc-icmp-eq aok-val arg-val) cmp:%i1%)) ; compare arg-val to a-o-k
		 (aok-block (cmp:irc-basic-block-create "aok-block"))
		 (possible-kw-block (cmp:irc-basic-block-create "possible-kw-block"))
		 (advance-arg-idx-block (cmp:irc-basic-block-create "advance-arg-idx-block"))
		 (bad-kw-block (cmp:irc-basic-block-create "bad-kw-block"))
		 (good-kw-block (cmp:irc-basic-block-create "good-kw-block")))
	    (cmp:irc-cond-br eq-aok-val-and-arg-val aok-block possible-kw-block)
	    (cmp:irc-begin-block aok-block)
	    (let* ((loop-saw-aok (%intrinsic-call "cc_allowOtherKeywords" (list phi-saw-aok kw-arg-val))))
	      (cmp:irc-br advance-arg-idx-block)
	      (cmp:irc-begin-block possible-kw-block)
	      ;; Generate a test for each keyword
	      (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
		    (key (car cur-key-arg) (car cur-key-arg))
		    (target (caddr cur-key-arg) (caddr cur-key-arg))
		    (supplied (cadddr cur-key-arg) (cadddr cur-key-arg))
		    (idx 0 (1+ idx))
		    #+(or)(next-kw-block (cmp:irc-basic-block-create "next-kw-block")
                                   (cmp:irc-basic-block-create "next-kw-block")) )
		   ((endp cur-key-arg))
		(cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create (core:bformat nil "kw-%s-test" key)))
		(cmp:irc-low-level-trace :arguments)
		(let* ((kw-val (%literal-value key (string key)))
		       (target-ref (translate-datum target))
		       (supplied-ref (translate-datum supplied))
		       (test-kw-and-arg (%intrinsic-call "cc_matchKeywordOnce" (list kw-val arg-val (cmp:irc-load supplied-ref))))
		       (no-kw-match (cmp:irc-icmp-eq test-kw-and-arg (%size_t 0)))
		       (matched-kw-block (cmp:irc-basic-block-create "matched-kw-block"))
		       (not-seen-before-kw-block (cmp:irc-basic-block-create "not-seen-before-kw-block"))
                       (next-kw-block (cmp:irc-basic-block-create "next-kw-block")))
		  (cmp:irc-cond-br no-kw-match next-kw-block matched-kw-block)
		  (cmp:irc-begin-block matched-kw-block)
		  (let ((kw-seen-already (cmp:irc-icmp-eq test-kw-and-arg (%size_t 2))))
		    (cmp:irc-cond-br kw-seen-already good-kw-block not-seen-before-kw-block)
		    (cmp:irc-begin-block not-seen-before-kw-block)
                    (%store kw-arg-val target-ref)
                    ;; Set the boolean flag to indicate that we saw this key
                    (%store true-val supplied-ref)
                    (cmp:irc-br good-kw-block)
                    (cmp:irc-begin-block next-kw-block))))
	      ;; We fell through all the keyword tests - this might be a unparameterized keyword
	      (cmp:irc-branch-to-and-begin-block bad-kw-block) ; fall through to here if no kw recognized
	      (let ((loop-bad-kw-idx (%intrinsic-call "cc_trackFirstUnexpectedKeyword"
                                                          (list phi-bad-kw-idx phi-arg-idx))))
		(cmp:irc-low-level-trace :arguments)
		(cmp:irc-br advance-arg-idx-block)
		(cmp:irc-begin-block good-kw-block) ; jump to here if kw was recognized
		(cmp:irc-low-level-trace :arguments)
		(cmp:irc-br advance-arg-idx-block)
		;; Now advance the arg-idx, finish up the phi-nodes
		;; and if we ran out of arguments branch out of the loop else branch to the top of the loop
		(cmp:irc-begin-block advance-arg-idx-block)
		(let* ((phi-arg-bad-good-aok (cmp:irc-phi cmp:%size_t% 3 "phi-this-was-aok"))
		       (phi.aok-bad-good.bad-kw-idx (cmp:irc-phi cmp:%size_t% 3 "phi.aok-bad-good.bad-kw-idx")))
		  (cmp:irc-phi-add-incoming phi-arg-bad-good-aok loop-saw-aok aok-block)
		  (cmp:irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok bad-kw-block)
		  (cmp:irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok good-kw-block)
		  (cmp:irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx aok-block)
		  (cmp:irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx loop-bad-kw-idx bad-kw-block)
		  (cmp:irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx good-kw-block)
		  (cmp:irc-low-level-trace :arguments)
		  (let* ((loop-arg-idx (cmp:irc-add phi-arg-idx (%size_t 2)))
			 (loop-arg-idx_lt_nargs (cmp:irc-icmp-slt loop-arg-idx (cmp:calling-convention-nargs args))))
		    (cmp:irc-phi-add-incoming phi-saw-aok phi-arg-bad-good-aok advance-arg-idx-block)
		    (cmp:irc-phi-add-incoming phi-bad-kw-idx phi.aok-bad-good.bad-kw-idx advance-arg-idx-block)
		    (cmp:irc-phi-add-incoming phi-arg-idx loop-arg-idx advance-arg-idx-block)
		    (cmp:irc-cond-br loop-arg-idx_lt_nargs loop-kw-args-block loop-cont-block)
		    (cmp:irc-begin-block loop-cont-block)
                    ;; FIXME    This must be an INVOKE if there is a cleanup clause in the function
		    (%intrinsic-invoke-if-landing-pad-or-call "cc_ifBadKeywordArgumentException" (list phi-arg-bad-good-aok phi.aok-bad-good.bad-kw-idx arg-val))
		    (let ((kw-done-block (cmp:irc-basic-block-create "kw-done-block")))
		      (cmp:irc-branch-to-and-begin-block kw-done-block)
		      (cmp:irc-branch-to-and-begin-block kw-exit-block)
		      (let ((phi-arg-idx-final (cmp:irc-phi cmp:%size_t% 2 "phi-arg-idx-final")))
			(cmp:irc-phi-add-incoming phi-arg-idx-final entry-arg-idx kw-start-block)
			(cmp:irc-phi-add-incoming phi-arg-idx-final loop-arg-idx kw-done-block)
			(cmp:irc-low-level-trace :arguments)
			phi-arg-idx-final))))))))))))

(defun compile-general-lambda-list-code (reqargs 
					 optargs 
					 rest-var
                                         varest-p
					 key-flag 
					 keyargs 
					 allow-other-keys
					 outputs
					 calling-conv )
  ;; TODO:  Should I be spilling the registers into the reg_save_area???
  ;;  (cmp:calling-convention-write-passed-arguments-to-multiple-values calling-conv INSERT-ENVIRONMENT!!!!)
  (let* ((arg-idx-alloca (alloca-size_t "arg-idx-alloca"))
	 true-val)
    (%store (%size_t 0) arg-idx-alloca)
    (gather-required-arguments reqargs calling-conv arg-idx-alloca)
    (dolist (req (cdr reqargs))
      (let ((req-alloca (translate-datum req))
	    (out-alloca (translate-datum (pop outputs))))
	(%store (cmp:irc-load req-alloca) out-alloca)))
    (when (or (> (first optargs) 0) key-flag)
      (setq true-val (%literal-value t "T")))
    (when (> (first optargs) 0)
      (gather-optional-arguments optargs calling-conv arg-idx-alloca true-val)
      (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	    (target (car cur-opt) (car cur-opt))
	    (supplied-p (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
	    (arg-idx (cmp:irc-load arg-idx-alloca) (cmp:irc-add arg-idx (%size_t 1) "arg-idx")))
	   ((endp cur-opt))
	(let ((target-alloca (translate-datum target))
	      (supplied-alloca (translate-datum supplied-p))
	      (target-out-alloca (translate-datum (pop outputs)))
	      (supplied-out-alloca (translate-datum (pop outputs))))
	  (%store (cmp:irc-load target-alloca) target-out-alloca)
	  (%store (cmp:irc-load supplied-alloca) supplied-out-alloca))))
    (when rest-var
      (compile-rest-argument rest-var varest-p calling-conv arg-idx-alloca)
      (%store (cmp:irc-load (translate-datum rest-var)) (translate-datum (pop outputs))))
    (when key-flag
      (compile-key-arguments keyargs allow-other-keys calling-conv arg-idx-alloca true-val)
      (do* ((cur-key (cdr keyargs) (cddddr cur-key))
	    (target (caddr cur-key) (caddr cur-key))
	    (supplied (cadddr cur-key) (cadddr cur-key)))
	   ((null cur-key))
	(let ((target-output-ref (translate-datum (pop outputs)))
	      (supplied-output-ref (translate-datum (pop outputs))))
	  (%store (cmp:irc-load (translate-datum target)) target-output-ref)
	  (%store (cmp:irc-load (translate-datum supplied)) supplied-output-ref))))
    (unless rest-var
      ;; Check if there were too many arguments passed
      (unless key-flag
        (cmp:compile-error-if-too-many-arguments (cmp:calling-convention-nargs calling-conv) (+ (car reqargs) (car optargs))))) 
    ))


(defun compile-all-register-required-arguments (reqargs outputs cc)
  (cmp:compile-error-if-wrong-number-of-arguments (cmp:calling-convention-nargs cc) (car reqargs))
  (let ((fixed-args (cmp:calling-convention-register-args cc)))
    (do* ((cur-target (cdr reqargs) (cdr cur-target))
	  (cur-fixed-args fixed-args (cdr cur-fixed-args))
	  (cur-output outputs (cdr cur-output))
	  (target (car cur-output) (car cur-output))
	  (arg (car cur-fixed-args) (car cur-fixed-args)))
	 ((null cur-target))
      (let ((dest (translate-datum target)))
	#+(or)(format t "compile-all-register-required-arguments store: ~a to ~a  target: ~a~%" arg dest target)
	(llvm-sys:create-store cmp:*irbuilder* arg dest nil)))))

(defun process-cleavir-lambda-list (lambda-list)
  ;; We assume that the lambda list is in its correct format:
  ;; 1) required arguments are lexical locations.
  ;; 2) optional arguments are (<lexical location> <lexical location>)
  ;; 3) keyword arguments are (<symbol> <lexical location> <lexical location>)
  ;; This lets us cheap out on parsing, except &rest and &allow-other-keys.
  (let (required optional rest-type rest key aok-p key-flag
        (required-count 0) (optional-count 0) (key-count 0))
    (dolist (item lambda-list)
      (case item
        ((&optional) #|ignore|#)
        ((&key) (setf key-flag t))
        ((&rest core:&va-rest) (setf rest-type item))
        ((&allow-other-keys) (setf aok-p t))
        (t (if (listp item)
               (cond ((= (length item) 2)
                      ;; optional
                      (incf optional-count)
                      ;; above, we expect (location -p whatever)
                      ;; though it's specified as (var init -p)
                      ;; FIX ME
                      (push (first item) optional)
                      (push (second item) optional)
                      (push nil optional))
                     (t ;; key, assumedly
                      (incf key-count)
                      (push (first item) key)
                      (push (first item) key)
                      ;; above, we treat this as being the location,
                      ;; even though from process-lambda-list it's
                      ;; the initform.
                      ;; This file needs work FIXME.
                      (push (second item) key)
                      (push (third item) key)))
               ;; nonlist; we picked off lambda list keywords, so it's an argument.
               (cond (rest-type
                      ;; we've seen a &rest lambda list keyword, so this must be that
                      (setf rest item))
                     ;; haven't seen anything, it's required
                     (t (incf required-count)
                        (push item required)))))))
    (values (cons required-count (nreverse required))
            (cons optional-count (nreverse optional))
            rest
            key-flag
            (cons key-count (nreverse key))
            aok-p
            nil ; aux-p; unused here
            (if (eq rest-type 'core:&va-rest) t nil))))

(defun compile-lambda-list-code (lambda-list outputs calling-conv)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys unused-auxs varest-p)
      (process-cleavir-lambda-list lambda-list)
    (if (cmp::calling-convention-use-only-registers calling-conv)
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
        (compile-all-register-required-arguments reqargs outputs calling-conv)
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (compile-general-lambda-list-code reqargs 
                                          optargs 
                                          rest-var
                                          varest-p
                                          key-flag 
                                          keyargs 
                                          allow-other-keys
                                          outputs
                                          calling-conv ))))


;;; Process arguments for bclasp
#+(or)
(defun bclasp-compile-lambda-list-code (lambda-list-handler
                                        old-env
                                        args
                                        new-env)
  (break "What do the inputs look like?")
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let (outputs)
      (dolist (req reqargs)
        (push (irc-alloca-tsp new-env) outputs))
      (dolist (opt optargs)
        (push (irc-alloca-tsp new-env) outputs) ;; opt target
        (push (irc-alloca-tsp new-env) outputs) ;; opt-p target
        )
      (if rest-var
          (push (irc-alloca-tsp new-env) outputs) ;; rest target
          )
      (dolist (key keyargs)
        (push (irc-alloca-tsp new-env) outputs) ;; key target
        (push (irc-alloca-tsp new-env) outputs) ;; key-p target
        )
      (setq outputs (nreverse outputs))
      (if (calling-convention-use-only-registers args)
          ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
          (compile-all-register-required-arguments reqargs old-env args new-env)
          ;; Test for
          ;; (x &optional y)
          ;; (x y &optional z)
          (compile-general-lambda-list-code lambda-list-handler
                                            old-env
                                            args
                                            new-env)))))
                                                        

(defun cclasp-maybe-alloc-cc-setup (lambda-list debug-on)
  "Maybe allocate slots in the stack frame to handle the calls
   depending on what is in the lambda-list (&rest, &key etc) and debug-on.
   Return a calling-convention-configuration object that describes what was allocated.
   See the bclasp version in lambdalistva.lsp."
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys unused-auxs varest-p)
      (core:process-lambda-list lambda-list 'core::function)
    ;; Currently if nargs <= +args-in-registers+ required arguments and (null debug-on)
    ;;      then can optimize and use the arguments in registers directly
    ;;  If anything else then allocate space to spill the registers
    ;;
    ;; Currently only cases:
    ;; (w)
    ;; (w x)
    ;; (w x y)
    ;; (w x y z)  up to the +args-in-registers+
    ;;    can use only registers
    ;; In the future add support for required + optional 
    ;; (x &optional y)
    ;; (x y &optional z) etc
    (let* ((req-opt-only (and (not rest-var)
                              (not key-flag)
                              (eql 0 (car keyargs))
                              (not allow-other-keys)))
           (num-req (car reqargs))
           (num-opt (car optargs))
           ;; Currently only required arguments are accepted
           ;;          and (<= num-req +args-in-register+)
           ;;          and not debugging
           ;;     --> Use only register arguments
           (may-use-only-registers (and req-opt-only (<= num-req cmp::+args-in-registers+) (eql 0 num-opt))))
      (if (and may-use-only-registers (null debug-on))
          (cmp::make-calling-convention-configuration
           :use-only-registers t)
          (cmp::make-calling-convention-configuration
           :use-only-registers may-use-only-registers ; if may-use-only-registers then debug-on is T and we could use only registers
           :vaslist* (alloca-vaslist "vaslist")
           :register-save-area* (alloca-register-save-area "register-save-area")
           :invocation-history-frame* (and debug-on (alloca-invocation-history-frame "invocation-history-frame")))))))


(defun cclasp-setup-calling-convention (arguments lambda-list debug-on)
  (let ((setup (cclasp-maybe-alloc-cc-setup lambda-list debug-on)))
    (cmp:initialize-calling-convention arguments setup)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; bclasp 
;;;

(defun bclasp-compile-lambda-list-code (cleavir-lambda-list register-env callconv)
  (multiple-value-bind (reqs opts rest key-flag keys aok-p auxargs-dummy va-rest-p)
      (process-cleavir-lambda-list cleavir-lambda-list)
    ;; Create the register lexicals using allocas
    (dolist (req (cdr reqs)) (core:register-environment-add-register req (irc-alloca-tsp)))
    (dolist (opt (cdr opts))
      (core:register-environment-add-register (first opt) (irc-alloca-tsp))
      (core:register-environment-add-register (second opt) (irc-alloca-tsp)))
    (when rest (core:register-environment-add-register rest (irc-alloca-tsp)))
    (dolist (key (cdr keys))
      (core:register-environment-add-register (second opt) (irc-alloca-tsp))
      (core:register-environment-add-register (third opt) (irc-alloca-tsp)))
    (compile-lambda-list-code cleavir-lambda-list
                              (lambda (datum)
                                (core:register-environment-lookup-register datum))
                              callconv)))
    
