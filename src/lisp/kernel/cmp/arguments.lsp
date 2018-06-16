(in-package :cmp)


(defvar *translate-datum*)


(defun compile-error-if-not-enough-arguments (nargs lv-required-number-of-arguments)
  "If nargs < lv-required-number-of-arguments then throw an exception - no cleanup needed because no new environment was created yet"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue")))
    (let ((cmp (irc-icmp-slt nargs lv-required-number-of-arguments "enough-args")))
      (irc-cond-br cmp error-block cont-block)
      (irc-begin-block error-block)
      (irc-intrinsic-call-or-invoke "va_notEnoughArgumentsException" (list (irc-constant-string-ptr *gv-current-function-name*) nargs lv-required-number-of-arguments ))
      (irc-unreachable)
      (irc-begin-block cont-block)
      )))

(defun compile-error-if-too-many-arguments (nargs maximum-number-of-arguments)
  "If nargs > lv-maximum-number-of-arguments then throw an exception - no cleanup needed/nothing was created"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue"))
	 (given-number-of-arguments nargs )
	 (maximum-number-of-arguments (jit-constant-size_t maximum-number-of-arguments))
	 (cmp (irc-icmp-sgt given-number-of-arguments maximum-number-of-arguments "max-num-args")))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke "va_tooManyArgumentsException" (list (irc-constant-string-ptr *gv-current-function-name*) given-number-of-arguments maximum-number-of-arguments))
    (irc-unreachable)
    (irc-begin-block cont-block)
    ))

(defun gather-required-arguments (reqargs
				  args ;;  calling-convention arguments
				  arg-idx-alloca ;; this is now in a register
				  )
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-required-arguments"))
  (cmp:compile-error-if-not-enough-arguments (cmp:calling-convention-nargs args)
					     (irc-size_t (car reqargs)))
  (cmp:dbg-set-current-debug-location-here)
  (cmp:irc-low-level-trace :arguments)
  ;; Now get the required arguments
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
        (target (car cur-req) (car cur-req))
        (arg-idx (cmp:irc-load arg-idx-alloca)
                 (cmp:irc-add arg-idx (irc-size_t 1) "arg-idx")))
       ((endp cur-req) (irc-store arg-idx arg-idx-alloca))
    (let* ((target-alloca (funcall *translate-datum* target))
           (val (cmp:calling-convention-args.va-arg args)))
      (irc-store val target-alloca))))



(defun gather-optional-arguments (optargs
				  args ;; nargs va-list
				  arg-idx-alloca
				  true-val)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-optional-arguments"))
  ;; Copy argument values or evaluate init forms
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
        (target (car cur-opt) (car cur-opt))
        (flag (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
        (arg-idx (cmp:irc-load arg-idx-alloca) (cmp:irc-add arg-idx (irc-size_t 1) "arg-idx")))
       ((endp cur-opt) (irc-store arg-idx arg-idx-alloca))
    (let ((opt-arg-block (cmp:irc-basic-block-create "opt-arg"))
          (opt-init-block (cmp:irc-basic-block-create "opt-init"))
          (opt-cont-block (cmp:irc-basic-block-create "opt-cont"))
          (cmp (cmp:irc-icmp-slt arg-idx (cmp:calling-convention-nargs args) "enough-given-args")))
      (cmp:irc-cond-br cmp opt-arg-block opt-init-block)
      (cmp:irc-begin-block opt-arg-block) ; opt-arg
      (let ((target-alloca (funcall *translate-datum* target))
            (flag-alloca (funcall *translate-datum* flag))
            (val (cmp:calling-convention-args.va-arg args)))
        (cmp:irc-low-level-trace :arguments)
        (irc-store val target-alloca)
        (irc-store true-val flag-alloca))
      (cmp:irc-br opt-cont-block)
      (cmp:irc-begin-block opt-init-block) ; opt-init
      (let ((flag-alloca (funcall *translate-datum* flag)))
        (cmp:irc-low-level-trace :arguments)
        (irc-store (irc-nil) flag-alloca))
      (cmp:irc-br opt-cont-block)
      (cmp:irc-begin-block opt-cont-block) ; opt-cont
      (cmp:irc-low-level-trace :arguments))))

(defun compile-rest-argument (rest-var
                              varest-p
			      args ;; nargs va-list
			      arg-idx-alloca)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (when rest-var
    ;; Copy argument values or evaluate init forms
    (let* ((arg-idx (cmp:irc-load arg-idx-alloca))
	   (rest (if varest-p
                     (let ((temp-valist (irc-alloca-vaslist :label "rest")))
                       (irc-intrinsic-call "cc_gatherVaRestArguments" 
                                           (list (cmp:calling-convention-va-list* args)
                                                 (cmp:calling-convention-remaining-nargs* args)
                                                 temp-valist)))
                     (irc-intrinsic-call "cc_gatherRestArguments" 
                                         (list (cmp:calling-convention-va-list* args)
                                               (cmp:calling-convention-remaining-nargs* args)))))
           (rest-alloca (funcall *translate-datum* rest-var)))
      (irc-store rest rest-alloca)
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
      (let ((supplied-alloca (funcall *translate-datum* supplied)))
	(irc-store (irc-nil) supplied-alloca)))
    (let* ((entry-saw-aok (irc-size_t (if lambda-list-allow-other-keys 2 0)))
	   (entry-bad-kw-idx (irc-size_t 65536))
	   (aok-val (irc-literal :allow-other-keys "aok"))
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
	  (irc-intrinsic-invoke-if-landing-pad-or-call "cc_ifNotKeywordException" (list arg-val phi-arg-idx (cmp:calling-convention-va-list* args) *current-function-description*))
	  (let* ((eq-aok-val-and-arg-val (cmp:irc-trunc (cmp:irc-icmp-eq aok-val arg-val) cmp:%i1%)) ; compare arg-val to a-o-k
		 (aok-block (cmp:irc-basic-block-create "aok-block"))
		 (possible-kw-block (cmp:irc-basic-block-create "possible-kw-block"))
		 (advance-arg-idx-block (cmp:irc-basic-block-create "advance-arg-idx-block"))
		 (bad-kw-block (cmp:irc-basic-block-create "bad-kw-block"))
		 (good-kw-block (cmp:irc-basic-block-create "good-kw-block")))
	    (cmp:irc-cond-br eq-aok-val-and-arg-val aok-block possible-kw-block)
	    (cmp:irc-begin-block aok-block)
	    (let* ((loop-saw-aok (irc-intrinsic-call "cc_allowOtherKeywords" (list phi-saw-aok kw-arg-val))))
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
		(let* ((kw-val (irc-literal key (string key)))
		       (target-ref (funcall *translate-datum* target))
		       (supplied-ref (funcall *translate-datum* supplied))
		       (test-kw-and-arg (irc-intrinsic-call "cc_matchKeywordOnce" (list kw-val arg-val (irc-load-t* supplied-ref))))
		       (no-kw-match (cmp:irc-icmp-eq test-kw-and-arg (irc-size_t 0)))
		       (matched-kw-block (cmp:irc-basic-block-create "matched-kw-block"))
		       (not-seen-before-kw-block (cmp:irc-basic-block-create "not-seen-before-kw-block"))
                       (next-kw-block (cmp:irc-basic-block-create "next-kw-block")))
		  (cmp:irc-cond-br no-kw-match next-kw-block matched-kw-block)
		  (cmp:irc-begin-block matched-kw-block)
		  (let ((kw-seen-already (cmp:irc-icmp-eq test-kw-and-arg (irc-size_t 2))))
		    (cmp:irc-cond-br kw-seen-already good-kw-block not-seen-before-kw-block)
		    (cmp:irc-begin-block not-seen-before-kw-block)
                    (irc-store kw-arg-val target-ref)
                    ;; Set the boolean flag to indicate that we saw this key
                    (irc-store true-val supplied-ref)
                    (cmp:irc-br good-kw-block)
                    (cmp:irc-begin-block next-kw-block))))
	      ;; We fell through all the keyword tests - this might be a unparameterized keyword
	      (cmp:irc-branch-to-and-begin-block bad-kw-block) ; fall through to here if no kw recognized
	      (let ((loop-bad-kw-idx (irc-intrinsic-call "cc_trackFirstUnexpectedKeyword"
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
		  (let* ((loop-arg-idx (cmp:irc-add phi-arg-idx (irc-size_t 2)))
			 (loop-arg-idx_lt_nargs (cmp:irc-icmp-slt loop-arg-idx (cmp:calling-convention-nargs args))))
		    (cmp:irc-phi-add-incoming phi-saw-aok phi-arg-bad-good-aok advance-arg-idx-block)
		    (cmp:irc-phi-add-incoming phi-bad-kw-idx phi.aok-bad-good.bad-kw-idx advance-arg-idx-block)
		    (cmp:irc-phi-add-incoming phi-arg-idx loop-arg-idx advance-arg-idx-block)
		    (cmp:irc-cond-br loop-arg-idx_lt_nargs loop-kw-args-block loop-cont-block)
		    (cmp:irc-begin-block loop-cont-block)
                    ;; FIXME    This must be an INVOKE if there is a cleanup clause in the function
		    (irc-intrinsic-invoke-if-landing-pad-or-call "cc_ifBadKeywordArgumentException" (list phi-arg-bad-good-aok phi.aok-bad-good.bad-kw-idx arg-val))
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
					 calling-conv
                                         &key translate-datum)
  (let ((*translate-datum* (lambda (datum) (funcall translate-datum datum))))
    (cmp-log "Entered compile-general-lambda-list-code%N")
    (let* ((arg-idx-alloca (irc-alloca-size_t :label "arg-idx-alloca"))
           true-val)
      (irc-store (irc-size_t 0) arg-idx-alloca)
      (gather-required-arguments reqargs calling-conv arg-idx-alloca)
      (dolist (req (cdr reqargs))
        (let ((req-alloca (funcall *translate-datum* req))
              (out-alloca (funcall *translate-datum* (pop outputs))))
          (irc-store (cmp:irc-load req-alloca) out-alloca)))
      (when (or (> (first optargs) 0) key-flag)
        (setq true-val (irc-t)))
      (when (> (first optargs) 0)
        (gather-optional-arguments optargs calling-conv arg-idx-alloca true-val)
        (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
              (target (car cur-opt) (car cur-opt))
              (supplied-p (cadr cur-opt) (cadr cur-opt)) ;; This is not an ordinary lambda list
              (arg-idx (cmp:irc-load arg-idx-alloca) (cmp:irc-add arg-idx (irc-size_t 1) "arg-idx")))
             ((endp cur-opt))
          (let ((target-alloca (funcall *translate-datum* target))
                (supplied-alloca (funcall *translate-datum* supplied-p))
                (target-out-alloca (funcall *translate-datum* (pop outputs)))
                (supplied-out-alloca (funcall *translate-datum* (pop outputs))))
            (irc-store (cmp:irc-load target-alloca) target-out-alloca)
            (irc-store (cmp:irc-load supplied-alloca) supplied-out-alloca))))
      (when rest-var
        (compile-rest-argument rest-var varest-p calling-conv arg-idx-alloca)
        (irc-store (cmp:irc-load (funcall *translate-datum* rest-var)) (funcall *translate-datum* (pop outputs))))
      (when key-flag
        (compile-key-arguments keyargs allow-other-keys calling-conv arg-idx-alloca true-val)
        (do* ((cur-key (cdr keyargs) (cddddr cur-key))
              (target (caddr cur-key) (caddr cur-key))
              (supplied (cadddr cur-key) (cadddr cur-key)))
             ((null cur-key))
          (let ((target-output-ref (funcall *translate-datum* (pop outputs)))
                (supplied-output-ref (funcall *translate-datum* (pop outputs))))
            (irc-store (cmp:irc-load (funcall *translate-datum* target)) target-output-ref)
            (irc-store (cmp:irc-load (funcall *translate-datum* supplied)) supplied-output-ref))))
      (unless rest-var
        ;; Check if there were too many arguments passed
        (unless key-flag
          (cmp:compile-error-if-too-many-arguments (cmp:calling-convention-nargs calling-conv) (+ (car reqargs) (car optargs))))))))


(defun compile-only-reg-and-opt-arguments (reqargs optargs outputs cc &key translate-datum)
  (let ((*translate-datum* (lambda (datum) (funcall translate-datum datum))))
    (let ((register-args (cmp:calling-convention-register-args cc))
          (req-bb (irc-basic-block-create "req-bb")))
      (if (> (first optargs) 0)
          (let* ((true-val (irc-t))
                 (false-val (irc-nil))
                 (opt-rel-idx (irc-sub (cmp:calling-convention-nargs cc) (jit-constant-size_t (first reqargs))))
                 (cases (let (cases)
                          (dotimes (i (1+ (first optargs)))
                            (push (irc-basic-block-create (core:bformat nil "case-opt%d-bb" i)) cases))
                          (nreverse cases)))
                 (sw (irc-switch opt-rel-idx (car cases) (first optargs))))
            (dotimes (opti (1+ (first optargs)))
              (let ((case-bb (elt cases opti)))
                (irc-begin-block case-bb)
                (if (= opti 0)
                    (irc-intrinsic "cc_check_if_wrong_number_of_arguments"
                                   (cmp:calling-convention-nargs cc)
                                   (jit-constant-size_t (car reqargs))
                                   (jit-constant-size_t (+ (car reqargs) (car optargs)))
                                   *current-function-description*)
                    (irc-add-case sw (jit-constant-size_t opti) case-bb))
                (do* ((optj 0 (1+ optj))
                      (cur-target (cdr optargs) (cdddr cur-target))
                      (cur-register-args (nthcdr (first reqargs) register-args) (cdr cur-register-args))
                      (cur-output (nthcdr (first reqargs) outputs) (cddr cur-output))
                      (target (first cur-output) (first cur-output))
                      (targetp (second cur-output) (second cur-output))
                      (arg (car cur-register-args) (car cur-register-args)))
                     ((null cur-target))
                  (let ((dest (funcall *translate-datum* target))
                        (destp (funcall *translate-datum* targetp)))
                    (if (>= optj opti)
                        (progn
                          (irc-store false-val destp))
                        (progn
                          (irc-store arg dest)
                          (irc-store true-val destp)))))
                (irc-br req-bb))))
          (progn
            (irc-intrinsic "cc_check_if_wrong_number_of_arguments"
                           (cmp:calling-convention-nargs cc)
                           (jit-constant-size_t (car reqargs))
                           (jit-constant-size_t (+ (car reqargs) (car optargs)))
                           *current-function-description*)
            (irc-br req-bb)))
      (irc-begin-block req-bb)
      (do* ((cur-target (cdr reqargs) (cdr cur-target))
            (cur-register-args register-args (cdr cur-register-args))
            (cur-output outputs (cdr cur-output))
            (target (car cur-output) (car cur-output))
            (arg (car cur-register-args) (car cur-register-args)))
           ((null cur-target))
        #+(or)(format t "compile-all-register-required-arguments store: ~a to ~a  target: ~a~%" arg dest target)
        (irc-store arg (funcall *translate-datum* target))))))

(defun process-cleavir-lambda-list (lambda-list)
  ;; We assume that the lambda list is in its correct format:
  ;; 1) required arguments are lexical locations.
  ;; 2) optional arguments are (<lexical location> <lexical location>)
  ;; 3) keyword arguments are (<symbol> <lexical location> <lexical location>)
  ;; This lets us cheap out on parsing, except &rest and &allow-other-keys.
  (cmp-log "process-cleavir-lambda-list lambda-list -> %s%N" lambda-list)
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

;;; compile-lambda-list-code
;;; You must provide the following lambdas
;;;   alloca-size_t (label) that allocas a size_t slot in the current function
;;;   alloca-vaslist (label) that allocas a vaslist slot in the current function
;;;   translate-datum (datum) that translates a datum into an alloca in the current function
(defun compile-lambda-list-code (lambda-list outputs calling-conv
                                 &key translate-datum)
  (cmp-log "About to process-cleavir-lambda-list%N")
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys unused-auxs varest-p)
      (process-cleavir-lambda-list lambda-list)
    (cmp-log "About to calling-convention-use-only-registers%N")
    (cmp-log "    reqargs -> %s%N" reqargs)
    (cmp-log "    optargs -> %s%N" optargs)
    (cmp-log "    keyargs -> %s%N" keyargs)
    (cmp-log "    outputs -> %s%N" outputs)
    (if (calling-convention-use-only-registers calling-conv)
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
        (progn
          (compile-only-reg-and-opt-arguments reqargs optargs outputs calling-conv
                                              :translate-datum translate-datum))
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (progn
          (compile-general-lambda-list-code reqargs 
                                            optargs 
                                            rest-var
                                            varest-p
                                            key-flag 
                                            keyargs 
                                            allow-other-keys
                                            outputs
                                            calling-conv
                                            :translate-datum translate-datum)))))

(defun maybe-alloc-cc-setup (lambda-list debug-on)
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
           ;; If only required or optional arguments are used
           ;; and the sum of required and optional arguments is less
           ;; than the number +args-in-register+ then use only registers.
           (may-use-only-registers (and req-opt-only (<= (+ num-req num-opt) +args-in-registers+))))
      (if (and may-use-only-registers (null debug-on))
          (make-calling-convention-configuration
           :use-only-registers t)
          (make-calling-convention-configuration
           :use-only-registers may-use-only-registers ; if may-use-only-registers then debug-on is T and we could use only registers
           :vaslist* (irc-alloca-vaslist :label "vaslist")
           :register-save-area* (irc-alloca-register-save-area :label "register-save-area")
           :invocation-history-frame* (and debug-on (irc-alloca-invocation-history-frame :label "invocation-history-frame")))))))


(defun cclasp-setup-calling-convention (arguments lambda-list debug-on)
  (let ((setup (maybe-alloc-cc-setup lambda-list debug-on)))
    (let ((cc (cmp:initialize-calling-convention arguments setup)))
      (calling-convention-args.va-start cc)
      cc)))


#+(or)
(defun cclasp-compile-lambda-list-code (lambda-list outputs calling-conv
                                        &key translate-datum)
  (let ((*translate-datum* (lambda (datum)
                             (funcall translate-datum datum))))
    (compile-lambda-list-code* lambda-list outputs calling-conv)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; bclasp 
;;;


(defun bclasp-map-lambda-list-symbols-to-indices (cleavir-lambda-list)
  (multiple-value-bind (reqs opts rest key-flag keys aok-p auxargs-dummy va-rest-p)
      (process-cleavir-lambda-list cleavir-lambda-list)
    ;; Create the register lexicals using allocas
    (let (bindings
          (index -1))
      (cmp-log "Processing reqs -> %s%N" reqs)
      (dolist (req (cdr reqs))
        (cmp-log "Add req %s%N" req)
        (push (cons req (incf index)) bindings))
      (cmp-log "Processing opts -> %s%N" opts)
      (do* ((cur (cdr opts) (cdddr cur))
            (opt (car cur) (car cur))
            (optp (cadr cur) (cadr cur)))
           ((null cur))
        (cmp-log "Add opt %s %s%N" opt optp)
        (push (cons opt (incf index)) bindings)
        (push (cons optp (incf index)) bindings))
      (cmp-log "Processing rest -> %s%N" rest)
      (when rest
        (push (cons rest (incf index)) bindings))
      (cmp-log "Processing keys -> %s%N" keys)
      (do* ((cur (cdr keys) (cddddr cur))
            (key (third cur) (third cur))
            (keyp (fourth cur) (fourth cur)))
           ((null cur))
        (push (cons key (incf index)) bindings)
        (push (cons keyp (incf index)) bindings))
      (nreverse bindings))))

(defun bclasp-compile-lambda-list-code (cleavir-lambda-list fn-env callconv)
  (cmp-log "Entered bclasp-compile-lambda-list-code%N")
  (let* ((output-bindings (bclasp-map-lambda-list-symbols-to-indices cleavir-lambda-list))
         (new-env (irc-new-unbound-value-environment-of-size
                   fn-env
                   :number-of-arguments (length output-bindings)
                   :label "arguments-env")))
    (irc-make-value-frame-set-parent new-env (length output-bindings) fn-env)
    (cmp-log "output-bindings: %s%N" output-bindings)
    (mapc (lambda (ob)
            (cmp-log "Adding to environment: %s%N" ob)
            (core:value-environment-define-lexical-binding new-env (car ob) (cdr ob)))
          output-bindings)
    (cmp-log "register-environment contents -> %s%N" new-env)
    (compile-lambda-list-code
     cleavir-lambda-list
     (mapcar #'car output-bindings)
     callconv
     :translate-datum (lambda (datum)
                        (let* ((info (assoc datum output-bindings))
                               (symbol (car info))
                               (index (cdr info))
                               (ref (codegen-lexical-var-reference symbol 0 index new-env new-env)))
;;;(bformat *debug-io* "translate-datum %s -> %s%N" datum ref)
                          ref)))
    new-env))


#+(or)
(defun clasp-maybe-alloc-cc-info (lambda-list debug-on)
  "Maybe allocate slots in the stack frame to handle the calls
   depending on what is in the lambda-list (&rest, &key etc) and debug-on.
   Return a calling-convention-configuration object that describes what was allocated.
   See the cclasp version in arguments.lisp "
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs varest-p)
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
           (may-use-only-registers (and req-opt-only (<= num-req +args-in-registers+) (eql 0 num-opt))))
      (if (and may-use-only-registers (null debug-on))
          (make-calling-convention-configuration
           :use-only-registers t)
          (make-calling-convention-configuration
           :use-only-registers may-use-only-registers ; if may-use-only-registers then debug-on is T and we could use only registers
           :vaslist* (irc-alloca-vaslist :label "vaslist")
           :register-save-area* (irc-alloca-register-save-area :label "register-save-area")
           :invocation-history-frame* (and debug-on (irc-alloca-invocation-history-frame :label "invocation-history-frame")))))))

(defun bclasp-setup-calling-convention (arguments lambda-list debug-on)
  (let ((setup (maybe-alloc-cc-setup lambda-list debug-on)))
    (let ((cc (initialize-calling-convention arguments setup)))
      (calling-convention-args.va-start cc)
      cc)))
