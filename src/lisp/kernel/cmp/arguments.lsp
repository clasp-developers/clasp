(in-package :cmp)

;; So that we can use this same code for both bclasp and cleavir.
;; Note to self: Calling this function may generate instructions. Be careful with blocks.
(defvar *translate-datum*)

;; Generate code to signal an error iff there weren't enough arguments provided.
(defun compile-error-if-not-enough-arguments (minimum cc)
  (let* ((nargs (calling-convention-nargs cc))
         (cmin (irc-size_t minimum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ult (calling-convention-nargs cc) cmin)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke
     "va_notEnoughArgumentsException"
     (list (irc-constant-string-ptr *gv-current-function-name*) ; FIXME: use function desc instead?
           nargs cmin))
    (irc-unreachable)
    (irc-begin-block cont-block)))

;; Ditto but with too many.
(defun compile-error-if-too-many-arguments (maximum cc)
  (let* ((nargs (calling-convention-nargs cc))
         (cmax (irc-size_t maximum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ugt (calling-convention-nargs cc) cmax)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke
     "va_tooManyArgumentsException"
     (list (irc-constant-string-ptr *gv-current-function-name*) ; FIXME: use function desc instead?
           nargs cmax))
    (irc-unreachable)
    (irc-begin-block cont-block)))

;; Generate code to bind the required arguments.
(defun compile-required-arguments (reqargs cc)
  ;; reqargs is as returned from process-lambda-list- (# ...) where # is the count.
  ;; cc is the calling-convention object.
  (dolist (req (cdr reqargs))
    (let ((target-alloca (funcall *translate-datum* req))
          (val (calling-convention-args.va-arg cc)))
      (irc-store val target-alloca))))

(defun compile-optional-arguments (optargs nreq calling-conv args-exhausted-block false true)
  ;; optargs is (# var suppliedp default ...)
  ;; Our code will basically work like, for (&optional a b) for example,
#|
if (nargs > 0) { a = va_arg(); a_p = [t]; } else goto ab;
if (nargs > 1) { b = va_arg(); b_p = [t]; } else goto b;
goto rest_key_parsing;
ab: a_p = [nil];
b: b_p = [nil];
goto empty_rest_key_parsing;
  |#
  ;; FIXME: we can probably do everything in one iteration.
  ;; FIXME: We could quite possibly generate a switch.
  (let ((fail-blocks nil)
        (start-block (irc-basic-block-create "parse-optional-arguments")))
    (irc-br start-block)
    ;; First we set up the fail blocks.
    (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
          (suppliedp (cadr cur-opt) (cadr cur-opt)))
         ((endp cur-opt))
      (let ((this-fail (irc-basic-block-create "opt-exhausted")))
        (unless (null fail-blocks) ; don't do this first iteration.
          ;; branch from previous block
          (irc-br this-fail))
        (push this-fail fail-blocks)
        (irc-begin-block this-fail)
        (irc-store false (funcall *translate-datum* suppliedp))))
    ;; from the last fail block (note: we have at least one optional argument!)
    (irc-br args-exhausted-block)
    (setf fail-blocks (nreverse fail-blocks))
    ;; Now the actual parsing
    (irc-begin-block start-block)
    (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
          (target (car cur-opt) (car cur-opt))
          (suppliedp (cadr cur-opt) (cadr cur-opt))
          (cur-fail fail-blocks (cdr cur-fail))
          (fail (car cur-fail) (car cur-fail))
          (min-args nreq (1+ min-args)))
         ((endp cur-opt))
      (let ((target-loc (funcall *translate-datum* target))
            (suppliedp-loc (funcall *translate-datum* suppliedp))
            (succeed (irc-basic-block-create "opt-present"))
            (cmp (irc-icmp-ugt (calling-convention-nargs calling-conv) (irc-size_t min-args))))
        (irc-cond-br cmp succeed fail)
        (irc-begin-block succeed)
        (irc-store true suppliedp-loc)
        (irc-store (calling-convention-args.va-arg calling-conv) target-loc)))))

(defun compile-rest-argument (rest-var varest-p nremaining calling-conv)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (when rest-var
    (let* ((rest-alloc (calling-convention-rest-alloc calling-conv))
	   (rest (cond
                   ((eq rest-alloc 'ignore)
                    ;; &rest variable is ignored- allocate nothing
                    nil)
                   ((eq rest-alloc 'dynamic-extent)
                    ;; Do the dynamic extent thing- alloca, then an intrinsic to initialize it.
                    (let ((rrest
                            (irc-alloca-dynamic-extent-list :irbuilder *irbuilder*
                                                            :length nremaining
                                                            :label "rrest")))
                      (irc-intrinsic-call "cc_gatherDynamicExtentRestArguments"
                                          (list (cmp:calling-convention-va-list* calling-conv)
                                                nremaining
                                                (irc-bit-cast rrest %t**%)))))
                   (varest-p
                    (let ((temp-valist (irc-alloca-vaslist :label "rest")))
                      (irc-intrinsic-call "cc_gatherVaRestArguments" 
                                          (list (cmp:calling-convention-va-list* calling-conv)
                                                nremaining
                                                temp-valist))))
                   (t
                    ;; general case- heap allocation
                    (irc-intrinsic-call "cc_gatherRestArguments" 
                                        (list (cmp:calling-convention-va-list* calling-conv)
                                              nremaining))))))
      (when rest
        (let ((rest-loc (funcall *translate-datum* rest-var)))
          (irc-store rest rest-loc))))))

;; a rest argument in the case where it's known no arguments remain.
(defun compile-empty-rest-argument (rest-var varest-p calling-conv iNIL)
  (unless (eq (calling-convention-rest-alloc calling-conv) 'ignore)
    (irc-store (if varest-p
                   ;; overcomplicated. FIXME
                   (irc-intrinsic-call "cc_gatherVaRestArguments"
                                       (list (calling-convention-va-list* calling-conv)
                                             (irc-size_t 0)
                                             (irc-alloca-vaslist :label "rest")))
                   iNIL)
               (funcall *translate-datum* rest-var))))

;;; Keyword processing is the most complicated part, unsurprisingly.
#|
Here is pseudo-C for the parser for (&key a). [foo] indicates an inserted constant.
This translation is not exact; no phi, switched some conditions around, etc.

if ((remaining_nargs % 2) == 1)
  cc_oddKeywordException([*current-function-description*]);
tstar bad_keyword;
bool seen_bad_keyword = false; // in the asm, done with phi
tstar a, a_p = [nil], allow_other_keys = [nil], allow_other_keys_p = [nil];
for (; remaining_nargs != 0; remaining_nargs -= 2) {
  tstar key = va_arg(valist), value = va_arg(valist);
  switch cc_matchKeywordOnce([:a], key, a_p) {
    case 0: break;
    case 1: a = value; a_p = [t]; continue;
    case 2: continue;
  }
  switch cc_matchKeywordOnce([:allow-other-keys], key, allow_other_keys_p) {
    case 0: break;
    case 1: allow_other_keys = value; allow_other_keys_p = [t]; continue;
    case 2: continue;
  }
  seen_bad_keyword = true; bad_keyword = key;
}
if (seen_bad_keyword)
  cc_ifBadKeywordArgumentException(allow_other_keys, bad_keyword, [*current-function-description*]);
|#

(defun compile-one-key-test (keyword target-alloca suppliedp-alloca cont-block kw-key kw-value true)
  ;; KEYWORD is the Lisp keyword we're matching.
  ;; TARGET-ALLOCA and SUPPLIEDP-ALLOCA are the allocas for the var and its -p respectively.
  ;; CONT-BLOCK is the block to jump to if the keyword matches.
  ;; KW-KEY and KW-VALUE are the keyword-value pair to test against.

  ;; FIXME: We could use a switch here, depending on how literals move.
  (irc-branch-to-and-begin-block (irc-basic-block-create (core:bformat nil "kw-%s-test" keyword)))
  (let* ((key-const (irc-literal keyword (string keyword)))
         ;; matchKeywordOnce returns 0 if the keys don't match, 2 if suppliedp is T
         ;; (i.e. the keyword has been seen already), and 1 otherwise.
         (test-keyword (irc-intrinsic-call "cc_matchKeywordOnce"
                                           (list key-const kw-key (irc-load-t* suppliedp-alloca))))
         (mismatch-block (irc-basic-block-create (core:bformat nil "not-%s" keyword)))
         (match-block (irc-basic-block-create (core:bformat nil "matched-%s" keyword)))
         (switch (irc-switch test-keyword mismatch-block 3)))
    (irc-add-case switch (irc-size_t 0) mismatch-block)
    (irc-add-case switch (irc-size_t 1) match-block)
    (irc-add-case switch (irc-size_t 2) cont-block) ; don't need to do any more tests here.
    ;; Hit a keyword for the first time! Store it, set alloca, continue to the loop conclusion.
    (irc-begin-block match-block)
    (irc-store kw-value target-alloca)
    (irc-store true suppliedp-alloca)
    (irc-br cont-block)
    ;; Keyword doesn't match- try the next.
    (irc-begin-block mismatch-block)))

(defun compile-key-arguments (keyargs lambda-list-aokp nremaining cc false true)
  ;; keyargs is as returned from process-lambda-list, # and then four element cycles:
  ;; (keyword initform variable suppliedp-variable)
  ;; We ignore the initform here as the function bodies initialize those based on the suppliedp,
  ;; same as with &optional.
  ;; nreqopt is the llvm value with the number of arguments remaining to process.

  ;; We treat allow-other-keys specially. You can do (&key allow-other-keys) if you want,
  ;; and then allow-other-keys is both a regular variable and has its magic meaning per 3.4.1.4.
  ;; So, we make sure to get allocas whether there's an allow-other-keys parameter or not,
  ;; and use those in the internal check.
  ;; We also initialize the aok to NIL, even before it's supplied. Body code will initialize it
  ;; based on the suppliedp so there's no problem, and it means we can just use the aok as a flag.
  (irc-branch-to-and-begin-block (irc-basic-block-create "parse-key-arguments"))
  ;; Set up all the blocks we need- except the ones for each keyword, those are later.
  (let ((initialize-suppliedps (irc-basic-block-create "initialize-suppliedps")) ; set all suppliedp to nil
        (kw-loop (irc-basic-block-create "kw-loop")) ; main processing loop
        (odd-kw (irc-basic-block-create "odd-kw")) ; odd-number-of-arguments error
        ;; Block we enter after traversing the whole arguments list.
        (args-depleted (irc-basic-block-create "args-depleted"))
        ;; Where we get one key-value pair from the arguments list.
        (parse-arg (irc-basic-block-create "parse-arg"))
        ;; Two used for phi wackiness (see below)
        (kw-matched (irc-basic-block-create "kw-matched"))
        (kw-unmatched (irc-basic-block-create "kw-unmatched"))
        ;; Once we've taken care of a keyword, go here to decrement the count and continue.
        (kw-loop-continue (irc-basic-block-create "kw-loop-continue"))
        ;; The end.
        (exit (irc-basic-block-create "kw-exit"))
        )
    ;; If the number of arguments remaining is odd, the call is invalid- error.
    (let* ((rem (irc-srem nremaining (irc-size_t 2))) ; parity
           (evenp (irc-icmp-eq rem (irc-size_t 0)))) ; is parity zero (is SUB even)?
      (irc-cond-br evenp initialize-suppliedps odd-kw))
    ;; There have been an odd number of arguments, so signal an error.
    (irc-begin-block odd-kw)
    (irc-intrinsic-invoke-if-landing-pad-or-call "cc_oddKeywordException"
                                                 (list *current-function-description*))
    (irc-unreachable)
    ;; Initialize all suppliedps (and any other allocas while we're at it).
    (irc-begin-block initialize-suppliedps)
    (let ((aok-parameter-p nil) ; are we in the weird &key allow-other-keys situation?
          aok-alloca aok-suppliedp-alloca
          ;; The first bad keyword we see.
          ;; We can't signal an error until the end, as :allow-other-keys T could suppress.
          ;; FIXME: Hypothetically we could just store a list of all of them.
          (bad-kw-alloca (irc-alloca-t* :label "bad-keyword")))
      ;; do the initializations.
      (do* ((cur-key (cdr keyargs) (cddddr cur-key))
            (key (car cur-key) (car cur-key))
            (suppliedp (cadddr cur-key) (cadddr cur-key)))
           ((null cur-key))
        (let ((suppliedp-alloca (funcall *translate-datum* suppliedp)))
          (irc-store false suppliedp-alloca)
          (when (eq key :allow-other-keys)
            (setf aok-parameter-p t
                  aok-alloca (funcall *translate-datum* (caddr cur-key))
                  aok-suppliedp-alloca suppliedp-alloca)
            (irc-store false aok-alloca))))
      ;; No allow-other-keys parameter, so make allocas for it.
      (unless aok-parameter-p
        (setf aok-alloca (irc-alloca-t* :label "allow-other-keys")
              aok-suppliedp-alloca (irc-alloca-t* :label "allow-other-keys-suppliedp"))
        (irc-store false aok-suppliedp-alloca)
        (irc-store false aok-alloca))
      (irc-br kw-loop)
      ;; Main loop.
      (irc-begin-block kw-loop)
      (let (;; This variable holds the number of args we have left to process.
            (nargs-remaining (irc-phi %size_t% 2 "nargs-remaining"))
            ;; Whether we've seen an invalid or unrecognized keyword.
            (seen-bad-kw (irc-phi %i1% 2 "seen-bad-kw")))
        ;; If we're just entering the loop, we have nremaining args left. (We subtract two each loop.)
        (irc-phi-add-incoming nargs-remaining nremaining initialize-suppliedps)
        ;; If we're just entering the loop, we haven't seen any bad keywords.
        (irc-phi-add-incoming seen-bad-kw (jit-constant-i1 0) initialize-suppliedps)
        ;; If there are no arguments remaining, we're done.
        (let ((zerop (irc-icmp-eq nargs-remaining (irc-size_t 0))))
          (irc-cond-br zerop args-depleted parse-arg))
        ;; Grab one key-value pair and start working on it.
        (irc-begin-block parse-arg)
        (let ((key-arg (calling-convention-args.va-arg cc)) (value-arg (calling-convention-args.va-arg cc)))
          ;; Generate a test for each known keyword.
          ;; FIXME: We could use a switch here, depending on how literals move.
          (do* ((cur-key (cdr keyargs) (cddddr cur-key))
                (key (car cur-key) (car cur-key))
                (target (caddr cur-key) (caddr cur-key))
                (suppliedp (cadddr cur-key) (cadddr cur-key)))
               ((endp cur-key))
            (compile-one-key-test
             key (funcall *translate-datum* target) (funcall *translate-datum* suppliedp)
             kw-matched key-arg value-arg true))
          ;; We've checked against all known keywords and come up short.
          ;; But if allow-other-keys isn't a parameter, we have to check that too.
          ;; (If it is a parameter we checked it in the loop.)
          (unless aok-parameter-p
            (compile-one-key-test :allow-other-keys aok-alloca aok-suppliedp-alloca
                                  kw-matched key-arg value-arg true))
          ;; At this point the keyword is definitely unknown or invalid.
          ;; If we haven't yet seen such a keyword, store it and trip our flag (via phi)
          ;; FIXME: we could conditionalize on the flag so that we store only the first bad keyword,
          ;; but it doesn't matter too much whether we report the first or last.
          (irc-br kw-unmatched)
          (irc-begin-block kw-unmatched)
          (irc-store key-arg bad-kw-alloca)
          (irc-br kw-loop-continue))
        ;; This block is used for phi. It's basically empty.
        ;; With more detail: there are N (= # of specified keywords) matched-foo that can hit the continue.
        ;; For all of them, we don't want to alter the value of the seen-bad-keyword flag.
        ;; But I don't want to make a phi with thirty clauses, so we shove all the matched-foo together.
        ;; LLVM optimizations will likely make such a phi, or otherwise axe this stupid block.
        (irc-begin-block kw-matched) (irc-br kw-loop-continue)
        ;; We're done with a pair. Move on.
        (irc-begin-block kw-loop-continue)
        (let ((sbkw (irc-phi %i1% 2 "other-seen-bad-kw"))) ; phi silliness time.
          (irc-phi-add-incoming sbkw seen-bad-kw kw-matched) ; don't change the existing value
          (irc-phi-add-incoming sbkw (jit-constant-i1 1) kw-unmatched) ; we've seen some shit.
          (irc-phi-add-incoming seen-bad-kw sbkw kw-loop-continue))
        (let ((dec (irc-sub nargs-remaining (irc-size_t 2))))
          (irc-phi-add-incoming nargs-remaining dec kw-loop-continue))
        (irc-br kw-loop)
        ;; OK! Main loop over! All args processed! Hoo-ray.
        (irc-begin-block args-depleted)
        ;; Only thing left is to maybe signal an error about bad keywords.
        (cond
          (lambda-list-aokp (irc-br exit)) ; No one cares.
          (t
           ;; Check if we saw a bad keyword. If we did, call an intrinsic that tests our aokp thing.
           ;; FIXME: change the jit-constants to getTrue and/or getFalse, mother fucker
           (let ((aok-check (irc-basic-block-create "aok-check")))
             (irc-cond-br #+(or) seen-bad-kw
                          #-(or) (irc-icmp-eq seen-bad-kw (jit-constant-i1 1))
                          aok-check exit)
             (irc-begin-block aok-check)
             (irc-intrinsic-invoke-if-landing-pad-or-call
              "cc_ifBadKeywordArgumentException"
              ;; aok was initialized to NIL, regardless of the suppliedp, so this is ok.
              (list (irc-load-t* aok-alloca) (irc-load-t* bad-kw-alloca)
                    *current-function-description*))
             ;; if it returned :allow-other-keys was passed, so no error
             (irc-br exit))))
        ;; We saw a bad keyword. Call this intrinsic that tests our seen-aok-alloca.
        (irc-begin-block exit)))))

(defun compile-empty-key-arguments (keyargs calling-conv false)
  (do* ((cur-key (cdr keyargs) (cddddr cur-key))
        (suppliedp (cadddr cur-key) (cadddr cur-key)))
       ((endp cur-key))
    (irc-store false (funcall *translate-datum* suppliedp))))

(defun compile-general-lambda-list-code (reqargs 
					 optargs 
					 rest-var
                                         varest-p
					 key-flag 
					 keyargs 
					 allow-other-keys
					 calling-conv
                                         &key translate-datum)
  (cmp-log "Entered compile-general-lambda-list-code%N")
  (let* ((*translate-datum* (lambda (datum) (funcall translate-datum datum)))
         (nreq (car reqargs))
         (nopt (car optargs))
         (nfixed (+ nreq nopt)))
    (unless (zerop nreq)
      (compile-error-if-not-enough-arguments nreq calling-conv)
      (compile-required-arguments reqargs calling-conv))
    (let ((final (irc-basic-block-create "check-too-many-args"))
          ;; note: atm, we won't be in this function if we only have required args,
          ;; so we basically always need these. But that can (hopefully will) change.
          (iNIL (irc-nil)) (iT (irc-t)))
      (unless (zerop nopt)
        ;; The optional parsing is a little more complicated, because if it
        ;; runs out we can jump immediately to a block that just sets any rest and key
        ;; to NIL, and we can skip the various not actually relevant arg count checks
        ;; that the key parser does.
        (let ((args-remain (irc-basic-block-create "optional-args-remain"))
              (args-exhausted (irc-basic-block-create "optional-args-exhausted")))
          (compile-optional-arguments optargs nreq calling-conv args-exhausted iNIL iT)
          (irc-br args-remain)
          (irc-begin-block args-exhausted)
          (when rest-var
            (compile-empty-rest-argument rest-var varest-p calling-conv iNIL))
          (when key-flag
            (compile-empty-key-arguments keyargs calling-conv iNIL))
          (irc-br final)
          (irc-begin-block args-remain)))
      (let ((nremaining (irc-sub (calling-convention-nargs calling-conv) (irc-size_t nfixed))))
        (when rest-var
          (compile-rest-argument rest-var varest-p nremaining calling-conv))
        (when key-flag
          (compile-key-arguments keyargs allow-other-keys nremaining calling-conv iNIL iT)))
      (unless (or rest-var key-flag)
        (compile-error-if-too-many-arguments nfixed calling-conv))
      (irc-branch-to-and-begin-block final))))

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
  (cmp-log "About to process-cleavir-lambda-list lambda-list: %s%N" lambda-list)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup the calling convention
;;
(defun setup-calling-convention (arguments
                                 &key lambda-list debug-on rest-alloc cleavir-lambda-list)
  (let ((setup (maybe-alloc-cc-setup lambda-list debug-on)))
    (let ((cc (initialize-calling-convention arguments
                                             setup
                                             :rewind t
                                             :rest-alloc rest-alloc
                                             :cleavir-lambda-list cleavir-lambda-list)))
      (calling-convention-args.va-start cc)
      cc)))

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

(defun bclasp-compile-lambda-list-code (fn-env callconv)
  (let ((cleavir-lambda-list (calling-convention-cleavir-lambda-list callconv)))
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
      new-env)))

