(in-package :cmp)

;; A function of two arguments, an LLVM Value and a variable.
;; The "variable" is just whatever is provided to this code
;; (so that it can work with either b or c clasp).
;; The function should put the Value into the variable, possibly generating code to do so.
(defvar *argument-out*)

;; Generate code to signal an error iff there weren't enough arguments provided.
(defun compile-error-if-not-enough-arguments (minimum nargs)
  (let* ((cmin (irc-size_t minimum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ult nargs cmin)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke
     "va_notEnoughArgumentsException"
     (list (irc-constant-string-ptr *gv-current-function-name*) ; FIXME: use function desc instead?
           nargs cmin))
    (irc-unreachable)
    (irc-begin-block cont-block)))

;; Ditto but with too many.
(defun compile-error-if-too-many-arguments (maximum nargs)
  (let* ((cmax (irc-size_t maximum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ugt nargs cmax)))
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
    (funcall *argument-out* (calling-convention-args.va-arg cc) req)))

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
  (let* ((nopt (first optargs))
         (nfixed (+ nopt nreq))
         (opts (rest optargs))
         (enough (irc-basic-block-create "enough-for-optional"))
         (undef (irc-undef-value-get %t*%))
         (sw (irc-switch (calling-convention-nargs calling-conv) enough nopt)))
    ;; Have a block for each case.
    (do ((i nreq (1+ i)))
        ((= i nfixed))
      (let ((new (irc-basic-block-create (core:bformat nil "supplied-%d-arguments" i))))
        (llvm-sys:add-case sw (irc-size_t i) new)
        (irc-begin-block new)
        ;; Assign each optional parameter accordingly.
        (do* ((cur-opt opts (cdddr cur-opt))
              (param (car cur-opt) (car cur-opt))
              (suppliedp (cadr cur-opt) (cadr cur-opt))
              (j nreq (1+ j))
              (enough (< j i) (< j i)))
             ((endp cur-opt))
          (funcall *argument-out* (if enough true false) suppliedp)
          (funcall *argument-out*
                   (if enough (calling-convention-args.va-arg calling-conv) undef)
                   param))
        ;; Go on.
        (irc-br args-exhausted-block)))
    ;; For the default we just always store, and leave the insert point afterwards.
    (irc-begin-block enough)
    (do* ((cur-opt opts (cdddr cur-opt))
          (param (car cur-opt) (car cur-opt))
          (suppliedp (cadr cur-opt) (cadr cur-opt)))
         ((endp cur-opt))
      (funcall *argument-out* true suppliedp)
      (funcall *argument-out*
               (calling-convention-args.va-arg calling-conv)
               param))))

(defun compile-rest-argument (rest-var varest-p nremaining calling-conv)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (when rest-var
    (let* ((rest-alloc (calling-convention-rest-alloc calling-conv))
	   (rest (cond
                   ((eq rest-alloc 'ignore)
                    ;; &rest variable is ignored- allocate nothing
                    (irc-undef-value-get %t*%))
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
      (funcall *argument-out* rest rest-var))))

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

(defun compile-one-key-test (keyword key-arg suppliedp-phi cont-block false)
  (let* ((keystring (string keyword))
         ;; NOTE: We might save a bit of time by moving this out of the loop.
         ;; Or maybe LLVM can handle it. I don't know.
         (key-const (irc-literal keyword keystring))
         (match (irc-basic-block-create (core:bformat nil "matched-%s" keystring)))
         (mismatch (irc-basic-block-create (core:bformat nil "not-%s" keystring))))
    (let ((test (irc-icmp-eq key-arg key-const)))
      (irc-cond-br test match mismatch))
    (irc-begin-block match)
    (let* ((new (irc-basic-block-create (core:bformat nil "new-%s" keystring)))
           (old (irc-basic-block-create (core:bformat nil "old-%s" keystring))))
      (let ((test (irc-icmp-eq suppliedp-phi false)))
        (irc-cond-br test new old))
      (irc-begin-block new) (irc-br cont-block)
      (irc-begin-block old) (irc-br cont-block)
      (irc-begin-block mismatch)
      (values new old))))
  
(defun compile-key-arguments (keyargs lambda-list-aokp nremaining cc false true)
  (macrolet ((do-keys ((keyword) &body body)
               `(do* ((cur-key (cdr keyargs) (cddddr cur-key))
                      (,keyword (car cur-key) (car cur-key)))
                     ((endp cur-key))
                  ,@body)))
    (let ((aok-parameter-p nil)
          allow-other-keys
          (nkeys (car keyargs))
          (undef (irc-undef-value-get %t*%))
          (start (irc-basic-block-create "parse-key-arguments"))
          (matching (irc-basic-block-create "match-keywords"))
          (after (irc-basic-block-create "after-kw-loop"))
          (unknown-kw (irc-basic-block-create "unknown-kw"))
          (kw-loop (irc-basic-block-create "kw-loop"))
          (kw-loop-continue (irc-basic-block-create "kw-loop-continue")))
      ;; Prepare for :allow-other-keys.
      (unless lambda-list-aokp
        ;; Is there an allow-other-keys argument?
        (do-keys (key)
          (when (eq key :allow-other-keys) (setf aok-parameter-p t) (return)))
        ;; If there's no allow-other-keys argument, add one.
        (unless aok-parameter-p
          (setf keyargs (list* (1+ (car keyargs))
                               ;; default, var, and suppliedp are of course dummies.
                               ;; At the end we can check aok-parameter-p to avoid
                               ;; actually assigning to them.
                               :allow-other-keys nil nil nil
                               (cdr keyargs)))))
      (irc-branch-to-and-begin-block start)
      ;; If the number of arguments remaining is odd, the call is invalid- error.
      (let* ((odd-kw (irc-basic-block-create "odd-kw"))
             (rem (irc-srem nremaining (irc-size_t 2))) ; parity
             (evenp (irc-icmp-eq rem (irc-size_t 0)))) ; is parity zero (is SUB even)?
        (irc-cond-br evenp kw-loop odd-kw)
        ;; There have been an odd number of arguments, so signal an error.
        (irc-begin-block odd-kw)
        (irc-intrinsic-invoke-if-landing-pad-or-call "cc_oddKeywordException"
                                                     (list *current-function-description*))
        (irc-unreachable))
      ;; Loop starts; welcome hell
      (irc-begin-block kw-loop)
      (let ((top-param-phis nil) (top-suppliedp-phis nil)
            (new-blocks nil) (old-blocks nil)
            (nargs-remaining (irc-phi %size_t% 2 "nargs-remaining"))
            (sbkw (irc-phi %i1% 2 "seen-bad-keyword"))
            (bad-keyword (irc-phi %t*% 2 "bad-keyword")))
        (irc-phi-add-incoming nargs-remaining nremaining start)
        (irc-phi-add-incoming sbkw (jit-constant-false) start)
        (irc-phi-add-incoming bad-keyword undef start)
        (do-keys (key)
          (let ((var-phi (irc-phi %t*% 2 (core:bformat nil "%s-top" (string key)))))
            (push var-phi top-param-phis)
            ;; If we're paying attention to :allow-other-keys, track it specially
            ;; and initialize it to NIL.
            (cond ((and (not lambda-list-aokp) (eq key :allow-other-keys))
                   (irc-phi-add-incoming var-phi false start)
                   (setf allow-other-keys var-phi))
                  (t (irc-phi-add-incoming var-phi undef start))))
          (let ((suppliedp-phi (irc-phi %t*% 2 (core:bformat nil "%s-suppliedp-top" (string key)))))
            (push suppliedp-phi top-suppliedp-phis)
            (irc-phi-add-incoming suppliedp-phi false start)))
        (setf top-param-phis (nreverse top-param-phis)
              top-suppliedp-phis (nreverse top-suppliedp-phis))
        ;; Are we done?
        (let ((zerop (irc-icmp-eq nargs-remaining (irc-size_t 0))))
          (irc-cond-br zerop after matching))
        (irc-begin-block matching)
        ;; Start matching keywords
        (let ((key-arg (calling-convention-args.va-arg cc))
              (value-arg (calling-convention-args.va-arg cc)))
          (do* ((cur-key (cdr keyargs) (cddddr cur-key))
                (key (car cur-key) (car cur-key))
                (suppliedp-phis top-suppliedp-phis (cdr suppliedp-phis))
                (suppliedp-phi (car suppliedp-phis) (car suppliedp-phis)))
               ((endp cur-key))
            (multiple-value-bind (new-block old-block)
                (compile-one-key-test key key-arg suppliedp-phi kw-loop-continue false)
              (push new-block new-blocks) (push old-block old-blocks)))
          (setf new-blocks (nreverse new-blocks) old-blocks (nreverse old-blocks))
          ;; match failure - as usual, works through phi
          (irc-branch-to-and-begin-block unknown-kw)
          (irc-br kw-loop-continue)
          ;; Go around again. And do most of the actual work in phis.
          (irc-begin-block kw-loop-continue)
          (let ((npreds (1+ (* 2 nkeys)))) ; two for each key, plus one for unknown-kw.
            (let ((bot-sbkw (irc-phi %i1% npreds "seen-bad-keyword-bottom"))
                  (bot-bad-keyword (irc-phi %t*% npreds "bad-keyword-bottom")))
              ;; Set up the top to use these.
              (irc-phi-add-incoming sbkw bot-sbkw kw-loop-continue)
              (irc-phi-add-incoming bad-keyword bot-bad-keyword kw-loop-continue)
              ;; If we're coming from unknown-kw, store that.
              (irc-phi-add-incoming bot-sbkw (jit-constant-true) unknown-kw)
              (irc-phi-add-incoming bot-bad-keyword key-arg unknown-kw)
              ;; If we're coming from a match block, don't change anything.
              (dolist (new-block new-blocks)
                (irc-phi-add-incoming bot-sbkw sbkw new-block)
                (irc-phi-add-incoming bot-bad-keyword bad-keyword new-block))
              (dolist (old-block old-blocks)
                (irc-phi-add-incoming bot-sbkw sbkw old-block)
                (irc-phi-add-incoming bot-bad-keyword bad-keyword old-block)))
            ;; OK now the actual keyword values.
            (do* ((var-new-blocks new-blocks (cdr var-new-blocks))
                  (var-new-block (car var-new-blocks) (car var-new-blocks))
                  (top-param-phis top-param-phis (cdr top-param-phis))
                  (top-param-phi (car top-param-phis) (car top-param-phis))
                  (top-suppliedp-phis top-suppliedp-phis (cdr top-suppliedp-phis))
                  (top-suppliedp-phi (car top-suppliedp-phis) (car top-suppliedp-phis)))
                 ((endp var-new-blocks))
              (let ((var-phi (irc-phi %t*% npreds))
                    (suppliedp-phi (irc-phi %t*% npreds)))
                ;; fix up the top part to take values from here
                (irc-phi-add-incoming top-param-phi var-phi kw-loop-continue)
                (irc-phi-add-incoming top-suppliedp-phi suppliedp-phi kw-loop-continue)
                ;; If coming from unknown-kw we keep our values the same.
                (irc-phi-add-incoming var-phi top-param-phi unknown-kw)
                (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi unknown-kw)
                ;; All new-blocks other than this key's stick with what they have.
                (dolist (new-block new-blocks)
                  (cond ((eq var-new-block new-block)
                         ;; Here, however, we get the new values
                         (irc-phi-add-incoming var-phi value-arg new-block)
                         (irc-phi-add-incoming suppliedp-phi true new-block))
                        (t
                         (irc-phi-add-incoming var-phi top-param-phi new-block)
                         (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi new-block))))
                ;; All old-blocks stick with what they have.
                (dolist (old-block old-blocks)
                  (irc-phi-add-incoming var-phi top-param-phi old-block)
                  (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi old-block))))))
        (let ((dec (irc-sub nargs-remaining (irc-size_t 2))))
          (irc-phi-add-incoming nargs-remaining dec kw-loop-continue))
        (irc-br kw-loop)
        ;; Loop over.
        (irc-begin-block after)
        ;; If we hit a bad keyword, and care, signal an error.
        (unless lambda-list-aokp
          (let ((aok-check (irc-basic-block-create "aok-check"))
                (kw-assigns (irc-basic-block-create "kw-assigns")))
            (irc-cond-br sbkw aok-check kw-assigns)
            (irc-begin-block aok-check)
            (irc-intrinsic-invoke-if-landing-pad-or-call
             "cc_ifBadKeywordArgumentException"
             ;; aok was initialized to NIL, regardless of the suppliedp, so this is ok.
             (list allow-other-keys bad-keyword *current-function-description*))
            (irc-br kw-assigns)
            (irc-begin-block kw-assigns)))
        (do* ((top-param-phis top-param-phis (cdr top-param-phis))
              (top-param-phi (car top-param-phis) (car top-param-phis))
              (top-suppliedp-phis top-suppliedp-phis (cdr top-suppliedp-phis))
              (top-suppliedp-phi (car top-suppliedp-phis) (car top-suppliedp-phis))
              (cur-key (cdr keyargs) (cddddr cur-key))
              (key (car cur-key) (car cur-key))
              (var (caddr cur-key) (caddr cur-key))
              (suppliedp (cadddr cur-key) (cadddr cur-key)))
             ((endp cur-key))
          (when (or (not (eq key :allow-other-keys)) lambda-list-aokp aok-parameter-p)
            (funcall *argument-out* top-param-phi var)
            (funcall *argument-out* top-suppliedp-phi suppliedp)))))))

(defun compile-general-lambda-list-code (reqargs 
					 optargs 
					 rest-var
                                         varest-p
					 key-flag 
					 keyargs 
					 allow-other-keys
					 calling-conv
                                         &key argument-out (safep t))
  (cmp-log "Entered compile-general-lambda-list-code%N")
  (let* ((*argument-out* argument-out)
         (nargs (calling-convention-nargs calling-conv))
         (nreq (car reqargs))
         (nopt (car optargs))
         (nfixed (+ nreq nopt)))
    (unless (zerop nreq)
      (when safep
        (compile-error-if-not-enough-arguments nreq nargs))
      (compile-required-arguments reqargs calling-conv))
    (let (;; note: atm, we won't be in this function if we only have required args,
          ;; so we basically always need these. But that can (hopefully will) change,
          ;; once handling is uniform.
          (iNIL (irc-nil)) (iT (irc-t)))
      (if (or rest-var key-flag)
          ;; We have &key and/or &rest, so parse with that expectation.
          ;; Specifically, we have to get a variable for how many arguments are left after &optional.
          (let ((nremaining
                  (if (zerop nopt)
                      ;; Having no optional arguments makes it easy.
                      (irc-sub nargs (irc-size_t nreq) "nremaining")
                      ;; But otherwise...
                      (let ((args-remain (irc-basic-block-create "args-remain"))
                            (args-exhausted (irc-basic-block-create "args-exhausted"))
                            (after-optional (irc-basic-block-create "after-optional")))
                        (compile-optional-arguments optargs nreq calling-conv args-exhausted iNIL iT)
                        (irc-branch-to-and-begin-block args-remain)
                        (let ((sub (irc-sub nargs (irc-size_t nfixed))))
                          (irc-br after-optional)
                          (irc-begin-block args-exhausted)
                          (irc-br after-optional)
                          (irc-begin-block after-optional)
                          (let ((phi (irc-phi %size_t% 2 "nremaining")))
                            ;; If we're out of arguments, we lie a bit and say there are zero args
                            ;; remaining. &key and &rest parsing then do the right thing.
                            (irc-phi-add-incoming phi (irc-size_t 0) args-exhausted)
                            (irc-phi-add-incoming phi sub args-remain)
                            phi))))))
            ;; Note that we don't need to check for too many arguments here.
            (when rest-var
              (compile-rest-argument rest-var varest-p nremaining calling-conv))
            (when key-flag
              (compile-key-arguments keyargs (or allow-other-keys (not safep)) nremaining calling-conv iNIL iT)))
          ;; We don't have &key or &rest, but we might still have &optional.
          (progn
            (let ((final (irc-basic-block-create "done-parsing-args")))
              (unless (zerop nopt)
                (compile-optional-arguments optargs nreq calling-conv final iNIL iT))
              (compile-error-if-too-many-arguments nfixed nargs)
              (irc-branch-to-and-begin-block final)))))))

(defun compile-only-reg-and-opt-arguments (reqargs optargs cc &key argument-out (safep t))
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
                  (when safep
                    (irc-intrinsic "cc_check_if_wrong_number_of_arguments"
                                   (cmp:calling-convention-nargs cc)
                                   (jit-constant-size_t (car reqargs))
                                   (jit-constant-size_t (+ (car reqargs) (car optargs)))
                                   *current-function-description*))
                  (irc-add-case sw (jit-constant-size_t opti) case-bb))
              (do* ((optj 0 (1+ optj))
                    (cur-target (cdr optargs) (cdddr cur-target))
                    (cur-register-args (nthcdr (first reqargs) register-args) (cdr cur-register-args))
                    (target (first cur-target) (first cur-target))
                    (targetp (second cur-target) (second cur-target))
                    (arg (car cur-register-args) (car cur-register-args)))
                   ((null cur-target))
                (if (>= optj opti)
                    (funcall argument-out false-val targetp)
                    (progn
                      (funcall argument-out arg target)
                      (funcall argument-out true-val targetp))))
              (irc-br req-bb))))
        (progn
          (when safep
            (irc-intrinsic "cc_check_if_wrong_number_of_arguments"
                           (cmp:calling-convention-nargs cc)
                           (jit-constant-size_t (car reqargs))
                           (jit-constant-size_t (+ (car reqargs) (car optargs)))
                           *current-function-description*))
          (irc-br req-bb)))
    (irc-begin-block req-bb)
    (do* ((cur-target (cdr reqargs) (cdr cur-target))
          (cur-register-args register-args (cdr cur-register-args))
          (target (car cur-target) (car cur-target))
          (arg (car cur-register-args) (car cur-register-args)))
         ((null cur-target))
      (funcall argument-out arg target))))

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
(defun compile-lambda-list-code (lambda-list calling-conv
                                 &key argument-out (safep t))
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
          (compile-only-reg-and-opt-arguments reqargs optargs calling-conv
                                              :argument-out argument-out
                                              :safep safep))
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
                                            :argument-out argument-out
                                            :safep safep)))))

(defun maybe-alloc-cc-setup (lambda-list debug-on)
  "Maybe allocate slots in the stack frame to handle the calls
   depending on what is in the lambda-list (&rest, &key etc) and debug-on.
   Return a calling-convention-configuration object that describes what was allocated.
   See the bclasp version in lambdalistva.lsp."
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys aux varest-p)
      (core:process-lambda-list lambda-list 'core::function)
    (declare (ignore aux))
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

(defun bclasp-compile-lambda-list-code (fn-env callconv &key (safep t))
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
       callconv
       :safep safep
       :argument-out (lambda (value datum)
                       (let* ((info (assoc datum output-bindings))
                              (symbol (car info))
                              (index (cdr info))
                              (ref (codegen-lexical-var-reference symbol 0 index new-env new-env)))
                         (irc-store value ref))))
      new-env)))
