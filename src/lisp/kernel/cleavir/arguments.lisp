(in-package :cmp)

(defgeneric xep-nargs (arguments))

(defclass xep-arguments () ())
(defclass general-xep-arguments (xep-arguments)
  ((%array :initarg :array :reader xep-array)
   (%nargs :initarg :nargs :reader xep-nargs)))
(defclass fixed-xep-arguments (xep-arguments)
  ((%arguments :initarg :arguments :reader xep-arguments)))

(defmethod xep-nargs ((arguments fixed-xep-arguments))
  (irc-size_t (length (xep-arguments arguments))))

;;; Generate code to get the nth argument (i.e. an LLVM Value) and return it.
;;; n is a constant (i.e. a Lisp integer).
(defgeneric nth-arg (arguments n))
(defmethod nth-arg ((args general-xep-arguments) n)
  (irc-t*-load (irc-typed-gep (llvm-sys:array-type-get %t*% 0)
                              (xep-array args) (list 0 n) "arg*")))
(defmethod nth-arg ((args fixed-xep-arguments) n)
  (nth n (xep-arguments args)))

;;; Generate code to get the arguments starting with the nth as an array.
;;; n is a constant (i.e. a Lisp integer).
(defgeneric remaining-args (args n))
(defmethod remaining-args ((args general-xep-arguments) n)
  ;; Note that n (a constant) must be less than nargs (a variable) for the
  ;; GEP to be a valid pointer. We don't load from it in that case, but we
  ;; still don't want to use an inbounds GEP because poison is bad.
  (irc-const-gep1-64 %t**% (xep-array args) n "args"))
(defmethod remaining-args ((args fixed-xep-arguments) n)
  ;; Here we have to actually allocate, and then fill it up.
  ;; Since we have a constant nargs and constant n, the size of the
  ;; allocation is fixed.
  (let* ((vals (xep-arguments args))
         (size (- (length vals) n)))
    (if (<= size 0)
        (llvm-sys:constant-pointer-null-get %t**%)
        (let ((res (alloca %t*% size)))
          ;; Fill it up.
          (loop for arg in (nthcdr n vals)
                for i from 0
                for addr = (irc-typed-gep (llvm-sys:array-type-get %t*% 0)
                                          res (list i))
                do (irc-store arg addr))
          res))))

(defun compile-wrong-number-arguments-block (fname nargs min max)
  ;; make a new irbuilder, so as to not disturb anything
  (with-irbuilder ((llvm-sys:make-irbuilder (thread-local-llvm-context)))
    (let ((errorb (irc-basic-block-create "wrong-num-args")))
      (irc-begin-block errorb)
      (irc-intrinsic "cc_wrong_number_of_arguments"
                     ;; We use low max to indicate no upper limit.
                     fname nargs min (or max (irc-size_t 0)))
      (irc-unreachable)
      errorb)))

;; Generate code to signal an error iff there weren't enough arguments provided.
(defun compile-error-if-not-enough-arguments (error-block cmin nargs)
  (let* ((cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ult nargs cmin)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block cont-block)))

;; Ditto but with too many.
(defun compile-error-if-too-many-arguments (error-block cmax nargs)
  (let* ((cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ugt nargs cmax)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block cont-block)))

;; Generate code to bind the required arguments. Return the LLVM values.
(defun compile-required-arguments (xepargs reqargs)
  ;; reqargs is as returned from process-lambda-list- (# ...) where # is the count.
  (loop for i from 0
        for req in (rest reqargs) ; maybe use for naming?
        collect (nth-arg xepargs i)))

(defgeneric compile-optional-arguments (xepargs optargs nreq false true))
(defmethod compile-optional-arguments ((xepargs general-xep-arguments)
                                       optargs nreq false true)
  ;; General case: variadic call.
  ;; optargs is (# var suppliedp default ...)
  ;; We basically generate a switch.
  ;; For (&optional a b) for example,
  #|
size_t nargs_remaining;
switch (nargs) {
  case 0: a = [nil]; a_p = [nil]; b = [nil]; b_p = [nil]; break;
  case 1: a = va_arg(); a_p = [t]; b = [nil]; b_p = [nil]; break;
  default: a = va_arg(); a_p = [t]; b = va_arg(); b_p = [t]; break;
}
  |#
  ;; All these assignments are done with phi so it's a bit more confusing to follow, unfortunately.
  (let* ((nargs (xep-nargs xepargs))
         (nopt (first optargs))
         (nfixed (+ nopt nreq))
         (opts (rest optargs))
         (enough (irc-basic-block-create "enough-for-optional"))
         (undef (irc-undef-value-get %t*%))
         (sw (irc-switch nargs enough nopt))
         (assn (irc-basic-block-create "optional-assignments"))
         (final (irc-basic-block-create "done-parsing-optionals")))
    ;; We generate the assignments first, although they occur last.
    ;; It's just a bit more convenient to do that way.
    (irc-begin-block assn)
    (let ((npreds (1+ nopt))
          (var-phis nil) (suppliedp-phis nil))
      (dotimes (i nopt)
        (push (irc-phi %t*% npreds) suppliedp-phis)
        (push (irc-phi %t*% npreds) var-phis))
      (irc-br final)
      ;; Generate a block for each case.
      (do ((i nreq (1+ i)))
          ((= i nfixed))
        (let ((new (irc-basic-block-create (core:fmt nil "supplied-{}-arguments" i))))
          (llvm-sys:add-case sw (irc-size_t i) new)
          (irc-begin-block new)
          ;; Assign each optional parameter accordingly.
          (loop for var-phi in var-phis
                for suppliedp-phi in suppliedp-phis
                for j from nreq
                for enough = (< j i)
                do (irc-phi-add-incoming suppliedp-phi (if enough true false) new)
                   (irc-phi-add-incoming var-phi (if enough (nth-arg xepargs i) undef) new))
          (irc-br assn)))
      ;; Default case: everything gets a value and a suppliedp=T.
      (irc-begin-block enough)
      (dolist (suppliedp-phi suppliedp-phis)
        (irc-phi-add-incoming suppliedp-phi true enough))
      (loop for var-phi in var-phis
            for i from nreq
            do (irc-phi-add-incoming var-phi (nth-arg xepargs i) enough))
      (irc-br assn)
      ;; ready to generate more code
      (irc-begin-block final)
      (loop for (var suppliedp) on opts by #'cdddr
            for var-phi in var-phis
            for suppliedp-phi in suppliedp-phis
            collect var-phi collect suppliedp-phi))))

(defmethod compile-optional-arguments ((xepargs fixed-xep-arguments)
                                       optargs nreq false true)
  ;; Specific case: Argcount is known. Optional processing is basically
  ;; trivial in this circumstance.
  (loop with args = (nthcdr nreq (xep-arguments xepargs))
        with undef = (irc-undef-value-get %t*%)
        for (var suppliedp) on (rest optargs) by #'cdddr
        for arg = (pop args)
        for val = (if (null arg) undef arg)
        for sp = (if (null arg) false true)
        collect val collect sp))

(defun compile-rest-argument (rest-var varest-p rest-alloc args nremaining)
  (declare (ignore rest-var)) ; old, maybe use for label name later?
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (list
   (cond ((eq rest-alloc 'ignore)
          ;; &rest variable is ignored- allocate nothing
          (irc-undef-value-get %t*%))
         ((eq rest-alloc 'dynamic-extent)
          ;; Do the dynamic extent thing- alloca, then an intrinsic to initialize it.
          (let ((rrest (alloca-dx-list :length nremaining :label "rrest")))
            (irc-intrinsic "cc_gatherDynamicExtentRestArguments"
                           args nremaining
                           (irc-bit-cast rrest %t**%))))
         (varest-p
          (let ((temp-vaslist (alloca-vaslist :label "rest")))
            (irc-intrinsic "cc_gatherVaRestArguments"
                           args nremaining temp-vaslist)))
         (t
          ;; general case- heap allocation
          (irc-intrinsic "cc_gatherRestArguments"
                         args nremaining)))))

#|
Keyword processing is the most complicated part, unsurprisingly.
We process the arguments from back to front, e.g. in :a 7 :a 8 we'd set A
to 8 first, and then to 7 later, since the semantics demand that A
eventually end up as 7.
Here is pseudo-C for the parser for (&key a). [foo] indicates an inserted constant.
Having to write with phi nodes unfortunately makes things rather more confusing.

if ((remaining_nargs % 2) == 1)
  cc_oddKeywordException([*fname*]);
tstar bad_keyword = undef;
bool seen_bad_keyword = false;
tstar allow_other_keys = [nil], allow_other_keys_p = [nil];
for (; remaining_nargs != 0; remaining_nargs -= 2) {
  tstar key = remaining_args[remaining_nargs - 2];
  tstar value = remaining_args[remaining_nargs - 1];
  if (key == [:a]) {
    a_p = [t]; a = value; continue;
  }
  ...ditto for other keys...
  if (key == [:allow-other-keys]) {
    allow_other_keys_p = [t]; allow_other_keys = value; continue;
  } else { seen_bad_keyword = true; bad_keyword = key; }
}
if (seen_bad_keyword)
  cc_ifBadKeywordArgumentException(allow_other_keys, bad_keyword, [*fname*]);
|#

;;; Returns a new block that is jumped to when the keyword does match.
;;; This is used to set up a phi to actually "assign" the value.
(defun compile-one-key-test (keyword key-arg cont-block)
  (let* ((keystring (string keyword))
         ;; NOTE: We might save a bit of time by moving this out of the loop.
         ;; Or maybe LLVM can handle it. I don't know.
         (key-const (clasp-cleavir::literal keyword))
         (match (irc-basic-block-create (core:fmt nil "matched-{}" keystring)))
         (mismatch (irc-basic-block-create (core:fmt nil "not-{}" keystring))))
    (irc-cond-br (irc-icmp-eq key-arg key-const) match mismatch)
    (irc-begin-block match)
    (irc-br cont-block)
    (irc-begin-block mismatch)
    match))

(defun compile-key-arguments (keyargs lambda-list-aokp nremaining remaining false true fname)
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
        (irc-intrinsic "cc_oddKeywordException" fname)
        (irc-unreachable))
      ;; Loop starts; welcome hell
      (irc-begin-block kw-loop)
      (let ((top-param-phis nil) (top-suppliedp-phis nil)
            (new-blocks nil)
            (nargs-remaining (irc-phi %size_t% 2 "nargs-remaining"))
            (sbkw (irc-phi %i1% 2 "seen-bad-keyword"))
            (bad-keyword (irc-phi %t*% 2 "bad-keyword")))
        (irc-phi-add-incoming nargs-remaining nremaining start)
        (irc-phi-add-incoming sbkw (jit-constant-false) start)
        (irc-phi-add-incoming bad-keyword undef start)
        (do-keys (key)
          (let ((var-phi (irc-phi %t*% 2 (core:fmt nil "{}-top" (string key)))))
            (push var-phi top-param-phis)
            ;; If we're paying attention to :allow-other-keys, track it specially
            ;; and initialize it to NIL.
            (cond ((and (not lambda-list-aokp) (eq key :allow-other-keys))
                   (irc-phi-add-incoming var-phi false start)
                   (setf allow-other-keys var-phi))
                  (t (irc-phi-add-incoming var-phi undef start))))
          (let ((suppliedp-phi (irc-phi %t*% 2 (core:fmt nil "{}-suppliedp-top" (string key)))))
            (push suppliedp-phi top-suppliedp-phis)
            (irc-phi-add-incoming suppliedp-phi false start)))
        (setf top-param-phis (nreverse top-param-phis)
              top-suppliedp-phis (nreverse top-suppliedp-phis))
        ;; Are we done?
        (let ((zerop (irc-icmp-eq nargs-remaining (irc-size_t 0))))
          (irc-cond-br zerop after matching))
        (irc-begin-block matching)
        ;; Start matching keywords
        ;; We process right to left. This means that we process the leftmost
        ;; instance of any keyword last, to match the language semantics.
        (let* (;; FIXME: These subs can be nuw
               (key-idx (irc-sub nargs-remaining (irc-size_t 2) "key-idx"))
               (key-addr (irc-typed-gep %t**% remaining (list key-idx) "key*"))
               (key-arg (irc-typed-load %t*% key-addr))
               (value-idx (irc-sub nargs-remaining (irc-size_t 1) "val-idx"))
               (val-addr (irc-typed-gep %t**% remaining (list value-idx) "value*"))
               (value-arg (irc-typed-load %t*% val-addr)))
          (do-keys (key)
            (push (compile-one-key-test key key-arg kw-loop-continue) new-blocks))
          (setf new-blocks (nreverse new-blocks))
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
                (irc-phi-add-incoming bot-bad-keyword bad-keyword new-block)))
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
                         (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi new-block))))))))
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
            (irc-intrinsic
             "cc_ifBadKeywordArgumentException"
             ;; aok was initialized to NIL, regardless of the suppliedp, so this is ok.
             allow-other-keys bad-keyword fname)
            (irc-br kw-assigns)
            (irc-begin-block kw-assigns)))
        (loop for top-param-phi in top-param-phis
              for top-suppliedp-phi in top-suppliedp-phis
              for (key) on (cdr keyargs) by #'cddddr
              when (or (not (eq key :allow-other-keys))
                       lambda-list-aokp aok-parameter-p)
                collect top-param-phi
                and collect top-suppliedp-phi)))))

(defun compile-general-lambda-list-code (reqargs
					 optargs
					 rest-var
                                         varest-p
					 key-flag 
					 keyargs 
					 allow-other-keys
					 xepargs
                                         &key (safep t)
                                           fname rest-alloc)
  (cmp-log "Entered compile-general-lambda-list-code%N")
  (let* ((nargs (xep-nargs xepargs))
         (nreq (car reqargs))
         (nopt (car optargs))
         (nfixed (+ nreq nopt))
         (creq (irc-size_t nreq))
         (cmax (if (or rest-var key-flag)
                   nil
                   (irc-size_t nfixed)))
         (wrong-nargs-block
           (when safep
             (compile-wrong-number-arguments-block fname nargs creq cmax)))
         ;; NOTE: Sometimes we don't actually need these.
         ;; We could save miniscule time by not generating.
         (iNIL (clasp-cleavir::%nil)) (iT (clasp-cleavir::%t)))
    (append
     (unless (zerop nreq)
       (when safep
         (compile-error-if-not-enough-arguments wrong-nargs-block creq nargs))
       (compile-required-arguments xepargs reqargs))
     (unless (zerop nopt)
       (compile-optional-arguments xepargs optargs nreq iNIL iT))
     (if (or rest-var key-flag)
         ;; We have &key and/or &rest, so parse with that expectation.
         ;; Specifically, we have to get a variable for how many arguments are left after &optional.
         (let* ((nremaining
                  (if (zerop nopt)
                      ;; With no optional arguments it's trivial.
                      (irc-sub nargs creq "nremaining")
                      ;; Otherwise we need nargs - nfixed, clamped to min 0.
                      ;; (Since nfixed > nargs is possible.)
                      ;; We used to have compile-optional-arguments return
                      ;; the number of remaining arguments, but that's a bit
                      ;; of pointless code for the rare case that we have
                      ;; both &optional and &rest/&key.
                      (irc-intrinsic "llvm.usub.sat.i64" nargs
                                     (irc-size_t nfixed))))
                (remaining (remaining-args xepargs nfixed)))
           ;; Note that we don't need to check for too many arguments here.
           (append
            (when rest-var
              (compile-rest-argument rest-var varest-p rest-alloc remaining nremaining))
            (when key-flag
              (compile-key-arguments keyargs (or allow-other-keys (not safep))
                                     nremaining remaining iNIL iT fname))))
        (when safep
          (cmp-log "Last if-too-many-arguments {} {}" cmax nargs)
          (compile-error-if-too-many-arguments wrong-nargs-block cmax nargs)
          nil)))))

(defun lambda-list-arguments (lambda-list)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs varest-p)
      (core:process-lambda-list lambda-list 'function)
    (declare (ignore auxargs allow-other-keys varest-p key-flag))
    (cmp-log "reqargs = {}%N" reqargs)
    (cmp-log "optargs = {}%N" optargs)
    (cmp-log "rest-var = {}%N" rest-var)
    (cmp-log "keyargs = {}%N" keyargs)
    (let ((args '()))
      (dolist (req (rest reqargs))
        (cmp-log "req-name = {}%N" req)
        (push req args))
      (do ((cur (rest optargs) (cdddr cur)))
          ((null cur) nil)
        (let ((opt-name (car cur))
              (opt-flag (cadr cur)))
          (cmp-log "opt cur = {}%N" cur)
          (cmp-log "opt-name = {}%N" opt-name)
          (cmp-log "opt-flag = {}%N" opt-flag)
          (push opt-name args)
          (when opt-flag (push opt-flag args))))
      (when rest-var (push rest-var args))
      (do ((cur (rest keyargs) (cddddr cur)))
          ((null cur) nil)
        (let ((key-name (caddr cur))
              (key-flag (cadddr cur)))
          (cmp-log "key-name = {}%N" key-name)
          (cmp-log "key-flag = {}%N" key-flag)
          (push key-name args)
          (when key-flag (push key-flag args))))
      (nreverse args))))

(defun calculate-cleavir-lambda-list-analysis (lambda-list)
  ;; we assume that the lambda list is in its correct format:
  ;; 1) required arguments are lexical locations.
  ;; 2) optional arguments are (<lexical location> <lexical location>)
  ;; 3) keyword arguments are (<symbol> <lexical location> <lexical location>)
  ;; this lets us cheap out on parsing, except &rest and &allow-other-keys.
  (cmp-log "calculate-cleavir-lambda-list-analysis lambda-list -> {}%N" lambda-list)
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
                      ;; fix me
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
                      ;; this file needs work fixme.
                      (push (second item) key)
                      (push (third item) key)))
               ;; nonlist; we picked off lambda list keywords, so it's an argument.
               (cond (rest-type
                      ;; we've seen a &rest lambda list keyword, so this must be that
                      (setf rest item))
                     ;; haven't seen anything, it's required
                     (t (incf required-count)
                        (push item required)))))))
    (let* ((cleavir-lambda-list (ensure-cleavir-lambda-list lambda-list))
           (arguments (lambda-list-arguments cleavir-lambda-list)))
      (make-cleavir-lambda-list-analysis
       :cleavir-lambda-list (ensure-cleavir-lambda-list lambda-list) ; Is this correct?
       :lambda-list-arguments arguments
       :required (cons required-count (nreverse required))
       :optional (cons optional-count (nreverse optional))
       :rest rest
       :key-flag key-flag
       :key-count (cons key-count (nreverse key))
       :aok-p aok-p
       :aux-p nil                       ; aux-p; unused here
       :va-rest-p (if (eq rest-type 'core:&va-rest) t nil)))))

;;; Main entry point. Returns the parsed arguments, i.e. a list of LLVM Values
;;; appropriate for the corresponding main function call. (But they're not cast.)
(defun compile-lambda-list-code (cleavir-lambda-list-analysis xepargs
                                 &key (safep t) rest-alloc
                                   (fname (clasp-cleavir::%nil)))
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys unused-auxs varest-p)
      (process-cleavir-lambda-list-analysis cleavir-lambda-list-analysis)
    (declare (ignore unused-auxs))
    (compile-general-lambda-list-code reqargs optargs rest-var varest-p
                                      key-flag keyargs allow-other-keys
                                      xepargs
                                      :safep safep :rest-alloc rest-alloc
                                      :fname fname)))
