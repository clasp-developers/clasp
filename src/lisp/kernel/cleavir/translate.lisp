(in-package #:clasp-cleavir)

#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

;;; Backend information associated with a BIR function.
(defclass llvm-function-info ()
  (;; In BIR, function environments are sets but we'd like to have it
   ;; be a list to ensure ordering.
   (%environment :initarg :environment :type list :reader environment)
   ;; The argument variables of the function lambda list.
   (%arguments :initarg :arguments :type list :reader arguments)
   ;; The eXternal Entry Point is in charge of loading values and
   ;; cells from the closure vector and parsing the number of arguments.
   (%xep-function :initarg :xep-function :reader xep-function)
   (%xep-function-description :initarg :xep-function-description :reader xep-function-description)
   (%main-function :initarg :main-function :reader main-function)
   ;; karlosz added main-function-description - but it isn't used by anything
   ;;  so I (meister) removed it
   #+(or)(%main-function-description :initarg :main-function-description :reader main-function-description)))

(defun lambda-list-too-hairy-p (lambda-list)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs aok aux varest-p)
      (cmp:process-bir-lambda-list lambda-list)
    (declare (ignore reqargs optargs keyargs aok aux rest-var varest-p))
    key-flag))

(defun nontrivial-mv-local-call-p (call)
  (cond ((typep call 'cc-bmir:fixed-mv-local-call)
         ;; Could still be nontrivial if the number of arguments is wrong
         (multiple-value-bind (req opt rest)
             (cmp:process-bir-lambda-list (bir:lambda-list (bir:callee call)))
           (let ((lreq (length (cc-bmir:rtype (second (bir:inputs call))))))
             (or (< lreq (car req))
                 (and (not rest) (> lreq (+ (car req) (car opt))))))))
        ((typep call 'bir:mv-local-call) t)
        (t nil)))

(defun xep-needed-p (function)
  (or (bir:enclose function)
      ;; We need a XEP for more involved lambda lists.
      (lambda-list-too-hairy-p (bir:lambda-list function))
      ;; or for mv-calls that might need to signal an error.
      (and (cleavir-set:some #'nontrivial-mv-local-call-p
                             (bir:local-calls function))
           (multiple-value-bind (req opt rest)
               (cmp:process-bir-lambda-list (bir:lambda-list function))
             (declare (ignore opt))
             (or (plusp (car req)) (not rest))))
      ;; Assume that a function with no enclose and no local calls is
      ;; toplevel and needs an XEP. Else it would have been removed or
      ;; deleted as it is unreferenced otherwise.
      (cleavir-set:empty-set-p (bir:local-calls function))))

(defun argument-rtype->llvm (arg)
  (let ((rtype (cc-bmir:rtype arg)))
    (assert (and (listp rtype) (= (length rtype) 1)))
    (vrtype->llvm (first rtype))))

;;; Given the rtype of a returni input, determine the llvm return type of the
;;; returni's function.
(defun return-rtype->llvm (rtype)
  (cond ((eq rtype :multiple-values) cmp:%tmv%)
        ((eq rtype :vaslist) cmp:%vaslist%)
        ((not (listp rtype)) (error "Bad rtype ~a" rtype))
        ((null rtype) cmp:%void%)
        ((null (rest rtype)) (vrtype->llvm (first rtype)))
        (t (llvm-sys:struct-type-get
            (cmp:thread-local-llvm-context)
            (mapcar #'vrtype->llvm rtype)
            nil))))

;;; Given an IR function, determine the llvm type for the local function's
;;; return value.
(defun main-function-return-type (ir)
  (let ((returni (bir:returni ir)))
    (if returni
        (return-rtype->llvm (cc-bmir:rtype (bir:input returni)))
        cmp:%void%)))

(defun allocate-llvm-function-info (function &key (linkage 'llvm-sys:internal-linkage))
  (let* ((lambda-name (get-or-create-lambda-name function))
         (jit-function-name (cmp:jit-function-name lambda-name))
         (function-info (calculate-function-info function lambda-name))
         (arguments
           (let ((arglist '()))
             (dolist (item (bir:lambda-list function))
               (unless (symbolp item)
                 (if (consp item)
                     (ecase (length item)
                       (2
                        (push (first item) arglist)
                        (push (second item) arglist))
                       (3
                        (push (second item) arglist)
                        (push (third item) arglist)))
                     (push item arglist))))
             (nreverse arglist))))
    (let ((function-description (cmp:irc-make-function-description function-info jit-function-name)))
      (multiple-value-bind (the-function local-entry-point)
          (cmp:irc-local-function-create
           (llvm-sys:function-type-get
            (main-function-return-type function)
            (nconc
             (loop repeat (cleavir-set:size (bir:environment function))
                   collect cmp:%t*%)
             (mapcar #'argument-rtype->llvm arguments)))
           'llvm-sys:internal-linkage ;; was llvm-sys:private-linkage
           jit-function-name
           cmp:*the-module*
           function-description)
        (let ((xep-group (if (xep-needed-p function)
                             (cmp:irc-xep-functions-create (cmp:function-info-cleavir-lambda-list-analysis function-info)
                                                           linkage
                                                           jit-function-name
                                                           cmp:*the-module*
                                                           function-description
                                                           the-function
                                                           local-entry-point)
                             :xep-unallocated)))
          (if (eq xep-group :xep-unallocated)
              (make-instance 'llvm-function-info
                             :environment (cleavir-set:set-to-list (bir:environment function))
                             :main-function the-function
                             :xep-function :xep-unallocated
                             :xep-function-description :xep-unallocated
                             :arguments arguments)
              (make-instance 'llvm-function-info
                             :environment (cleavir-set:set-to-list (bir:environment function))
                             :main-function the-function
                             :xep-function xep-group
                             :xep-function-description function-description
                             :arguments arguments)))))))

;;; Return value is unspecified/irrelevant.
(defgeneric translate-simple-instruction (instruction abi))

;;; Ditto
(defgeneric translate-terminator (instruction abi next))

(defun inst-source (inst)
  (origin-source (bir:origin inst)))

;;; Put in source info.
(defmethod translate-simple-instruction :around
    ((instruction bir:instruction) abi)
  (declare (ignore abi))
  (cmp:with-debug-info-source-position ((ensure-origin
                                         (inst-source instruction)
                                         999902))
    (call-next-method)))
(defmethod translate-terminator :around
    ((instruction bir:instruction) abi next)
  (declare (ignore abi next))
  (cmp:with-debug-info-source-position ((ensure-origin
                                         (inst-source instruction)
                                         999903))
    (call-next-method)))

(defmethod translate-terminator ((instruction bir:unreachable)
                                 abi next)
  (declare (ignore abi next))
  (cmp:irc-unreachable))

(defmethod translate-terminator ((instruction bir:returni) abi next)
  (declare (ignore abi next))
  (let* ((inp (bir:input instruction))
         (rt (cc-bmir:rtype inp)) (inv (in inp)))
    (cond ((eq rt :multiple-values) (cmp:irc-ret inv))
          ((eq rt :vaslist) (cmp:irc-ret inv))
          ((not (listp rt)) (error "Bad rtype ~a" rt))
          ((null rt) (cmp:irc-ret-void))
          ((null (rest rt)) (cmp:irc-ret inv))
          (t ; fixed values return: construct an aggregate and return it.
           (let* ((stype (return-rtype->llvm rt))
                  (s (llvm-sys:undef-value-get stype)))
             (loop for i from 0
                   for v in inv
                   do (setf s (cmp:irc-insert-value s v (list i))))
             (cmp:irc-ret s))))))

(defmethod translate-terminator ((inst bir:values-save) abi next)
  (declare (ignore abi))
  (let* ((tmv (in (first (bir:inputs inst))))
         (outp (bir:output inst))
         (nvals (cmp:irc-tmv-nret tmv))
         (primary (cmp:irc-tmv-primary tmv))
         ;; NOTE: Must be done BEFORE the alloca.
         (save (%intrinsic-call "llvm.stacksave" nil))
         (mv-temp (cmp:alloca-temp-values nvals))
         (label (datum-name-as-string outp))
         (s2 (cmp::irc-make-vaslist nvals mv-temp label)))
    (setf (dynenv-storage inst) save)
    (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
    (out s2 outp))
  ;; Continue
  (cmp:irc-br (first next)))

(defgeneric undo-dynenv (dynamic-environment tmv))

(defmethod undo-dynenv ((dynamic-environment bir:dynamic-leti) tmv)
  ;; Could undo stack allocated cells here
  (declare (ignore tmv)))
(defmethod undo-dynenv ((dynenv bir:come-from) tmv)
  (declare (ignore tmv))
  (let ((old-de-stack (first (dynenv-storage dynenv))))
    (when old-de-stack
      (%intrinsic-call "cc_set_dynenv_stack" (list old-de-stack)))))
(defmethod undo-dynenv ((dynenv bir:values-save) tmv)
  (declare (ignore tmv))
  (%intrinsic-call "llvm.stackrestore" (list (dynenv-storage dynenv))))
(defmethod undo-dynenv ((dynenv bir:values-collect) tmv)
  (declare (ignore tmv))
  (let ((storage (dynenv-storage dynenv)))
    (when storage
      (%intrinsic-call "llvm.stackrestore" (list storage)))))

(defun translate-local-unwind (jump tmv)
  (loop with target = (bir:dynamic-environment
                       (first (bir:next jump)))
        for de = (bir:dynamic-environment jump)
          then (bir:parent de)
        when (eq target de)
          do (loop-finish)
        when (typep de 'bir:function)
          do (error "BUG: Dynamic environment chain screwed up somehow")
        do (undo-dynenv de tmv)))

(defmethod translate-terminator ((instruction bir:jump) abi next)
  (declare (ignore abi))
  (assert (= (length next) 1))
  (when (bir:unwindp instruction)
    (if (and (= (length (bir:outputs instruction)) 1)
             (eq (cc-bmir:rtype (first (bir:outputs instruction)))
                 :multiple-values))
        (translate-local-unwind instruction
                                (in (first (bir:inputs instruction))))
        (translate-local-unwind instruction nil)))
  (loop for in in (bir:inputs instruction)
        for out in (bir:outputs instruction)
        do (phi-out (in in) out (llvm-sys:get-insert-block cmp:*irbuilder*)))
  (cmp:irc-br (first next)))

(defgeneric translate-conditional-test (instruction next))

(defmethod translate-conditional-test (instruction next)
  ;; When the test is not a conditional test, just grab the value and
  ;; compare to NIL.
  (cmp:irc-cond-br
   (cmp:irc-icmp-eq (in (first (bir:outputs instruction))) (%nil)
                    (datum-name-as-string (first (bir:outputs instruction))))
   (second next) (first next)))
(defmethod translate-conditional-test
    ((instruction bir:conditional-test) next)
  (declare (ignore next))
  (error "Don't know how to translate this conditional test ~a." instruction))

(defmethod translate-conditional-test ((instruction bir:eq-test) next)
  (let ((inputs (bir:inputs instruction)))
    (cmp:irc-cond-br
     (cmp:irc-icmp-eq (in (first inputs)) (in (second inputs))
                      (datum-name-as-string (bir:output instruction)))
     (first next) (second next))))

(defmacro define-tag-test (inst mask tag)
  `(defmethod translate-conditional-test ((instruction ,inst) next)
     (cmp:compile-tag-check (in (first (bir:inputs instruction)))
                            ,mask ,tag
                            (first next) (second next))))
(define-tag-test cc-bmir:fixnump cmp:+fixnum-mask+ cmp:+fixnum00-tag+)
(define-tag-test cc-bmir:consp cmp:+immediate-mask+ cmp:+cons-tag+)
(define-tag-test cc-bmir:characterp cmp:+immediate-mask+ cmp:+character-tag+)
(define-tag-test cc-bmir:single-float-p
  cmp:+immediate-mask+ cmp:+single-float-tag+)
(define-tag-test cc-bmir:generalp cmp:+immediate-mask+ cmp:+general-tag+)

(defmethod translate-conditional-test ((instruction cc-bmir:headerq) next)
  (cmp:compile-header-check
   (cc-bmir:info instruction)
   (in (first (bir:inputs instruction)))
   (first next) (second next)))

(defmethod translate-conditional-test ((inst cc-vaslist:nendp) next)
  (cmp:irc-cond-br
   (cmp:irc-icmp-ugt (cmp:irc-vaslist-nvals (in (bir:input inst)))
                     (%size_t 0))
   (first next) (second next)))

(defmethod translate-conditional-test ((inst bir:primop) next)
  (translate-conditional-primop (cleavir-primop-info:name (bir:info inst))
                                inst next))

(defmethod translate-terminator ((instruction bir:ifi) abi next)
  (declare (ignore abi))
  (let ((in (first (bir:inputs instruction))))
    (etypecase in
      (bir:output
       (translate-conditional-test (bir:definition in) next))
      ((or bir:phi bir:argument)
       (cmp:irc-cond-br (cmp:irc-icmp-eq (in in) (%nil))
                        (second next) (first next))))))

(defmethod translate-simple-instruction
    ((instruction bir:conditional-test) abi)
  ;; Since we translate-conditional-test from the ifi instead, do nothing.
  (declare (ignore instruction abi)))

(defmethod translate-terminator ((instruction bir:case) abi next)
  (declare (ignore abi))
  (assert (= (length next) (length (bir:next instruction))
             (1+ (length (bir:comparees instruction)))))
  (let* ((input (in (first (bir:inputs instruction))))
         (default (first (last next)))
         (dests (butlast next))
         (comparees (bir:comparees instruction))
         (ncases (loop for list in comparees summing (length list)))
         (only-fixnums-p (loop for list in comparees
                               always (every #'core:fixnump list)))
         ;; LLVM does better with contiguous ranges. It's not smart enough to
         ;; recognize that it could get a contiguous range by shifting.
         ;; (Or maybe it doesn't care. How often does that happen?)
         (rinput (if only-fixnums-p
                     (let ((fixnum-block (cmp:irc-basic-block-create "is-fixnum")))
                       ;; same as fixnump instruction
                       (cmp:compile-tag-check input
                                              cmp:+fixnum-mask+
                                              cmp:+fixnum00-tag+
                                              fixnum-block default)
                       (cmp:irc-begin-block fixnum-block)
                       (cmp:irc-untag-fixnum input cmp:%fixnum% "switch-input"))
                     (cmp:irc-ptr-to-int input cmp:%fixnum%)))
         (switch (cmp:irc-switch rinput default ncases)))
    (loop for list in comparees
          for dest in dests
          do (loop for object in list
                   for immediate = (core:create-tagged-immediate-value-or-nil object)
                   do (assert (not (null immediate)))
                      (cmp:irc-add-case switch
                                        (%i64 (if only-fixnums-p
                                                  (ash immediate -2)
                                                  immediate))
                                        dest)))))

;;; Given a come-from and the successor LLVM blocks,
;;; return multiple values:
;;; 1) The iblocks that can be unwound to.
;;; 2) Their corresponding LLVM blocks.
;;; 3) New LLVM blocks to place between the come-from and the above blocks.
;;; 4) A boolean indicating whether we need to use a BlockDynEnv.
;;; (This last could alternately be kept from the original source?)
(defun categorize-come-from (come-from asuccessors)
  ;; We only care about iblocks that are actually unwound to.
  (loop with escp = nil
        for iblock in (rest (bir:next come-from))
        for successor in (rest asuccessors)
        when (and (has-entrances-p iblock)
                  ;; See bug #1321: Sometimes we have duplicates, in which case
                  ;; only the one entry is needed.
                  (not (member iblock iblocks :test #'eq)))
          collect iblock into iblocks
          and collect (cmp:irc-basic-block-create "come-from-restore")
                into blocks
          and collect successor into successors
          and do (unless (eq (bir:dynamic-environment iblock) come-from)
                   (setf escp t))
        finally (return (values iblocks blocks successors escp))))

(defun phi-out-for-come-from (phi block)
  (let ((mv (restore-multiple-value-0))
        (rt (cc-bmir:rtype phi)))
    (cond ((null rt))
          ((equal rt '(:object))
           (phi-out (cmp:irc-tmv-primary mv) phi block))
          ((eq rt :multiple-values) (phi-out mv phi block))
          ((every (lambda (x) (eq x :object)) rt)
           (phi-out
            (list* (cmp:irc-tmv-primary mv)
                   (loop for i from 1 below (length rt)
                         collect (cmp:irc-t*-load (return-value-elt i))))
            phi block))
          (t (error "BUG: Bad rtype ~a" rt)))))

(defun translate-come-from (come-from successors)
  (let* ((simplep (bir-transformations:simple-unwinding-p
                   come-from *clasp-system*))
         (frame
           (unless simplep
             (%intrinsic-call "llvm.frameaddress.p0i8"
                              (list (%i32 0)))))
         (normal-successor (first successors))
         (bufp (cmp:alloca cmp::%jmp-buf-tag% 1 "come-from-jmp-buf")))
    (multiple-value-bind (iblocks blocks successors blockp)
        (categorize-come-from come-from successors)
      (let* ((default (cmp:irc-basic-block-create "come-from-default"))
             (old-de-stack
               (unless simplep
                 (%intrinsic-call "cc_get_dynenv_stack" nil)))
             (dcons-space
               (unless simplep
                 (cmp:alloca-i8 cmp:+cons-size+ :alignment cmp:+alignment+
                                :label "come-from-dynenv-cons")))
             (dynenv
               (unless simplep
                 (%intrinsic-invoke-if-landing-pad-or-call
                  (if blockp
                      "cc_createAndPushBlockDynenv"
                      "cc_createAndPushTagbodyDynenv")
                  (list dcons-space frame bufp))))
             (de-stack
               (unless simplep
                 (if blockp old-de-stack (%intrinsic-call "cc_get_dynenv_stack" nil))))
             ;; Set the continuation for use by bir:unwind insts.
             (_ (out
                 (if simplep (cmp:irc-bit-cast bufp cmp:%t*%) dynenv)
                 come-from))
             (sj (%intrinsic-call "_setjmp" (list bufp)))
             (sw (cmp:irc-switch sj default (1+ (length iblocks)))))
        (declare (ignore _))
        ;; Set the dynenv storage. We explicitly put in NIL for
        ;; simple unwinds, so that we can figure out if there is a
        ;; dynenv later without dynenv-storage signaling an error.
        ;; Otherwise, it's a list of two dynenv stacks. The first is
        ;; where to go when totally exiting, and the second where to
        ;; go when unwinding here.
        (setf (dynenv-storage come-from)
              (if simplep nil (list old-de-stack de-stack)))
        (cmp:irc-begin-block default)
        (cmp:irc-unreachable)
        (cmp:irc-add-case sw (%i32 0) normal-successor)
        (loop for succ in successors
              for block in blocks
              for iblock in iblocks
              for destination-id = (get-destination-id iblock)
              ;; Note that the INPUTS can be NIL.
              for phi = (first (bir:inputs iblock))
              do (cmp:irc-add-case sw (%i32 destination-id) block)
                 (cmp:irc-begin-block block)
                 (when phi (phi-out-for-come-from phi block))
                 (cmp:irc-br succ))))))

(defmethod translate-terminator ((instruction bir:come-from) abi next)
  (declare (ignore abi))
  (cond ((cleavir-set:empty-set-p (bir:unwinds instruction))
         ;; Make sure undo-dynenv can correctly determine that it
         ;; doesn't need to do anything for us.
         (setf (dynenv-storage instruction) nil)
         (cmp:irc-br (first next)))
        (t
         (translate-come-from instruction next))))

(defmethod translate-terminator ((instruction bir:unwind) abi next)
  (declare (ignore abi next))
  (let* ((cont (in (bir:come-from instruction)))
         (inputs (bir:inputs instruction))
         (rv (first inputs))
         ;; Force the return values into a tmv for transmission.
         (rrv (when rv
                (let ((rt (cc-bmir:rtype rv)))
                  (cond ((equal rt '(:object))
                         (cmp:irc-make-tmv (%size_t 1) (in rv)))
                        ((eq rt :multiple-values) (in rv))
                        ((null rt) (cmp:irc-make-tmv (%size_t 0) (%nil)))
                        ((every (lambda (x) (eq x :object)) rt)
                         (let ((vals (in rv))
                               (nvals (length rt)))
                           (loop for i from 1 below nvals
                                 for v in (rest vals)
                                 do (cmp:irc-store v (return-value-elt i)))
                           (cmp:irc-make-tmv (%size_t nvals) (first vals))))
                        (t (error "BUG: Bad rtype ~a" rt))))))
         (destination (bir:destination instruction))
         (destination-id (get-destination-id destination)))
    ;; Transmit values
    (when rv (save-multiple-value-0 rrv))
    ;; unwind
    (if (bir-transformations:simple-unwinding-p
         (bir:come-from instruction) *clasp-system*)
        ;; Simple SJLJ
        ;; (Note: No landing pad because in order for SJLJ to occur,
        ;;  the dynamic environment must just be the function.)
        (let ((bufp (cmp:irc-bit-cast cont cmp::%jmp-buf-tag*%)))
          (%intrinsic-invoke-if-landing-pad-or-call
           "_longjmp" (list bufp (%i32 destination-id))))
        ;; Complex SJLJ
        (cmp:with-landing-pad (never-entry-landing-pad
                               (bir:dynamic-environment instruction))
          (%intrinsic-invoke-if-landing-pad-or-call
           "cc_sjlj_unwind"
           (list cont (%size_t destination-id))))))
  (cmp:irc-unreachable))

(defmethod translate-terminator ((instruction bir:unwind-protect) abi next)
  (declare (ignore abi))
  (let* ((cleanup (cmp:irc-basic-block-create "unwind-protect-cleanup"))
         (bufp (cmp:alloca cmp::%jmp-buf-tag% 1 "unwind-protect-buf"))
         (de-cons-mem (cmp:alloca-i8 cmp:+cons-size+ :alignment cmp:+alignment+
                                     :label "upde-cons-mem"))
         (upde-mem (cmp:alloca-i8 cmp:+unwind-protect-dynenv-size+
                                  :alignment cmp:+alignment+
                                  :label "unwind-protect-dynenv-mem"))
         (old-de-stack (%intrinsic-call "cc_get_dynenv_stack" nil))
         (upde (%intrinsic-call "cc_initializeAndPushCleanupDynenv"
                                (list upde-mem de-cons-mem bufp)
                                "unwind-protect-dynenv"))
         (sj (%intrinsic-call "_setjmp" (list bufp))))
    (declare (ignore upde))
    (setf (dynenv-storage instruction) old-de-stack) ; used in landing-pad
    ;; if the setjmp returns 0 (i.e. is not a nonlocal exit), we can proceed
    ;; directly to the successor; the cleanup call will be generated by
    ;; undo-dynenv.
    (cmp:irc-cond-br (cmp:irc-icmp-eq sj (%i32 0)) (first next) cleanup)
    (cmp:irc-begin-block cleanup)
    ;; Save values, call the cleanup, continue unwinding.
    ;; Note that we don't need to pop the dynenv, as the unwinder does so.
    (let* ((nvals (%intrinsic-call "cc_nvalues" nil "nvals"))
           (mv-temp (cmp:alloca-temp-values nvals)))
      (%intrinsic-call "cc_save_all_values" (list nvals mv-temp))
      (cmp:with-landing-pad (maybe-entry-landing-pad (bir:parent instruction)
                                                     *tags*)
        (closure-call-or-invoke (in (first (bir:inputs instruction))) nil)
        (%intrinsic-call "cc_load_all_values" (list nvals mv-temp))
        (%intrinsic-invoke-if-landing-pad-or-call "cc_sjlj_continue_unwinding" nil))
      (cmp:irc-unreachable)))
  #+(or)
  (cmp:irc-br (first next)))

(defmethod undo-dynenv ((dynenv bir:unwind-protect) tmv)
  (flet ((cleanup ()
           ;; This is maybe-entry for the sake of e.g.
           ;; (block nil (unwind-protect (... (return ...)) ... (return ...)))
           (cmp:with-landing-pad (maybe-entry-landing-pad
                                  (bir:parent dynenv) *tags*)
             (closure-call-or-invoke (in (first (bir:inputs dynenv))) nil))))
    (%intrinsic-call "cc_set_dynenv_stack" (list (dynenv-storage dynenv)))
    ;; We have to save values around it if we're in the middle of
    ;; returning values.
    (if tmv
        (let* ((nvals (cmp:irc-tmv-nret tmv))
               (primary (cmp:irc-tmv-primary tmv))
               (mv-temp (cmp:alloca-temp-values nvals)))
          (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
          (cleanup)
          (%intrinsic-call "cc_load_values" (list nvals mv-temp)))
        (cleanup))))

(defmethod translate-terminator ((instruction bir:bind) abi next)
  (declare (ignore abi))
  (let* ((bde-cons-mem (cmp:alloca-i8 cmp:+cons-size+ :alignment cmp:+alignment+
                                      :label "binding-dynenv-cons-mem"))
         (bde-mem (cmp:alloca-i8 cmp:+binding-dynenv-size+
                                 :alignment cmp:+alignment+
                                 :label "binding-dynenv-mem"))
         (inputs (bir:inputs instruction))
         (sym (in (first inputs)))
         (val (in (second inputs)))
         (old (%intrinsic-call "cc_TLSymbolValue" (list sym)))
         (old-de-stack (%intrinsic-call "cc_get_dynenv_stack" nil)))
    (%intrinsic-call "cc_initializeAndPushBindingDynenv"
                     (list bde-mem bde-cons-mem sym old))
    (setf (dynenv-storage instruction) (list sym old old-de-stack))
    (%intrinsic-call "cc_setTLSymbolValue" (list sym val)))
  (cmp:irc-br (first next)))

(defmethod undo-dynenv ((dynenv bir:bind) tmv)
  (declare (ignore tmv))
  (let ((store (dynenv-storage dynenv)))
    (%intrinsic-call "cc_resetTLSymbolValue"
                     (list (first store) (second store)))
    (%intrinsic-call "cc_set_dynenv_stack" (list (third store)))))

(defmethod translate-terminator
    ((instruction cc-bir:header-stamp-case) abi next)
  (declare (ignore abi))
  (let* ((stamp (in (first (bir:inputs instruction))))
         (stamp-i64 (cmp:irc-ptr-to-int stamp cmp:%i64%))
         (where (cmp:irc-and stamp-i64 (%i64 cmp:+where-tag-mask+)))
         (defaultb (cmp:irc-basic-block-create "impossible-default"))
         (sw (cmp:irc-switch where defaultb 4)))
    (cmp:irc-add-case sw (%i64 cmp:+derivable-where-tag+) (first next))
    (cmp:irc-add-case sw (%i64 cmp:+rack-where-tag+) (second next))
    (cmp:irc-add-case sw (%i64 cmp:+wrapped-where-tag+) (third next))
    (cmp:irc-add-case sw (%i64 cmp:+header-where-tag+) (fourth next))
    (cmp:irc-begin-block defaultb)
    (cmp:irc-unreachable)))

(defmethod translate-simple-instruction ((instruction bir:thei) abi)
  (declare (ignore abi))
  (out (in (bir:input instruction)) (bir:output instruction)))

(defmethod translate-simple-instruction ((instruction bir:enclose) abi)
  (declare (ignore abi))
  (out (enclose (find-llvm-function-info (bir:code instruction))
                (bir:extent instruction))
       (bir:output instruction)))

(defun maybe-insert-step-before (inst)
  (when (policy:policy-value (bir:policy inst)
                             'insert-step-conditions)
    (let ((origin (bir:origin inst)))
      (when (typep origin 'cst:cst)
        (let* ((frame (%intrinsic-call "llvm.frameaddress.p0i8"
                                       (list (%i32 0)) "stepper-frame"))
               (raw (cst:raw origin))
               (index
                 (handler-case (literal:reference-literal raw t)
                   (serious-condition ()
                     (literal:reference-literal
                      "<error dumping form>" t))))
               (lit
                 (multiple-value-bind (literals literals-type)
                     (literal:ltv-global)
                   (cmp:irc-t*-load
                    (cmp:irc-typed-gep-variable literals-type literals
                                                (list (%size_t 0) (%i64 index))
                                                "step-source")))))
          (%intrinsic-invoke-if-landing-pad-or-call
           "cc_breakstep" (list lit frame)))))))

(defmethod translate-simple-instruction :before
    ((instruction bir:abstract-call) abi)
  (declare (ignore abi))
  ;; We must force all closure initializers to run before a call.
  (force-initializers)
  ;; Cooperation with the stepper
  (maybe-insert-step-before instruction))

(defun maybe-insert-step-after (inst)
  (when (and (policy:policy-value (bir:policy inst)
                                          'insert-step-conditions)
             (typep (bir:origin inst) 'cst:cst))
    ;; OK, we inserted a cc_breakstep call in the above method,
    ;; so now we need to put in the cc_breakstep_after to support
    ;; the step-over facility.
    (%intrinsic-call "cc_breakstep_after"
                     (list (%intrinsic-call "llvm.frameaddress.p0i8"
                                            (list (%i32 0))
                                            "stepper-frame")))))

(defmethod translate-simple-instruction :after
    ((instruction bir:abstract-call) abi)
  (declare (ignore abi))
  (maybe-insert-step-after instruction))

;; LETI is a subclass of WRITEVAR, so we use a :before to bind the var.
(defmethod translate-simple-instruction :before ((instruction bir:leti) abi)
  (declare (ignore abi))
  (bind-variable (bir:output instruction)))

(defmethod translate-simple-instruction ((instruction bir:writevar)
                                         abi)
  (declare (ignore abi))
  (let* ((i (in (bir:input instruction))) (o (bir:output instruction))
         (rt (cc-bmir:rtype o)))
    (unless (or (null rt)
                (llvm-sys:type-equal (vrtype->llvm (first rt))
                                     (llvm-sys:get-type i)))
      (cleavir-bir-disassembler:display
       (bir:module (bir:function instruction)))
      (error "~a has wrong rtype; definitions ~a with definition-rtype ~a; input rtype ~a"
             (first (bir:inputs instruction))
             (bir:definitions (first (bir:inputs instruction)))
             (cc-bir-to-bmir::definition-rtype (first (bir:inputs instruction)))
             (cc-bmir:rtype (first (bir:inputs instruction)))))
    (variable-out i o)))

(defmethod translate-simple-instruction ((instruction bir:readvar) abi)
  (declare (ignore abi))
  (out (variable-in (bir:input instruction))
       (bir:output instruction)))

(defun gen-rest-list (present-arguments)
  (if (null present-arguments)
      (%nil)
      ;; Generate a call to cc_list.
      ;; TODO: DX &rest lists.
      (%intrinsic-invoke-if-landing-pad-or-call
       "cc_list" (list* (%size_t (length present-arguments))
                        present-arguments))))

(defun maybe-boxed-vaslist (boxp nvals vals)
  (let ((vas (cmp:irc-make-vaslist nvals vals)))
    (if boxp
        (let ((mem (cmp:alloca cmp:%vaslist% 1)))
          (cmp:irc-store vas mem)
          (cmp:irc-tag-vaslist mem))
        vas)))

(defun gen-va-rest-list (present-arguments boxp)
  (let* ((nargs (length present-arguments))
         ;; nargs is constant, so this alloca is just in the intro block.
         (dat (cmp:alloca cmp:%t*% nargs "local-va-rest")))
    ;; Store the arguments into the allocated memory.
    (loop for arg in present-arguments
          for i from 0
          do (cmp:irc-store arg (cmp:irc-typed-gep cmp:%t*% dat (list i))))
    ;; Make and return the va list object.
    (maybe-boxed-vaslist boxp (%size_t nargs) dat)))

(defun parse-local-call-optional-arguments (opt arguments)
  (loop for (op) on (rest opt) by #'cdddr
        if arguments
          collect (translate-cast (pop arguments) '(:object)
                                  (cc-bmir:rtype op))
          and collect (cmp::irc-t)
        else
          collect (cmp:irc-undef-value-get (argument-rtype->llvm op))
          and collect (%nil)))

;; Create than argument list for a local call by parsing the callee's
;; lambda list and filling in the correct values at compile time. We
;; assume that we have already checked the validity of this call.
(defun parse-local-call-arguments (req opt rest rest-vrtype arguments)
  (let* ((nreq (car req)) (nopt (car opt))
         (reqargs (subseq arguments 0 nreq))
         (more (nthcdr nreq arguments))
         (optargs (parse-local-call-optional-arguments opt more))
         (rest
           (cond ((not rest) nil)
                 ((eq rest :unused)
                  (list (cmp:irc-undef-value-get cmp:%t*%)))
                 ((eq rest :va-rest)
                  (list (gen-va-rest-list (nthcdr nopt more)
                                          (eq rest-vrtype :object))))
                 (t (list (gen-rest-list (nthcdr nopt more)))))))
    (append reqargs optargs rest)))

(defun enclose (code-info extent &optional (delay t))
  (let* ((environment (environment code-info))
         (enclosed-xep-group (xep-function code-info))
         (entry-point-reference (cmp:xep-group-entry-point-reference enclosed-xep-group)))
    (when (eq enclosed-xep-group :xep-unallocated)
      (error "BUG: Tried to ENCLOSE a function with no XEP"))
    (if environment
        (let* ((ninputs (length environment))
               (sninputs (%size_t ninputs))
               (enclose
                 (ecase extent
                   (:dynamic
                    (%intrinsic-call
                     "cc_stack_enclose"
                     (list (cmp:alloca-i8 (core:closure-with-slots-size ninputs)
                                           :alignment cmp:+alignment+
                                           :label "stack-allocated-closure")
                           (literal:constants-table-value (cmp:entry-point-reference-index entry-point-reference))
                           sninputs)))
                   (:indefinite
                    (%intrinsic-invoke-if-landing-pad-or-call
                     "cc_enclose"
                     (list (literal:constants-table-value (cmp:entry-point-reference-index entry-point-reference))
                           sninputs))))))
          ;; We may not initialize the closure immediately in case it partakes
          ;; in mutual reference.
          ;; (If DELAY NIL is passed this delay is not necessary.)
          (if delay
              (delay-initializer
               (lambda ()
                 (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_initialize_closure"
                  (list* enclose sninputs
                         (mapcar #'variable-as-argument environment)))))
              (%intrinsic-invoke-if-landing-pad-or-call
               "cc_initialize_closure"
               (list* enclose sninputs
                      (mapcar #'variable-as-argument environment))))
          enclose)
        ;; When the function has no environment, it can be compiled and
        ;; referenced as literal.
        (%closurette-value enclosed-xep-group))))

(defun rest-vrtype (rest-var)
  ;; We want :object or :vaslist only, even if the rest
  ;; param is unused etc., because our local call convention
  ;; doesn't account for unused parameters.
  ;; this is a bit suboptimal: see compute-rtype bir:argument method
  (if (and rest-var (equal (cc-bmir:rtype rest-var) '(:vaslist)))
      :vaslist
      :object))

(defun gen-local-call (callee arguments outputrt)
  (let ((callee-info (find-llvm-function-info callee)))
    (cond ((lambda-list-too-hairy-p (bir:lambda-list callee))
           ;; Has &key or something, so use the normal call protocol.
           ;; We allocate a fresh closure for every call. Hopefully this
           ;; isn't too expensive. We can always use stack allocation since
           ;; there's no possibility of this closure being stored in a closure
           ;; (If we local-call a self-referencing closure, the closure cell
           ;;  will get its value from some enclose.
           ;;  FIXME we could use that instead?)
           (translate-cast (closure-call-or-invoke
                            (enclose callee-info :dynamic nil)
                            arguments)
                           :multiple-values outputrt))
          (t
           ;; Call directly.
           (multiple-value-bind (req opt rest-var key-flag keyargs aok aux
                                 varest-p)
               (cmp:process-bir-lambda-list (bir:lambda-list callee))
             (declare (ignore keyargs aok aux))
             (assert (not key-flag))
             (let ((largs (length arguments)))
               (when (or (< largs (car req))
                         (and (not rest-var)
                              (> largs (+ (car req) (car opt)))))
                 ;; too many or too few args; we can get here from
                 ;; fixed-mv-local-calls for instance.
                 (return-from gen-local-call
                   (translate-cast (closure-call-or-invoke
                                    (enclose callee-info :dynamic nil)
                                    arguments)
                                   :multiple-values outputrt))))
             (let* ((rest-id (cond ((null rest-var) nil)
                                   ((bir:unused-p rest-var) :unused)
                                   (varest-p :va-rest)
                                   (t t)))
                    (rest-vrtype (rest-vrtype rest-var))
                    (subargs
                      (parse-local-call-arguments
                       req opt rest-id rest-vrtype arguments))
                    (args (append (mapcar #'variable-as-argument
                                          (environment callee-info))
                                  subargs))
                    (function (main-function callee-info))
                    (function-type (llvm-sys:get-function-type function))
                    (result-in-registers
                      (cmp::irc-call-or-invoke function-type function args)))
               #+(or)
               (llvm-sys:set-calling-conv result-in-registers 'llvm-sys:fastcc)
               (local-call-rv->inputs result-in-registers outputrt)))))))

(defmethod translate-simple-instruction ((instruction bir:local-call)
                                         abi)
  (declare (ignore abi))
  (let* ((callee (bir:callee instruction))
         (args (mapcar #'in (rest (bir:inputs instruction))))
         (output (bir:output instruction))
         (call (gen-local-call callee args (cc-bmir:rtype output))))
    (out call output)))

(defmethod translate-simple-instruction ((instruction bir:call) abi)
  (declare (ignore abi))
  (maybe-note-failed-transforms instruction)
  (let* ((inputs (bir:inputs instruction))
         (iinputs (mapcar #'in inputs))
         (output (bir:output instruction)))
    (out (closure-call-or-invoke
          (first iinputs) (rest iinputs)
          :label (datum-name-as-string output))
         output)))

(defun general-mv-local-call-vas (callee-info vaslist label outputrt)
  (translate-cast (cmp:irc-apply (enclose callee-info :dynamic nil)
                                 (cmp:irc-vaslist-nvals vaslist)
                                 (cmp:irc-vaslist-values vaslist)
                                 label)
                  :multiple-values outputrt))

(defun direct-mv-local-call-vas (vaslist callee-info req opt rest-var varest-p
                                 label outputrt)
  (let* ((nreq (car req))
         (nopt (car opt))
         (rnret (cmp:irc-vaslist-nvals vaslist))
         (rvalues (cmp:irc-vaslist-values vaslist))
         (nfixed (+ nreq nopt))
         (mismatch
           (unless (and (zerop nreq) rest-var)
             (cmp:irc-basic-block-create "lmvc-arg-mismatch")))
         (mte (if rest-var
                  (cmp:irc-basic-block-create "lmvc-more-than-enough")
                  mismatch))
         (merge (cmp:irc-basic-block-create "lmvc-after"))
         (sw (cmp:irc-switch rnret mte (+ 1 nreq nopt)))
         (environment (environment callee-info))
         (rest-vaboxp (not (eq (rest-vrtype rest-var) :vaslist))))
    (labels ((load-return-value (n)
               (cmp:irc-t*-load (cmp:irc-typed-gep cmp:%t*% rvalues (list n))))
             (load-return-values (low high)
               (loop for i from low below high
                     collect (load-return-value i)))
             (optionals (n)
               (parse-local-call-optional-arguments
                opt (load-return-values nreq (+ nreq n)))))
      ;; Generate phis for the merge block's call.
      (cmp:irc-begin-block merge)
      (let ((opt-phis
              (loop for (op s-p) on (rest opt) by #'cdddr
                    for op-ty = (argument-rtype->llvm op)
                    for s-p-ty = (argument-rtype->llvm s-p)
                    collect (cmp:irc-phi op-ty (1+ nopt))
                    collect (cmp:irc-phi s-p-ty (1+ nopt))))
            (rest-phi
              (cond ((null rest-var) nil)
                    ((bir:unused-p rest-var)
                     (cmp:irc-undef-value-get cmp:%t*%))
                    (t (cmp:irc-phi (argument-rtype->llvm rest-var)
                                    (1+ nopt))))))
        ;; Generate the mismatch block, if it exists.
        (when mismatch
          (cmp:irc-begin-block mismatch)
          (cmp::irc-intrinsic-call-or-invoke
           "cc_wrong_number_of_arguments"
           (list (enclose callee-info :indefinite nil) rnret
                 (%size_t nreq) (%size_t nfixed)))
          (cmp:irc-unreachable))
        ;; Generate not-enough-args cases.
        (loop for i below nreq
              do (cmp:irc-add-case sw (%size_t i) mismatch))
        ;; Generate optional arg cases, including the exactly-enough case.
        (loop for i upto nopt
              for b = (cmp:irc-basic-block-create
                       (format nil "lmvc-optional-~d" i))
              do (cmp:irc-add-case sw (%size_t (+ nreq i)) b)
                 (cmp:irc-begin-block b)
                 (loop for phi in opt-phis
                       for val in (optionals i)
                       do (cmp:irc-phi-add-incoming phi val b))
                 (when (and rest-var (not (bir:unused-p rest-var)))
                   (cmp:irc-phi-add-incoming
                    rest-phi
                    (if varest-p
                        (maybe-boxed-vaslist
                         rest-vaboxp (%size_t 0)
                         (llvm-sys:constant-pointer-null-get cmp:%t**%))
                        (%nil))
                    b))
                 (cmp:irc-br merge))
        ;; If there's a &rest, generate the more-than-enough arguments case.
        (when rest-var
          (cmp:irc-begin-block mte)
          (loop for phi in opt-phis
                for val in (optionals nopt)
                do (cmp:irc-phi-add-incoming phi val mte))
          (unless (bir:unused-p rest-var)
            (cmp:irc-phi-add-incoming
             rest-phi
             (if varest-p
                 (maybe-boxed-vaslist
                  rest-vaboxp
                  (cmp:irc-sub rnret (%size_t nfixed))
                  (cmp:irc-typed-gep cmp:%t*% rvalues (list nfixed)))
                 (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_mvcGatherRest2"
                  (list (cmp:irc-typed-gep cmp:%t*% rvalues (list nfixed))
                        (cmp:irc-sub rnret (%size_t nfixed)))))
             mte))
          (cmp:irc-br merge))
        ;; Generate the call, in the merge block.
        (cmp:irc-begin-block merge)
        (let* ((arguments
                 (nconc
                  (mapcar #'variable-as-argument environment)
                  (loop for r in (rest req)
                        for j from 0
                        collect (translate-cast (load-return-value j) '(:object)
                                                (cc-bmir:rtype r)))
                  opt-phis
                  (when rest-var (list rest-phi))))
               (function (main-function callee-info))
               (function-type (llvm-sys:get-function-type function))
               (call
                 (cmp:irc-call-or-invoke function-type function arguments
                                         cmp:*current-unwind-landing-pad-dest*
                                         label)))
          #+(or)(llvm-sys:set-calling-conv call 'llvm-sys:fastcc)
          (local-call-rv->inputs call outputrt))))))

(defmethod translate-simple-instruction
    ((instruction bir:mv-local-call) abi)
  (declare (ignore abi))
  (let* ((output (bir:output instruction))
         (outputrt (cc-bmir:rtype output))
         (oname (datum-name-as-string output))
         (callee (bir:callee instruction))
         (callee-info (find-llvm-function-info callee))
         (mvarg (second (bir:inputs instruction)))
         (mvargrt (cc-bmir:rtype mvarg))
         (mvargi (in mvarg)))
    (assert (eq mvargrt :vaslist))
    (out
     (multiple-value-bind (req opt rest-var key-flag keyargs
                           aok aux varest-p)
         (cmp::process-bir-lambda-list (bir:lambda-list callee))
       (declare (ignore keyargs aok aux))
       (if key-flag
           (general-mv-local-call-vas callee-info mvargi oname outputrt)
           (direct-mv-local-call-vas
            mvargi callee-info req opt rest-var varest-p oname outputrt)))
     output)))

(defmethod translate-simple-instruction
    ((instruction cc-bmir:fixed-mv-local-call) abi)
  (declare (ignore abi))
  (let* ((output (bir:output instruction))
         (callee (bir:callee instruction))
         (mvarg (second (bir:inputs instruction)))
         (mvargrt (cc-bmir:rtype mvarg))
         (mvargi (in mvarg)))
    (assert (and (listp mvargrt)
                 (= (length mvargrt) (bir:nvalues instruction))))
    (out
     (gen-local-call callee (if (= (length mvargrt) 1)
                                (list mvargi)
                                mvargi)
                     (cc-bmir:rtype output))
     output)))

(defmethod translate-simple-instruction ((instruction bir:mv-call) abi)
  (declare (ignore abi))
  (let* ((fun (in (first (bir:inputs instruction))))
         (bargs (second (bir:inputs instruction)))
         (args-rtype (cc-bmir:rtype bargs))
         (args (in bargs))
         (output (bir:output instruction))
         (label (datum-name-as-string output)))
    (assert (eq args-rtype :vaslist))
    (out
     (cmp:irc-apply fun (cmp:irc-vaslist-nvals args)
                    (cmp:irc-vaslist-values args)
                    label)
     output)))

(defmethod translate-simple-instruction ((instruction cc-bmir:fixed-mv-call)
                                         abi)
  (declare (ignore abi))
  (let* ((fun (in (first (bir:inputs instruction))))
         (args (rest (bir:inputs instruction)))
         (rargs (loop for arg in args
                      for rt = (cc-bmir:rtype arg)
                      do (assert (listp rt))
                      when (= (length rt) 1)
                        collect (in arg)
                      else append (in arg)))
         (output (bir:output instruction))
         (label (datum-name-as-string output)))
    (assert (= (length rargs) (bir:nvalues instruction)))
    (out
     (closure-call-or-invoke fun rargs :label label)
     output)))

(defmethod translate-simple-instruction ((instruction cc-bir:mv-foreign-call)
                                         abi)
  (let ((output (bir:output instruction)))
    (out (unsafe-multiple-value-foreign-call
          (cc-bir:function-name instruction)
          (mapcar #'in (bir:inputs instruction)) abi
          :label (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction cc-bir:foreign-call-pointer) abi)
  (let ((inputs (bir:inputs instruction)))
    (out (clasp-cleavir:unsafe-foreign-call-pointer
          :call (cc-bir:foreign-types instruction) (in (first inputs))
          (mapcar #'in (rest inputs)) abi)
         (bir:output instruction))))

(defmethod translate-simple-instruction
    ((instruction cc-bir:defcallback) (abi abi-x86-64))
  (let* ((args (cc-bir:defcallback-args instruction))
         (closure (in (first (bir:inputs instruction)))))
    (cmp::gen-defcallback
     (first args) (second args) (third args) (fourth args)
     (fifth args) (sixth args) (seventh args) (eighth args)
     closure)))

(defmethod translate-simple-instruction
    ((instruction bir:fixed-to-multiple) (abi abi-x86-64))
  (let* ((inputs (bir:inputs instruction))
         (output (bir:output instruction))
         (outputrt (cc-bmir:rtype output))
         (ninputs (length inputs)))
    (cond ((null outputrt) ; e.g. unused, so ignore inputs
           (out nil output))
          (t
           (assert (equal (length outputrt) ninputs))
           (out (if (= ninputs 1) (in (first inputs)) (mapcar #'in inputs))
                output)))))

(defun %cast-to-mv (values)
  (cond ((null values) (cmp:irc-make-tmv (%size_t 0) (%nil)))
        (t
         (loop for i from 1 for v in (rest values)
               do (cmp:irc-store v (return-value-elt i)))
         (cmp:irc-make-tmv (%size_t (length values)) (first values)))))

(defgeneric cast-one (from to value)
  (:method (from to value)
    (if (eql from to)
        value
        (error "BUG: Don't know how to cast ~a ~a to ~a" from value to))))

(defmethod cast-one ((from (eql :single-float)) (to (eql :object)) value)
  (cmp:irc-box-single-float value))
(defmethod cast-one ((from (eql :object)) (to (eql :single-float)) value)
  (cmp:irc-unbox-single-float value))

(defmethod cast-one ((from (eql :double-float)) (to (eql :object)) value)
  (cmp:irc-box-double-float value))
(defmethod cast-one ((from (eql :object)) (to (eql :double-float)) value)
  (cmp:irc-unbox-double-float value))

(defmethod cast-one ((from (eql :fixnum)) (to (eql :object)) value)
  (cmp:irc-int-to-ptr value cmp:%t*%))
(defmethod cast-one ((from (eql :object)) (to (eql :fixnum)) value)
  (cmp:irc-ptr-to-int value cmp:%fixnum%))

(defmethod cast-one ((from (eql :object)) (to (eql :vaslist)) value)
  ;; We only generate these when we know for sure the input is a vaslist,
  ;; so we don't do checking.
  (cmp:irc-unbox-vaslist value))

(defun %cast-some (inputrt outputrt inputv)
  (let ((Lin (length inputrt)) (Lout (length outputrt))
        (pref (mapcar #'cast-one inputrt outputrt inputv)))
    (cond ((<= Lout Lin) pref)
          (t
           (assert (every (lambda (r) (eq r :object)) (subseq outputrt Lin)))
           (nconc pref (loop repeat (- Lout Lin) collect (%nil)))))))

(define-condition box-emitted (ext:compiler-note)
  ((%name :initarg :name :reader name)
   (%inputrt :initarg :inputrt :reader inputrt)
   (%outputrt :initarg :outputrt :reader outputrt))
  (:report (lambda (condition stream)
             (format stream "Emitting box~@[ of ~a~] from ~a to ~a"
                     (name condition)
                     (inputrt condition) (outputrt condition)))))

(defun notable-cast-p (inputrt outputrt)
  ;; Is this cast expensive enough to alert the optimization-focused
  ;; user? For now our answer is essentially that a cast is only
  ;; notable if it involves boxing.
  (flet ((box-from-p (ivrt)
           (member ivrt '(:single-float :double-float))))
    (and (listp inputrt)
         (if (listp outputrt)
             (some (lambda (i o)
                     (and (eq o :object) (box-from-p i)))
                   inputrt outputrt)
             (some #'box-from-p inputrt)))))

(defun maybe-note-box (policy name origin inputrt outputrt)
  (when (policy:policy-value policy 'note-boxing)
    (cmp:note 'box-emitted
              :inputrt inputrt :outputrt outputrt
              :name name :origin (origin-source origin))))

(defun translate-cast (inputv inputrt outputrt)
  ;; most of this is special casing crap due to 1-value values not being
  ;; passed around as lists.
  (cond ((eq inputrt :multiple-values)
         (cond ((eq outputrt :multiple-values)
                ;; A NOP like this isn't generated within code, but the
                ;; translate-cast in layout-xep can end up here.
                inputv)
               ((not (listp outputrt)) (error "BUG: Bad rtype ~a" outputrt))
               ((= (length outputrt) 1)
                (cast-one :object (first outputrt)
                          (cmp:irc-tmv-primary inputv)))
               ((null outputrt) nil)
               (t (cons (cast-one :object (first outputrt)
                                  (cmp:irc-tmv-primary inputv))
                        (loop for i from 1
                              for ort in (rest outputrt)
                              for val = (cmp:irc-t*-load (return-value-elt i))
                              collect (cast-one :object ort val))))))
        ((eq inputrt :vaslist)
         (cond ((eq outputrt :multiple-values)
                (%intrinsic-call "cc_load_values"
                                 (list (cmp:irc-vaslist-nvals inputv)
                                       (cmp:irc-vaslist-values inputv))))
               ((and (listp outputrt) (= (length outputrt) 1))
                (cast-one :object (first outputrt)
                          (cmp:irc-vaslist-nth (%size_t 0) inputv)))
               (t (error "BUG: Cast from ~a to ~a" inputrt outputrt))))
        ((not (listp inputrt)) (error "BUG: Bad rtype ~a" inputrt))
        ;; inputrt must be a list (fixed values)
        ((= (length inputrt) 1)
         (cond ((eq outputrt :multiple-values)
                (cmp:irc-make-tmv (%size_t 1)
                                  (cast-one (first inputrt) :object inputv)))
               ((not (listp outputrt))
                (error "BUG: Cast from ~a to ~a" inputrt outputrt))
               ((null outputrt) nil)
               ((= (length outputrt) 1)
                (cast-one (first inputrt) (first outputrt) inputv))
               (t ;; pad with nil
                (assert (every (lambda (r) (eq r :object)) (rest outputrt)))
                (cons (cast-one (first inputrt) (first outputrt) inputv)
                      (loop repeat (length (rest outputrt))
                            collect (%nil))))))
        (t
         (cond ((eq outputrt :multiple-values)
                (%cast-to-mv
                 (loop for inv in inputv for irt in inputrt
                       collect (cast-one irt :object inv))))
               ((not (listp outputrt))
                (error "BUG: Cast from ~a to ~a" inputrt outputrt))
               ((= (length outputrt) 1)
                (cond ((null inputrt)
                       (ecase (first outputrt)
                         ((:object) (%nil))
                         ;; We can end up here with a variety of output vrtypes
                         ;; in some unusual situations where a primop expects
                         ;; a value, but control will never actually reach it.
                         ;; Ideally the compiler would not bother compiling
                         ;; such unreachable code, but sometimes it's stupid.
                         ((:fixnum) (llvm-sys:undef-value-get cmp:%fixnum%))
                         ((:single-float)
                          (llvm-sys:undef-value-get cmp:%float%))
                         ((:double-float)
                          (llvm-sys:undef-value-get cmp:%double%))))
                      (t
                       (cast-one (first inputrt) (first outputrt)
                                 (first inputv)))))
               (t (%cast-some inputrt outputrt inputv))))))

(defmethod translate-simple-instruction ((instr cc-bmir:cast) (abi abi-x86-64))
  (let* ((input (bir:input instr)) (inputrt (cc-bmir:rtype input))
         (output (bir:output instr)) (outputrt (cc-bmir:rtype output)))
    (maybe-note-box (bir:policy instr)
                    (or (bir:name output) (bir:name input))
                    (bir:origin instr) inputrt outputrt)
    (out (translate-cast (in input) inputrt outputrt) output)))

(defmethod translate-simple-instruction ((inst cc-blir:memref2) abi)
  (declare (ignore abi))
  (out (cmp::gen-memref-address (in (first (bir:inputs inst)))
                                (cc-blir:offset inst))
       (bir:output inst)))

(defmethod translate-simple-instruction ((inst cc-blir:load) abi)
  (declare (ignore abi))
  (out (cmp:irc-t*-load-atomic (in (first (bir:inputs inst)))
                            :order (cmp::order-spec->order (cc-bir:order inst))
                            :label (datum-name-as-string
                                    (bir:output inst)))
       (bir:output inst)))

(defmethod translate-simple-instruction ((inst cc-blir:store) abi)
  (declare (ignore abi))
  (cmp:irc-store-atomic
   (in (first (bir:inputs inst)))
   (in (second (bir:inputs inst)))
   :order (cmp::order-spec->order (cc-bir:order inst))))

(defmethod translate-simple-instruction ((inst cc-bir:fence) abi)
  (declare (ignore abi))
  (cmp::gen-fence (cc-bir:order inst)))

(defmethod translate-simple-instruction ((inst cc-blir:cas) abi)
  (declare (ignore abi))
  (out (cmp:irc-cmpxchg (in (first (bir:inputs inst)))
                        (in (second (bir:inputs inst)))
                        (in (third (bir:inputs inst)))
                        :order (cmp::order-spec->order (cc-bir:order inst))
                        :label (datum-name-as-string (bir:output inst)))
       (bir:output inst)))

(defmethod translate-simple-instruction ((inst cc-vaslist:values-list) abi)
  (declare (ignore abi))
  ;; This is just a change in rtype, from (:vaslist) to :vaslist,
  ;; so it's really a nop.
  (out (in (bir:input inst)) (bir:output inst)))
(defmethod translate-simple-instruction ((inst cc-vaslist:nth) abi)
  (declare (ignore abi))
  (let* ((inputs (bir:inputs inst))
         (index (in (first inputs))) (vaslist (in (second inputs)))
         (uindex (cmp:irc-untag-fixnum index cmp:%fixnum%))
         (output (bir:output inst))
         (label (datum-name-as-string output)))
    (out (cmp:irc-vaslist-nth uindex vaslist label) output)))
(defmethod translate-simple-instruction ((inst cc-vaslist:nthcdr) abi)
  (declare (ignore abi))
  (let* ((inputs (bir:inputs inst))
         (index (in (first inputs))) (vaslist (in (second inputs)))
         (uindex (cmp:irc-untag-fixnum index cmp:%fixnum%))
         (output (bir:output inst))
         (label (datum-name-as-string output)))
    (out (cmp:irc-vaslist-nthcdr uindex vaslist label) output)))
(defmethod translate-simple-instruction ((inst cc-vaslist:last) abi)
  (declare (ignore abi))
  (let* ((inputs (bir:inputs inst))
         (index (in (first inputs))) (vaslist (in (second inputs)))
         (uindex (cmp:irc-untag-fixnum index cmp:%fixnum%))
         (output (bir:output inst))
         (label (datum-name-as-string output)))
    (out (cmp:irc-vaslist-last uindex vaslist label) output)))
(defmethod translate-simple-instruction ((inst cc-vaslist:butlast) abi)
  (declare (ignore abi))
  (let* ((inputs (bir:inputs inst))
         (index (in (first inputs))) (vaslist (in (second inputs)))
         (uindex (cmp:irc-untag-fixnum index cmp:%fixnum%))
         (output (bir:output inst))
         (label (datum-name-as-string output)))
    (out (cmp:irc-vaslist-butlast uindex vaslist label) output)))
(defmethod translate-simple-instruction ((inst cc-vaslist:length) abi)
  (declare (ignore abi))
  (let* ((vaslist (in (bir:input inst)))
         (untagged-length (cmp:irc-vaslist-nvals vaslist))
         (fix (cmp:irc-tag-fixnum untagged-length "length")))
    (out fix (bir:output inst))))

(defmethod translate-simple-instruction ((inst bir:primop) abi)
  (declare (ignore abi))
  (translate-primop (cleavir-primop-info:name (bir:info inst)) inst))

(defmethod translate-primop ((name (eql 'symbol-value)) inst)
  (out (%intrinsic-invoke-if-landing-pad-or-call
        "cc_safe_symbol_value" (list (in (first (bir:inputs inst))))
        (datum-name-as-string (first (bir:outputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'fdefinition)) inst)
  (let ((symbol (in (first (bir:inputs inst))))
        (out (first (bir:outputs inst))))
    (out (cmp:irc-fdefinition symbol (datum-name-as-string out)) out)))
(defmethod translate-primop ((name (eql 'cc-bir::setf-fdefinition)) inst)
  (let ((setf-symbol (in (first (bir:inputs inst))))
        (outp (first (bir:outputs inst))))
    (out (cmp:irc-setf-fdefinition setf-symbol (datum-name-as-string outp))
         outp)))
(defmethod translate-primop ((name (eql 'core::vector-length)) inst)
  (out (cmp::gen-vector-length (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::%displacement)) inst)
  (out (cmp:irc-real-array-displacement (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::%displaced-index-offset)) inst)
  (out
   (cmp:irc-tag-fixnum
    (cmp:irc-real-array-index-offset (in (first (bir:inputs inst))))
    (datum-name-as-string (first (bir:outputs inst))))
   (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::%array-total-size)) inst)
  (out (cmp:irc-tag-fixnum
        (cmp:irc-array-total-size (in (first (bir:inputs inst))))
        (datum-name-as-string (first (bir:outputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::%array-rank)) inst)
  (out (cmp:irc-tag-fixnum
        (cmp:irc-array-rank (in (first (bir:inputs inst))))
        (datum-name-as-string (first (bir:outputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::%array-dimension)) inst)
  (out (cmp:gen-%array-dimension (in (first (bir:inputs inst)))
                                 (in (second (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'cleavir-primop:slot-read)) inst)
  (out (cmp::gen-instance-ref (in (first (bir:inputs inst)))
                              (in (second (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'cleavir-primop:slot-write)) inst)
  (let ((inputs (bir:inputs inst)))
    (cmp::gen-instance-set (in (first inputs)) (in (second inputs))
                           (in (third inputs)))))
(defmethod translate-primop ((name (eql 'core::instance-cas)) inst)
  (let ((inputs (bir:inputs inst)))
    (out (cmp::gen-instance-cas (in (third inputs)) (in (fourth inputs))
                                (in (first inputs)) (in (second inputs)))
         (first (bir:outputs inst)))))
(defmethod translate-primop ((name (eql 'core:vaslist-pop)) inst)
  (out (cmp:gen-vaslist-pop (let ((vaslist-tagged (in (first (bir:inputs inst)))))
                              (cmp:irc-untag-vaslist vaslist-tagged)))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core:vaslist-length)) inst)
  (out (cmp:gen-vaslist-length (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::header-stamp)) inst)
  (out (cmp:irc-header-stamp (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::rack-stamp)) inst)
  (out (cmp:irc-rack-stamp (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::wrapped-stamp)) inst)
  (out (cmp:irc-wrapped-stamp (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core::derivable-stamp)) inst)
  (out (cmp:irc-derivable-stamp (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core:instance-rack)) inst)
  (out (cmp:gen-instance-rack (in (first (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core:instance-rack-set)) inst)
  (cmp:gen-instance-rack-set (in (first (bir:inputs inst)))
                             (in (second (bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:rack-ref)) inst)
  (out (cmp:gen-rack-ref (in (first (bir:inputs inst)))
                         (in (second (bir:inputs inst))))
       (first (bir:outputs inst))))
(defmethod translate-primop ((name (eql 'core:rack-set)) inst)
  (cmp:gen-rack-set (in (first (bir:inputs inst)))
                    (in (second (bir:inputs inst)))
                    (in (third (bir:inputs inst)))))

(defmethod translate-primop ((name cons) inst) ; FIXME
  (cond ((equal name '(setf symbol-value))
         (%intrinsic-invoke-if-landing-pad-or-call
          "cc_setSymbolValue" (mapcar #'in (bir:inputs inst))))
        (t
         (error "BUG: Don't know how to translate primop ~a" name))))

(defmethod translate-simple-instruction ((inst cc-bir:atomic-rack-read) abi)
  (declare (ignore abi))
  (out (cmp:gen-rack-ref (in (first (bir:inputs inst)))
                         (in (second (bir:inputs inst)))
                         :order (cmp::order-spec->order (cc-bir:order inst)))
       (bir:output inst)))
(defmethod translate-simple-instruction ((inst cc-bir:atomic-rack-write) abi)
  (declare (ignore abi))
  (cmp:gen-rack-set (in (second (bir:inputs inst)))
                    (in (third (bir:inputs inst)))
                    (in (first (bir:inputs inst)))
                    :order (cmp::order-spec->order (cc-bir:order inst))))
(defmethod translate-simple-instruction ((inst cc-bir:cas-rack) abi)
  (declare (ignore abi))
  (out (cmp:irc-cmpxchg (cmp::irc-rack-slot-address
                         (in (third (bir:inputs inst)))
                         (cmp:irc-untag-fixnum
                          (in (fourth (bir:inputs inst)))
                          cmp:%size_t% "slot-location"))
                        (in (first (bir:inputs inst)))
                        (in (second (bir:inputs inst)))
                        :order (cmp::order-spec->order (cc-bir:order inst))
                        :label (datum-name-as-string (bir:output inst)))
       (bir:output inst)))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((vtype (cmp::simple-vector-llvm-type element-type))
         (type (llvm-sys:type-get-pointer-to vtype))
         (cast (cmp:irc-bit-cast array type))
         (untagged (cmp:irc-untag-fixnum index fixnum-type "vector-index")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; For layout of the vector, check simple-vector-llvm-type's definition.
    ;; untagged is the actual offset.
    (cmp:irc-typed-gep-variable vtype
                          cast
                          (list (%i32 0) (%i32 cmp::+simple-vector-data-slot+) untagged)
                          "aref")))

(defmethod translate-simple-instruction ((inst cc-bir:vref) abi)
  (let ((inputs (bir:inputs inst)))
    (out (cmp:irc-t*-load-atomic
          (gen-vector-effective-address
           (in (first inputs)) (in (second inputs)) (cc-bir:element-type inst)
           (%default-int-type abi))
          :order (cmp::order-spec->order (cc-bir:order inst)))
         (bir:output inst))))
(defmethod translate-simple-instruction ((inst cc-bir:vset) abi)
  (let ((inputs (bir:inputs inst)))
    (cmp:irc-store-atomic
     (in (first inputs))
     (gen-vector-effective-address
      (in (second inputs)) (in (third inputs)) (cc-bir:element-type inst)
      (%default-int-type abi))
     :order (cmp::order-spec->order (cc-bir:order inst)))))
(defmethod translate-simple-instruction ((inst cc-bir:vcas) abi)
  (let ((et (cc-bir:element-type inst))
        (inputs (bir:inputs inst)))
    (out (cmp:irc-cmpxchg
          ;; This will err if et = bit or the like.
          (gen-vector-effective-address
           (in (third inputs)) (in (fourth inputs)) et
           (%default-int-type abi))
          (in (first inputs)) (in (second inputs)))
         (bir:output inst))))

(defmethod translate-simple-instruction ((inst cc-bmir:mtf) abi)
  (declare (ignore abi))
  (let* ((input (bir:input inst)) (output (bir:output inst))
         (irt (cc-bmir:rtype input)) (ort (cc-bmir:rtype output)))
    (out
     (cond ((listp irt)
            (cond (ort ; nop
                   (assert (equal irt ort))
                   (in input))
                  ;; unused
                  (t nil)))
           ((eq irt :multiple-values)
            (let ((nvalues (bir:nvalues inst)))
              (case nvalues
                ((0) nil)
                ((1) (cmp:irc-tmv-primary (in input)))
                (t (let ((inp (in input)))
                     (cons (cmp:irc-tmv-primary inp)
                           (loop for i from 1 below nvalues
                                 collect (cmp:irc-t*-load
                                          (return-value-elt i)))))))))
           ((eq irt :vaslist)
            (let ((ls
                    (loop with vals = (cmp:irc-vaslist-values (in input))
                          for i below (bir:nvalues inst)
                          for ptr = (cmp:irc-typed-gep cmp:%t*% vals (list i))
                          collect (cmp:irc-t*-load ptr))))
              (if (= (bir:nvalues inst) 1)
                  (first ls)
                  ls)))
           (t (error "BUG: Bad rtype ~a" irt)))
     output)))

(defmethod translate-simple-instruction ((inst bir:values-restore) abi)
  (declare (ignore abi))
  (let ((input (bir:input inst))
        (output (bir:output inst)))
    (out
     (if (listp (cc-bmir:rtype output))
         ;; Totally fixed values; we just alias.
         (in input)
         ;; Now output rtype must be :multiple-values.
         (let* ((in (in input))
                (irt (cc-bmir:rtype input)))
           (cond ((eq irt :vaslist)
                  (%intrinsic-call "cc_load_values"
                                   (list
                                    (cmp:irc-vaslist-nvals in)
                                    (cmp:irc-vaslist-values in))))
                 ((listp irt)
                  (let* ((lirt (length irt)))
                    ;; FIXME: In safe code, we might want to check that the
                    ;; values count is correct,
                    ;; if the type tests do not do this already.
                    (case lirt
                      ((0) (cmp:irc-make-tmv (%size_t 0) (%nil)))
                      ((1) (cmp:irc-make-tmv (%size_t 1) in))
                      (otherwise
                       (loop for i from 1
                             for idat in (rest in)
                             do (cmp:irc-store idat (return-value-elt i)))
                       (cmp:irc-make-tmv (%size_t lirt) (first in))))))
                 (t (error "BUG: Bad rtype ~a" irt)))))
     output)))

(defun values-collect-multi-vas (inst)
  ;; First, assert that there's only one input that isn't a values-save.
  (loop with seen-non-save = nil
        for input in (bir:inputs inst)
        for rt = (cc-bmir:rtype input)
        if (not (or (eq rt :vaslist) (listp rt)))
          do (if seen-non-save
                 (error "BUG: Can only have one variable non-save-values input, but saw ~a and ~a!" inst seen-non-save)
                 (setf seen-non-save inst)))
  (let* ((liven nil)          ; index for the :variable storage.
         ;; Collect the form of each input.
         ;; Each datum is (symbol nvalues extra).
         ;; For saved values, the extra is the storage for it. For the current
         ;; values the extra is the primary value (since it's stored
         ;; separately)
         (data (loop for idx from 0
                     for input in (bir:inputs inst)
                     for in = (in input)
                     for irt = (cc-bmir:rtype input)
                     collect (cond ((eq irt :vaslist)
                                    (list :saved
                                          (cmp:irc-vaslist-nvals
                                           in "nret-saved")
                                          (cmp:irc-vaslist-values
                                           in "values-saved")))
                                   ((listp irt)
                                    (let ((len (length irt)))
                                      (list :fixed
                                            (%size_t len)
                                            (list* len (if (= len 1)
                                                           (list in)
                                                           in)))))
                                   ((eq irt :multiple-values)
                                    (setf liven idx)
                                    (list :variable
                                          (cmp:irc-tmv-nret in "nret-variable")
                                          (cmp:irc-tmv-primary in)))
                                   (t (error "BUG: Bad rtype ~a" irt)))))
         ;; Collect partial sums of the number of values.
         (partial-sums
           (loop for (_1 size _2) in data
                 for n = size then (cmp:irc-add n size "sum-nret")
                 collect n))
         (n-total-values (first (last partial-sums)))
         (stacksave (%intrinsic-call "llvm.stacksave" nil "values-collect"))
         (tv-size
           ;; In order to store the primary value unconditionally below, we
           ;; need to allocate one extra word. This is important for the
           ;; situation (mv-call ... (foo)) where FOO returns no values.
           ;; Without this extra word, the primary could be written to just
           ;; beyond the allocated memory, which would cause problems.
           (if liven
               (cmp:irc-add n-total-values (%size_t 1))
               n-total-values))
         ;; LLVM type is t**, i.e. this is a pointer to the 0th value.
         (valvec (cmp:alloca-temp-values tv-size "values-collect-temp")))
    (setf (dynenv-storage inst) stacksave)
    ;; Generate code to copy all the values into the temp storage.
    ;; First we need to move any live values, since otherwise they could be
    ;; overridden, unless they're at zero position since then they're already
    ;; in place. (in practice, this never happens right now, but maybe later?)
    (when (and liven (not (zerop liven)))
      (destructuring-bind (size primary) (rest (nth liven data))
        (let* ((spos (nth (1- liven) partial-sums))
               ;; LLVM type is t**, i.e. this is a pointer to the 0th value.
               (mvalues (%gep cmp:%t*[0]% (multiple-value-array-address)
                              '(0 0) "multiple-values"))
               (sdest (cmp:irc-typed-gep-variable cmp:%t*% valvec (list spos) "var-dest"))
               ;; Add one, since we store the primary separately
               (dest (%gep cmp:%t*% sdest '(1) "var-dest-subsequent"))
               (source (%gep cmp:%t*% mvalues '(1) "var-source-subsequent"))
               ;; Number of elements to copy out of the values vector.
               ;; This is a bit tricky, in that we want to copy nvalues-1,
               ;; unless nvalues is zero in which case we want zero.
               ;; Therefore we use umax to turn a 0 into 1.
               ;; We could alternately branch, but that's probably slower,
               ;; and definitely more of a pain to generate.
               (adjusted-nvalues
                 (%intrinsic-call "llvm.umax.i64"
                                  (list size (%i64 1))
                                  "adjusted-nret-variable"))
               (ncopy (cmp:irc-sub adjusted-nvalues (%size_t 1) "ntocopy")))
          ;; Copy the rest
          (%intrinsic-call "llvm.memcpy.p0i8.p0i8.i64"
                           (list (cmp:irc-bit-cast dest cmp:%i8*%
                                                   "var-dest-subsequent")
                                 ;; read from the 1st value of the mv vector
                                 (cmp:irc-bit-cast source cmp:%i8*%
                                                   "var-source-subsequent")
                                 ;; Multiply size by sizeof(T_O*)
                                 ;; (subtract one for the primary, again)
                                 (cmp::irc-shl ncopy 3 :nuw t
                                                       :label "real-ntocopy")
                                 ;; non volatile
                                 (%i1 0)))
          ;; Store the primary
          (cmp:irc-store primary sdest))))
    ;; Now copy the rest
    (loop for (key size extra) in data
          for startn = (%size_t 0) then finishn
          for finishn in partial-sums
          for dest = (cmp:irc-typed-gep-variable cmp:%t*% valvec (list startn) "dest")
          do (ecase key
               ((:saved)
                (%intrinsic-call "llvm.memcpy.p0i8.p0i8.i64"
                                 (list (cmp:irc-bit-cast dest cmp:%i8*% "dest")
                                       (cmp:irc-bit-cast extra cmp:%i8*%
                                                         "source")
                                       ;; Multiply by sizeof(T_O*)
                                       (cmp::irc-shl size 3 :nuw t
                                                            :label "real-ntocopy")
                                       (%i1 0))))
               ((:fixed)
                (loop for i below (first extra) ; size
                      for v in (rest extra)
                      do (cmp:irc-store v (%gep cmp:%t*% dest (list i) "fixed-dest"))))
               ((:variable))))          ; done already
    ;; Now just return a T_mv. We load the primary from the vector again, which
    ;; is technically slightly inefficient.
    (cmp:irc-make-vaslist n-total-values valvec "values-collected")))

(defmethod translate-terminator ((inst bir:values-collect) abi next)
  (declare (ignore abi))
  (let ((output (bir:output inst)))
    (assert (eq (cc-bmir:rtype output) :vaslist))
    (out
     (cond ((= (length (bir:inputs inst)) 1)
            (let* ((inp (first (bir:inputs inst)))
                   (in (in inp))
                   (irt (cc-bmir:rtype inp)))
              (cond ((eq irt :vaslist)
                     (setf (dynenv-storage inst) nil)
                     in)
                    ((eq irt :multiple-values)
                     (let* ((nret (cmp:irc-tmv-nret in))
                            (save (%intrinsic-call "llvm.stacksave" nil))
                            (values (cmp:alloca-temp-values nret)))
                       (setf (dynenv-storage inst) save)
                       (%intrinsic-call "cc_save_values"
                                        (list
                                         nret
                                         (cmp:irc-tmv-primary in)
                                         values))
                       (cmp:irc-make-vaslist nret values)))
                    ;; Fixed values would have been lowered away in
                    ;; insert-casts.
                    (t (error "BUG: Bad rtype ~a" irt)))))
           (t ; hard case
            (values-collect-multi-vas inst)))
     output))
  (cmp:irc-br (first next)))

(defmethod translate-simple-instruction ((inst cc-bmir:append-values) abi)
  (declare (ignore abi))
  (out
   (loop for inp in (bir:inputs inst)
         for rt = (cc-bmir:rtype inp)
         do (assert (listp rt))
         if (= (length rt) 1)
           collect (in inp) into result
         else
           append (in inp) into result
         finally (return (if (= (length result) 1)
                             (first result)
                             result)))
   (bir:output inst)))

(defmethod translate-simple-instruction
    ((inst bir:load-time-value-reference) abi)
  (declare (ignore abi))
  (out (let* ((ltv (first (bir:inputs inst)))
              (index (gethash ltv *constant-values*))
              (label (datum-name-as-string (bir:output inst))))
         (cmp:irc-t*-load (%indexed-literal-ref index label)))
       (bir:output inst)))

(defmethod translate-simple-instruction ((inst bir:constant-reference)
                                         abi)
  (declare (ignore abi))
  (out (let* ((constant (first (bir:inputs inst)))
              ;; NOTE: Printing out the constant for a label is problematic,
              ;; because LLVM will reject (assert failure) if a label has
              ;; any null bytes in it. Null bytes can arise in non-obvious
              ;; ways, e.g. from non-ASCII Unicode characters.
              (label "")
              (immediate-or-index (gethash constant *constant-values*)))
         (assert immediate-or-index () "Constant not found!")
         (if (integerp immediate-or-index)
             (multiple-value-bind (literals literals-type)
                 (literal:ltv-global)
               (cmp:irc-t*-load
                (cmp:irc-typed-gep-variable literals-type
                                      literals
                                      (list (%size_t 0)
                                            (%i64 immediate-or-index))
                                      label)
                label))
             immediate-or-index))
       (bir:output inst)))

(defmethod translate-simple-instruction
    ((inst cc-bmir:unboxed-constant-reference) abi)
  (declare (ignore abi))
  (let ((val (bir:constant-value inst))
        (out (bir:output inst)))
    (out (ecase (first (cc-bmir:rtype out))
           ((:single-float)
            (llvm-sys:constant-fp-get-type-double cmp:%float% val))
           ((:double-float)
            (llvm-sys:constant-fp-get-type-double cmp:%double% val)))
         out)))

(defun initialize-iblock-translation (iblock)
  (let ((phis (bir:inputs iblock)))
    (unless (null phis)
      (cmp:irc-begin-block (iblock-tag iblock))
      (let ((ndefinitions (+ (cleavir-set:size (bir:predecessors iblock))
                             (cleavir-set:size (bir:entrances iblock)))))
        (loop for phi in phis
              for rt = (cc-bmir:rtype phi)
              for dat
                = (cond ((eq rt :multiple-values)
                         (cmp:irc-phi cmp::%tmv% ndefinitions))
                        ((eq rt :vaslist)
                         (cmp:irc-phi cmp:%vaslist% ndefinitions))
                        ((and (listp rt) (= (length rt) 1))
                         (cmp:irc-phi (vrtype->llvm (first rt))
                                      ndefinitions))
                        ((listp rt)
                         (loop for vrt in rt
                               for ll = (vrtype->llvm vrt)
                               collect (cmp:irc-phi ll ndefinitions)))
                        (t (error "BUG: Bad rtype ~a" rt)))
              do (setf (gethash phi *datum-values*) dat))))))

(defun layout-iblock (iblock abi)
  (cmp:irc-begin-block (iblock-tag iblock))
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         (bir:dynamic-environment iblock) *tags*)
    (let ((*enclose-initializers* '()))
      (loop with end = (bir:end iblock)
            for instruction = (bir:start iblock)
              then (bir:successor instruction)
            until (eq instruction end)
            do (translate-simple-instruction instruction abi)
            finally (progn
                      (force-initializers)
                      (translate-terminator
                       instruction abi
                       (mapcar #'iblock-tag (bir:next end))))))))

(defun function-source-pos-info (irfunction)
  (let ((origin (bir:origin irfunction)))
    (ensure-origin (origin-spi (origin-source origin)) 999909)))

(defun calculate-function-info (irfunction lambda-name)
  (let* ((origin (origin-source (bir:origin irfunction)))
         (spi (origin-spi origin)))
    (let ((cleavir-lambda-list-analysis (cmp:calculate-cleavir-lambda-list-analysis (bir:lambda-list irfunction))))
      (cmp:make-function-info
       :function-name lambda-name
       :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
       :lambda-list (bir:original-lambda-list irfunction)
       :docstring (bir:docstring irfunction)
       :declares nil
       :spi spi))))

(defun iblock-name (iblock)
  (let ((name (bir:name iblock)))
    (if name
        (string-downcase (symbol-name name))
        "iblock")))

(defun local-call-rv->inputs (llvm-value rtype)
  (cond ((eq rtype :multiple-values) llvm-value)
        ((eq rtype :vaslist) llvm-value)
        ((not (listp rtype)) (error "BUG: Bad rtype ~a" rtype))
        ((null rtype) nil)
        ((null (rest rtype)) llvm-value)
        ((cc-bir-to-bmir::too-big-return-rtype-p rtype)
         (translate-cast llvm-value :multiple-values rtype))
        (t (loop for i from 0 below (length rtype)
                 collect (cmp:irc-extract-value llvm-value (list i))))))

(defun layout-xep-function* (xep-group arity the-function ir calling-convention abi)
  (declare (ignore abi))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
      ;; Parse lambda list.
    (let ((ret (cmp:compile-lambda-list-code (cmp:xep-group-cleavir-lambda-list-analysis xep-group)
                                             calling-convention
                                             arity
                                             :argument-out #'out)))
      (unless ret
        (error "cmp:compile-lambda-list-code returned NIL which means this is not a function that should be generated")))
    ;; Import cells.
    (let* ((closure-vec (first (llvm-sys:get-argument-list the-function)))
           (llvm-function-info (find-llvm-function-info ir))
           (environment-values
             (loop for import in (environment llvm-function-info)
                   for i from 0
                   for offset = (cmp:%closure-with-slots%.offset-of[n]/t* i)
                   collect (cmp:irc-t*-load-atomic
                            (cmp::gen-memref-address closure-vec offset))))
           (source-pos-info (function-source-pos-info ir)))
      ;; Tail call the real function.
      (cmp:with-debug-info-source-position (source-pos-info)
        (let* ((function-type (llvm-sys:get-function-type (main-function llvm-function-info)))
               (arguments
                 (mapcar (lambda (arg)
                           (translate-cast (in arg)
                                           '(:object) (cc-bmir:rtype arg)))
                         (arguments llvm-function-info)))
               (c
                 (cmp:irc-create-call-wft
                  function-type
                  (main-function llvm-function-info)
                  ;; Augment the environment lexicals as a local call would.
                  (nconc environment-values arguments)))
               (returni (bir:returni ir))
               (rrtype (and returni (cc-bmir:rtype (bir:input returni)))))
          #+(or)(llvm-sys:set-calling-conv c 'llvm-sys:fastcc)
          ;; Box/etc. results of the local call.
          (if returni
              (cmp:irc-ret (translate-cast
                            (local-call-rv->inputs c rrtype)
                            rrtype :multiple-values))
              (cmp:irc-unreachable))))))
  the-function)

(defun layout-main-function* (the-function ir
                              body-irbuilder body-block
                              abi &key (linkage 'llvm-sys:internal-linkage))
  (declare (ignore linkage))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (cmp:with-irbuilder (body-irbuilder)
      (with-catch-pad-prep
          (cmp:irc-begin-block body-block)
        (cmp:with-landing-pad (never-entry-landing-pad ir)
          ;; Bind the arguments and the environment values
          ;; appropriately.
          (let ((llvm-function-info (find-llvm-function-info ir)))
            (loop for arg in (llvm-sys:get-argument-list the-function)
                  for lexical in (append (environment llvm-function-info)
                                         (arguments llvm-function-info))
                  do (setf (gethash lexical *datum-values*) arg)))
          ;; Branch to the start block.
          (cmp:irc-br (iblock-tag (bir:start ir)))
          ;; Lay out blocks.
          (bir:do-iblocks (ib ir)
            (layout-iblock ib abi))))))
  ;; Finish up by jumping from the entry block to the body block
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (cmp:irc-br body-block))
  the-function)

(defun layout-main-function (function lambda-name abi
                             &aux (linkage 'llvm-sys:internal-linkage)) ; llvm-sys:private-linkage
  (let* ((*tags* (make-hash-table :test #'eq))
         (*datum-values* (make-hash-table :test #'eq))
         (*dynenv-storage* (make-hash-table :test #'eq))
         (jit-function-name (cmp:jit-function-name lambda-name))
         (cmp:*current-function-name* jit-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string jit-function-name "fn-name"))
         (llvm-function-info (find-llvm-function-info function))
         (the-function (main-function llvm-function-info))
         (llvm-function-type (llvm-sys:get-function-type the-function))
         #+(or)(function-description (main-function-description llvm-function-info))
         (cmp:*current-function* the-function)
         (entry-block (cmp:irc-basic-block-create "entry" the-function))
         (*function-current-multiple-value-array-address*
           nil)
         (cmp:*irbuilder-function-alloca*
           (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
         (body-irbuilder (llvm-sys:make-irbuilder
                          (cmp:thread-local-llvm-context)))
         (body-block (cmp:irc-basic-block-create "body"))
         (source-pos-info (function-source-pos-info function))
         (lineno (core:source-pos-info-lineno source-pos-info)))
    (cmp:with-guaranteed-*current-source-pos-info* ()
      (cmp:with-dbg-function (:lineno lineno
                              :function-type llvm-function-type
                              :function the-function)
        #+(or)(llvm-sys:set-calling-conv the-function 'llvm-sys:fastcc)
        (llvm-sys:set-personality-fn the-function
                                     (cmp:irc-personality-function))
        (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-uwtable)
        (when (null (bir:returni function))
          (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-no-return))
        (unless (policy:policy-value (bir:policy function)
                                             'perform-optimization)
          (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-no-inline)
          (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-optimize-none))
        (cmp:with-irbuilder (body-irbuilder)
          (bir:map-iblocks
           (lambda (ib)
             (setf (gethash ib *tags*)
                   (cmp:irc-basic-block-create
                    (iblock-name ib)))
             (initialize-iblock-translation ib))
           function))
        (cmp:irc-set-insert-point-basic-block entry-block
                                              cmp:*irbuilder-function-alloca*)
        (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
          (cmp:with-debug-info-source-position (source-pos-info)
            (cmp:with-dbg-lexical-block
                (:lineno (core:source-pos-info-lineno source-pos-info))
              (layout-main-function* the-function function
                                     body-irbuilder body-block
                                     abi :linkage linkage))))))))

(defun compute-rest-alloc (cleavir-lambda-list-analysis)
  ;; FIXME: We seriously need to not reparse lambda lists a million times
  (let ((rest-var (cmp:cleavir-lambda-list-analysis-rest cleavir-lambda-list-analysis)))
    (cond ((not rest-var) nil)      ; don't care
          ((bir:unused-p rest-var) 'ignore)
          #+(or)
          ((eq (cc-bmir:rtype rest-var) :vaslist) :vaslist)
          ;; TODO: Dynamic extent?
          (t nil))))

(defun layout-xep-function (xep-arity xep-group function lambda-name abi)
  (let* ((*datum-values* (make-hash-table :test #'eq))
         (jit-function-name (cmp:jit-function-name lambda-name))
         (cmp:*current-function-name* jit-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string jit-function-name "fn-name")))
    (let* ((arity (cmp:xep-arity-arity xep-arity))
           (xep-arity-function (cmp:xep-arity-function-or-placeholder xep-arity)))
      (if (literal:general-entry-placeholder-p xep-arity-function)
          (progn
            )
          (progn
            (let* ((llvm-function-type (cmp:fn-prototype arity))
                   (cmp:*current-function* xep-arity-function)
                   (entry-block (cmp:irc-basic-block-create "entry" xep-arity-function))
                   (*function-current-multiple-value-array-address* nil)
                   (cmp:*irbuilder-function-alloca*
                     (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
                   (source-pos-info (function-source-pos-info function))
                   (lineno (core:source-pos-info-lineno source-pos-info)))
              (cmp:with-guaranteed-*current-source-pos-info* ()
                (cmp:with-dbg-function (:lineno lineno
                                        :function-type llvm-function-type
                                        :function xep-arity-function)
                  (llvm-sys:set-personality-fn xep-arity-function
                                               (cmp:irc-personality-function))
                  (llvm-sys:add-fn-attr xep-arity-function 'llvm-sys:attribute-uwtable)
                  (when (null (bir:returni function))
                    (llvm-sys:add-fn-attr xep-arity-function
                                          'llvm-sys:attribute-no-return))
                  (unless (policy:policy-value (bir:policy function)
                                                       'perform-optimization)
                    (llvm-sys:add-fn-attr xep-arity-function 'llvm-sys:attribute-no-inline)
                    (llvm-sys:add-fn-attr xep-arity-function 'llvm-sys:attribute-optimize-none))
                  (cmp:irc-set-insert-point-basic-block entry-block
                                                        cmp:*irbuilder-function-alloca*)
                  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                    (cmp:with-debug-info-source-position (source-pos-info)
                      (let* ((cleavir-lambda-list-analysis (cmp:xep-group-cleavir-lambda-list-analysis xep-group))
                             (calling-convention
                               (cmp:setup-calling-convention xep-arity-function
                                                             arity
                                                             :debug-on
                                                             (policy:policy-value
                                                              (bir:policy function)
                                                              'save-register-args)
                                                             :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
                                                             :rest-alloc (compute-rest-alloc cleavir-lambda-list-analysis))))
                        (layout-xep-function* xep-group arity xep-arity-function function calling-convention abi))))))))))))




(defun maybe-note-return-cast (function)
  (let ((returni (bir:returni function)))
    (when returni
      (let* ((inp (bir:input returni))
             (inrt (cc-bmir:rtype inp))
             (name (or (bir:name inp)
                       (format nil "<values returned from ~a>"
                               (bir:name function))))
             (policy (bir:policy function)))
        (maybe-note-box policy name (bir:origin function)
                        inrt :multiple-values)))))

(defun layout-xep-group (function lambda-name abi)
  ;; This goes way up here because we want it only noted once, not
  ;; once for each arity we happen to emit.
  (maybe-note-return-cast function)
  (let* ((llvm-function-info (find-llvm-function-info function))
         (xep-group (xep-function llvm-function-info)))
    (dolist (xep-arity (cmp:xep-group-arities xep-group))
      (layout-xep-function xep-arity xep-group function lambda-name abi))))

(defun layout-procedure (function lambda-name abi
                         &key (linkage 'llvm-sys:internal-linkage))
  (declare (ignore linkage))
  (when (xep-needed-p function)
    (layout-xep-group function lambda-name abi))
  (layout-main-function function lambda-name abi))

(defun get-or-create-lambda-name (bir)
  (or (bir:name bir) 'top-level))

;;; Given a BIR module, allocate its constants and load time
;;; values. We translate immediates directly, and use an index into
;;; the literal table for non-immediate constants.
(defun allocate-module-constants (module)
  (cleavir-set:doset (constant (bir:constants module))
    (let* ((value (bir:constant-value constant))
           (immediate (core:create-tagged-immediate-value-or-nil value)))
      (setf (gethash constant *constant-values*)
            (if immediate
                (cmp:irc-int-to-ptr
                 (%i64 immediate)
                 cmp:%t*%)
                (literal:reference-literal value t)))))
  (assert (or (cleavir-set:empty-set-p (bir:load-time-values module))
              (eq cst-to-ast:*compiler* 'cl:compile-file))
          ()
          "Found load-time-values to dump but not file compiling!")
  (cleavir-set:doset (load-time-value (bir:load-time-values module))
    (let ((form (bir:form load-time-value)))
      (setf (gethash load-time-value *constant-values*)
            ;; Allocate an index in the literal table for this load-time-value.
            (literal:load-time-value-from-thunk
             (compile-form form *clasp-env*))))))

(defun layout-module (module abi &key (linkage 'llvm-sys:internal-linkage))
  ;; Create llvm IR functions for each BIR function.
  (bir:do-functions (function module)
    ;; Assign IDs to unwind destinations. We start from 1 to allow
    ;; things to work with setjmp, which cannot return 0 from longjmp.
    (let ((i 1))
      (cleavir-set:doset (entrance (bir:entrances function))
        (setf (gethash entrance *unwind-ids*) i)
        (incf i)))
    (setf (gethash function *function-info*)
          (allocate-llvm-function-info function :linkage linkage)))
  (allocate-module-constants module)
  (bir:do-functions (function module)
    (layout-procedure function (get-or-create-lambda-name function)
                      abi :linkage linkage)))

(defun translate (bir &key abi linkage)
  (let* ((*unwind-ids* (make-hash-table :test #'eq))
         (*function-info* (make-hash-table :test #'eq))
         (*constant-values* (make-hash-table :test #'eq)))
    (layout-module (bir:module bir) abi :linkage linkage)
    (cmp::potentially-save-module)
    (xep-function (find-llvm-function-info bir))))

(defun conversion-error-handler (condition)
  ;; Resignal the condition to see if anything higher up wants to handle it.
  ;; If not, continue compilation by replacing the errant form with a form
  ;; that will signal an error if it's reached at runtime.
  ;; The nature of this form is a bit tricky because it can't just include
  ;; the original condition, if we're in COMPILE-FILE - conditions aren't
  ;; necessarily dumpable, and nor is the source.
  ;; For now we just assume we're in COMPILE-FILE.
  (signal condition)
  (let* ((cst (cst-to-ast:cst condition))
         (form (cst:raw cst))
         (origin (origin-source cst)))
    (invoke-restart 'cst-to-ast:substitute-cst
                    (cst:reconstruct
                     `(error 'cmp:compiled-program-error
                             :form ,(with-standard-io-syntax
                                      (write-to-string form
                                                       :escape t :pretty t
                                                       :circle t :array nil))
                             :origin ',(origin-spi origin)
                             :condition ,(princ-to-string condition))
                     cst clasp-cleavir:*clasp-system* :default-source origin))))

(defun cst->ast (cst &optional (env *clasp-env*))
  "Compile a cst into an AST and return it.
Does not hoist.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (handler-bind
      ((cst-to-ast:no-variable-info
         (lambda (condition)
           (cmp:warn-undefined-global-variable
            (origin-spi (cmp:compiler-condition-origin condition))
            (cst-to-ast:name condition))
           (invoke-restart 'cst-to-ast:consider-special)))
       (cst-to-ast:no-function-info
         (lambda (condition)
           (cmp:register-global-function-ref
            (cst-to-ast:name condition)
            (origin-spi (cmp:compiler-condition-origin condition)))
           (invoke-restart 'cst-to-ast:consider-global)))
       (cst-to-ast:compiler-macro-expansion-error
         (lambda (condition)
           (warn 'cmp:compiler-macro-expansion-error-warning
                 :origin (origin-spi (cmp:compiler-condition-origin condition))
                 :condition condition)
           (continue condition)))
       ((and cst-to-ast:compilation-program-error
             ;; If something goes wrong evaluating an eval-when,
             ;; we just want a normal error signal-
             ;; we can't recover and keep compiling.
             (not cst-to-ast:eval-error))
         #'conversion-error-handler))
    (cst-to-ast:cst-to-ast cst env clasp-cleavir:*clasp-system*)))

;;; Given an AST that may not be a function-ast, wrap it
;;; in a function AST. Useful for the pattern of
;;; (eval form) = (funcall (compile nil `(lambda () ,form)))
;;; as this essentially does the lambda wrap.
(defun wrap-ast (ast)
  (ast:make-function-ast
   ast nil
   :origin (ast:origin ast)
   :policy (ast:policy ast)))

(defun ast->bir (ast system)
  (cleavir-ast-to-bir:compile-toplevel ast system))

;;; These variables can be bound to debug the bir transformations.
;;; T means they apply after every transformation. Or, you can bind
;;; them to a list (of the keys in bir-transformations below), in
;;; which case they'll happen after the given transformation.
;;; *verify-transformations* will just run the verifier.
;;; *display-transformations* will dump a BIR disassembly and BREAK.
(defvar *verify-transformations* t)
(defvar *display-transformations* nil)

(defun maybe-debug-transformation (module key)
  (when (or (eq *verify-transformations* t)
            (member key *verify-transformations*))
    (handler-bind
        ((error
           (lambda (e)
             (declare (ignore e))
             (warn "Verification failed after ~a" key))))
      (cleavir-bir:verify module)))
  (when (or (eq *display-transformations* t)
            (member key *display-transformations*))
    (cleavir-bir-disassembler:display module)
    (break)))

(defun bir-transformations (module system)
  (maybe-debug-transformation module :start)
  (bir-transformations:module-eliminate-come-froms module)
  (maybe-debug-transformation module :eliminate-come-froms)
  (bir-transformations:find-module-local-calls module)
  (maybe-debug-transformation module :local-calls)
  (bir-transformations:module-optimize-variables module)
  (maybe-debug-transformation module :optimize-vars)
  (bir-transformations:meta-evaluate-module module system)
  (maybe-debug-transformation module :meta-evaluate)
  (cc-vaslist:maybe-transform-module module)
  (bir-transformations:module-generate-type-checks module system)
  (cc-bir-to-bmir:reduce-module-instructions module)
  (cc-bmir-to-blir:reduce-module-instructions module)
  ;; These should happen after higher level optimizations since they are like
  ;; "post passes" which do not modify the flow graph.
  ;; NOTE: These must come in this order to maximize analysis.
  (bir-transformations:determine-function-environments module)
  (bir-transformations:determine-closure-extents module)
  (bir-transformations:determine-variable-extents module)
  ;; These currently use information about variable extents, which is why they're
  ;; after the "post" passes. It may be better to not use that information so
  ;; these can be before them?
  (cc-bir-to-bmir:assign-module-rtypes module)
  (cc-bir-to-bmir:insert-casts-into-module module)
  (maybe-debug-transformation module :final)
  (values))

(defun translate-ast (ast &key (abi *abi-x86-64*)
                               (linkage 'llvm-sys:internal-linkage)
                               (system *clasp-system*))
  (let* ((bir (ast->bir ast system))
         (module (bir:module bir)))
    ;;(bir:verify module)
    (bir-transformations module system)
    (bir:verify module)
    (translate bir :abi abi :linkage linkage)))

(defun bir-compile (form env pathname
                    &key (linkage 'llvm-sys:internal-linkage) name)
  (bir-compile-cst (cst:cst-from-expression form) env pathname
                   :linkage linkage :name name))

(defun cleavir-compile (name &optional definition)
  (let ((cmp:*cleavir-compile-hook* #'bir-compile)
        (core:*use-cleavir-compiler* t))
    (compile name definition)))

(defun bir-compile-cst (cst env pathname
                        &key (linkage 'llvm-sys:internal-linkage) name)
  (declare (ignore linkage))
  (let* (function
         ordered-raw-constants-list constants-table startup-shutdown-id
         (cst-to-ast:*compiler* 'cl:compile)
         (ast (cst->ast cst env))
         (name (or name (ast:name ast))))
    (declare (ignorable constants-table))
    (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
      (multiple-value-setq (ordered-raw-constants-list constants-table startup-shutdown-id)
        (literal:with-rtv
            (setq function (translate-ast ast)))))
    (unless function
      (error "There was no function returned by translate-ast"))
    ;;(llvm-sys:dump-module cmp:*the-module* *standard-output*)
    (cmp:jit-add-module-return-function
     cmp:*the-module*
     function startup-shutdown-id ordered-raw-constants-list :name name)))

(defun bir-compile-in-env (form &optional env)
  (bir-compile-cst-in-env (cst:cst-from-expression form) env))

(defun bir-compile-cst-in-env (cst &optional env)
  (let ((cst-to-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (cmp:compile-in-env cst env #'bir-compile-cst cmp:*default-compile-linkage*)))

(defun compile-form (form &optional (env *clasp-env*))
  (let* ((cst (cst:cst-from-expression form))
         (pre-ast (cst->ast cst env))
         (ast (wrap-ast pre-ast)))
    (translate-ast ast)))

(defun compile-file-cst (cst &optional (env *clasp-env*))
  (let* ((cmp:*default-condition-origin* (origin-spi (cst:source cst)))
         (pre-ast (cst->ast cst env))
         (ast (wrap-ast pre-ast)))
    (literal:arrange-thunk-as-top-level
     (translate-ast ast :linkage cmp:*default-linkage*))))

(defun bir-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym))
        (eclector.reader:*client* *cst-client*)
        (eclector.readtable:*readtable* cl:*readtable*)
        (cst-to-ast:*compiler* 'cl:compile-file)
        (core:*use-cleavir-compiler* t))
    (loop
      ;; Required to update the source pos info. FIXME!?
      (peek-char t source-sin nil)
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let* ((core:*current-source-pos-info* (cmp:compile-file-source-pos-info source-sin))
             (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value)))
        #+debug-monitor(sys:monitor-message "source-pos ~a" core:*current-source-pos-info*)
        (if (eq cst eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form (cst:raw cst)))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (compile-file-cst cst environment))))))))

(defun cleavir-compile-file (input-file &rest kwargs)
  (let ((core:*use-cleavir-compiler* t)
        (cmp:*cleavir-compile-file-hook*
          'bir-loop-read-and-compile-file-forms)
        (cmp:*cleavir-compile-hook* 'bir-compile))
    (apply #'compile-file input-file kwargs)))
