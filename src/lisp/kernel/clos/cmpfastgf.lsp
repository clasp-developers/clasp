;;; ------------------------------------------------------------
;;;
;;; Generic function dispatch compiler
;;;   This implements the algorithm described by Robert Strandh for fast generic function dispatch.
;;;   See dtree.lsp for an explanation of dtrees.

(in-package :clos)

(defvar *log-gf* nil)
(defvar *debug-cmpfastgf-trace* nil)
(defvar *message-counter* nil)

(defvar *generate-outcomes*) ; alist (outcome . tag)

;;; Basic entry point for codegen (bottom of this file)
(defun generate-dispatcher (generic-function dtree &key generic-function-name)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (generate-dispatcher-from-dtree dtree
                                    :nreq min
                                    :max-nargs max
                                    :generic-function-name generic-function-name
                                    :generic-function-form generic-function)))

;;; Actual code generator
;;; It's written to not need an actual generic-function object.
;;; This is useful during satiation (because with some work, we could do it without
;;;  actually having a gf) and for non-generic discriminators, like typeq or vref.
(defun generate-dispatcher-from-dtree (dtree
                                       &key extra-bindings generic-function-name
                                         nreq max-nargs generic-function-form
                                         (miss-operator 'dispatch-miss))
  ;; GENERIC-FUNCTION-FORM is used when we want to feed this form to COMPILE-FILE,
  ;; in which case it can't have literal generic functions in it.
  ;; If we're doing this at runtime, though, we should put the actual GF in, so that
  ;; things don't break if we have an anonymous GF or suchlike.
  ;; EXTRA-BINDINGS is also used in the compile-file case, and gets methods and stuff
  ;; with load-time-value.
  (let* (;; Can we use just required parameters?
         ;; NOTE: We used to use the call history here. This won't work:
         ;; During compile time satiation, the dtree may be artificially produced
         ;; i.e. out of sync with the call history. Or in a multithreaded environment,
         ;; the gf could maybe be called in one thread while another thread is here.
         ;; Keep these inconsistencies in mind.
         (need-vaslist-p (dtree-requires-vaslist-p dtree))
         (block-name (if generic-function-name
                         (core:function-block-name generic-function-name)
                         (gensym "DISCRIMINATION-BLOCK")))
         ;; List of gensyms, one for each required argument
         (required-args (let ((res nil))
                          (dotimes (i nreq res)
                            (push (gensym "DISPATCH-ARG") res)))))
    `(lambda ,(if need-vaslist-p
                  `(core:&va-rest .method-args.)
                  required-args)
       ,@(when generic-function-name
           `((declare (core:lambda-name ,generic-function-name))))
       (let ((.generic-function. ,generic-function-form))
         ,(when need-vaslist-p
            ;; Our discriminating function ll is just (&va-rest r), so we need
            ;; to check argument counts. What we really need to check is the minimum,
            ;; since vaslist-pop has undefined behavior if there's nothing to pop,
            ;; but we ought to do both, really.
            ;; FIXME: Should be possible to not check, on low safety.
            ;; Remember that argument checking by methods is disabled (see method.lsp)
            `(let ((nargs (core:vaslist-length .method-args.)))
               ;; stupid tagbody to avoid redundant error signaling code
               (core::local-tagbody
                (if (cleavir-primop:fixnum-less nargs ,nreq)
                    (go err))
                ,@(when max-nargs
                    `((if (cleavir-primop:fixnum-less ,max-nargs nargs)
                          (go err))))
                (go done)
                err
                (error 'core:wrong-number-of-arguments
                       :called-function .generic-function.
                       :given-nargs nargs
                       :min-nargs ,nreq :max-nargs ,max-nargs)
                done)))
         (let (,@extra-bindings
               ,@(if need-vaslist-p
                     (mapcar (lambda (req)
                               `(,req (core:vaslist-pop .method-args.)))
                             required-args)
                     nil))
           (declare (ignorable ,@required-args))
           ,(generate-dispatch
             dtree required-args
             (if need-vaslist-p
                 `(progn
                    (core:vaslist-rewind .method-args.)
                    (apply #',miss-operator .generic-function. .method-args.))
                 `(,miss-operator .generic-function. ,@required-args))
             block-name))))))

;;; code generator part two
;;; This generates the actual dispatch code. MISS is the form to return the value
;;; of if the dtree hits a dispatch-miss. BLOCK-NAME is internal.
(defun generate-dispatch (dtree dispatch-args miss &optional block-name)
  (let ((*generate-outcomes* nil))
    `(core::local-block ,block-name
       (core::local-tagbody
          ,(generate-node dispatch-args dtree)
          ,@(generate-tagged-outcomes *generate-outcomes* block-name dispatch-args)
        dispatch-miss
          (return-from ,block-name ,miss)))))

;;; outcomes
;;; we cache them to avoid generating calls/whatever more than once
;;; they're generated after all the discrimination code is; the
;;; discriminating part of the function just jumps to these tags.

(defun generate-tagged-outcomes (list block-name required-arguments)
  (mapcan (lambda (pair)
            (let ((outcome (car pair)) (tag (cdr pair)))
              (list tag
                    `(return-from ,block-name
                       ,(generate-outcome required-arguments outcome)))))
          list))

(defun generate-outcome (reqargs outcome)
  (cond ((optimized-slot-reader-p outcome)
         (generate-slot-reader reqargs outcome))
        ((optimized-slot-writer-p outcome)
         (generate-slot-writer reqargs outcome))
        ((fast-method-call-p outcome)
         (generate-fast-method-call reqargs outcome))
        ((effective-method-outcome-p outcome)
         (generate-effective-method-call outcome))
        ((custom-outcome-p outcome)
         (generate-custom reqargs outcome))
        (t (error "BUG: Bad thing to be an outcome: ~a" outcome))))

(defun generate-slot-reader (arguments outcome)
  (let ((location (optimized-slot-reader-index outcome))
        (slot-name (optimized-slot-reader-slot-name outcome))
        (class (optimized-slot-reader-class outcome)))
    (cond ((fixnump location)
           ;; instance location- easy
           `(let* ((instance ,(first arguments))
                   (value (core:instance-ref instance ',location)))
              (if (cleavir-primop:eq value (load-time-value (core:unbound) t))
                  (slot-unbound ,class instance ',slot-name)
                  value)))
          ((consp location)
           ;; class location. we need to find the new cell at load time.
           `(let* ((location
                     (load-time-value
                      (slot-definition-location
                       (or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
                           (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                                  ',slot-name ,class)))))
                   (value (car location)))
              (if (cleavir-primop:eq value (core:unbound))
                  (slot-unbound ,class ,(first arguments) ',slot-name)
                  value)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defun generate-slot-writer (arguments outcome)
  (let ((location (optimized-slot-writer-index outcome)))
    (cond ((fixnump location)
           `(let ((value ,(first arguments))
                  (instance ,(second arguments)))
              (si:instance-set instance ,location value)))
          ((consp location)
           ;; class location- annoying
           ;; Note we don't actually need the instance.
           (let ((slot-name (optimized-slot-reader-slot-name outcome))
                 (class (optimized-slot-reader-class outcome)))
             `(let ((value ,(first arguments))
                    (location
                      (load-time-value
                       (slot-definition-location
                        (or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
                            (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                                   ',slot-name ,class))))))
                (rplaca location value)
                value)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defun generate-fast-method-call (arguments outcome)
  (let ((fmf (fast-method-call-function outcome)))
    `(cleavir-primop:funcall ,fmf ,@arguments)))

(defun generate-effective-method-call (outcome)
  `(progn
     (core:vaslist-rewind .method-args.)
     ;; if a form was provided, just throw it in.
     ;; Otherwise generate a function call.
     ;; NOTE: We use NIL to mean "no form provided".
     ;; Hypothetically the form could actually BE nil,
     ;; but I'm not holding my breath here- the backup is probably fine.
     ,(cond ((effective-method-outcome-form outcome))
            ((effective-method-outcome-function outcome)
             `(cleavir-primop:funcall
               ,(effective-method-outcome-function outcome) .method-args. nil))
            (t (error "BUG: Outcome ~a is messed up" outcome)))))

(defun generate-custom (reqargs outcome)
  `(progn
     ,@(when (custom-outcome-requires-vaslist-p outcome)
         '((core:vaslist-rewind .method-args.)))
     ,(funcall (custom-outcome-generator outcome)
               reqargs (custom-outcome-data outcome))))

;;; discrimination

(defun generate-node (arguments node)
  (cond ((outcome-p node) (generate-go-outcome node))
        ((advance-p node) (generate-advance arguments node))
        ((tag-test-p node) (generate-tag-test arguments node))
        ((stamp-read-p node) (generate-stamp-read arguments node))
        ((<-branch-p node) (generate-<-branch arguments node))
        ((=-check-p node) (generate-=-check arguments node))
        ((range-check-p node) (generate-range-check arguments node))
        ((eql-search-p node) (generate-eql-search arguments node))
        ((miss-p node) (generate-miss arguments node))
        (t (error "BUG: Bad thing to be a dtree node: ~a" node))))

(defun generate-go-outcome (outcome)
  (let ((existing (assoc outcome *generate-outcomes* :test #'outcome=)))
    (if (null existing)
        ;; no match: put it on there
        (let ((tag (gensym "OUTCOME")))
          (push (cons outcome tag) *generate-outcomes*)
          `(go ,tag))
        ;; match: goto existing tag
        `(go ,(cdr existing)))))

(defun generate-advance (arguments node)
  `(let ((arg ,(pop arguments)))
     ,(generate-node arguments (advance-next node))))

(defvar *ordered-tag-tests* (mapcar #'fourth (llvm-sys:tag-tests)))

(defun generate-tag-test (arguments node)
  `(cond ,@(loop for next across (tag-test-tags node)
                 for test in *ordered-tag-tests*
                 unless (miss-p next)
                   collect `((,test arg) ,(generate-node arguments next)))
         ,@(unless (miss-p (tag-test-default node))
             `(((cleavir-primop:typeq arg core:general)
                ,(generate-node arguments (tag-test-default node)))))
         (t (go dispatch-miss))))

(defun generate-stamp-read (arguments node)
  `(let ((stamp (core::header-stamp arg)))
     (let ((stamp (core::header-stamp-case stamp
                                           (core::derivable-stamp arg)
                                           (core::rack-stamp arg)
                                           (core::wrapped-stamp arg)
                                           ;; we rely on the outcome GO-ing somewhere
                                           ;; to skip returning from here.
                                           ,(generate-node arguments (stamp-read-c++ node)))))
       ,(generate-node arguments (stamp-read-other node)))))

(defun generate-<-branch (arguments node)
  `(if (cleavir-primop:fixnum-less stamp ,(<-branch-pivot node))
       ,(generate-node arguments (<-branch-left node))
       ,(generate-node arguments (<-branch-right node))))

(defun generate-=-check (arguments node)
  `(if (cleavir-primop:fixnum-equal stamp ,(=-check-pivot node))
       ,(generate-node arguments (=-check-next node))
       (go dispatch-miss)))

(defun generate-range-check (arguments node)
  `(if (cleavir-primop:fixnum-not-greater stamp ,(range-check-max node))
       (if (cleavir-primop:fixnum-not-greater ,(range-check-min node) stamp)
           ,(generate-node arguments (range-check-next node))
           (go dispatch-miss))
       (go dispatch-miss)))

(defun generate-eql-search (arguments node)
  `(case arg
     ,@(loop for object across (eql-search-objects node)
             for next across (eql-search-nexts node)
             collect `((,object) ,(generate-node arguments next)))
     (otherwise ,(generate-node arguments (eql-search-default node)))))

(defun generate-miss (arguments node)
  (declare (ignore arguments node))
  '(go dispatch-miss))

;;; Keeps track of the number of dispatchers that were compiled and
;;;   is used to give the roots array in each dispatcher a unique name.
#+threads(defvar *dispatcher-count-lock* (mp:make-lock :name '*dispatcher-count-lock* ))
(defvar *dispatcher-count* 0)
(defun increment-dispatcher-count ()
  #-threads(incf *dispatcher-count*)
  #+threads(unwind-protect
       (progn
         (mp:lock *dispatcher-count-lock* t)
         (incf *dispatcher-count*))
    (mp:unlock *dispatcher-count-lock*)))
(defun dispatcher-count ()
  #-threads *dispatcher-count*
  #+threads(unwind-protect
                (progn
                  (mp:lock *dispatcher-count-lock* t)
                  *dispatcher-count*)
             (mp:unlock *dispatcher-count-lock*)))

(defvar *fastgf-use-compiler* nil)
(defvar *fastgf-timer-start*)
(defun codegen-dispatcher (call-history specializer-profile generic-function
                           &rest args &key generic-function-name output-path log-gf force-compile)
  (let* ((*log-gf* log-gf)
         (*fastgf-timer-start* (get-internal-real-time))
         (compiled (calculate-dtree call-history specializer-profile)))
    (unwind-protect
         (if (or force-compile *fastgf-use-compiler*)
             (cmp:bclasp-compile nil (generate-dispatcher
                                      generic-function compiled
                                      :generic-function-name generic-function-name))
             (let ((program (coerce (linearize compiled) 'vector)))
               (lambda (core:&va-rest args)
                 (declare (core:lambda-name interpreted-discriminating-function))
                 (clos:interpret-dtree-program program generic-function args))))
      (let ((delta-seconds (/ (float (- (get-internal-real-time) *fastgf-timer-start*) 1d0)
                              internal-time-units-per-second)))
        (generic-function-increment-compilations generic-function)
        (gctools:accumulate-discriminating-function-compilation-seconds delta-seconds)))))

(export '(codegen-dispatcher))
