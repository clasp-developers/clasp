;;; ------------------------------------------------------------
;;;
;;; Generic function dispatch compiler
;;;   This implements the algorithm described by Robert Strandh for fast generic function dispatch
;;;
;;;   generic-function-call-history is an alist of (past-call-signature . outcome)
;;;      Outcome is either a function, or one of the outcome structures defined below.
;;;        Functions must have lambda-list (vaslist ignore), and will be passed the arguments and NIL.
;;;      The past-call-signature is a simple-vector of class specializers or (list eql-spec)
;;;        for eql-specializers. The CAR of eql-spec is the EQL value.

(in-package :clos)

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *echo-repl-read* t))

(defvar *log-gf* nil)
(defvar *debug-cmpfastgf-trace* nil)
(defvar *message-counter* nil)

;;; ------------------------------------------------------------
;;;
;;; Generate Common Lisp code for a fastgf dispatcher given a
;;;   DTREE internal representation

(defvar *generate-outcomes*) ; alist (outcome . tag)

;;; main entry point
(defun generate-dispatcher-from-dtree (generic-function dtree
                                       &key extra-bindings generic-function-name
                                         (generic-function-form generic-function))
  ;; GENERIC-FUNCTION-FORM is used when we want to feed this form to COMPILE-FILE,
  ;; in which case it can't have literal generic functions in it.
  ;; If we're doing this at runtime, though, we should put the actual GF in, so that
  ;; things don't break if we have an anonymous GF or suchlike.
  ;; EXTRA-BINDINGS is also used in the compile-file case, and gets methods and stuff
  ;; with load-time-value.
  (let* (;; We need to know the number of arguments to dispatch on, and to have
         ;; to the discriminating function if we can manage that.
         ;; FIXME: This will work, due to how the s-profile is initialized,
         ;; but it's weird and indirect.
         (nreq (length (generic-function-specializer-profile generic-function)))
         ;; and we need this to see if we can manage that
         (need-vaslist-p (call-history-needs-vaslist-p
                          (generic-function-call-history generic-function)))
         (block-name (if generic-function-name
                         (core:function-block-name generic-function-name)
                         (gensym "DISCRIMINATION-BLOCK")))
         ;; List of gensyms, one for each required argument
         (required-args (let ((res nil))
                          (dotimes (i nreq res)
                            (push (gensym "DISPATCH-ARG") res))))
         (*generate-outcomes* nil))
    `(lambda ,(if need-vaslist-p
                  `(core:&va-rest .method-args.)
                  required-args)
       (let (,@extra-bindings
             (.generic-function. ,generic-function-form)
             ,@(if need-vaslist-p
                   (mapcar (lambda (req)
                             `(,req (core:vaslist-pop .method-args.)))
                           required-args)
                   nil))
         (declare (ignorable ,@required-args))
         (block ,block-name
           (tagbody
              ,(generate-node-or-outcome required-args (dtree-root dtree))
              ;; note: we need generate-node-or-outcome to run to fill *generate-outcomes*.
              ,@(generate-tagged-outcomes *generate-outcomes* block-name required-args)
            dispatch-miss
              ,@(if need-vaslist-p
                    `((core:vaslist-rewind .method-args.)
                      (return-from ,block-name
                        (dispatch-miss .generic-function. .method-args.)))
                    `((return-from ,block-name
                        (dispatch-miss-with-args .generic-function. ,@required-args))))))))))

(defun generate-node-or-outcome (arguments node-or-outcome)
  (if (outcome-p node-or-outcome)
      (generate-go-outcome node-or-outcome)
      (generate-node arguments node-or-outcome)))

;;; outcomes
;;; we cache them to avoid generating calls/whatever more than once
;;; they're generated after all the discrimination code is.

(defun generate-go-outcome (outcome)
  (ensure-outcome-or-error outcome)
  (let ((existing (assoc outcome *generate-outcomes* :test #'outcome=)))
    (if (null existing)
        ;; no match: put it on there
        (let ((tag (gensym "OUTCOME")))
          (push (cons outcome tag) *generate-outcomes*)
          `(go ,tag))
        ;; match: goto existing tag
        `(go ,(cdr existing)))))

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
    `(funcall ,fmf ,@arguments)))

(defun generate-effective-method-call (outcome)
  `(progn
     (core:vaslist-rewind .method-args.)
     ;; if a form was provided, just throw it in.
     ;; Otherwise generate a function call.
     ;; NOTE: We use NIL to mean "no form provided".
     ;; Hypothetically the form could actually BE nil,
     ;; but I'm not holding my breath here- the backup is probably fine.
     ,(cond ((effective-method-outcome-form outcome))
            ((functionp (effective-method-outcome-function outcome))
             `(funcall ,(effective-method-outcome-function outcome) .method-args. nil))
            (t (error "BUG: Outcome ~a is messed up" outcome)))))

;;; discrimination

(defun generate-node (arguments node)
  (let ((arg (pop arguments)))
    (cond ((skip-node-p node)
           (generate-skip-node arguments node))
          ;; We avoid the eql CASE when possible. This isn't really necessary,
          ;; but let's try to avoid pressuring the optimizer when it's easy.
          ((has-eql-specializers-p node)
           `(case ,arg
              ,@(generate-eql-specializers arguments arg node)
              (otherwise
               ,(generate-class-specializers arguments arg node))))
          (t (generate-class-specializers arguments arg node)))))

(defun generate-skip-node (arguments node)
  (let ((skip-node (first (node-class-specializers node))))
    (generate-node-or-outcome arguments (skip-outcome skip-node))))

(defun has-eql-specializers-p (node)
  (not (zerop (hash-table-count (node-eql-specializers node)))))

(defun generate-eql-specializers (arguments arg node)
  (declare (ignore arg)) ; just for parity with generate-class-specializers
  ;; could also loop
  (let ((result nil))
    (maphash (lambda (spec outcome)
               (push (generate-eql-specializer arguments spec outcome) result))
             (node-eql-specializers node))
    result))

(defun generate-eql-specializer (arguments spec outcome)
  `((,spec) ,(generate-node-or-outcome arguments outcome)))

(defun generate-class-specializers (arguments arg node)
  (let ((stamp-var (gensym "STAMP")))
    `(let ((,stamp-var ,(generate-read-stamp arg)))
       ,(generate-class-binary-search arguments (node-class-specializers node) stamp-var))))

(defun generate-read-stamp (arg)
  `(core:instance-stamp ,arg))

(defun generate-class-binary-search (arguments matches stamp-var)
  (cond
    ((null matches)
     `(go dispatch-miss))
    ((= (length matches) 1)
     (let ((match (first matches)))
       (if (single-p match)
           `(if (cleavir-primop:fixnum-equal ; =
                 ,stamp-var ,(range-first-stamp match))
                ,(generate-node-or-outcome arguments (range-outcome match))
                (go dispatch-miss))
           ;; note: the primop needs to be the literal IF condition,
           ;; so the obvious AND isn't quite going to work.
           `(if (cleavir-primop:fixnum-not-greater ; <=
                 ,(range-first-stamp match) ,stamp-var)
                (if (cleavir-primop:fixnum-not-greater ; <=
                     ,stamp-var ,(range-last-stamp match))
                    ,(generate-node-or-outcome arguments (match-outcome match))
                    (go dispatch-miss))
                (go dispatch-miss)))))
    (t
     (let* ((len-div-2 (floor (length matches) 2))
            (left-matches (subseq matches 0 len-div-2))
            (right-matches (subseq matches len-div-2))
            (right-head (first right-matches))
            (right-stamp (range-first-stamp right-head)))
       `(if (cleavir-primop:fixnum-less ; <
             ,stamp-var ,right-stamp)
            ,(generate-class-binary-search arguments left-matches stamp-var)
            ,(generate-class-binary-search arguments right-matches stamp-var))))))

(defun gather-sorted-outcomes (eql-selectors outcomes)
  (labels ((extract-outcome (outcome)
             ;; An outcome is an effective-method or an optimized slot reader or writer.
             ;; Figure out which case it is and return the effective-method or the
             ;;   optimization info necessary to evaluate the slot reader or writer.
             (let ((oc (cdr outcome)))
               (if (consp oc)
                   (cdr oc)
                   oc))))
    (let ((values nil))
      (maphash (lambda (k v)
                 (push (cons v k) values))
               eql-selectors)
      (maphash (lambda (k v)
                   (push (cons v k) values))
               outcomes)
      (let ((sorted (sort values #'< :key #'car)))
        (mapcar #'extract-outcome sorted)))))

(defun call-history-needs-vaslist-p (call-history)
  ;; Functions, i.e. method functions or full EMFs, are the only things that need vaslists.
  (mapc (lambda (entry)
          (when (effective-method-outcome-p (cdr entry))
            (return-from call-history-needs-vaslist-p t)))
        call-history)
  nil)

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
(defun codegen-dispatcher (raw-call-history specializer-profile generic-function
                           &rest args &key generic-function-name output-path log-gf force-compile)
  (let* ((*log-gf* log-gf)
         (*fastgf-timer-start* (get-internal-real-time))
         (dtree (calculate-dtree raw-call-history specializer-profile)))
    (unwind-protect
         (if (or force-compile *fastgf-use-compiler*)
             (cmp:bclasp-compile nil (generate-dispatcher-from-dtree
                                      generic-function dtree
                                      :generic-function-name generic-function-name))
             (core:make-dtree-interpreter generic-function (compile-interpreted-dtree dtree)))
      (let ((delta-seconds (/ (float (- (get-internal-real-time) *fastgf-timer-start*) 1d0)
                              internal-time-units-per-second)))
        (generic-function-increment-compilations generic-function)
        (gctools:accumulate-discriminating-function-compilation-seconds delta-seconds)))))

(export '(codegen-dispatcher))
