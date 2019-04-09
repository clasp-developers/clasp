;;; ------------------------------------------------------------
;;;
;;; Generic function dispatch compiler
;;;   This implements the algorithm described by Robert Strandh for fast generic function dispatch
;;;
;;;   clos:generic-function-call-history is an alist of (past-call-signature . outcome)
;;;      Outcome is either a function, or one of the outcome structures defined below.
;;;        Functions must have lambda-list (vaslist ignore), and will be passed the arguments and NIL.
;;;      The past-call-signature is a simple-vector of class specializers or (list eql-spec)
;;;        for eql-specializers. The CAR of eql-spec is the EQL value.

(in-package :cmp)

;;; ------------------------------------------------------------
;;;
;;; Debugging code
;;;
;;; Add :DEBUG-CMPGF to *features* and recompile for lots of debugging info
;;;   during fastgf compilation and execution.
;;;
;;; Add :LOG-CMPGF to log fastgf messages during the slow path.
;;;    

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *echo-repl-read* t))

(defvar *log-gf* nil)
(defvar *debug-cmpfastgf-trace* nil)
(defvar *message-counter* nil)
#+debug-cmpfastgf
(progn
  (defun debug-argument (arg arg-tag)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 1) arg))
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 2) arg-tag))))
  (defun debug-pointer (ptr)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 4) ptr))))
  (defun debug-arglist (ptr)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 3) ptr))))
  (defun debug-va_list (ptr)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 5) ptr))))
  (defun debug-stamp (ptr)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 6) ptr))))
  (defun debug-dispatch (ptr)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke "cc_dispatch_debug" (list (jit-constant-i32 7) ptr))))
  (defun debug-call (fn args)
    (when *debug-cmpfastgf-trace*
      (irc-intrinsic-call-or-invoke fn args))))

#-debug-cmpgf
(progn
  (defun debug-argument (arg arg-tag))
  (defun debug-pointer (ptr))
  (defun debug-arglist (ptr))
  (defun debug-va_list (ptr))
  (defun debug-stamp (ptr))
  (defun debug-dispatch (ptr))
  (defun debug-call (fn args)))


(defmacro cf-log (fmt &body args)
  #+(or)nil
  `(core:bformat *log-gf* ,fmt ,@args))

;;; ------------------------------------------------------------
;;;
;;; Convert a generic function call-history to an internal representation
;;;   called a DTREE (dispatch-tree) made up of the structs below.

(defvar *outcomes* nil
  "Map effective methods to indices in the *gf-data* and then to basic-blocks")
(defvar *gf-data-id* nil)
(defvar *gf-data* nil
  "Store the global variable that will store effective methods")
(defvar *bad-tag-bb*)
(defvar *eql-selectors*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sanity check for slot index value
;;;
;;; index must the at position 1 of the optimized-slot-(reader|writer)
;;;
(eval-when (:compile-toplevel :execute :load-toplevel)
  (unless (= +optimized-slot-index-index+ 1)
    (error "The index of the slot index must be 1")))
;; the emf slot here is for debugging only - i.e., comparing with the optimized slot value. Usually unused.
(defstruct (outcome (:type vector) :named))
(defstruct (optimized-slot-reader (:type vector) (:include outcome) :named) index #| << must be here |# effective-method-function slot-name method class)
(defstruct (optimized-slot-writer (:type vector) (:include outcome) :named) index #| << must be here |# effective-method-function slot-name method class)
(defstruct (fast-method-call (:type vector) (:include outcome) :named) function)
;; a thing that will be called like an effective method function, but isn't cached or anything.
(defstruct (function-outcome (:type vector) (:include outcome) :named) function)
;; applicable-methods slot is for clos; see closfastgf.lsp's find-existing-emf
(defstruct (effective-method-outcome (:type vector) (:include outcome) :named) applicable-methods function)

(defstruct (klass (:type vector) :named) stamp name)
(defstruct (match (:type vector) :named) outcome)
(defstruct (range (:include match) (:type vector) :named) first-stamp last-stamp reversed-classes)
(defstruct (skip (:include match) (:type vector) :named))
(defstruct (node (:type vector) :named) (eql-specializers (make-hash-table :test #'eql) :type hash-table)
           (class-specializers nil :type list)
           (interpreter nil :type vector))

(defun ensure-outcome-or-error (obj)
  (unless (outcome-p obj) (error "~s is not an outcome" obj))
  obj)

;; Degenerate range node with only one stamp are useful to create/note.
(defun make-stamp-matcher (&key stamp class outcome)
  (make-range :reversed-classes (list class) :outcome outcome :first-stamp stamp :last-stamp stamp))
(defun single-p (match)
  (and (range-p match)
       (= (range-first-stamp match) (range-last-stamp match))))

(defstruct (argument-holder (:type vector) :named)
  ;; Slots to store the arguments
  return-value
  generic-function
  register-arguments
  register-save-area*
  remaining-register-arguments
  dispatch-arguments
  dispatch-va-list*
  vaslist*
  methods-vaslist-t*
  continue-after-dispatch
  miss-basic-block)

(defun argument-get (arguments index)
  (elt (argument-holder-dispatch-arguments arguments) index))

(defstruct (dtree (:type vector) :named) root)

(defun dtree-add-call-history (dtree call-history)
  "Add call-history for one generic-function to the dtree"
  (dolist (one call-history)
    (let ((signature (car one))
	  (outcome (cdr one)))
      #++(parse-call-history-entry one)
      (if (> (length signature) 0)
          (progn
            (when (null (dtree-root dtree)) (setf (dtree-root dtree) (make-node)))
            (node-add (dtree-root dtree) (svref signature 0) 1 signature outcome))
          (progn
            (setf (dtree-root dtree) (ensure-outcome-or-error outcome))))))
  dtree)

(defun eql-specializer-p (spec)
  "Return t if the spec is an eql specializer - they are represented as CONS cells
   with the value in the CAR"
  (consp spec))
(defun eql-specializer-value (spec)
  (car spec))

(defun node-add (node spec argument-index specializers goal)
  (if (eql-specializer-p spec)
      (node-eql-add node spec argument-index specializers goal)
      (node-class-add node spec argument-index specializers goal)))

(defun insert-sorted (item lst &optional (test #'<) (key #'range-first-stamp))
  (if (null lst)
      (list item)
      (let* ((firstp (funcall test (funcall key item) (funcall key (car lst))))
             (sorted (if firstp
                         (cons item lst) 
                         (cons (car lst) (insert-sorted item (cdr lst) test key)))))
        #+debug-fastgf(if (verify-node-class-specializers-sorted-p sorted)
                          t
                          (error "insert-sorted failed - tried to insert ~a into ~a result: ~a" item lst sorted))
        sorted)))

(defun ensure-outcome (argument-index specs goal)
  (if (>= argument-index (length specs))
      (ensure-outcome-or-error goal)
      (let ((node (make-node)))
        (node-add node (svref specs argument-index) (1+ argument-index) specs goal)
        node)))

(defun safe-class-name (class-designator)
  (cond
    ((symbolp class-designator) class-designator)
    ((clos::classp class-designator) (class-name class-designator))
    ((klass-p class-designator) (klass-name class-designator))
    (t (error "Illegal class-designator"))))

(defun node-class-add (node spec argument-index specializers goal)
  (or (<= argument-index (length specializers))
      (error "Overflow in argument-index ~a must be <= ~a" argument-index (length specializers)))
  (if spec
      (let* ((stamp (cond
                      ((clos:classp spec) (core:class-stamp-for-instances spec))
                      ((symbolp spec) (core:class-stamp-for-instances (find-class spec)))
                      ((klass-p spec) (klass-stamp spec))
                      (t (error "Illegal specializer ~a" spec))))
             (match (find stamp (node-class-specializers node) :test #'eql :key #'range-first-stamp)))
        (if match
            (if (outcome-p (match-outcome match))
                (warn "The dispatch function has two selectors with different outcomes~%---- argument-index: ~a~%---- specializers: ~a~%---- goal: ~a~%---- current node: ~a~%---- stamp: ~a~%---- match: ~a" argument-index specializers goal node stamp match)
                (node-add (match-outcome match) (svref specializers argument-index) (1+ argument-index) specializers goal))
            (setf (node-class-specializers node)
                  (insert-sorted (make-stamp-matcher :stamp stamp
                                                     :class spec
                                                     :outcome (ensure-outcome argument-index specializers goal))
                                 (node-class-specializers node)))))
      (let ((match (first (node-class-specializers node))))
        (if match
            (node-add (match-outcome match) (svref specializers argument-index) (1+ argument-index) specializers goal)
            (setf (node-class-specializers node)
                  (list (make-skip :outcome (ensure-outcome argument-index specializers goal))))))))

(defun node-eql-add (node spec argument-index specializers goal)
  (let* ((eql-value (eql-specializer-value spec))
	 (eql-ht (node-eql-specializers node))
         (node (gethash eql-value eql-ht nil)))
    (if node
        (progn
          (or (node-p node) (error "The node ~a must be of node type" node))
          (node-add node (svref specializers argument-index) (1+ argument-index) specializers goal))
        (let ((outcome (ensure-outcome argument-index specializers goal)))
          (setf (gethash eql-value eql-ht) outcome)))))

(defun outcome= (outcome1 outcome2)
  (or (eq outcome1 outcome2) ; covers effective-method-outcome due to closfastgf caching
      (cond ((optimized-slot-reader-p outcome1)
             (and (optimized-slot-reader-p outcome2)
                  ;; could also do class slot locations somehow,
                  ;; but it doesn't seem like a big priority.
                  (fixnump (optimized-slot-reader-index outcome1))
                  (fixnump (optimized-slot-reader-index outcome2))
                  (= (optimized-slot-reader-index outcome1)
                     (optimized-slot-reader-index outcome2))))
            ((optimized-slot-writer-p outcome1)
             (and (optimized-slot-writer-p outcome2)
                  (fixnump (optimized-slot-writer-index outcome1))
                  (fixnump (optimized-slot-writer-index outcome2))
                  (= (optimized-slot-writer-index outcome1)
                     (optimized-slot-writer-index outcome2))))
            ((fast-method-call-p outcome1)
             (and (fast-method-call-p outcome2)
                  (eq (fast-method-call-function outcome1)
                      (fast-method-call-function outcome2))))
            (t nil))))

(defun verify-node-class-specializers-sorted-p (specializers)
  (let ((working (first specializers)))
    (dolist (next (rest specializers))
      (unless (<= (range-first-stamp working) (range-last-stamp working))
        (return-from verify-node-class-specializers-sorted-p nil))
      (if (< (range-last-stamp working) (range-first-stamp next))
          (setf working next)
          (return-from verify-node-class-specializers-sorted-p nil))))
  t)

(defun prepare-node-for-interpreter (node)
  (let* ((specializers (node-class-specializers node))
         (layout (make-array (1+ (* 3 (length specializers)))))
         (first-spec (and specializers (elt specializers 0))))
    (cond
      ((null specializers)
       (setf (elt layout 0) 'cmp:range)) ;; No 
      ((skip-p first-spec)
       (setf (elt layout 0) 'cmp:skip)
       (setf (elt layout 1) (skip-outcome first-spec)))
      ((range-p first-spec)
       (setf (elt layout 0) 'cmp:range)
       (dotimes (i (length specializers))
         (let ((spec (car specializers))
               (start (1+ (* i 3))))
           (setf (elt layout (+ start 0)) (range-outcome spec)
                 (elt layout (+ start 1)) (range-first-stamp spec)
                 (elt layout (+ start 2)) (range-last-stamp spec)))
         (setf specializers (cdr specializers))))
      (t (error "cannot prepare interpreter for ~s" specializers)))
    (setf (node-interpreter node) layout)))

;; Given a node that isn't a skip node (i.e. all "specializers" are ranges),
;; modify its specializers so that adjacent ranges are merged together.
(defun optimize-node-with-class-specializers (node)
  (when (node-class-specializers node)
    (let* ((class-specializers (node-class-specializers node))
           ;; FIXME: copy is due to an abundance of paranoia, and may not be necessary.
           (sorted (sort (copy-list class-specializers) #'< :key #'range-first-stamp))
           (working (first sorted))
           merged)
      ;; Now we proceed through the list trying to merge the first with the rest.
      ;; Once we find one we can't merge with, throw it on the complete list.
      (dolist (match (rest sorted))
        (if (and (= (1+ (range-last-stamp working)) (range-first-stamp match))
                 (outcome= (range-outcome working) (range-outcome match)))
            ;; ranges touch: merge by increasing working's last stamp.
            (setf (range-last-stamp working) (range-last-stamp match)
                  ;; note: i think this reversed-classes field is only used for debugging,
                  ;; and perhaps could be removed or simplified.
                  (range-reversed-classes working)
                  (append (range-reversed-classes match) (range-reversed-classes working)))
            ;; range is distinct: throw working on the pile and continue anew.
            (setf merged (cons working merged) working match)))
      (push working merged)
      (let ((sorted (nreverse merged)))
        (setf (node-class-specializers node) sorted)
        #+debug-fastgf(if (verify-node-class-specializers-sorted-p sorted)
                          t
                          (error "optimize-node-with-class-specializers has a problem stamps aren't sorted --> ~a" sorted))
        sorted))))

(defun skip-node-p (node)
  (let ((class-specializers (node-class-specializers node)))
    (and (= 1 (length class-specializers)) (skip-p (car class-specializers)))))

(defun optimize-node (node)
  "Create a list from the argument list and merge matches
   that can be considered adjacent into a range object."
  (if (skip-node-p node)
      nil                   ; There is one class-specializer and
                                        ; it's a skip class-specializer do
                                        ; nothing but prepare for the interpreter.
      (optimize-node-with-class-specializers node))
  (prepare-node-for-interpreter node))

(defun optimize-node-and-children (maybe-node)
  (when (node-p maybe-node)
    ;; Loop over eql-specializers
    (maphash (lambda (specializer maybe-child-node)
               (optimize-node-and-children maybe-child-node))
             (node-eql-specializers maybe-node))
    ;; loop over class-specializers
    (dolist (spec (node-class-specializers maybe-node))
      (let ((child-maybe-node (match-outcome spec)))
	(optimize-node-and-children child-maybe-node)))
    ;; optimize this node
    (optimize-node maybe-node)))
	    


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
         (nreq (length (clos:generic-function-specializer-profile generic-function)))
         ;; and we need this to see if we can manage that
         (need-vaslist-p (call-history-needs-vaslist-p
                          (clos:generic-function-call-history generic-function)))
         (vaslist-args (if need-vaslist-p
                           (gensym "DISPATCH-VA-ARGS")
                           '(error "BUG: Discriminator tried to use vaslist")))
         (block-name (if generic-function-name
                         (core:function-block-name generic-function-name)
                         (gensym "DISCRIMINATION-BLOCK")))
         (gfsym (gensym "GENERIC-FUNCTION"))
         ;; List of gensyms, one for each required argument
         (required-args (let ((res nil))
                          (dotimes (i nreq res)
                            (push (gensym "DISPATCH-ARG") res))))
         (*generate-outcomes* nil))
    `(lambda ,(if need-vaslist-p
                  `(core:&va-rest ,vaslist-args)
                  required-args)
       (let (,@extra-bindings
             ;; Bound because at some point in the unknown future we may actually support
             ;; the :generic-function option to define-method-combination.
             (,gfsym ,generic-function-form)
             ,@(if need-vaslist-p
                   (mapcar (lambda (req)
                             `(,req (core:vaslist-pop ,vaslist-args)))
                           required-args)
                   nil))
         (declare (ignorable ,@required-args))
         (block ,block-name
           (tagbody
              ,(generate-node-or-outcome required-args (dtree-root dtree))
              ;; note: we need generate-node-or-outcome to run to fill *generate-outcomes*.
              ,@(generate-tagged-outcomes *generate-outcomes* block-name vaslist-args required-args)
            dispatch-miss
              ,@(if need-vaslist-p
                    `((core:vaslist-rewind ,vaslist-args)
                      (return-from ,block-name
                        (clos::dispatch-miss ,gfsym ,vaslist-args)))
                    `((return-from ,block-name
                        (clos::dispatch-miss-with-args ,gfsym ,@required-args))))))))))

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

(defun generate-tagged-outcomes (list block-name vaslist-arguments required-arguments)
  (mapcan (lambda (pair)
            (let ((outcome (car pair)) (tag (cdr pair)))
              (list tag
                    `(return-from ,block-name
                       ,(generate-outcome vaslist-arguments required-arguments outcome)))))
          list))


(defun generate-outcome (vaslist-arguments reqargs outcome)
  (cond ((optimized-slot-reader-p outcome)
         (generate-slot-reader reqargs outcome))
        ((optimized-slot-writer-p outcome)
         (generate-slot-writer reqargs outcome))
        ((fast-method-call-p outcome)
         (generate-fast-method-call reqargs outcome))
        ((effective-method-outcome-p outcome)
         (generate-effective-method-call vaslist-arguments (effective-method-outcome-function outcome)))
        ((function-outcome-p outcome)
         (generate-effective-method-call vaslist-arguments (function-outcome-function outcome)))
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
                      (clos:slot-definition-location
                       (or (find ',slot-name (clos:class-slots ,class))
                           (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                                  ',slot-name ,class)))))
                   (value (car location)))
              (if (eq value (core:unbound))
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
                       (clos:slot-definition-location
                        (or (find ',slot-name (clos:class-slots ,class))
                            (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                                   ',slot-name ,class))))))
                (rplaca location value)
                value)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defun generate-fast-method-call (arguments outcome)
  (let ((fmf (fast-method-call-function outcome)))
    `(funcall ,fmf ,@arguments)))

(defun generate-effective-method-call (arguments outcome)
  (if (consp outcome)
      ;; if the outcome is a cons, we magically assume it's a form to include entirely.
      ;; This happens from the COMPILE-TIME-DISCRIMINATOR machinery in clos/satiation.lsp.
      ;; FIXME: probably clean up. 
      `(progn
         (core:vaslist-rewind ,arguments)
         (let ((clos::.method-args. ,arguments))
           ,outcome))
      (let ((emf outcome))
        `(progn
           (core:vaslist-rewind ,arguments)
           (funcall ,emf ,arguments nil)))))

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

;;; draw a picture for debugging

(defun draw-node (fout node)
  (cond
    ((null node)
     #+(or)(let ((nodeid (gensym)))
	     (core:bformat fout "%s [shape = circle];%N" (string nodeid))
	     (core:bformat fout "%s [label = \"nil\"];%N" (string nodeid))
	     nodeid)
     nil)
    ((outcome-p node)
     (let ((nodeid (gensym)))
       (core:bformat fout "%s [shape=ellipse,label=\"HIT-%s\"];%N" (string nodeid) (core:object-address (ensure-outcome-or-error node)))
       nodeid))
    ((node-p node)
     (let* ((nodeid (gensym))
	    (idx 0)
	    (eql-entries (let (result)
                           (maphash (lambda (key value)
                                      (push (list (prog1 idx (incf idx))
                                                  (core:bformat nil "eql %s" key)
                                                  (draw-node fout value))
                                            result))
                                    (node-eql-specializers node))
                           result)
              #+(or)(loop for key being the hash-keys of (node-eql-specializers node)
                       using (hash-value value)
                       collect (list (prog1 idx (incf idx))
                                     (core:bformat nil "eql %s" key)
                                     (draw-node fout value))))
	    (class-entries
             (let (result)
               (dolist (x (node-class-specializers node))
                 (push (list (prog1 idx (incf idx))
                             (cond
                               ((single-p x)
                                (core:bformat nil "%s;%s" (range-first-stamp x)
                                              (safe-class-name (first (range-reversed-classes x)))))
                               ((range-p x)
                                (core:bformat nil "%s-%s;%s"
                                              (range-first-stamp x) (range-last-stamp x)
                                              (mapcar #'safe-class-name (reverse (range-reversed-classes x)))))
                               ((skip-p x)
                                (core:bformat nil "SKIP"))
                               (t (error "Unknown class-specializer type ~a" x)))
                             (draw-node fout (match-outcome x))) result))
               (let ((rev-res (reverse result)))
                 rev-res)))
	    (entries (append eql-entries class-entries)))
       (core:bformat fout "%s [shape = record, label = \"" (string nodeid))
       (let ((first-one t))
         (dolist (x entries)
           (if first-one
               (setq first-one nil)
               (core:bformat fout "| "))
           (core:bformat fout " <f%s> %s " (first x) (second x)))
         (core:bformat fout "\" ];%N"))
       (mapc (lambda (x)
               (core:bformat fout "%s:<f%s> -> %s;%N" (string nodeid) (first x) (string (third x))))
             entries)
       #+(or)(loop for x in entries
                do (core:bformat fout "~a:<f~a> -> ~a;%N" (string nodeid) (first x) (string (third x))))
       nodeid))
    (t (error "Handle draw-node for ~a" node )
       #+(or)(let ((id (gensym)))
	       (core:bformat fout "~a [ label = \"%s\"];%N" id node)
	       id))))


;;; ------------------------------------------------------------
;;;
;;; Generate a graphviz representation of a DTREE
;;;

(defmacro with-graph ((name fout &rest open-args) &body body)
  `(with-open-file (,fout ,@open-args)
     (core:bformat ,fout "digraph %s {%N" ,name)
     ,@body
     (core:bformat ,fout "}%N")))

(defun draw-graph (pathname dtree)
  (with-graph ("G" fout pathname :direction :output)
    (core:bformat fout "graph [ rankdir = \"LR\"];%N")
    (let ((startid (gensym)))
      (core:bformat fout "%s [ label = \"Start\", shape = diamond ];%N" (string startid))
      (core:bformat fout "%s -> %s;%N" (string startid) (string (draw-node fout (dtree-root dtree)))))))

(defun register-runtime-data (value table)
  "Register a integer index into the run time literal table for the discriminating function.
   This table stores eql specializers and effective method functions."
  (let ((existing-index (gethash value table)))
    (if existing-index
        existing-index
        (let ((index (prog1 *gf-data-id* (incf *gf-data-id*))))
          (setf (gethash value table) index)
          index))))

(defun lookup-eql-selector (eql-test)
  (let ((tagged-immediate (core:create-tagged-immediate-value-or-nil eql-test)))
    (if tagged-immediate
	(irc-int-to-ptr (jit-constant-i64 tagged-immediate) %t*%)
	(let ((eql-selector-id (gethash eql-test *eql-selectors*)))
	  (unless eql-selector-id
            (setf eql-selector-id (register-runtime-data eql-test *eql-selectors*)))
	  (let* ((eql-selector (irc-load (irc-gep *gf-data*
                                                  (list (jit-constant-size_t 0)
                                                        (jit-constant-size_t eql-selector-id))) "load")))
	    eql-selector)))))

;;; ------------------------------------------------------------
;;;
;;; Generate an LLVM Module that codes for the dispatch function
;;;   described by a DTREE.
;;;

(defun codegen-remaining-eql-tests (arguments cur-arg eql-tests eql-fail-branch)
  (cf-log "codegen-remaining-eql-tests%N")
  (if (null eql-tests)
      (irc-br eql-fail-branch)
      (let* ((eql-test (car eql-tests))
	     (eql-rest (cdr eql-tests))
	     (eql-spec (first eql-test))
	     (eql-outcome (second eql-test))
	     (eql-selector (lookup-eql-selector eql-spec))
	     (eq-cmp (irc-icmp-eq (argument-get arguments cur-arg) eql-selector))
	     (eql-branch (irc-basic-block-create "eql-branch"))
	     (else-eql-test (irc-basic-block-create "else-eql-test")))
	(irc-cond-br eq-cmp eql-branch else-eql-test)
	(irc-begin-block else-eql-test)
	(let* ((eql-i32 (irc-intrinsic-call-or-invoke "cc_eql" (list (argument-get arguments cur-arg) eql-selector)))
	       (eql-cmp (irc-icmp-eq eql-i32 (jit-constant-i32 1)))
	       (next-eql-test (irc-basic-block-create "next-eql-test")))
	  (irc-cond-br eql-cmp eql-branch next-eql-test)
	  (irc-begin-block next-eql-test)
	  (codegen-remaining-eql-tests arguments cur-arg eql-rest eql-fail-branch))
	(irc-begin-block eql-branch)
	(codegen-node-or-outcome arguments cur-arg eql-outcome))))

(defun codegen-eql-specializers (arguments cur-arg node)
  (cf-log "codegen-eql-specializers%N")
  (let ((eql-tests (let (result)
                     (maphash (lambda (key value)
                                (push (list key value) result))
                              (node-eql-specializers node))
                     result)
          #+(or)(loop for key being the hash-keys of (node-eql-specializers node)
                   using (hash-value value)
                   collect (list key value)))
	(on-to-class-specializers (irc-basic-block-create "on-to-class-specializers")))
    (codegen-remaining-eql-tests arguments cur-arg eql-tests on-to-class-specializers)
    (irc-begin-block on-to-class-specializers)))

;;; FIXME: It's possible we're not always ending the vaslist properly.
;;; We don't end it for the case of a full emf call. That might be okay if the call ends it, but
;;; I am uncertain if that is the case.
;;; If the call doesn't, we should only end the vaslist in the epilogue after the outcome is performed.
(defun end-vaslist-if-extant (arguments-holder)
  (let ((vaslist (argument-holder-methods-vaslist-t* arguments-holder)))
    (when vaslist
      ;; FIXME   The argument-holder-gf-args is a vaslist!!!! not a va_list - they are just coincident!!!
      (irc-intrinsic-call-or-invoke "cc_vaslist_end" (list vaslist)))))

(defun codegen-slot-reader (arguments outcome)
  (cf-log "entered codegen-slot-reader%N")
  (cf-log "arguments %s%N" arguments)
  (cf-log "outcome %s%N" outcome)
;;; If the (cdr outcome) is a fixnum then we can generate code to read the slot
;;;    directly and remhash the outcome from the *outcomes* hash table.
;;; otherwise create an entry for the outcome and call the slot reader.
  (let ((gf-data-id (register-runtime-data outcome *outcomes*))
	(effective-method-block (irc-basic-block-create "effective-method")))
    (irc-branch-to-and-begin-block effective-method-block)
    (end-vaslist-if-extant arguments) ; we don't use the vaslist.
    (let ((slot-read-info
            (irc-load (irc-gep *gf-data*
                               (list (jit-constant-size_t 0)
                                     (jit-constant-size_t gf-data-id))) "load")))
      (let* ((opt-data (optimized-slot-reader-index outcome))
             (retval (cond
                       ((fixnump opt-data)
                        (let ((value (irc-intrinsic-call-or-invoke #+debug-slot-accessors "cc_dispatch_slot_reader_index_debug"
                                                         #-debug-slot-accessors "cc_dispatch_slot_reader_index"
                                                         (list (jit-constant-size_t opt-data) (first (argument-holder-dispatch-arguments arguments))))))
                          (irc-intrinsic-call-or-invoke "cc_bound_or_error" (list slot-read-info (first (argument-holder-dispatch-arguments arguments)) value))))
                       ((consp opt-data)
                        (cf-log "Generating slot reader for cons at %s%N" (core:object-address opt-data))
                        (let* ((value (irc-intrinsic-call-or-invoke "cc_dispatch_slot_reader_cons" (list slot-read-info))))
                          (irc-intrinsic-call-or-invoke "cc_bound_or_error" (list slot-read-info (first (argument-holder-dispatch-arguments arguments)) value))))
                       (t (error "Unknown opt-data type ~a" opt-data)))))
        (irc-t*-result retval (argument-holder-return-value arguments))
        (irc-br (argument-holder-continue-after-dispatch arguments))))))

(defun codegen-slot-writer (arguments outcome)
  (cf-log "codegen-slot-writer%N")
;;; If the (optimized-slot-writer-data outcome) is a fixnum then we can generate code to read the slot
;;;    directly and remhash the outcome from the *outcomes* hash table.
;;; otherwise create an entry for the outcome and call the slot reader.
  (let ((gf-data-id (register-runtime-data outcome *outcomes*))
	(effective-method-block (irc-basic-block-create "effective-method")))
    (irc-branch-to-and-begin-block effective-method-block)
    (end-vaslist-if-extant arguments)
    (let ((slot-write-info
            (irc-load (irc-gep *gf-data*
                               (list (jit-constant-size_t 0)
                                     (jit-constant-size_t gf-data-id))) "write")))
      (let* ((opt-data (optimized-slot-writer-index outcome))
             (retval (cond
                       ((fixnump opt-data)
                        (irc-intrinsic-call-or-invoke #+debug-slot-accessors "cc_dispatch_slot_writer_index_debug"
                                            #-debug-slot-accessors "cc_dispatch_slot_writer_index"
                                            (list #+debug-slot-accessors effective-method
                                                  (first (argument-holder-dispatch-arguments arguments)) ; value
                                                  (jit-constant-size_t opt-data) ; index
                                                  (second (argument-holder-dispatch-arguments arguments))))) ; instance
                       ((consp opt-data)
                        (let ()
                          (irc-intrinsic-call-or-invoke "cc_dispatch_slot_writer_cons" (list (first (argument-holder-dispatch-arguments arguments)) ; falue
                                                                                             slot-write-info))))
                       (t (error "Unknown opt-data ~a" opt-data)))))
        (irc-t*-result retval (argument-holder-return-value arguments))
        (irc-br (argument-holder-continue-after-dispatch arguments))))))

(defun codegen-fast-method-call (arguments outcome)
  (cf-log "codegen-fast-method-call%N")
  (let* ((fmf (fast-method-call-function outcome))
         (gf-data-id (register-runtime-data fmf *outcomes*))
         (effective-method-block (irc-basic-block-create "effective-method")))
    (irc-branch-to-and-begin-block effective-method-block)
    (end-vaslist-if-extant arguments)
    (let* ((fmf (irc-load (irc-gep *gf-data*
                                   (list (jit-constant-size_t 0)
                                         (jit-constant-size_t gf-data-id)))
                          "load"))
           ;; what we're doing here is duplicating irc-funcall, except passing a function the actual LLVM-level
           ;; arguments passed to the dispatcher (the -register-arguments).
           (entry-point (irc-calculate-entry fmf))
           (result-in-registers (irc-call-or-invoke
                                 entry-point
                                 ;; We have to pass the correct closure.
                                 ;; Remember that the arguments are closure,nargs,arg1,arg2,arg3,arg4,...
                                 ;; This whole thing could use some major cleanup (FIXME).
                                 (list* fmf (rest (argument-holder-register-arguments arguments)))
                                 *current-unwind-landing-pad-dest* "fast-method-call")))
      (irc-tmv-result result-in-registers (argument-holder-return-value arguments))
      (irc-br (argument-holder-continue-after-dispatch arguments)))))

(defun codegen-effective-method-call (arguments outcome)
  (cf-log "codegen-effective-method-call %s%N" outcome)
  (let ((gf-data-id (register-runtime-data outcome *outcomes*))
	(effective-method-block (irc-basic-block-create "effective-method")))
    (irc-branch-to-and-begin-block effective-method-block)
    (let ((effective-method
            (irc-load (irc-gep *gf-data*
                               (list (jit-constant-size_t 0)
                                     (jit-constant-size_t gf-data-id))) "load")))
      (debug-pointer (irc-ptr-to-int
                      (irc-gep *gf-data*
                               (list (jit-constant-size_t 0)
                                     (jit-constant-size_t gf-data-id))) %uintptr_t%))
      (debug-call "debugPointer" (list (irc-bit-cast effective-method %i8*%)))
      ;; This is where I could insert the slot reader if the effective method wraps a single accessor
      ;;
      (debug-dispatch effective-method)
      (irc-funcall (argument-holder-return-value arguments)
                   effective-method
                   (list (argument-holder-methods-vaslist-t* arguments)
                         (irc-intrinsic "cc_fastgf_nil")))
      (irc-br (argument-holder-continue-after-dispatch arguments)))))

(defun codegen-outcome (arguments node)
  (cf-log "codegen-outcome%N")
  ;; The effective method will be found in a slot in the modules *gf-data* array
  ;;    the slot index will be in gf-data-id
  (let ((outcome (ensure-outcome-or-error node)))
    #+(or)(when *log-gf*
            (core:bformat *log-gf* "About to codegen-outcome -> %s%N" outcome))
    (cond
      ((optimized-slot-reader-p outcome)
       (codegen-slot-reader arguments outcome))
      ((optimized-slot-writer-p outcome)
       (codegen-slot-writer arguments outcome))
      ((fast-method-call-p outcome)
       (codegen-fast-method-call arguments outcome))
      ((effective-method-outcome-p outcome)
       (codegen-effective-method-call arguments (effective-method-outcome-function outcome)))
      ((function-outcome-p outcome) ; method-function and no-required-method. maybe change to be cleaner?
       (codegen-effective-method-call arguments (function-outcome-function outcome)))
      (t (error "BUG: Bad thing to be an outcome: ~a" outcome)))))

(defun codegen-class-binary-search (arguments cur-arg matches stamp-var)
  (cf-log "codegen-class-binary-search%N")
  (cond
    ((null matches)
     (irc-br (argument-holder-miss-basic-block arguments)))
    ((= (length matches) 1)
     (let ((match (car matches)))
       (if (single-p match)
	   (let ((cmpeq (irc-icmp-eq stamp-var (jit-constant-i64 (range-first-stamp match)) "eq"))
		 (true-branch (irc-basic-block-create "match"))
		 (false-branch (irc-basic-block-create "cont")))
	     (irc-cond-br cmpeq true-branch false-branch)
	     (irc-begin-block true-branch)
	     (codegen-node-or-outcome arguments cur-arg (range-outcome match))
	     (irc-begin-block false-branch)
	     (irc-br (argument-holder-miss-basic-block arguments)))
	   (let ((ge-first-branch (irc-basic-block-create "gefirst"))
		 (le-last-branch (irc-basic-block-create "lelast"))
		 (miss-branch (argument-holder-miss-basic-block arguments))
		 (cmpge (irc-icmp-sge stamp-var (jit-constant-i64 (range-first-stamp match)) "ge")))
	     (irc-cond-br cmpge ge-first-branch miss-branch)
	     (irc-begin-block ge-first-branch)
	     (let ((cmple (irc-icmp-sle stamp-var (jit-constant-i64 (range-last-stamp match)) "le")))
	       (irc-cond-br cmple le-last-branch miss-branch)
	       (irc-begin-block le-last-branch)
	       (codegen-node-or-outcome arguments cur-arg (match-outcome match)))))))
    (t
     (let* ((len-div-2 (floor (length matches) 2))
	    (left-matches (subseq matches 0 len-div-2))
	    (right-matches (subseq matches len-div-2))
	    (right-head (car right-matches))
	    (right-stamp (range-first-stamp right-head)))
       (let ((lt-branch (irc-basic-block-create "lt-branch"))
	     (gte-branch (irc-basic-block-create "gte-branch"))
	     (cmplt (irc-icmp-slt stamp-var (jit-constant-i64 right-stamp) "lt")))
	 (irc-cond-br cmplt lt-branch gte-branch)
	 (irc-begin-block lt-branch)
	 (codegen-class-binary-search arguments cur-arg left-matches stamp-var)
	 (irc-begin-block gte-branch)
	 (codegen-class-binary-search arguments cur-arg right-matches stamp-var))))))

(defun codegen-arg-stamp (arguments cur-arg)
  (cf-log "codegen-arg-stamp%N")
  "Return a uintptr_t llvm::Value that contains the stamp for this object"
  ;; First check the tag
  (let ((stamp (irc-intrinsic-call-or-invoke "cc_read_stamp" (list (irc-bit-cast (argument-get arguments cur-arg)  %i8*% "header*")))))
    (debug-stamp stamp)
    stamp))

(defun codegen-class-specializers (arguments cur-arg node)
  (cf-log "entered codegen-class-specializer%N")
      (let ((arg-stamp (codegen-arg-stamp arguments cur-arg)))
        (codegen-class-binary-search arguments cur-arg (node-class-specializers node) arg-stamp)))

(defun codegen-skip-node (arguments cur-arg node)
  (cf-log "entered codegen-skip-node%N")
  ;;;
  ;;; Here I could do an optimization.
  ;;;   If it's skip-nodes all the way to the outcome
  ;;;    - then just do the outcome
  (let ((skip-node (first (node-class-specializers node))))
    (codegen-node-or-outcome arguments cur-arg (skip-outcome skip-node))))

(defun codegen-node (arguments cur-arg node)
  (cf-log "entered codegen-node%N")
  (let ((cur-arg (1+ cur-arg)))
    (debug-call "debugPointer" (list (irc-bit-cast (argument-get arguments cur-arg) %i8*%)))
    (if (skip-node-p node)
        (codegen-skip-node arguments cur-arg node)
        (progn
          (codegen-eql-specializers arguments cur-arg node)
          (codegen-class-specializers arguments cur-arg node)))))

(defun codegen-node-or-outcome (arguments cur-arg node-or-outcome)
  (cf-log "entered codegen-node-or-outcome%N")
  (if (outcome-p node-or-outcome)
      (codegen-outcome arguments node-or-outcome)
      (codegen-node arguments cur-arg node-or-outcome)))

;;; --------------------------------------------------
;;;
;;; Debugging a generic function dispatcher
;;;
(defun debug-save-dispatcher (module &optional (output-path #P"/tmp/dispatcher.ll"))
  "Save everything about the generic function so that it can be saved to a file and then edited and re-installed"
  (when (wild-pathname-p output-path)
    (setf output-path (pathname (substitute #\_ #\* (namestring output-path)))))
  (let ((fout (open output-path :direction :output)))
    (unwind-protect (llvm-sys:dump-module module fout)
      (close fout))))

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

(defun optimized-call-history (call-history specializer-profile)
  (let* ((specializer-length (let ((pos (position-if #'identity specializer-profile :from-end t)))
                               (if pos
                                   (1+ pos)
                                   0)))
         (profiled (make-hash-table :test #'equalp)))
    (unless (every #'consp call-history)
      (error "The call history not an alist: ~a" call-history))
    (unless specializer-profile
      (error "The specializer-profile is NIL"))
    (dolist (entry call-history)
      (let ((key (car entry))
            (outcome (cdr entry))
            (new-key (make-array specializer-length :initial-element nil)))
        (dotimes (i specializer-length)
          (setf (svref new-key i)
                (if (svref specializer-profile i) (svref key i) nil)))
        (setf (gethash new-key profiled) outcome)))
    (let ((res nil))
      (maphash (lambda (k v) (push (cons k v) res)) profiled)
      res)))

(defun call-history-needs-vaslist-p (call-history)
  ;; Functions, i.e. method functions or full EMFs, are the only things that need vaslists.
  ;; Debugging note: If this is not computed correctly, you'll get an error like
  ;; type-error: datum NIL, expected type LLVM-SYS:VALUE
  ;; in code generation - because the vaslist holder will be nil instead of an llvm value.
  (mapc (lambda (entry)
          (when (or (effective-method-outcome-p (cdr entry))
                    (function-outcome-p (cdr entry)))
            (return-from call-history-needs-vaslist-p t)))
        call-history)
  nil)

;;; Determine if any of the effective methods need the full method arguments list
;;; If any of them do - then we have to allocate space for a vaslist 
(defun analyze-generic-function-make-arguments (generic-function register-arguments debug-on miss-basic-block)
  (let* ((call-history (clos:generic-function-call-history generic-function))
         (specializer-profile (clos:generic-function-specializer-profile generic-function))
         (last-specializer-index (position t specializer-profile :from-end t))
         (dispatch-needs-va-list-when-registers-are-exhausted (when last-specializer-index (>= last-specializer-index +args-in-registers+)))
         (effective-methods-need-full-method-arguments (call-history-needs-vaslist-p call-history)))
    ;; Check if there are any specialized arguments after the
    ;; register arguments will run out.
    ;; The debug frame needs the vaslist and the vaslist needs the va-list
    ;; so define these variables with those dependencies.
    (let* ((need-va-list (or dispatch-needs-va-list-when-registers-are-exhausted
                             effective-methods-need-full-method-arguments
                             debug-on))
           (need-vaslist (or effective-methods-need-full-method-arguments
                             debug-on))
           (closure (first register-arguments))
           (number-of-arguments-passed (second register-arguments))
           (return-value (alloca-return "return-value"))
           (continue-after-dispatch (irc-basic-block-create "continue-after-dispatch"))
           (va-list* (let ((va-list* (alloca-va_list "dispatch-va_list*")))
                       (when need-va-list
                         (irc-intrinsic-call-or-invoke "llvm.va_start" (list (irc-bit-cast va-list* %i8*% "va-list*-i8*"))))
                       va-list*))
           (register-save-area* (irc-register-save-area :label "reg-save-area*"))
           (vaslist* (alloca-vaslist :label "vaslist*"))
           (vaslist-t* (when need-vaslist
                         (maybe-spill-to-register-save-area register-arguments register-save-area*)
                         (irc-intrinsic "cc_rewind_vaslist" vaslist* va-list* register-save-area*))))
      ;; We have va-list* and register-arguments for the arguments
      ;; We also have specializer-profile - now we can gather up the arguments we need to dispatch on.
      (flet ((next-argument (idx dispatch-register-arguments)
               (if (>= idx +args-in-registers+)
                   (irc-va_arg va-list* %t*%)
                   (elt dispatch-register-arguments idx))))
        (let* ((dispatch-register-arguments (cddr register-arguments))
               dispatch-args)
          (when last-specializer-index
            (dotimes (idx (1+ last-specializer-index))
              (let ((arg (next-argument idx dispatch-register-arguments)))
                (push arg dispatch-args))))
          (when va-list* (irc-intrinsic-call-or-invoke "llvm.va_end" (list (irc-pointer-cast va-list* %i8*%))))
          (make-argument-holder :generic-function generic-function
                                :return-value return-value
                                :continue-after-dispatch continue-after-dispatch
                                :dispatch-arguments (nreverse dispatch-args)
                                :register-arguments register-arguments
                                :register-save-area* register-save-area*
                                :remaining-register-arguments (cddr register-arguments)
                                :dispatch-va-list* va-list*
                                :vaslist* vaslist*
                                :methods-vaslist-t* vaslist-t*
                                :miss-basic-block miss-basic-block))))))


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

(defun calculate-dtree (raw-call-history specializer-profile)
  (let ((call-history (optimized-call-history raw-call-history specializer-profile)))
    (let ((dt (make-dtree)))
      (cond
        (call-history (dtree-add-call-history dt call-history))
        (raw-call-history
         (dtree-add-call-history dt (list (cons #() (cdr (car raw-call-history))))))
        (t (error "codegen-dispatcher was called with an empty call-history - no dispatcher can be generated")))
      (optimize-node-and-children (dtree-root dt))
      dt)))

(defun codegen-dispatcher-from-dtree (generic-function dtree &key (generic-function-name "discriminator") output-path log-gf (debug-on t debug-on-p))
  (let ((debug-on (if debug-on-p
                      debug-on
                      (core:get-funcallable-instance-debug-on generic-function)))
        (module (create-run-time-module-for-compile)))
    #+(or)(unless call-history
            (core:bformat t "codegen-dispatcher %s  optimized-call-history -> %s%N" generic-function-name call-history)
            (core:bformat t "  raw-call-history -> %s%N" raw-call-history)
            (core:bformat t "  specializer-profile -> %s%N" specializer-profile))
    (with-module (:module module
                  :optimize nil)
      (with-source-pathnames (:source-pathname nil)
        (let* ((*current-function-name* (jit-function-name generic-function-name))
               (*gv-current-function-name* (module-make-global-string *current-function-name* "fn-name"))
               (_ (cf-log "codegen-dispatcher-from-dtree dispatcher name %s generic function name %s%N" *current-function-name* generic-function-name))
               (disp-fn (irc-simple-function-create *current-function-name*
                                                    %fn-registers-prototype% #| was %fn-gf% |#
                                                    'llvm-sys::External-linkage
                                                    module
                                                    :argument-names +fn-registers-prototype-argument-names+)))
          ;;(1) Create a function with a regular signature
          ;;(2) Allocate space for a va_list and copy the va_list passed into it.
          ;;(3) compile the dispatch function to llvm-ir refering to the eql specializers and stamps and
          ;;      the va_list passed.
          ;;(4) Reach an outcome and either call the effective method with the saved va_list
          ;;      or call the miss function with the saved va_list
          (let* ((irbuilder-alloca (llvm-sys:make-irbuilder *llvm-context*))
                 (irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
                 (*irbuilder-function-alloca* irbuilder-alloca)
                 (*irbuilder-function-body* irbuilder-body)
                 (*current-function* disp-fn)
                 (*gf-data* 
                   (llvm-sys:make-global-variable module
                                                  cmp:%t*[DUMMY]% ; type
                                                  nil ; isConstant
                                                  'llvm-sys:internal-linkage
                                                  (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                                  ;; nil ; initializer
                                                  (next-value-table-holder-name "dummy")))
                 (*gcroots-in-module* 
                   (llvm-sys:make-global-variable module
                                                  cmp:%gcroots-in-module% ; type
                                                  nil ; isConstant
                                                  'llvm-sys:internal-linkage
                                                  (llvm-sys:undef-value-get cmp:%gcroots-in-module%)
                                                  ;; nil ; initializer
                                                  "GCRootsHolder"))
                 (*gf-data-id* 0)
                 (*message-counter* 0)
                 (*eql-selectors* (make-hash-table :test #'eql))
                 (*outcomes* (make-hash-table))
                 (entry-bb (irc-basic-block-create "entry" disp-fn))
                 (*bad-tag-bb* (irc-basic-block-create "bad-tag" disp-fn))
                 (register-arguments (llvm-sys:get-argument-list disp-fn))
                 (gf (first register-arguments))
                 (num-args (second register-arguments)))
            (cmp:irc-set-insert-point-basic-block entry-bb irbuilder-alloca)
            (let ((body-bb (irc-basic-block-create "body" disp-fn))
                  (miss-bb (irc-basic-block-create "miss" disp-fn)))
              (irc-set-insert-point-basic-block body-bb irbuilder-body)
              (with-irbuilder (irbuilder-body)
                ;; Analyze the call history to see if there are optimizations that we can
                ;; use when doing gf dispatch.
                ;;  (1) If none of the methods need the arguments in a vaslist - then we
                ;;      don't need to allocate or rewind a vaslist.
                ;;  (2) If there are more required arguments than the number of register arguments
                ;;      then we need to allocate a va_list to use when we run out of register arguments.
                ;; Setup exception handling and cleanup landing pad
                (let* ((arguments (analyze-generic-function-make-arguments generic-function register-arguments debug-on miss-bb)))
                  ;; This will ALWAYS spill the arguments.  If we have simple accessors - we don't want to do this.
                  (maybe-spill-to-register-save-area
                   (argument-holder-register-arguments arguments)
                   (argument-holder-register-save-area* arguments))
                  (flet ((codegen-rest-of-dispatcher ()
                           (with-irbuilder (*irbuilder-function-alloca*)
                             (irc-br body-bb))
                           (irc-begin-block *bad-tag-bb*)
                           (irc-intrinsic-call-or-invoke "cc_bad_tag" (list gf))
                           (irc-unreachable)
                           (irc-begin-block miss-bb)
                           (let* ((vaslist-t* (or (argument-holder-methods-vaslist-t* arguments)
                                                  (let ((vaslist* (argument-holder-vaslist* arguments)))
                                                    (irc-intrinsic-call-or-invoke
                                                     "llvm.va_start"
                                                     (list
                                                      (irc-bit-cast
                                                       (argument-holder-dispatch-va-list* arguments)
                                                       %i8*%)))
                                                    #+(or)(maybe-spill-to-register-save-area
                                                           (argument-holder-register-arguments arguments)
                                                           (argument-holder-register-save-area* arguments))
                                                    (prog1
                                                        (irc-intrinsic
                                                         "cc_rewind_vaslist"
                                                         vaslist*
                                                         (argument-holder-dispatch-va-list* arguments)
                                                         (argument-holder-register-save-area* arguments))
                                                      (irc-intrinsic-call-or-invoke
                                                       "llvm.va_end"
                                                       (list
                                                        (irc-bit-cast
                                                         (argument-holder-dispatch-va-list* arguments)
                                                         %i8*%))))))))
                             (irc-simple-store (irc-intrinsic "cc_dispatch_miss" gf vaslist-t*)
                                               (argument-holder-return-value arguments))
                             (argument-holder-return-value arguments))
                           (irc-br (argument-holder-continue-after-dispatch arguments))))
                    (multiple-value-bind (min max)
                        (clos:generic-function-min-max-args generic-function)
                      (with-landing-pad nil
                        (unless (zerop min)
                          (compile-error-if-not-enough-arguments min num-args))
                        (unless (null max)
                          (compile-error-if-too-many-arguments max num-args))
                        (codegen-node-or-outcome arguments -1 (dtree-root dtree))
                        (codegen-rest-of-dispatcher)
                        (irc-begin-block (argument-holder-continue-after-dispatch arguments)))))
                  (irc-ret (irc-load (argument-holder-return-value arguments))))))
            (let* ((array-type (llvm-sys:array-type-get cmp:%t*% *gf-data-id*))
                   (correct-size-holder (llvm-sys:make-global-variable module
                                                                       array-type
                                                                       nil ; isConstant
                                                                       'llvm-sys:internal-linkage
                                                                       (llvm-sys:undef-value-get array-type)
                                                                       (bformat nil "CONSTANTS-%d" (increment-dispatcher-count))))
                   (bitcast-correct-size-holder (irc-bit-cast correct-size-holder %t*[DUMMY]*% "bitcast-table")))
              (multiple-value-bind (startup-fn shutdown-fn)
                  (codegen-startup-shutdown module *gcroots-in-module* correct-size-holder *gf-data-id* nil)
                (llvm-sys:replace-all-uses-with *gf-data* bitcast-correct-size-holder)
                (llvm-sys:erase-from-parent *gf-data*)
                #+debug-cmpgf(progn
                               (core:bformat t "Dumping the module from codegen-dispatcher%N")
                               (llvm-sys:dump-module module))
                (let ((sorted-roots (gather-sorted-outcomes *eql-selectors* *outcomes*)))
                  (when output-path
                    (debug-save-dispatcher module output-path))
                  (jit-add-module-return-dispatch-function
                   module disp-fn startup-fn shutdown-fn sorted-roots
                   :output-path output-path
                   ))))))))))

(defvar *fastgf-use-compiler* nil)
(defvar *fastgf-timer-start*)
(defun codegen-dispatcher (raw-call-history specializer-profile generic-function
                           &rest args &key generic-function-name output-path log-gf)
  (let* ((*log-gf* log-gf)
         (*fastgf-timer-start* (get-internal-real-time))
         (dtree (calculate-dtree raw-call-history specializer-profile)))
    (unwind-protect
         (if *fastgf-use-compiler*
             (apply 'codegen-dispatcher-from-dtree generic-function dtree args)
             (core:make-dtree-interpreter dtree))
      (let ((delta-seconds (/ (float (- (get-internal-real-time) *fastgf-timer-start*) 1d0)
                              internal-time-units-per-second)))
        (clos:generic-function-increment-compilations generic-function)
        (gctools:accumulate-discriminating-function-compilation-seconds delta-seconds)))))

#+(or)
(defun codegen-dispatcher (raw-call-history specializer-profile generic-function
                           &rest args &key generic-function-name output-path log-gf)
  (let* ((*log-gf* log-gf)
         (dtree (calculate-dtree raw-call-history specializer-profile)))
    (increment-dispatcher-count)
    (clos:generic-function-increment-compilations generic-function)
    (bclasp-compile nil (generate-dispatcher-from-dtree
                         generic-function dtree
                         :generic-function-name generic-function-name))))

(export '(make-dtree
	  dtree-add-call-history
	  draw-graph
	  codegen-dispatcher))



(defun disassemble-fastgf (generic-function)
  (if (clos:generic-function-call-history generic-function)
      (let* ((*save-module-for-disassemble* t)
             (*saved-module-from-clasp-jit* nil)
             (call-history (clos:generic-function-call-history generic-function))
             (specializer-profile (clos:generic-function-specializer-profile generic-function))
             (dispatcher (codegen-dispatcher call-history specializer-profile generic-function :generic-function-name 'disassemble))
             (module cmp:*saved-module-from-clasp-jit*))
        (if module
            (llvm-sys:dump-module module)
            (core:bformat t "Could not obtain module for disassemble of generic-function %s dispatcher -> %s%N" generic-function dispatcher))))
  (core:bformat t "The dispatcher cannot be built because there is no call history%N"))

(defun generate-dot-file (generic-function output)
  (let* ((raw-call-history (clos:generic-function-call-history generic-function))
         (specializer-profile (clos:generic-function-specializer-profile generic-function))
         (call-history (optimized-call-history raw-call-history specializer-profile))
         (dispatch-tree (let ((dt (make-dtree)))
                          (dtree-add-call-history dt call-history)
                          dt)))
    (cmp::draw-graph (namestring output) dispatch-tree)))

(defun graph-fastgf-dispatch-function (generic-function)
  (generate-dot-file generic-function "/tmp/dispatch.dot")
  (ext:system "/usr/local/bin/dot -Tpdf -o /tmp/dispatch.pdf /tmp/dispatch.dot")
  (sleep 0.2)
  (ext:system "open /tmp/dispatch.pdf"))
  

(export '(generate-dot-file graph-fastgf-dispatch-function disassemble-fastgf))

