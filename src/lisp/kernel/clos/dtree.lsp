(in-package "CLOS")

;;;; A DTREE (dispatch- or discriminating- tree) describes the discriminating
;;;; part of a discriminating function, in fastgf's stamp terms. This means it's
;;;; a tree of nodes representing discriminations. The leaves are OUTCOMES, which
;;;; describe effective methods.

;;; We use vectors instead of real structures because we're in the middle of CLOS.
;;; This is also why EQL specializers are conses instead of themselves, etc.

;;; WARNING! The structures here are known intimately in C++ for the dtree interpreter.
;;; Make sure funcallableInstance.h agrees with these definitions if you change anything.

(defstruct (outcome (:type vector) :named))
(defstruct (optimized-slot-reader (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (optimized-slot-writer (:type vector) (:include outcome) :named)
  index slot-name method class)
(defstruct (fast-method-call (:type vector) (:include outcome) :named) function)
;; see closfastgf.lsp's find-existing-emf for use of applicable-methods slot
(defstruct (effective-method-outcome (:type vector) (:include outcome) :named)
  applicable-methods form function)

(defstruct (match (:type vector) :named) outcome)
(defstruct (range (:include match) (:type vector) :named) first-stamp last-stamp reversed-classes)
(defstruct (skip (:include match) (:type vector) :named))
(defstruct (node (:type vector) :named) (eql-specializers (make-hash-table :test #'eql) :type hash-table)
           (class-specializers nil :type list)
           (interpreter nil :type vector))

(defstruct (dtree (:type vector) :named) root)

;;; Miscellaneous stuff

(defun eql-specializer-p (spec)
  "Return t if the spec is an eql specializer - they are represented as CONS cells
   with the value in the CAR"
  (consp spec))
(defun eql-specializer-value (spec)
  (car spec))

(defun single-p (match)
  (and (range-p match)
       (= (range-first-stamp match) (range-last-stamp match))))

(defun ensure-outcome-or-error (obj)
  (unless (outcome-p obj) (error "~s is not an outcome" obj))
  obj)

(defun ensure-outcome (argument-index specs goal)
  (if (>= argument-index (length specs))
      (ensure-outcome-or-error goal)
      (let ((node (make-node)))
        (node-add node (svref specs argument-index) (1+ argument-index) specs goal)
        node)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Construct a DTREE from a call history.

;;; Optimize a call history by removing unspecialized parameters from keys.
;;; Note this is different from the optimize-node stuff below.
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

;;; Degenerate range node with only one stamp are useful to create/note.
(defun make-stamp-matcher (&key stamp class outcome)
  (make-range :reversed-classes (list class) :outcome outcome :first-stamp stamp :last-stamp stamp))

(defun node-class-add (node spec argument-index specializers goal)
  (or (<= argument-index (length specializers))
      (error "Overflow in argument-index ~a must be <= ~a" argument-index (length specializers)))
  (if spec
      (let* ((stamp (cond
                      ((classp spec) (core:class-stamp-for-instances spec))
                      ((symbolp spec) (core:class-stamp-for-instances (find-class spec)))
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

(defun node-add (node spec argument-index specializers goal)
  (if (eql-specializer-p spec)
      (node-eql-add node spec argument-index specializers goal)
      (node-class-add node spec argument-index specializers goal)))

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

#+debug-fastgf
(defun verify-node-class-specializers-sorted-p (specializers)
  (let ((working (first specializers)))
    (dolist (next (rest specializers))
      (unless (<= (range-first-stamp working) (range-last-stamp working))
        (return-from verify-node-class-specializers-sorted-p nil))
      (if (< (range-last-stamp working) (range-first-stamp next))
          (setf working next)
          (return-from verify-node-class-specializers-sorted-p nil))))
  t)

(defun skip-node-p (node)
  (let ((class-specializers (node-class-specializers node)))
    (and (= 1 (length class-specializers)) (skip-p (car class-specializers)))))

;;; Put in some extra data to help the dtree interpreter.
(defun prepare-node-for-interpreter (node)
  (let* ((specializers (node-class-specializers node))
         (layout (make-array (1+ (* 3 (length specializers)))))
         (first-spec (and specializers (elt specializers 0))))
    (cond
      ((null specializers)
       (setf (elt layout 0) 'clos:range)) ;; No 
      ((skip-p first-spec)
       (setf (elt layout 0) 'clos:skip)
       (setf (elt layout 1) (skip-outcome first-spec)))
      ((range-p first-spec)
       (setf (elt layout 0) 'clos:range)
       (dotimes (i (length specializers))
         (let ((spec (car specializers))
               (start (1+ (* i 3))))
           (setf (elt layout (+ start 0)) (range-outcome spec)
                 (elt layout (+ start 1)) (range-first-stamp spec)
                 (elt layout (+ start 2)) (range-last-stamp spec)))
         (setf specializers (cdr specializers))))
      (t (error "cannot prepare interpreter for ~s" specializers)))
    (setf (node-interpreter node) layout)))

(defun optimize-node (node)
  "Create a list from the argument list and merge matches
   that can be considered adjacent into a range object."
  (if (skip-node-p node)
      nil                   ; There is one class-specializer and
                                        ; it's a skip class-specializer do
                                        ; nothing but prepare for the interpreter.
      (optimize-node-with-class-specializers node))
  (prepare-node-for-interpreter node))

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

;;; Main entry point.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw a picture for debugging

(defun safe-class-name (class-designator)
  (cond
    ((symbolp class-designator) class-designator)
    ((classp class-designator) (class-name class-designator))
    (t (error "Illegal class-designator"))))

(defun draw-node (fout node)
  (cond
    ((null node) nil)
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
                           result))
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
       nodeid))
    (t (error "Handle draw-node for ~a" node ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate a graphviz representation of a dtree

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

(defun generate-dot-file (generic-function output)
  (let* ((raw-call-history (generic-function-call-history generic-function))
         (specializer-profile (generic-function-specializer-profile generic-function))
         (call-history (optimized-call-history raw-call-history specializer-profile))
         (dispatch-tree (let ((dt (make-dtree)))
                          (dtree-add-call-history dt call-history)
                          dt)))
    (draw-graph (namestring output) dispatch-tree)))

(defun graph-fastgf-dispatch-function (generic-function)
  (generate-dot-file generic-function "/tmp/dispatch.dot")
  (ext:system "/usr/local/bin/dot -Tpdf -o /tmp/dispatch.pdf /tmp/dispatch.dot")
  (sleep 0.2)
  (ext:system "open /tmp/dispatch.pdf"))
