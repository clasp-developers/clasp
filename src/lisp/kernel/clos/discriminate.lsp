(in-package #:clos)

(defvar *insert-debug-code* nil)

#+(or)
(progn
  (defclass node () ())
  (defclass ntest (node)
    ((%paths :initarg :paths :reader ntest-paths)))
  (defun make-ntest (paths) (make-instance 'ntest :paths paths))
  (defun ntest-p (object) (typep object 'ntest))
  (defclass leaf (node)
    ((%form :initarg :form :reader leaf-form)))
  (defun make-leaf (form) (make-instance 'leaf :form form))
  (defun leaf-p (object) (typep object 'leaf)))

#-(or)
(progn
  (defstruct (ntest (:type vector) :named
                    (:constructor make-ntest (paths)))
    (paths nil :read-only t))
  (defstruct (leaf (:type vector) :named
                   (:constructor make-leaf (form)))
    (form nil :read-only t)))

;;; Passed from above through symbol-macrolet so that I can
;;; survive the debugging process.
(define-symbol-macro +gf-being-compiled+ nil)
(defun gf-being-compiled (&optional env)
  (values (macroexpand-1 '+gf-being-compiled+ env)))

(defun check-clause (clause spec-length seen-specs &optional name)
  (unless (and (consp clause)
               (typep (car clause) 'sequence)
               (leaf-p (cdr clause)))
    (error "Bad syntax for clause ~a~@[ while compiling ~a~]"
           clause name))
  (let ((specs (car clause)))
    (unless (= (length specs) spec-length)
      (error "Mismatch in length of specializers ~a~@[ while compiling ~a~]"
             specs name))
    (when (find specs seen-specs :test #'equal)
      (error "Duplicate specializer sequence ~a in ~a~@[ while compiling ~a~]"
             specs seen-specs name))
    specs))

;;; NOTE: We could probably store the unreduced tree
;;; instead of the call history.

(defun basic-ntree (clauses default spec-length &optional name)
  (if (null clauses)
      default
      ;; Verify things are hunky dory, then pass to %basic-tree.
      (loop for clause in clauses
            collect (check-clause clause spec-length seen-specs name)
              into seen-specs
            finally (return (%basic-tree clauses spec-length)))))

;; Each clause is a cons (specializer-sequence . leaf),
;; where the sequences all have the same lengths and there are
;; no duplicates.
(defun %basic-tree (clauses spec-length)
  (case spec-length
    ((0) ; degenerate
     ;; Just return the leaf. No dispatch.
     (cdr (first clauses)))
    ((1) ; nearly degenerate
     (loop for (specializers . leaf) in clauses
           for spec = (elt specializers 0)
           collect (cons spec leaf) into paths
           finally (return (make-ntest paths))))
    (t
     (loop for (specializers . leaf) in clauses
           for this = (elt specializers 0)
           for next = (subseq specializers 1)
           for new-clause = (cons next leaf)
           for existing-path = (assoc this paths)
           if existing-path
             ;; We checked for duplicates in basic-tree.
             do (push new-clause (cdr existing-path))
           else ; haven't seen this specializer yet.
           collect (cons this (list new-clause)) into paths
           finally
              (return
                (make-ntest
                 (loop with new-spec-length = (1- spec-length)
                       for (spec . clauses)
                         in paths
                       for test = (%basic-tree clauses
                                               new-spec-length)
                       collect (cons spec test))))))))

(defvar *reduction-table*)

(defun reduce-tree (tree)
  (let ((*reduction-table* (make-hash-table :test #'equal)))
    (reduce-node tree)))

#+(or)
(progn
  (defgeneric reduce-node (node))
  (defmethod reduce-node ((node leaf)) (reduce-leaf node))
  (defmethod reduce-node ((node ntest)) (reduce-test node)))

#-(or)
(progn
  (defun reduce-node (node)
    (cond ((leaf-p node) (reduce-leaf node))
          ((ntest-p node) (reduce-test node))
          (t (error "BUG: Not a node: ~a" node)))))

(defun reduce-leaf (leaf) leaf) ; nothing to be done.
(defun reduce-test (node)
  (loop for (spec . next) in (ntest-paths node)
        for rnext = (reduce-node next)
        collect (cons spec rnext) into new-paths
        finally
           (return
             (or (gethash new-paths *reduction-table*)
                 (setf (gethash new-paths *reduction-table*)
                       (make-ntest new-paths))))))

;;;

(defun partition (sequence &key (test #'eql) (key #'identity) (getter #'identity))
  (let ((test (core::coerce-fdesignator test))
        (key (core::coerce-fdesignator key))
        (getter (core::coerce-fdesignator getter)))
    (loop with result = nil
          for item in sequence
          for k = (funcall key item)
          for existing = (assoc k result :test test)
          if existing
            do (push (funcall getter item) (cdr existing))
          else
            do (push (list k (funcall getter item)) result)
          finally (return result))))

;;;

(defun rangify (list)
  (if (null list)
      nil
      (let ((list (sort (copy-list list) #'< :key #'car)))
        (loop with this = (cdr (first list))
              with this-left = (car (first list))
              with this-rite = this-left
              for (next . tag) in (rest list)
              if (and (= (1+ this-rite) next) (eql tag this))
                do (setf this-rite next)
              else collect (cons (cons this-left this-rite) this) into res
                   and do (setf this-left next
                                this-rite next
                                this tag)
              finally (return
                        (append res (list (cons (cons this-left this-rite)
                                                this))))))))

(defun gen-binary-search (form ranges default)
  (cond
    ((null ranges) `(go ,default))
    ((null (rest ranges))
     (let ((match (first ranges)))
       (if (= (caar match) (cdar match))
           `(if (cleavir-primop:fixnum-equal ,form ',(caar match))
                (go ,(cdr match))
                (go ,default))
           `(if (cleavir-primop:fixnum-not-greater ,form ',(cdar match))
                (if (cleavir-primop:fixnum-not-greater ',(caar match) ,form)
                    (go ,(cdr match))
                    (go ,default))
                (go ,default)))))
    (t (let* ((len-div-2 (floor (length ranges) 2))
              (left-ranges (subseq ranges 0 len-div-2))
              (right-ranges (subseq ranges len-div-2))
              (right-head (first right-ranges))
              (right-stamp (caar right-head)))
         `(if (cleavir-primop:fixnum-less ,form ',right-stamp)
              ,(gen-binary-search form left-ranges default)
              ,(gen-binary-search form right-ranges default))))))

(defmacro %fixnumcase (form default &rest clauses)
  (loop with bname = (gensym "FIXNUM-CASE")
        with default-tag = (gensym "DEFAULT")
        for (keys . body) in clauses
        for tag = (gensym)
        nconcing (list tag `(return-from ,bname (progn ,@body)))
          into tbody
        nconcing (loop for key in keys
                       collect (cons key tag))
          into dispatch
        finally
           (return
             `(core::local-block ,bname
                (core::local-tagbody
                   ,(gen-binary-search form (rangify dispatch)
                                       default-tag)
                   ,@tbody
                   ,default-tag
                   (return-from ,bname ,default))))))

(defmacro fixnumcase (form default &rest clauses)
  (let ((keyg (gensym "KEY")))
    `(let ((,keyg ,form))
       (%fixnumcase ,keyg ,default ,@clauses))))

(defvar *tag-tests* (llvm-sys:tag-tests))

(defun tag-spec-p (class) ; is CLASS one that's manifested as a tag test?
  (member (core:class-stamp-for-instances class) *tag-tests* :key #'second))

(defun tag-type (class)
  ;; This is essentially just class-name, but avoiding the generic function call.
  (case (first (find (core:class-stamp-for-instances class)  *tag-tests*
                     :key #'second))
    ((:fixnum-tag) 'fixnum)
    ((:single-float-tag) 'single-float)
    ((:character-tag) 'character)
    ((:cons-tag) 'cons)
    (t (error "BUG: Unknown tag class ~a" class))))

;;; These are fake eql specializers used throughout fastgf so that the
;;; eql specializer object can be read without a generic function call.
;;; (Note that a user could technically subclass eql-specializer and
;;;  get the slot a different location, so we can't just use a direct
;;;  instance reference.)
(defun fake-eql-specializer-p (spec) (consp spec))
(defun fake-eql-specializer-object (spec) (car spec))

;;; This is kind of a shitheap, but then, so is the underlying operation.
(defmacro %speccase (form default &rest clauses)
  (loop with bname = (gensym "SPECIALIZER-CASE")
        with stamp = (gensym "STAMP")
        with default-tag = (gensym "DEFAULT")
        with eql-specs
        with tag-specs
        with c++-specs
        with other-specs
        for clause in clauses
        for (specs . body) = clause
        for tag = (gensym)
        nconc (list tag `(return-from ,bname ,@body))
          into tbody
        do (loop for spec in specs
                 for pair = (cons spec tag)
                 do (cond #+(or)
                          ((eql-specializer-flag spec)
                           (push (cons (eql-specializer-object spec)
                                       tag)
                                 eql-specs))
                          ;; The fake eql specializers used by fastgf
                          #-(or)
                          ((fake-eql-specializer-p spec)
                           (push (cons (fake-eql-specializer-object spec) tag)
                                 eql-specs))
                          ((tag-spec-p spec)
                           (push (cons (tag-type spec) tag) tag-specs))
                          ((< (core:class-stamp-for-instances spec)
                              cmp:+c++-stamp-max+)
                           (push (cons (core:class-stamp-for-instances spec)
                                       tag)
                                 c++-specs))
                          (t
                           (push (cons (core:class-stamp-for-instances spec)
                                       tag)
                                 other-specs))))
        finally
           (return
             `(core::local-block ,bname
                (core::local-tagbody
                   (case ,form
                     ,@(loop for (tag . objects)
                               in (partition eql-specs :key #'cdr :getter #'car)
                             collect `((,@objects) (go ,tag)))
                     (otherwise
                      (cond ,@(loop for (type . tag) in tag-specs
                                    collect `((cleavir-primop:typeq ,form ,type)
                                              (go ,tag)))
                            ((cleavir-primop:typeq ,form core:general)
                             (let ((,stamp (core::header-stamp ,form)))
                               (let ((,stamp
                                       (core::header-stamp-case ,stamp
                                         (core::derivable-stamp ,form)
                                         (core::rack-stamp ,form)
                                         (core::wrapped-stamp ,form)
                                         (%fixnumcase ,stamp
                                             (go ,default-tag)
                                           ,@(loop for (tag . pairs)
                                                     in (partition c++-specs
                                                                   :key #'cdr
                                                                   :getter #'car)
                                                   collect `((,@pairs) (go ,tag)))))))
                                 (%fixnumcase ,stamp
                                     (go ,default-tag)
                                   ,@(loop for (tag . pairs)
                                             in (partition other-specs :key #'cdr
                                                           :getter #'car)
                                           collect `((,@pairs) (go ,tag)))))))
                            (t (go ,default-tag)))))
                   ;; back in the tagbody
                   ,@tbody
                   ,default-tag
                   (return-from ,bname ,default))))))

(defmacro speccase (form default &rest clauses)
  (let ((keyg (gensym "KEY")))
    `(let ((,keyg ,form))
       (%speccase ,keyg ,default ,@clauses))))

;;;

(defvar *codegen-map*)
(defvar *block-name*)
(defvar *default-tag*)

#+(or)
(progn
  (defgeneric node-code (node syms))
  (defmethod node-code ((node ntest) syms) (test-code node syms))
  (defmethod node-code ((node leaf) syms) (leaf-code node syms)))

#-(or)
(progn
  (defun node-code (node syms)
    (cond ((leaf-p node) (leaf-code node syms))
          ((ntest-p node) (test-code node syms))
          (t (error "BUG: Not a node: ~a" node)))))

(defun test-code (node syms)
  `(%speccase ,(pop syms)
     (go ,*default-tag*)
     ,@(loop for (next . specs) in (partition (ntest-paths node)
                                              :key #'cdr :getter #'car)
             collect `((,@specs) (go ,(node-tag next syms))))))

(defun leaf-code (node syms)
  (declare (ignore syms))
  `(return-from ,*block-name*
     ,(leaf-form node)))

(defun node-tag (node syms)
  (let ((existing (gethash node *codegen-map*)))
    (car
     (or existing
         (setf (gethash node *codegen-map*)
               (cons (gensym) (node-code node syms)))))))

(defun acycle-code (acycle default syms)
  (let ((*codegen-map* (make-hash-table))
        (*block-name* (gensym "DISCRIMINATION"))
        (*default-tag* (gensym "DEFAULT")))
    `(core::local-block ,*block-name*
       (core::local-tagbody
          (go ,(node-tag acycle syms))
          ,@(loop for (tag . code)
                    being each hash-value of *codegen-map*
                  collect tag
                  collect code)
          ,*default-tag*
          (return-from ,*block-name*
            ,(leaf-form default))))))

(defmacro discriminate ((&rest forms) default &rest clauses
                        &environment env)
  (let* ((syms (loop for form in forms collecting (gensym "OBJECT")))
         (slength (length forms))
         (default-leaf (make-leaf default))
         (clauses (loop for (key . body) in clauses
                        for leaf = (make-leaf `(progn ,@body))
                        nconcing (loop for spec-seq in key
                                       collect (cons spec-seq leaf))))
         (maybe-gf-name (gf-being-compiled env))
         (tree (basic-ntree clauses default-leaf slength maybe-gf-name))
         (acycle (reduce-tree tree))
         (body (acycle-code acycle default-leaf syms)))
    `(let (,@(mapcar #'list syms forms))
       ,body)))

;;;

(defun generate-outcome (reqargs need-vaslist-p outcome)
  (cond ((optimized-slot-reader-p outcome)
         (generate-slot-reader reqargs outcome))
        ((optimized-slot-writer-p outcome)
         (generate-slot-writer reqargs outcome))
        ((effective-method-outcome-p outcome)
         (generate-effective-method-call need-vaslist-p outcome))
        (t (error "BUG: Bad thing to be an outcome: ~a" outcome))))

(defun class-cell-form (slot-name class)
  `(load-time-value
    (slot-definition-location
     (or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
         (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                ',slot-name ,class)))))

(defun generate-slot-reader (arguments outcome)
  (let* ((location (optimized-slot-reader-index outcome))
         (slot-name (optimized-slot-reader-slot-name outcome))
         (class (optimized-slot-reader-class outcome))
         (valuef
           (cond ((fixnump location)
                  ;; instance location- easy
                  `(core:instance-ref ,(first arguments) ',location))
                 ((consp location)
                  ;; class location. we need to find the new cell at load time.
                  `(car ,(class-cell-form slot-name class)))
                 (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))
    `(let ((value ,valuef))
       (if (cleavir-primop:eq value (core:unbound))
           (slot-unbound ,class ,(first arguments) ',slot-name)
           value))))

(defun generate-slot-writer (arguments outcome)
  (let ((location (optimized-slot-writer-index outcome)))
    (cond ((fixnump location)
           `(si:instance-set ,(second arguments) ,location ,(first arguments)))
          ((consp location)
           ;; class location
           ;; Note we don't actually need the instance.
           `(rplaca ,(class-cell-form (optimized-slot-reader-slot-name outcome)
                                      (optimized-slot-reader-class outcome))
                    ,(first arguments)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defun generate-effective-method-call (need-vaslist-p outcome)
  (let ((form
          ;; if a form was provided, just throw it in.
          ;; Otherwise generate a function call.
          ;; NOTE: We use NIL to mean "no form provided".
          ;; Hypothetically the form could actually BE nil,
          ;; but I'm not holding my breath here- the backup is probably fine.
          (cond ((effective-method-outcome-form outcome))
                ((effective-method-outcome-function outcome)
                 `(apply
                   ,(effective-method-outcome-function outcome) .method-args.))
                (t (error "BUG: Outcome ~a is messed up" outcome)))))
    (if need-vaslist-p
        `(progn
           (core:vaslist-rewind .method-args.)
           ,form)
        form)))

;;;

(defun generate-discrimination (call-history specializer-profile
                                need-vaslist-p required-params miss)
  (let* ((part (partition call-history :key #'cdr :getter #'car
                                       :test #'outcome=))
         (bname (gensym "DISCRIMINATION"))
         (map (loop for (outcome) in part
                    collect (cons outcome (gensym "OUTCOME"))))
         (tbody (loop for (outcome . tag) in map
                      collect tag
                      collect `(return-from ,bname
                                 ,(generate-outcome
                                   required-params need-vaslist-p
                                   outcome)))))
    (flet ((collect-list (list)
             (loop for e in list for s across specializer-profile
                   when s collect e))
           (collect-vec (vec)
             (loop for e across vec for s across specializer-profile
                   when s collect e))
           (outcome-tag (outcome)
             (or (cdr (assoc outcome map))
                 (error "BUG: Inconsistent outcome map"))))
      `(core::local-block ,bname
         (core::local-tagbody
            (discriminate (,@(collect-list required-params))
                (return-from ,bname ,miss)
              ,@(loop for (outcome . specseqs) in part
                      ;; We can end up with duplicates due to
                      ;; the specializer profile cutting things out,
                      ;; but DISCRIMINATE will reject them.
                      collect `((,@(remove-duplicates
                                    (mapcar #'collect-vec specseqs)
                                    :test #'equal))
                                ,@(when *insert-debug-code*
                                    `((format t "~&Chose ~a~%" ',specseqs)))
                                (go ,(outcome-tag outcome)))))
            ,@tbody)))))

;;; This must stay coordinated with call-method in combin.lsp,
;;; and is therefore a bit KLUDGEy.
(defun emo-requires-vaslist-p (outcome)
  (let ((form (effective-method-outcome-form outcome)))
    (not (and (consp form)
              (eq (car form) 'call-method)
              (method-p (second form))
              (fast-method-function (second form))))))

(defun outcome-requires-vaslist-p (outcome)
  (cond ((effective-method-outcome-p outcome)
         (emo-requires-vaslist-p outcome))
        ((outcome-p outcome) nil)
        (t (error "BUG: Unknown outcome: ~a" outcome))))

(defun call-history-requires-vaslist-p (call-history)
  (loop for (ignore . outcome) in call-history
          thereis (outcome-requires-vaslist-p outcome)))

;;; We pass the list of required parameters to CALL-METHOD
;;; in this fashion.
(defun discriminator-required-arguments (&optional environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 '+discriminator-required-arguments+
                     environment)
    (if expanded
        (values expansion expanded)
        (values nil nil))))

(defun generate-discriminator-from-data
    (call-history specializer-profile generic-function-form nreq
     &key generic-function-name
       max-nargs (miss-operator 'dispatch-miss))
  (let ((need-vaslist-p (call-history-requires-vaslist-p call-history))
        (required-args (loop repeat nreq
                             collect (gensym "DISCRIMINATION-ARG"))))
    `(lambda ,(if need-vaslist-p
                  `(core:&va-rest .method-args.)
                  required-args)
       ,@(when generic-function-name
           `((declare (core:lambda-name ,generic-function-name))))
       ,@(when *insert-debug-code*
           `((format t "~&Entering ~a~%" ',generic-function-name)))
       (symbol-macrolet ((+discriminator-required-arguments+
                           (,@required-args))
                         (+gf-being-compiled+ ,generic-function-name))
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
           (let (,@(if need-vaslist-p
                       (mapcar (lambda (req)
                                 `(,req (core:vaslist-pop .method-args.)))
                               required-args)
                       nil))
             ,(generate-discrimination
               call-history specializer-profile
               need-vaslist-p required-args
               `(progn
                  ,@(when *insert-debug-code*
                      `((format t "~&Dispatch miss~%")))
                  ,(if need-vaslist-p
                       `(progn
                          (core:vaslist-rewind .method-args.)
                          (apply #',miss-operator .generic-function. .method-args.))
                       `(,miss-operator .generic-function. ,@required-args))))))))))

(defun safe-gf-specializer-profile (gf)
  (with-early-accessors (+standard-generic-function-slots+)
    (generic-function-specializer-profile gf)))

(defun generate-discriminator (generic-function)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (generate-discriminator-from-data
     (generic-function-call-history generic-function)
     (safe-gf-specializer-profile generic-function)
     generic-function min
     :max-nargs max
     :generic-function-name (core:function-name generic-function))))
