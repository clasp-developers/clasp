(in-package #:clos)

(defstruct (ntest (:constructor make-ntest (paths)))
  (paths nil :read-only t))
(defstruct (leaf (:constructor make-leaf (form)))
  (form nil :read-only t))

;;; Passed from above through symbol-macrolet so that I can
;;; survive the debugging process.
(define-symbol-macro +gf-being-compiled+ nil)
(defun gf-being-compiled (&optional env)
  (values (macroexpand-1 '+gf-being-compiled+ env)))

(defun check-clause (clause spec-length seen-specs &optional name)
  (declare (ignore seen-specs))
  (unless (and (consp clause)
               (typep (car clause) 'sequence)
               (leaf-p (cdr clause)))
    (error "Bad syntax for clause ~a~@[ while compiling ~a~]"
           clause name))
  (let ((specs (car clause)))
    (unless (= (length specs) spec-length)
      (error "Mismatch in length of specializers ~a~@[ while compiling ~a~]"
             specs name))
    #+(or)(when (find specs seen-specs :test #'equal)
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
            when (not (find (car clause) seen-specs :test #'equal))
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
           ;; We have to use EQUAL because of the fake eql
           ;; specializers (FIXME)
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

(defgeneric reduce-node (node))
(defmethod reduce-node ((node leaf)) node) ; nothing to be done.
(defmethod reduce-node ((node ntest))
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

(defvar *tag-tests* (llvm-sys:tag-tests))

(defun tag-spec-p (class) ; is CLASS one that's manifested as a tag test?
  (member (core:class-stamp-for-instances class) *tag-tests* :key #'second))

(defun tag-type-test (class)
  (or (fourth (find (core:class-stamp-for-instances class)  *tag-tests*
                    :key #'second))
      (error "BUG: Unknown tag class ~a" class)))

(defun casify-specs (stampf default-tag specs)
  `(case ,stampf
     ,@(loop for (tag . keys)
               in (partition specs :key #'cdr :getter #'car)
             collect `((,@keys) (go ,tag)))
     (otherwise (go ,default-tag))))

;;; This is kind of a shitheap, but then, so is the underlying operation.
(defmacro %speccase (form default &rest clauses)
  (loop with bname = (gensym "SPECIALIZER-CASE")
        with stamp = (gensym "STAMP")
        with default-tag = (gensym "DEFAULT")
        with eql-specs
        with tag-specs
        with header-specs
        with derivable-specs
        with wrapped-specs
        with rack-specs
        for clause in clauses
        for (specs . body) = clause
        for tag = (gensym)
        nconc (list tag `(return-from ,bname ,@body))
          into tbody
        do (loop for spec in specs
                 for pair = (cons spec tag)
                 do (cond ((eql-specializer-p spec)
                           (push (cons (eql-specializer-object spec)
                                       tag)
                                 eql-specs))
                          ((tag-spec-p spec)
                           (push (cons (tag-type-test spec) tag) tag-specs))
                          (t
                           (let* ((stamp (core:class-stamp-for-instances spec))
                                  (pair (cons stamp tag)))
                             (core::header-stamp-case stamp
                               (push pair derivable-specs)
                               (push pair rack-specs)
                               (push pair wrapped-specs)
                               (push pair header-specs))))))
        finally
           (let* ((header-case (casify-specs stamp default-tag header-specs))
                  (wrapped-case
                    (casify-specs `(core::wrapped-stamp ,form)
                                  default-tag wrapped-specs))
                  (derivable-case
                    (casify-specs `(core::derivable-stamp ,form)
                                  default-tag derivable-specs))
                  (rack-case
                    (casify-specs `(core::rack-stamp ,form)
                                  default-tag rack-specs)))
             (return
               `(block ,bname
                  (tagbody
                   (case ,form
                     ,@(loop for (tag . objects)
                               in (partition eql-specs :key #'cdr :getter #'car)
                             collect `((,@objects) (go ,tag)))
                     (otherwise
                      (cond ,@(loop for (test . tag) in tag-specs
                                     collect `((,test ,form) (go ,tag)))
                            ((core:generalp ,form)
                             (let ((,stamp (core::header-stamp ,form)))
                               (core::header-stamp-case ,stamp
                                 ,derivable-case ,rack-case
                                 ,wrapped-case ,header-case)))
                            (t (go ,default-tag)))))
                   ;; back in the tagbody
                   ,@tbody
                   ,default-tag
                   (return-from ,bname ,default)))))))

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
    `(block ,*block-name*
       (tagbody
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

(defun class-cell-form (slot-name class)
  `(load-time-value
    (slot-definition-location
     (or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
         (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                ',slot-name ,class)))))

(defmacro em-slot-read (location slot-name class &environment env)
  (let ((arguments (effective-method-parameters env)))
    (unless (>= (length arguments) 1)
      (error "BUG: SLOT-READ effective method has insufficient required parameters"))
    (let ((valuef
            (cond ((si:fixnump location)
                   ;; instance location- easy
                   `(core:instance-ref ,(first arguments) ',location))
                  ((consp location)
                   ;; class location. we need to find the new cell at load time.
                   `(car ,(class-cell-form slot-name class)))
                  (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))
      `(let ((value ,valuef))
         (if (eq value (core:unbound))
             (slot-unbound ,class ,(first arguments) ',slot-name)
             value)))))

(defmacro em-slot-write (location slot-name class &environment env)
  (let ((arguments (effective-method-parameters env)))
    (unless (>= (length arguments) 2)
      (error "BUG: SLOT-WRITE effective method has insufficient required parameters"))
    (cond ((si:fixnump location)
           `(setf (si:instance-ref ,(second arguments) ,location) ,(first arguments)))
          ((consp location)
           ;; class location
           ;; Note we don't actually need the instance.
           `(rplaca ,(class-cell-form slot-name class) ,(first arguments)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defgeneric generate-outcome (outcome))

(defmethod generate-outcome ((outcome optimized-slot-reader))
  `(em-slot-read ,(optimized-slot-accessor-index outcome)
                 ,(optimized-slot-accessor-slot-name outcome)
                 ,(optimized-slot-accessor-class outcome)))

(defmethod generate-outcome ((outcome optimized-slot-writer))
  `(em-slot-write ,(optimized-slot-accessor-index outcome)
                  ,(optimized-slot-accessor-slot-name outcome)
                  ,(optimized-slot-accessor-class outcome)))

;;; This can be T, meaning prefer the form, NIL, meaning prefer the function,
;;; or CL:REQUIRE, meaning use the form or signal an error if it's missing.
;;; Inlining effective methods means putting the result of
;;; compute-effective-method directly into the discriminating function.
;;; Pros:
;;; * No function call overhead
;;; * With some trickiness, discriminating function becomes dumpable
;;; Cons:
;;; * Noticeably higher compilation time
;;; The compilation time problem can be rather severe, as a dispatch miss
;;; results in compiling all the effective methods again.
;;; As such, we do not inline effective methods in the runtime.
;;; Satiated generic functions have to inline so they can be dumped, though.
(defvar *inline-effective-methods* nil)

;;; Apply a function to the effective method parameters.
;;; The MORE forms are put in front.
(defmacro em-apply (function &rest more &environment env)
  `(apply ,function ,@more ,@(effective-method-parameters env)))

(defmethod generate-outcome ((outcome effective-method-outcome))
  (let ((form (effective-method-outcome-form outcome))
        (function (effective-method-outcome-function outcome)))
    (when (and (eq *inline-effective-methods* 'cl:require)
               ;; NOTE: We use NIL to mean "no form provided".
               ;; Hypothetically the form could actually BE nil,
               ;; if the method combination returned it, but that's
               ;; probably not actually going to happen?
               (not form))
      (error "BUG: No form provided to inline effective method"))
    (if (or (and *inline-effective-methods* form)
            (not function))
        form
        `(em-apply ,function))))

;;;

(defun generate-discrimination (call-history specializer-profile
                                required-params miss)
  (let ((part (partition call-history :key #'cdr :getter #'car
                                      :test #'outcome=)))
    (flet ((collect-list (list)
             (loop for e in list for s across specializer-profile
                   when s collect e))
           (collect-vec (vec)
             (loop for e across vec for s across specializer-profile
                   when s collect e)))
      `(discriminate (,@(collect-list required-params))
           ,miss
         ,@(loop for (outcome . specseqs) in part
                 ;; We can end up with duplicates due to
                 ;; the specializer profile cutting things out,
                 ;; but DISCRIMINATE will reject them.
                 collect `((,@(remove-duplicates
                               (mapcar #'collect-vec specseqs)
                               :test #'equal))
                           ,(generate-outcome outcome)))))))

(defun generate-discriminator-from-data
    (call-history specializer-profile generic-function-form nreq max-nargs
     &key generic-function-name (miss-operator 'miss)
       ((:inline-effective-methods *inline-effective-methods*)
        *inline-effective-methods*))
  (let* ((more-args (if (or (not max-nargs) (> max-nargs nreq)) 'more-args nil))
         (required-args (loop repeat nreq
                              collect (gensym "DISCRIMINATION-ARG"))))
    `(lambda (,@required-args
              ,@(when more-args `(&rest ,more-args)))
       ,@(when generic-function-name
           `((declare (core:lambda-name ,generic-function-name))))
       (with-effective-method-parameters (,@required-args ,more-args)
         (symbol-macrolet ((+gf-being-compiled+ ,generic-function-name))
           (let ((.generic-function. ,generic-function-form))
             ,(when (and more-args max-nargs) ; Check argcount.
                ;; FIXME: Should be possible to not check, on low safety.
                `(let ((nmore (length ,more-args)))
                   (if (< ,(- max-nargs nreq) nmore)
                       (error 'core:wrong-number-of-arguments
                              :called-function .generic-function.
                              :given-nargs (+ nmore ,nreq)
                              :min-nargs ,nreq :max-nargs ,max-nargs))))
             ;; TODO: In the future just bind this, so that a MOP method functions
             ;; can use it without consing it themselves. Let the compiler delete
             ;; it if it's not used (it can't do this yet)
             (let (#+(or)
                   (.method-args.
                     (list* ,@required-args
                            ,(if more-args
                                 more-args
                                 nil))))
               ,(generate-discrimination
                 call-history specializer-profile
                 required-args
                 `(apply #',miss-operator .generic-function. ,@required-args ,more-args)))))))))

(defun generate-discriminator (generic-function)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (generate-discriminator-from-data
     (generic-function-call-history generic-function)
     (generic-function-specializer-profile generic-function)
     generic-function min max
     :generic-function-name (core:function-name generic-function))))

;;; exported interface
(defun compile-discriminating-function (generic-function)
  ;; Ensure an up to date (i.e. won't need to miss on these arguments)
  ;; interpreted discriminator is installed, so that if Cleavir calls the
  ;; generic function we're compiling it won't go recursive.
  (set-funcallable-instance-function
   generic-function (bytecode-interpreted-discriminator generic-function))
  (set-funcallable-instance-function
   generic-function
   (compile nil (generate-discriminator generic-function)))
  (values))

(defun maybe-compile-named-gf (name)
  (when (fboundp name)
    (let ((f (fdefinition name)))
      (when (and (typep f 'standard-generic-function)
                 (not (null (generic-function-call-history f))))
        (clos:compile-discriminating-function f)))))

(defun compile-all-generic-functions ()
  (do-all-symbols (s)
    (maybe-compile-named-gf s)
    (maybe-compile-named-gf `(setf ,s))))
