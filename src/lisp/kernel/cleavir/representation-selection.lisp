(in-package #:cc-bir-to-bmir)

(defvar *unboxed-return* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representation types ("rtypes")
;;;
;;; An rtype describes how a value or values is represented in the runtime.
;;; An rtype is either
;;; * :multiple-values, meaning several T_O*s stored in the thread local
;;;   multiple values vector
;;; * :vaslist, meaning several T_O*s stored in a transient vector. Output by
;;;   e.g. values-save.
;;; * a list of value rtypes.
;;; A value rtype can be either
;;; * :object, meaning T_O*
;;; * :single-float, meaning an unboxed single float
;;; * :double-float, meaning an unboxed double float
;;; * :fixnum, meaning a tagged fixnum
;;; * :vaslist, meaning an unboxed vaslist
;;; So e.g. (:object :object) means a pair of T_O*.

;;; vaslists have both a "multiple value" rtype and a "single value" rtype
;;; to reflect their translation from lists (in vaslist.lisp). (:vaslist)
;;; means an "unboxed list" being passed around as a normal value, whereas
;;; :vaslist is the output of values-save or cc-vaslist:values-list.

;;; the :fixnum rtype is indistinguishable from :object at runtime. it
;;; exists anyway in order to avoid repeated inttoptr/ptrtoint instructions
;;; which LLVM doesn't seem to remove itself.

;;; TODO: While (re)writing this I've realized that cast placement here is not
;;; really optimal. It might have to be more sophisticated, e.g. moving it out
;;; of loops and stuff.

;;; This is bound by definition-rtype on variables and phis, and use-rtype on
;;; variables. (Not on phis, since phis are once use and that's not itself.)
;;; Because variables and phis can input from themselves and/or output to
;;; themselves, without a recursion check determination is indefinitely
;;; recursive. Instead, we can treat a recursive definition/use as rtype (),
;;; nothing, since it doesn't mandate anything as an actual def/use would.
;;; This can also happen indirectly (like (setq a b) (setq b a)).
(defvar *chasing-rtypes-of* nil)

;; Given a datum, determine the rtype given by its writer(s).
(defgeneric definition-rtype (instruction))
;; Given an instruction (writer) and a datum, determine the rtype output.
(defgeneric %definition-rtype (instruction datum))
(defmethod %definition-rtype ((inst bir:instruction) (datum bir:datum))
  ;; usually correct default
  '(:object))
(defmethod %definition-rtype ((inst bir:abstract-call) (datum bir:datum))
  :multiple-values)
;; The ABI can only handle aggregates of a certain size before it starts
;; hitting assertion failures (specifically: in X86RegisterInfo.cpp,
;; "Expected the FPP as base register"). So for big returns we punt back to
;; multiple values. Empirically, the limit seems to be four, but - FIXME -
;; this will change based on ABI and target, and we should reflect that here,
;; or perhaps use LLVM's sret arguments instead.
(defun too-big-return-rtype-p (rtype) (and (listp rtype) (> (length rtype) 4)))
(defun fixup-return-rtype (def-rtype)
  (if (too-big-return-rtype-p def-rtype)
      :multiple-values
      def-rtype))
(defun return-definition-rtype (returni)
  ;; KLUDGE: We need to avoid infinite recursion on return inputs of type
  ;; OUTPUT, which aren't otherwise put in chasing-rtypes-of. But if the input
  ;; is a PHI, which happens often, the method on definition-rtype will also
  ;; do the chasing-rtypes-of rigamarole. Therefore,
  (fixup-return-rtype
   (let ((datum (bir:input returni)))
     (etypecase datum
       (bir:phi (definition-rtype datum))
       (bir:linear-datum
        (if (member datum *chasing-rtypes-of* :test #'eq)
            '()
            (let ((*chasing-rtypes-of* (cons datum *chasing-rtypes-of*)))
              (definition-rtype datum))))))))
(defmethod %definition-rtype ((inst bir:abstract-local-call) (datum bir:datum))
  (let ((returni (bir:returni (bir:callee inst))))
    (if returni
        (if *unboxed-return*
            #+(or)
            (fixup-return-rtype (definition-rtype (bir:input returni)))
            ;;#+(or)
            (return-definition-rtype returni)
            :multiple-values)
        '())))
(defmethod %definition-rtype ((inst bir:values-save) (datum bir:datum))
  (let ((def (definition-rtype (bir:input inst))))
    (if (listp def)
        def
        :vaslist)))
(defmethod %definition-rtype ((inst cc-bmir:mtf) (datum bir:datum))
  (loop repeat (bir:nvalues inst) collect :object))
(defmethod %definition-rtype ((inst bir:values-restore) (datum bir:datum))
  (let ((irt (definition-rtype (bir:input inst))))
    (if (member irt '(:multiple-values :vaslist))
        :multiple-values
        irt)))
(defmethod %definition-rtype ((inst bir:values-collect) (datum bir:datum))
  (let ((irts (mapcar #'definition-rtype (bir:inputs inst))))
    (if (or (member :multiple-values irts) (member :vaslist irts))
        :vaslist
        (reduce #'append irts))))
(defmethod %definition-rtype ((inst cc-vaslist:values-list) (datum bir:datum))
  :vaslist)
(defmethod %definition-rtype ((inst cc-vaslist:nthcdr) (datum bir:datum))
  '(:vaslist))
(defmethod %definition-rtype ((inst cc-vaslist:last) (datum bir:datum))
  '(:vaslist))
(defmethod %definition-rtype ((inst cc-vaslist:butlast) (datum bir:datum))
  '(:vaslist))
(defmethod %definition-rtype ((inst cc-bir:mv-foreign-call) (datum bir:datum))
  :multiple-values)
(defmethod %definition-rtype ((inst bir:thei) (datum bir:datum))
  ;; THEI really throws a wrench in some stuff.
  (definition-rtype (bir:input inst)))
(defmethod %definition-rtype ((inst bir:fixed-to-multiple) (datum bir:datum))
  ;; pass through without alteration
  (loop for inp in (bir:inputs inst)
        for rt = (definition-rtype inp)
        collect (cond ((member rt '(:vaslist :multiple-values)) :object)
                      ((null rt) :object)
                      (t (first rt)))))
(defmethod %definition-rtype ((inst bir:primop) (datum bir:datum))
  (first (clasp-cleavir:primop-rtype-info (bir:info inst))))
(defmethod %definition-rtype ((inst bir:writevar) (datum bir:datum))
  (definition-rtype (bir:input inst)))
(defmethod %definition-rtype ((inst bir:readvar) (datum bir:datum))
  (definition-rtype (bir:input inst)))

;;; if we already have an rtype computed, don't do it again
(defmethod definition-rtype ((datum cc-bmir:datum)) (cc-bmir:rtype datum))

(defmethod definition-rtype ((datum bir:output))
  (%definition-rtype (bir:definition datum) datum))
(defun argument-definition-rtype (arg)
  ;; For now, only does required parameters.
  (let* ((fun (bir:function arg))
         (calls (bir:local-calls fun))
         (ll (bir:lambda-list fun))
         (pos (position arg ll :test #'eq)))
    (cond ((or (bir:enclose fun) ; XEP
               (cleavir-set:empty-set-p calls) ; entry to module
               ;; FIXME: We could handle fixed mv calls
               (cleavir-set:some (lambda (call)
                                   (typep call 'bir:mv-local-call))
                                 calls))
           '(:object))
          (pos ; required parameter
           ;; similar to a variable, we only care about primaries.
           (let ((rt '()))
             (cleavir-set:doset (call calls rt)
               (let* ((call-arg (nth pos (rest (bir:inputs call))))
                      (next-rt (definition-rtype call-arg))
                      (real-next-rt
                        (cond ((null next-rt) nil)
                              ((member next-rt '(:vaslist
                                                 :multiple-values))
                               '(:object))
                              (t (list (first next-rt))))))
                 (setf rt (cond ((null rt) real-next-rt)
                                ((null real-next-rt) rt)
                                (t
                                 (list
                                  (max-vrtype
                                   (first rt)
                                   (first real-next-rt))))))))))
          (t '(:object)))))
(defmethod definition-rtype ((datum bir:argument))
  '(:object)
  #+(or)
  (argument-definition-rtype datum))

(defmethod definition-rtype ((phi bir:phi))
  (when (member phi *chasing-rtypes-of* :test #'eq)
    (return-from definition-rtype ()))
  (let ((*chasing-rtypes-of* (cons phi *chasing-rtypes-of*))
        rt (have-rt nil))
    (cleavir-set:doset (def (bir:definitions phi) rt)
      (etypecase def
        ((or bir:jump bir:unwind)
         (let* ((in (nth (position phi (bir:outputs def)) (bir:inputs def)))
                (inrt (definition-rtype in)))
           (cond ((eq inrt :multiple-values) (return inrt)) ; nothing for it
                 ((not have-rt) (setf have-rt t rt inrt))
                 ((not (listp inrt))
                  (error "BUG: Bad rtype ~a" rt))
                 ((= (length rt) (length inrt))
                  (setf rt (mapcar #'max-vrtype rt inrt)))
                 ;; different value counts
                 (t (return :multiple-values)))))))))

(defmethod definition-rtype ((var bir:variable))
  ;; The rtype of a variable can only be exactly zero or one values.
  ;; With this constraint, we take the maximum rtype among writers.
  ;; For example, if one rtype is a single float but another is object,
  ;; we have to use object so that the other write can complete. This
  ;; is the case even if it happens that the use-rtype ends up as
  ;; single-float, because it is not an error as long as the first write
  ;; is always overwritten by another (though keeping the write is then
  ;; suboptimal of previous compiler stages).
  (when (member var *chasing-rtypes-of* :test #'eq)
    (return-from definition-rtype ()))
  (let ((*chasing-rtypes-of* (cons var *chasing-rtypes-of*))
        (rt nil))
    (declare (type (or null (cons t null)) rt)) ; single value rt at most.
    (cleavir-set:doset (writer (bir:writers var) rt)
      (let* ((next-rt (%definition-rtype writer var))
             ;; Take the primary value, or no values.
             (real-next-rt
               (cond ((null next-rt) nil)
                     ((member next-rt '(:vaslist :multiple-values))
                      '(:object))
                     (t (list (first next-rt))))))
        (declare (type (or null (cons t null)) real-next-rt))
        (setf rt (cond ((null rt) real-next-rt)
                       ((null real-next-rt) rt)
                       (t (list (max-vrtype (first rt)
                                            (first real-next-rt))))))))))

;;; Given a datum, determine what rtype its use requires.
(defgeneric use-rtype (datum))
;; Given a user (instruction) and a datum, determine the rtype required.
(defgeneric %use-rtype (instruction datum))
(defmethod %use-rtype ((inst bir:instruction) (datum bir:datum))
  ;; Having this as a default is mildly dicey but should work: instructions
  ;; that need multiple value inputs are a definite minority.
  '(:object))
(defmethod %use-rtype ((inst bir:mv-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :vaslist '(:object)))
(defmethod %use-rtype ((inst bir:mv-local-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :vaslist '(:object)))
(defmethod %use-rtype ((inst bir:returni) (datum bir:datum))
  (if *unboxed-return* (return-use-rtype (bir:function inst) datum) :multiple-values))
(defmethod %use-rtype ((inst bir:values-save) (datum bir:datum))
  (use-rtype (bir:output inst)))
(defmethod %use-rtype ((inst cc-bmir:mtf) (datum bir:datum))
  (use-rtype (bir:output inst)))
(defmethod %use-rtype ((inst bir:values-restore) (datum bir:datum))
  (use-rtype (bir:output inst)))
(defmethod %use-rtype ((inst bir:values-collect) (datum bir:datum))
  :multiple-values)
(defmethod %use-rtype ((inst cc-vaslist:values-list) (datum bir:datum))
  '(:vaslist))
(defmethod %use-rtype ((inst cc-vaslist:nendp) (datum bir:datum))
  '(:vaslist))
(defmethod %use-rtype ((inst cc-vaslist:nth) (datum bir:datum))
  (if (eq datum (second (bir:inputs inst)))
      '(:vaslist)
      '(:object)))
(defmethod %use-rtype ((inst cc-vaslist:nthcdr) (datum bir:datum))
  (if (eq datum (second (bir:inputs inst)))
      '(:vaslist)
      '(:object)))
(defmethod %use-rtype ((inst cc-vaslist:last) (datum bir:datum))
  (if (eq datum (second (bir:inputs inst)))
      '(:vaslist)
      '(:object)))
(defmethod %use-rtype ((inst cc-vaslist:butlast) (datum bir:datum))
  (if (eq datum (second (bir:inputs inst)))
      '(:vaslist)
      '(:object)))
(defmethod %use-rtype ((inst bir:primop) (datum bir:datum))
  (list (nth (position datum (bir:inputs inst))
             (rest (clasp-cleavir:primop-rtype-info (bir:info inst))))))
(defmethod %use-rtype ((inst bir:unwind) (datum bir:datum))
  (use-rtype (nth (position datum (bir:inputs inst)) (bir:outputs inst))))
(defmethod %use-rtype ((inst bir:jump) (datum bir:datum))
  (use-rtype (nth (position datum (bir:inputs inst)) (bir:outputs inst))))
(defmethod %use-rtype ((inst bir:thei) (datum bir:datum))
  ;; actual type tests, which need multiple values, should have been turned
  ;; into mv calls by this point. but out of an abundance of caution,
  (if (symbolp (bir:type-check-function inst))
      (use-rtype (bir:output inst))
      :multiple-values))
(defmethod %use-rtype ((inst bir:fixed-to-multiple) (datum bir:datum))
  ;; Use the destination rtype
  (let ((ort (use-rtype (bir:output inst))))
    (cond ((member ort '(:vaslist :multiple-values)) '(:object))
          ((null ort) '())
          (t (let ((pos (position datum (bir:inputs inst))))
               (assert pos)
               (let ((rt (nth pos ort)))
                 (if rt
                     (list rt)
                     ;; out of range of ort: unused
                     nil)))))))

(defun basic-use-rtype (datum)
  (let ((use (bir:use datum)))
    (if (null use)
        () ; don't need any value at all
        (%use-rtype use datum))))

(defmethod use-rtype ((datum bir:phi)) (basic-use-rtype datum))
(defmethod use-rtype ((datum bir:output)) (basic-use-rtype datum))
(defmethod use-rtype ((datum bir:argument)) (basic-use-rtype datum))

(defmethod use-rtype ((datum bir:variable))
  ;; Take the maximum of all uses. This is to minimize boxing: if we have
  ;; one use :double-float and one use :object, if our variable is :double-float
  ;; the latter use will cons every time.
  ;; We always return a single value type, unless the variable is unused.
  (when (member datum *chasing-rtypes-of* :test #'eq)
    (return-from use-rtype '()))
  (let ((*chasing-rtypes-of* (cons datum *chasing-rtypes-of*))
        (rt nil))
    (cleavir-set:doset (reader (bir:readers datum) rt)
      (let* ((next-rt (use-rtype (bir:output reader)))
             (real-next-rt
               (cond ((null next-rt) nil)
                     ((member next-rt '(:vaslist :multiple-values)) '(:object))
                     (t (list (first next-rt))))))
        (setf rt (max-rtype real-next-rt rt))))))

(defmethod use-rtype ((datum cc-bmir:datum)) (cc-bmir:rtype datum))

;;; Given two value rtypes, return the most preferable.
;;; More sophisticated representation selection may be required in the future.
(defgeneric min-vrtype (vrt1 vrt2))
(defmethod min-vrtype (vrt1 vrt2)
  (if (eql vrt1 vrt2)
      vrt1
      (error "BUG: ~a not defined on ~a ~a" 'min-vrtype vrt1 vrt2)))
(defmethod min-vrtype ((vrt1 (eql :object)) vrt2) vrt2)
(defmethod min-vrtype (vrt1 (vrt2 (eql :object))) vrt1)

(defgeneric max-vrtype (vrt1 vrt2))
(defmethod max-vrtype (vrt1 vrt2)
  (if (eql vrt1 vrt2)
      vrt1
      :object))
(defmethod max-vrtype ((vrt1 (eql :object)) vrt2)
  (declare (ignore vrt2))
  vrt1)
(defmethod max-vrtype (vrt1 (vrt2 (eql :object)))
  (declare (ignore vrt1))
  vrt2)

;;; Given two rtypes, return the most preferable rtype.
(defun min-rtype (rt1 rt2)
  (cond ((listp rt1)
         (cond ((listp rt2)
                ;; Shorten
                (mapcar #'min-vrtype rt1 rt2))
               (t
                (assert (member rt2 '(:vaslist :multiple-values)))
                rt1)))
        ((eq rt1 :multiple-values) rt2)
        ((eq rt1 :vaslist)
         (if (eq rt2 :multiple-values)
             rt1
             rt2))
        (t (error "Bad rtype: ~a" rt1))))

;;; ...and least.
(defun max-rtype (rt1 rt2)
  (cond ((listp rt1)
         (cond ((listp rt2)
                ;; Lengthen
                (let ((longer (if (< (length rt1) (length rt2)) rt2 rt1)))
                  (map-into (copy-list longer) #'max-vrtype rt1 rt2)))
               (t
                (assert (member rt2 '(:vaslist :multiple-values)))
                rt2)))
        ((eq rt1 :multiple-values) rt1)
        ((eq rt1 :vaslist)
         (if (eq rt2 :multiple-values)
             rt2
             rt1))
        (t (error "Bad rtype: ~a" rt1))))

(defun phi-rtype (datum)
  ;; PHIs are trickier. If the destination is single-value, the phi can be too.
  ;; If not, then the phi could still be single-value, but only if EVERY
  ;; definition is, and otherwise we need to use multiple values.
  (let ((dest (use-rtype datum)))
    (if (member dest '(:vaslist :multiple-values))
        (definition-rtype datum)
        dest)))

(defun variable-rtype (datum)
  ;; Just take the minimum of the writers and readers. FIXME: Think harder.
  (min-rtype (definition-rtype datum) (use-rtype datum)))

;;; Determine the use rtype of the return value of a function.
(defun return-use-rtype (function returni-input)
  ;; We have to watch out for loops since the return value of a call could
  ;; be used as an argument to another call of the same function.
  (when (member returni-input *chasing-rtypes-of* :test #'eq)
    (return-from return-use-rtype '()))
  (let ((*chasing-rtypes-of* (cons returni-input *chasing-rtypes-of*))
        (rt nil)
        (local-calls (bir:local-calls function)))
    (if (or (bir:enclose function) (cleavir-set:empty-set-p local-calls))
        ;; The function is enclosed, so it could be called from anywhere, and
        ;; we need to use the pessimistic protocol. No enclose and no local
        ;; calls means it's the top level function, so the same situation.
        :multiple-values
        ;; No enclose, so we can look at all the call sites, and if they're
        ;; amenable, do something smarter.
        (cleavir-set:doset (call local-calls rt)
          (setf rt (max-rtype rt (use-rtype (bir:output call))))))))

(defgeneric maybe-assign-rtype (datum))
(defmethod maybe-assign-rtype ((datum cc-bmir:output)))
(defmethod maybe-assign-rtype ((datum cc-bmir:phi)))
(defmethod maybe-assign-rtype ((datum cc-bmir:variable)))
(defmethod maybe-assign-rtype ((datum bir:load-time-value)))
(defmethod maybe-assign-rtype ((datum bir:constant)))
(defmethod maybe-assign-rtype ((datum bir:output))
  (let* ((source (definition-rtype datum))
         (dest (use-rtype datum))
         (rtype (min-rtype source dest)))
    (change-class datum 'cc-bmir:output :rtype rtype)
    rtype))
(defmethod maybe-assign-rtype ((datum bir:phi))
  (change-class datum 'cc-bmir:phi :rtype (phi-rtype datum)))
(defmethod maybe-assign-rtype ((datum bir:variable))
  ;; At the moment we can only have unboxed local variables.
  ;; TODO: Extend to unboxed DX variables. Indefinite would be harder, since
  ;; closure vectors are full of boxed data.
  (change-class datum 'cc-bmir:variable
                :rtype (if (eq (bir:extent datum) :local)
                           (variable-rtype datum)
                           '(:object))))
(defmethod maybe-assign-rtype ((datum bir:argument))
  (change-class datum 'cc-bmir:argument
                :rtype
                #+(or)'(:object)
                (let ((use (use-rtype datum)))
                         (if (null use)
                             '(:object)
                             (min-rtype use '(:object))))))

(defun assign-instruction-rtypes (inst)
  (mapc #'maybe-assign-rtype (bir:outputs inst)))

(defun assign-lambda-list-rtypes (lambda-list)
  (dolist (item lambda-list)
    (typecase item
      (symbol)
      (cons
       (ecase (length item)
         ((2)
          (maybe-assign-rtype (first item))
          (maybe-assign-rtype (second item)))
         ((3)
          (maybe-assign-rtype (second item))
          (maybe-assign-rtype (third item)))))
      (t (maybe-assign-rtype item)))))

(defun assign-function-rtypes (function)
  (assign-lambda-list-rtypes (bir:lambda-list function))
  (bir:map-local-instructions #'assign-instruction-rtypes function))

(defun assign-module-rtypes (module)
  (bir:map-functions #'assign-function-rtypes module))

;;;

;; here our datum actually has its rtype, but we (maybe) need to convert it
;; for the sake of some instruction.
(defun maybe-cast-before (inst datum needed-rtype)
  (unless (equal (cc-bmir:rtype datum) needed-rtype)
    (let* ((new (make-instance 'cc-bmir:output
                  :rtype needed-rtype :derived-type (bir:ctype datum)))
           (cast (make-instance 'cc-bmir:cast
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :outputs (list new))))
      (bir:insert-instruction-before cast inst)
      (bir:replace-uses new datum)
      (setf (bir:inputs cast) (list datum))))
  (values))

;;; Here, the rtype we want is what datum has, and the actual rtype is what
;;; instruction actually outputs. DATUM must be the output of INST.
(defun maybe-cast-after (inst datum actual-rtype)
  (unless (or (equal (cc-bmir:rtype datum) actual-rtype)
              ;; Don't bother casting if the datum is unused.
              ;; Inserting a cast anyway can result in BIR verification
              ;; failure for certain special instructions; see #1224.
              (not (bir:use datum)))
    (let* ((new (make-instance 'cc-bmir:output
                  :rtype actual-rtype :derived-type (bir:ctype datum)))
           (cast (make-instance 'cc-bmir:cast
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :inputs (list new))))
      (bir:insert-instruction-after cast inst)
      (setf (bir:outputs inst) (list new)
            (bir:outputs cast) (list datum))))
  (values))

;;; This is different from above because MTF is not a cast, as mentioned
;;; in bmir.lisp where it's defined.
(defun insert-mtf-before (inst datum target)
  (let* ((new (make-instance 'cc-bmir:output
                :rtype target :derived-type (bir:ctype datum)))
         (mtf (make-instance 'cc-bmir:mtf
                :origin (bir:origin inst) :policy (bir:policy inst)
                :nvalues (length target) :outputs (list new))))
    (bir:insert-instruction-before mtf inst)
    (bir:replace-uses new datum)
    (setf (bir:inputs mtf) (list datum)))
  (values))

(defgeneric insert-casts (instruction))

(defun cast-inputs (instruction needed-rtype
                    &optional (inputs (bir:inputs instruction)))
  (loop for inp in inputs do (maybe-cast-before instruction inp needed-rtype)))

(defun object-input (instruction input)
  (maybe-cast-before instruction input '(:object)))

(defun object-inputs (instruction
                      &optional (inputs (bir:inputs instruction)))
  (cast-inputs instruction '(:object) inputs))

(defun cast-output (instruction actual-rtype)
  (maybe-cast-after instruction (first (bir:outputs instruction)) actual-rtype))

(defun object-output (instruction) (cast-output instruction '(:object)))

(defmethod insert-casts ((instruction bir:instruction))
  ;; Default method: Assume we need all :objects, and if we output anything,
  ;; it's (:object).
  (object-inputs instruction)
  (unless (null (bir:outputs instruction)) (object-output instruction)))

(defmethod insert-casts ((instruction bir:fixed-to-multiple))
  ;; recapitulates definition-rtype, but makes sure the inputs have been
  ;; reduced from :multiple-values.
  (let ((ortype (loop for inp in (bir:inputs instruction)
                      for rt = (cc-bmir:rtype inp)
                      do (assert (listp rt))
                      collect (if (null rt) :object (first rt)))))
    ;; Cast any () to a value.
    (loop for inp in (bir:inputs instruction)
          for rt in ortype
          ;; this WHEN is redundant with maybe-cast-before's checking,
          ;; but saves a bit of consing of (list rt) in the common case.
          when (null (cc-bmir:rtype inp))
            do (maybe-cast-before instruction inp (list rt)))
    ;; Cast the output to whatever
    (cast-output instruction ortype)))

;;; Make sure we don't insert things infinitely
(defmethod insert-casts ((instruction cc-bmir:cast)))

;;; Might need to cast its input,
;;; but doesn't necessarily need object inputs.
;;; One situation in which a cast is required is when the input to THEI is
;;; a function argument. In that situation, the argument can be forced to
;;; have rtype (:OBJECT), while the output has some reduced rtype in
;;; accordance with how it is actually used.
;;; FIXME: Probably this could be more intelligent.
(defmethod insert-casts ((instruction bir:thei))
  (maybe-cast-before instruction
                     (bir:input instruction)
                     (cc-bmir:rtype (bir:output instruction))))

(defun insert-jump-coercion (instruction)
  (loop for inp in (bir:inputs instruction)
        for outp in (bir:outputs instruction)
        for inprt = (cc-bmir:rtype inp)
        for outprt = (cc-bmir:rtype outp)
        do (maybe-cast-before instruction inp outprt)))

(defmethod insert-casts ((instruction bir:jump))
  (insert-jump-coercion instruction))
(defmethod insert-casts ((instruction bir:unwind))
  (insert-jump-coercion instruction))

(defmethod insert-casts ((instruction bir:call))
  (object-inputs instruction)
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction bir:mv-call))
  ;; NOTE: If for some reason the arguments here is a fixed number of values,
  ;; we are working very suboptimally - this could have been a fixed-mv-call.
  ;; Type inference is being stupid.
  ;; This applies to mv-local-call, below, as well.
  ;; TODO: Output a note? But it's really not the user's fault.
  ;; Anyway, if this is the case, make us a fixed-mv-call, since the normal
  ;; mv-call cannot handle fixed inputs.
  (let* ((inputs (bir:inputs instruction))
         (fun (first inputs))
         (args (second inputs)))
    (cond ((listp (cc-bmir:rtype args))
           (change-class instruction 'cc-bmir:fixed-mv-call
                         :nvalues (length (cc-bmir:rtype args)))
           (insert-casts instruction))
          (t
           (object-input instruction fun)
           (cast-inputs instruction :vaslist (rest (bir:inputs instruction)))
           (cast-output instruction :multiple-values)))))

(defmethod insert-casts ((instruction cc-bmir:fixed-mv-call))
  (object-input instruction (first (bir:inputs instruction)))
  (let* ((args (second (bir:inputs instruction)))
         (args-rtype (cc-bmir:rtype args))
         (nvalues (bir:nvalues instruction))
         (target (make-list nvalues :initial-element :object)))
    (cond ((listp args-rtype)
           ;; Null rtype can happen e.g. if the input is a local call that is
           ;; known to not return. But in that case this call really should
           ;; have been eliminated, so, FIXME. I see it running the numerics
           ;; regression tests.
           (assert (or (null args-rtype) (= (length args-rtype) nvalues)))
           (maybe-cast-before instruction args target))
          (t (insert-mtf-before instruction args target))))
  (cast-output instruction :multiple-values))

(defun cast-local-call-output (instruction)
  (let* ((fun (bir:callee instruction))
         (returni (bir:returni fun))
         (actual-rt (if returni
                        (if *unboxed-return*
                            (cc-bmir:rtype (bir:input returni))
                            :multiple-values)
                        '())))
    (cast-output instruction actual-rt)))

(defmethod insert-casts ((instruction bir:local-call))
  ;; KLUDGE: For now, we only consider required arguments as being
  ;; possible to pass unboxed.
  (loop with requiredp = t
        for item in (bir:lambda-list (bir:callee instruction))
        for arg in (rest (bir:inputs instruction))
        unless (typep item 'bir:argument)
          do (setf requiredp nil)
        do (maybe-cast-before instruction arg (if requiredp
                                                  (cc-bmir:rtype item)
                                                  '(:object))))
  (cast-local-call-output instruction))
(defmethod insert-casts ((instruction bir:mv-local-call))
  (let* ((inputs (bir:inputs instruction))
         (args (second inputs)))
    (cond ((listp (cc-bmir:rtype args))
           (change-class instruction 'cc-bmir:fixed-mv-local-call
                         :nvalues (length (cc-bmir:rtype args)))
           (insert-casts instruction))
          (t
           (cast-inputs instruction :vaslist (rest (bir:inputs instruction)))
           (cast-local-call-output instruction)))))
(defmethod insert-casts ((instruction cc-bmir:fixed-mv-local-call))
  (let* ((args (second (bir:inputs instruction)))
         (args-rtype (cc-bmir:rtype args))
         (nvalues (bir:nvalues instruction))
         (target
           (loop with requiredp = t
                 repeat nvalues
                 for item in (bir:lambda-list (bir:callee instruction))
                 unless (typep item 'bir:argument)
                   do (setf requiredp nil)
                 collect (if requiredp
                             (first (cc-bmir:rtype item))
                             :object))))
    (cond ((listp args-rtype)
           (assert (= (length args-rtype) nvalues))
           (maybe-cast-before instruction args target))
          (t (insert-mtf-before instruction args target))))
  (cast-local-call-output instruction))
(defmethod insert-casts ((instruction cc-bir:mv-foreign-call))
  (object-inputs instruction)
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction bir:values-save))
  (let* ((input (bir:input instruction)) (output (bir:output instruction))
         (inputrt (cc-bmir:rtype input)) (outputrt (cc-bmir:rtype output)))
    (cond ((eq inputrt :vaslist)
           ;; We're already getting a vaslist, so this is a nop to delete
           (assert (eq outputrt :vaslist))
           (cleavir-bir:replace-terminator
            (make-instance 'bir:jump
              :origin (bir:origin instruction) :policy (bir:policy instruction)
              :inputs () :outputs () :next (bir:next instruction))
            instruction)
           ;; Don't need to recompute flow order since we haven't changed it.
           ;; We also don't merge iblocks because we're mostly done optimizing
           ;; at this point anyway.
           (bir:replace-uses input output))
          ((eq outputrt :multiple-values)
           (cast-inputs instruction :multiple-values))
          ((listp outputrt)
           ;; The number of values is fixed, so this is a nop to delete.
           (assert (equal inputrt outputrt))
           (cleavir-bir:replace-terminator
            (make-instance 'bir:jump
              :origin (bir:origin instruction) :policy (bir:policy instruction)
              :inputs () :outputs () :next (bir:next instruction))
            instruction)
           (bir:replace-uses input output)))))
(defmethod insert-casts ((inst cc-bmir:mtf))
  (let* ((input (bir:input inst)) (output (bir:output inst))
         (inputrt (cc-bmir:rtype input)) (outputrt (cc-bmir:rtype output)))
    (assert (listp outputrt))
    (when (and (listp inputrt)
               ;; no need to cast if unused
               (not (null outputrt)))
      ;; Inputs may not already be objects, so possibly cast.
      (assert (= (length inputrt) (length outputrt)))
      (maybe-cast-before inst input outputrt))))
(defmethod insert-casts ((instruction bir:values-restore))
  (let* ((input (bir:input instruction))
         (inputrt (cc-bmir:rtype input))
         (output (bir:output instruction))
         (outputrt (cc-bmir:rtype output)))
    (cond ((not (eq outputrt :multiple-values))
           ;; fixed values, so this is a nop to delete.
           (bir:replace-uses input output)
           (bir:delete-instruction instruction))
          ((listp inputrt)
           (cast-output instruction inputrt))
          (t (cast-output instruction :multiple-values)))))
(defmethod insert-casts ((instruction bir:values-collect))
  (let* ((inputs (bir:inputs instruction))
         (output (bir:output instruction))
         (outputrt (cc-bmir:rtype output)))
    (when (listp outputrt)
      (if (= (length inputs) 1)
          ;; fixed values, so this is a nop to delete.
          (bir:replace-uses (first inputs) output)
          ;; lower to append-values.
          (bir:insert-instruction-before
           (make-instance 'cc-bmir:append-values
             :origin (bir:origin instruction) :policy (bir:policy instruction)
             :inputs inputs :outputs (list output))
           instruction))
      (bir:replace-terminator
       (make-instance 'bir:jump
         :origin (bir:origin instruction) :policy (bir:policy instruction)
         :inputs () :outputs () :next (bir:next instruction))
       instruction))))
          
(defmethod insert-casts ((instruction cc-vaslist:values-list))
  (cast-inputs instruction '(:vaslist))
  (cast-output instruction :vaslist))
(defmethod insert-casts ((instruction cc-vaslist:nendp))
  (cast-inputs instruction '(:vaslist))
  (object-output instruction))
(defmethod insert-casts ((instruction cc-vaslist:nth))
  (let ((inputs (bir:inputs instruction)))
    (maybe-cast-before instruction (first inputs) '(:object))
    (maybe-cast-before instruction (second inputs) '(:vaslist)))
  (object-output instruction))
(defmethod insert-casts ((instruction cc-vaslist:nthcdr))
  (let ((inputs (bir:inputs instruction)))
    (maybe-cast-before instruction (first inputs) '(:object))
    (maybe-cast-before instruction (second inputs) '(:vaslist)))
  (cast-output instruction '(:vaslist)))
(defmethod insert-casts ((instruction cc-vaslist:last))
  (let ((inputs (bir:inputs instruction)))
    (maybe-cast-before instruction (first inputs) '(:object))
    (maybe-cast-before instruction (second inputs) '(:vaslist)))
  (cast-output instruction '(:vaslist)))
(defmethod insert-casts ((instruction cc-vaslist:butlast))
  (let ((inputs (bir:inputs instruction)))
    (maybe-cast-before instruction (first inputs) '(:object))
    (maybe-cast-before instruction (second inputs) '(:vaslist)))
  (cast-output instruction '(:vaslist)))

;; returni just passes out whatever it's given. (or will)
(defmethod insert-casts ((instruction bir:returni))
  (when (or (not *unboxed-return*)
            (too-big-return-rtype-p (cc-bmir:rtype (bir:input instruction))))
    (cast-inputs instruction :multiple-values)))
(defmethod insert-casts ((inst bir:primop))
  (let* ((info (bir:info inst))
         (rt-info (clasp-cleavir:primop-rtype-info info))
         (ret (first rt-info))
         (args (rest rt-info)))
    (loop for input in (bir:inputs inst)
          for art in args
          do (maybe-cast-before inst input (list art)))
    (unless (null (bir:outputs inst))
      (cast-output inst ret))))
(defmethod insert-casts ((inst bir:writevar))
  (cast-inputs inst (cc-bmir:rtype (bir:output inst))))
(defmethod insert-casts ((inst bir:readvar))
  (cast-output inst (cc-bmir:rtype (bir:input inst))))

;;; Unbox constants if possible.
;;; Note that constant-reference is still understood to output an :object
;;; above; this is because, if a constant is actually used as an object
;;; (passed to a general function, etc.) we don't want to box it every time.

(defgeneric constant-unboxable-p (value rtype))
(defmethod constant-unboxable-p (value (rt (eql :object)))
  (declare (ignore value))
  nil)
(defmethod constant-unboxable-p (value (rt (eql nil)))
  (declare (ignore value))
  nil)
(defmethod constant-unboxable-p ((value single-float) (rt (eql :single-float)))
  t)
(defmethod constant-unboxable-p ((value t) (rt (eql :single-float))) nil)
(defmethod constant-unboxable-p ((value double-float) (rt (eql :double-float)))
  t)
(defmethod constant-unboxable-p ((value t) (rt (eql :double-float))) nil)
;; no point since they're just integers anyway
(defmethod constant-unboxable-p ((value t) (rt (eql :fixnum))) nil)

(defun unbox-constant-reference (inst value)
  (let ((constant (bir:input inst)))
    (change-class inst 'cc-bmir:unboxed-constant-reference
                  :inputs ()
                  :value value)
    ;; BUG? in cleavir - it won't delete the constant from the change-class,
    ;; even though it makes the constant unreferenced. As a KLUDGE we do it
    ;; ourselves here.
    (when (cleavir-set:empty-set-p (bir:readers constant))
      (cleavir-set:nremovef (bir:constants (bir:module (bir:function inst)))
                            constant))))
(defmethod insert-casts ((inst bir:constant-reference))
  (let ((value (bir:constant-value (bir:input inst)))
        (rt (first (cc-bmir:rtype (bir:output inst)))))
    (if (constant-unboxable-p value rt)
        (unbox-constant-reference inst value)
        (call-next-method))))

(defun insert-casts-into-function (function)
  (cleavir-bir:map-local-instructions #'insert-casts function))

(defun insert-casts-into-module (module)
  (bir:map-functions #'insert-casts-into-function module))
