(in-package #:cc-bir-to-bmir)

(defun process-typeq-type (ts)
  ;; Undo some parsing. KLUDGE.
  (cond
    ((equal ts '(integer #.most-negative-fixnum #.most-positive-fixnum))
     'fixnum)
    ((equal ts '(or (integer * (#.most-negative-fixnum))
                 (integer (#.most-positive-fixnum) *)))
     'bignum)
    ((equal ts '(single-float * *)) 'single-float)
    ((equal ts '(double-float * *)) 'double-float)
    ((equal ts '(rational * *)) 'rational)
    ((equal ts '(real * *)) 'real)
    ((equal ts '(complex *)) 'complex)
    ((equal ts '(array * *)) 'array)
    ((equal ts '(cons t t)) 'cons)
    ;; simple-bit-array becomes (simple-array bit (*)), etc.
    ((and (consp ts) (eq (car ts) 'simple-array))
     (core::simple-vector-type (second ts)))    
    ((or (equal ts '(or (simple-array base-char (*))
                     (simple-array character (*))))
         (equal ts '(or (simple-array character (*))
                     (simple-array base-char (*)))))
     (setf ts 'simple-string))
    ((and (consp ts) (eq (car ts) 'function))
     ;; We should check that this does not specialize, because
     ;; obviously we can't check that.
     'function)
    (t ts)))

(defun replace-typeq (typeq)
  (let ((ts (process-typeq-type (bir:test-ctype typeq))))
    (case ts
      ((fixnum) (change-class typeq 'cc-bmir:fixnump))
      ((cons) (change-class typeq 'cc-bmir:consp))
      ((character) (change-class typeq 'cc-bmir:characterp))
      ((single-float) (change-class typeq 'cc-bmir:single-float-p))
      ((core:general) (change-class typeq 'cc-bmir:generalp))
      (t (let ((header-info (gethash ts core:+type-header-value-map+)))
           (cond (header-info
                  (check-type header-info (or integer cons)) ; sanity check
                  (change-class typeq 'cc-bmir:headerq :info header-info))
                 (t (error "BUG: Typeq for unknown type: ~a" ts))))))))

(defun reduce-local-typeqs (function)
  (bir:map-iblocks
   (lambda (ib)
     (let ((term (bir:end ib)))
       (when (typep term 'bir:ifi)
         (let ((test-out (bir:input term)))
           (when (typep test-out 'bir:output)
             (let ((test (bir:definition test-out)))
               (when (typep test 'bir:typeq-test)
                 (replace-typeq test))))))))
   function))

(defun reduce-module-typeqs (module)
  (cleavir-bir:map-functions #'reduce-local-typeqs module))

(defun maybe-replace-primop (primop)
  (case (cleavir-primop-info:name (bir:info primop))
    ((cleavir-primop:car)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:cdr)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:load :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs in :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list nout)))))
    ((cleavir-primop:rplaca)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))
    ((cleavir-primop:rplacd)
     (let ((in (bir:inputs primop))
           (nout (make-instance 'bir:output)))
       (change-class primop 'cc-bmir:store :inputs ())
       (let ((mr (make-instance 'cc-bmir:memref2
                   :inputs (list (first in)) :outputs (list nout)
                   :offset (- cmp:+cons-cdr-offset+ cmp:+cons-tag+)
                   :origin (bir:origin primop)
                   :policy (bir:policy primop))))
         (bir:insert-instruction-before mr primop)
         (setf (bir:inputs primop) (list (second in) nout)))))))

(defun reduce-primops (function)
  (bir:map-local-instructions
   (lambda (i)
     (when (typep i 'bir:primop)
       (maybe-replace-primop i)))
   function))

(defun reduce-module-primops (module)
  (cleavir-set:mapset nil #'reduce-primops (bir:functions module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representation types ("rtypes")
;;;
;;; An rtype describes how a value or values is represented in the runtime.
;;; An rtype is either :multiple-values, meaning several T_O*s stored in the
;;; thread local multiple values vector, or a list of value rtypes. A value
;;; rtype can be either
;;; * :object, meaning T_O*
;;; * :single-float, meaning an unboxed single float
;;; * :double-float, meaning an unboxed double float
;;; So e.g. (:object :object) means a pair of T_O*.

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
(defmethod %definition-rtype ((inst bir:values-save) (datum bir:datum))
  (definition-rtype (bir:input inst)))
(defmethod %definition-rtype ((inst bir:values-collect) (datum bir:datum))
  (if (= (length (bir:inputs inst)) 1)
      (definition-rtype (first (bir:inputs inst)))
      :multiple-values))
(defmethod %definition-rtype ((inst cc-bir:mv-foreign-call) (datum bir:datum))
  :multiple-values)
(defmethod %definition-rtype ((inst bir:thei) (datum bir:datum))
  ;; THEI really throws a wrench in some stuff.
  (definition-rtype (bir:input inst)))
(defmethod %definition-rtype ((inst bir:fixed-to-multiple) (datum bir:datum))
  ;; pass through without alteration
  (loop for inp in (bir:inputs inst)
        for rt = (definition-rtype inp)
        collect (cond ((eq rt :multiple-values) :object)
                      ((null rt) :object)
                      (t (first rt)))))
(defmethod %definition-rtype ((inst bir:vprimop) (datum bir:datum))
  (first (cc-bir:primop-rtype-info (bir:info inst))))
(defmethod %definition-rtype ((inst bir:writevar) (datum bir:datum))
  (definition-rtype (bir:input inst)))
(defmethod %definition-rtype ((inst bir:readvar) (datum bir:datum))
  (definition-rtype (bir:input inst)))

;;; if we already have an rtype computed, don't do it again
(defmethod definition-rtype ((datum cc-bmir:datum)) (cc-bmir:rtype datum))

(defmethod definition-rtype ((datum bir:output))
  (%definition-rtype (bir:definition datum) datum))
(defmethod definition-rtype ((datum bir:argument)) '(:object))

(defmethod definition-rtype ((phi bir:phi))
  (when (member phi *chasing-rtypes-of* :test #'eq)
    (return-from definition-rtype ()))
  (let ((*chasing-rtypes-of* (cons phi *chasing-rtypes-of*))
        (rt nil))
    (cleavir-set:doset (def (bir:definitions phi) rt)
      (etypecase def
        ((or bir:jump bir:unwind)
         (let* ((in (nth (position phi (bir:outputs def)) (bir:inputs def)))
                (inrt (definition-rtype in)))
           (cond ((eq inrt :multiple-values) (return inrt)) ; nothing for it
                 ((null rt) (setf rt inrt))
                 ((= (length rt) (length inrt))
                  (setf rt (mapcar #'max-vrtype rt inrt)))
                 ;; different value counts
                 (t (return :multiple-values)))))))))

(defmethod definition-rtype ((var bir:variable))
  ;; See the use-rtype for variables below for logic
  (when (member var *chasing-rtypes-of* :test #'eq)
    (return-from definition-rtype ()))
  (let ((*chasing-rtypes-of* (cons var *chasing-rtypes-of*))
        (rt nil))
    (cleavir-set:doset (writer (bir:writers var) rt)
      (let* ((next-rt (%definition-rtype writer var))
             (real-next-rt
               (cond ((null next-rt) (if (null rt) nil '(:object)))
                     ((eq next-rt :multiple-values) '(:object))
                     (t (list (first next-rt))))))
        (setf rt (if (null rt) real-next-rt (min-rtype real-next-rt rt)))))))

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
      :multiple-values '(:object)))
(defmethod %use-rtype ((inst bir:mv-local-call) (datum bir:datum))
  (if (member datum (rest (bir:inputs inst)))
      :multiple-values '(:object)))
(defmethod %use-rtype ((inst bir:returni) (datum bir:datum)) :multiple-values)
(defmethod %use-rtype ((inst bir:values-save) (datum bir:datum))
  (use-rtype (bir:output inst)))
(defmethod %use-rtype ((inst bir:values-collect) (datum bir:datum))
  (if (= (length (bir:inputs inst)) 1)
      (use-rtype (bir:output inst))
      :multiple-values))
(defmethod %use-rtype ((inst bir:vprimop) (datum bir:datum))
  (list (nth (position datum (bir:inputs inst))
             (rest (cc-bir:primop-rtype-info (bir:info inst))))))
(defmethod %use-rtype ((inst bir:unwind) (datum bir:datum))
  (use-rtype (nth (position datum (bir:inputs inst)) (bir:outputs inst))))
(defmethod %use-rtype ((inst bir:jump) (datum bir:datum))
  (use-rtype (nth (position datum (bir:inputs inst)) (bir:outputs inst))))
(defmethod %use-rtype ((inst bir:thei) (datum bir:datum))
  ;; actual type tests, which need multiple values, should have been turned
  ;; into mv calls by this point. but out of an abundance of caution,
  (if (symbolp (bir:type-check-function inst))
      (use-rtype (first (bir:outputs inst)))
      :multiple-values))
(defmethod %use-rtype ((inst bir:fixed-to-multiple) (datum bir:datum))
  ;; Use the destination rtype
  (let ((ort (use-rtype (bir:output inst))))
    (cond ((eq ort :multiple-values) '(:object))
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
  ;; Take the minimum of all uses, except we only get () if every reader has
  ;; it, and otherwise always return a single value type.
  (when (member datum *chasing-rtypes-of* :test #'eq)
    (return-from use-rtype ()))
  (let ((*chasing-rtypes-of* (cons datum *chasing-rtypes-of*))
        (rt nil))
    (cleavir-set:doset (reader (bir:readers datum) rt)
      (let* ((next-rt (use-rtype (bir:output reader)))
             (real-next-rt
               (cond ((null next-rt) (if (null rt) nil '(:object)))
                     ((eq next-rt :multiple-values) '(:object))
                     (t (list (first next-rt))))))
        (setf rt (if (null rt) real-next-rt (min-rtype real-next-rt rt)))))))

(defmethod use-rtype ((datum cc-bmir:datum)) (cc-bmir:rtype datum))

;;; Given two value rtypes, return the most preferable.
;;; More sophisticated representation selection may be required in the future.
(defun min-vrtype (vrt1 vrt2)
  (ecase vrt1
    ((:single-float) (ecase vrt2 ((:single-float :object) vrt1)))
    ((:double-float) (ecase vrt2 ((:double-float :object) vrt1)))
    ((:object) (ecase vrt2 ((:single-float :double-float :object) vrt2)))))

(defun max-vrtype (vrt1 vrt2)
  (ecase vrt1
    ((:single-float) (ecase vrt2 ((:single-float :object) vrt2)))
    ((:double-float) (ecase vrt2 ((:double-float :object) vrt2)))
    ((:object) (ecase vrt2 ((:single-float :double-float :object) vrt1)))))

;;; Given two rtypes, return the most preferable rtype.
(defun min-rtype (rt1 rt2)
  (cond ((listp rt1)
         (cond ((listp rt2)
                ;; Shorten
                (mapcar #'min-vrtype rt1 rt2))
               (t
                (assert (member rt2 '(:multiple-values)))
                rt1)))
        ((eq rt1 :multiple-values) rt2)
        (t (error "Bad rtype: ~a" rt1))))

(defun phi-rtype (datum)
  ;; PHIs are trickier. If the destination is single-value, the phi can be too.
  ;; If not, then the phi could still be single-value, but only if EVERY
  ;; definition is, and otherwise we need to use multiple values.
  (let ((dest (use-rtype datum)))
    (if (eq dest :multiple-values)
        (definition-rtype datum)
        dest)))

(defun variable-rtype (datum)
  ;; Just take the minimum of the writers and readers.
  (min-rtype (definition-rtype datum) (use-rtype datum)))

(defgeneric maybe-assign-rtype (datum))
(defmethod maybe-assign-rtype ((datum cc-bmir:output)))
(defmethod maybe-assign-rtype ((datum cc-bmir:phi)))
(defmethod maybe-assign-rtype ((datum cc-bmir:variable)))
(defmethod maybe-assign-rtype ((datum bir:argument)))
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

(defun assign-instruction-rtypes (inst)
  (mapc #'maybe-assign-rtype (bir:outputs inst)))

(defun assign-function-rtypes (function)
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
  (unless (equal (cc-bmir:rtype datum) actual-rtype)
    (let* ((new (make-instance 'cc-bmir:output
                  :rtype actual-rtype :derived-type (bir:ctype datum)))
           (cast (make-instance 'cc-bmir:cast
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :inputs (list new))))
      (bir:insert-instruction-after cast inst)
      (setf (bir:outputs inst) (list new)
            (bir:outputs cast) (list datum))))
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
;;; Doesn't need to do anything, and might not have all :object inputs
(defmethod insert-casts ((instruction bir:thei)))

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
  (object-input instruction (first (bir:inputs instruction)))
  (cast-inputs instruction :multiple-values (rest (bir:inputs instruction)))
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction bir:local-call))
  (object-inputs instruction (rest (bir:inputs instruction)))
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction bir:mv-local-call))
  (cast-inputs instruction :multiple-values (rest (bir:inputs instruction)))
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction cc-bir:mv-foreign-call))
  (object-inputs instruction)
  (cast-output instruction :multiple-values))
(defmethod insert-casts ((instruction bir:values-save))
  (let* ((input (bir:input instruction)) (output (bir:output instruction))
         (inputrt (cc-bmir:rtype input)) (outputrt (cc-bmir:rtype output))
         (nde (bir:dynamic-environment instruction)))
    (cond ((eq outputrt :multiple-values)
           (cast-inputs instruction :multiple-values))
          (t
           ;; The number of values is fixed, so this is a nop to delete.
           (assert (equal inputrt outputrt))
           (cleavir-set:doset (s (cleavir-bir:scope instruction))
             (setf (cleavir-bir:dynamic-environment s) nde))
           (cleavir-bir:replace-terminator
            (make-instance 'bir:jump
              :origin (bir:origin instruction) :policy (bir:policy instruction)
              :inputs () :outputs () :next (bir:next instruction))
            instruction)
           ;; Don't need to recompute flow order since we haven't changed it.
           ;; We also don't merge iblocks because we're mostly done optimizing
           ;; at this point anyway.
           (bir:replace-uses input output)))))
(defmethod insert-casts ((instruction bir:values-collect))
  (let* ((inputs (bir:inputs instruction)) (output (bir:output instruction))
         (outputrt (cc-bmir:rtype output)))
    (cond ((and (= (length inputs) 1) (not (eq outputrt :multiple-values)))
           ;; fixed values, so this is a nop to delete.
           (setf (bir:inputs instruction) nil)
           (bir:replace-uses (first inputs) output)
           (bir:delete-instruction instruction))
          (t
           (cast-output instruction :multiple-values)))))
(defmethod insert-casts ((instruction bir:returni))
  (cast-inputs instruction :multiple-values))
(defmethod insert-casts ((inst bir:vprimop))
  (let* ((info (bir:info inst))
         (rt-info (cc-bir:primop-rtype-info info))
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

(defun constant-unboxable-p (value rt)
  (ecase rt
    ((nil :object) nil) ; not sure nil is actually possible, but hey
    ((:single-float) (typep value 'single-float))
    ((:double-float) (typep value 'double-float))))
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
