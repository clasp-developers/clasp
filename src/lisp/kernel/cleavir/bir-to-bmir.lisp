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
;;; rtype can be either :object, meaning T_O*, or :single-float, meaning an
;;; unboxed single float. So e.g. (:object :object) means a
;;; pair of T_O*.

;; Given an instruction, determine what rtype it outputs.
(defgeneric definition-rtype (instruction))
;; usually correct default
(defmethod definition-rtype ((inst bir:instruction)) '(:object))
(defmethod definition-rtype ((inst bir:abstract-call)) :multiple-values)
(defmethod definition-rtype ((inst bir:values-save))
  (let ((input (bir:input inst)))
    (maybe-assign-rtype input)
    (cc-bmir:rtype input)))
(defmethod definition-rtype ((inst bir:values-collect))
  (if (= (length (bir:inputs inst)) 1)
      (let ((input (first (bir:inputs inst))))
        (maybe-assign-rtype input)
        (cc-bmir:rtype input))
      :multiple-values))
(defmethod definition-rtype ((inst cc-bir:mv-foreign-call)) :multiple-values)
(defmethod definition-rtype ((inst bir:thei))
  ;; THEI really throws a wrench in some stuff.
  (let ((input (first (bir:inputs inst))))
    (maybe-assign-rtype input)
    (cc-bmir:rtype input)))
(defmethod definition-rtype ((inst bir:fixed-to-multiple))
  (make-list (length (bir:inputs inst)) :initial-element :object))
(defmethod definition-rtype ((inst bir:vprimop))
  (list (first (cc-bir:primop-rtype-info (bir:info inst)))))

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
  (error "BUG: transitive-rtype should make this impossible!"))
(defmethod %use-rtype ((inst bir:jump) (datum bir:datum))
  (error "BUG: transitive-rtype should make this impossible!"))
(defmethod %use-rtype ((inst bir:thei) (datum bir:datum))
  ;; actual type tests, which need multiple values, should have been turned
  ;; into mv calls by this point. but out of an abundance of caution,
  (if (symbolp (bir:type-check-function inst))
      (use-rtype (first (bir:outputs inst)))
      :multiple-values))
             
;; Determine the rtype a datum needs to end up as by chasing transitive use.
(defun transitive-rtype (datum)
  (loop (let ((use (bir:use datum)))
          (etypecase use
            (null (return '())) ; don't need any value at all
            ((or bir:jump bir:unwind)
             (setf datum (nth (position datum (bir:inputs use))
                              (bir:outputs use))))
            (bir:instruction (return (%use-rtype use datum)))))))
(defmethod use-rtype ((datum bir:phi)) (transitive-rtype datum))
(defmethod use-rtype ((datum bir:output)) (transitive-rtype datum))
(defmethod use-rtype ((datum bir:argument)) (transitive-rtype datum))

;;; Given two value rtypes, return the most preferable.
;;; More sophisticated representation selection may be required in the future.
(defun min-vrtype (vrt1 vrt2)
  (if (or (eql vrt1 :single-float) (eql vrt2 :single-float))
      :single-float
      :object))

(defun max-vrtype (vrt1 vrt2)
  (if (and (eql vrt1 :single-float) (eql vrt2 :single-float))
      :single-float
      :object))

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

(defun assign-output-rtype (datum)
  (let* ((source (definition-rtype (bir:definition datum)))
         (dest (use-rtype datum))
         (rtype (min-rtype source dest)))
    (change-class datum 'cc-bmir:output :rtype rtype)
    rtype))

(defun phi-rtype (datum)
  ;; PHIs are trickier. If the destination is single-value, the phi can be too.
  ;; If not, then the phi could still be single-value, but only if EVERY
  ;; definition is, and otherwise we need to use multiple values.
  (let ((rt :any) (dest (use-rtype datum)))
    (if (eq dest :multiple-values)
        ;; If the phi definitions are all non-mv and agree on a number of
        ;; values, that works.
        (cleavir-set:doset (def (bir:definitions datum) rt)
          (etypecase def
            ((or bir:jump bir:unwind)
             (let ((in (nth (position datum (bir:outputs def))
                            (bir:inputs def))))
               (maybe-assign-rtype in)
               (let ((irt (cc-bmir:rtype in)))
                 (cond ((eq irt :multiple-values) (return irt)) ; nothing for it
                       ((eq rt :any) (setf rt irt))
                       ((= (length rt) (length irt))
                        (setf rt (mapcar #'max-vrtype rt irt)))
                       ;; different value counts
                       (t (return :multiple-values))))))))
        ;; Destination only needs some fixed thing - do that
        dest)))

(defun assign-phi-rtype (datum)
  (change-class datum 'cc-bmir:phi :rtype (phi-rtype datum)))

(defgeneric maybe-assign-rtype (datum))
(defmethod maybe-assign-rtype ((datum cc-bmir:output)))
(defmethod maybe-assign-rtype ((datum cc-bmir:phi)))
(defmethod maybe-assign-rtype ((datum bir:variable)))
(defmethod maybe-assign-rtype ((datum bir:argument)))
(defmethod maybe-assign-rtype ((datum bir:load-time-value)))
(defmethod maybe-assign-rtype ((datum bir:constant)))
(defmethod maybe-assign-rtype ((datum bir:output))
  (assign-output-rtype datum))
(defmethod maybe-assign-rtype ((datum bir:phi))
  (assign-phi-rtype datum))

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
  (object-inputs instruction)
  (cast-output instruction (make-list (length (bir:inputs instruction))
                                      :initial-element :object)))
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
      (cast-output inst (list ret)))))

(defun insert-casts-into-function (function)
  (cleavir-bir:map-local-instructions #'insert-casts function))

(defun insert-casts-into-module (module)
  (bir:map-functions #'insert-casts-into-function module))
