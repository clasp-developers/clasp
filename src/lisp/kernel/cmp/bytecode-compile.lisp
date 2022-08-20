#+(or)
(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile))

(in-package #:cmp)

(setq *print-circle* t)

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                #-clasp ; collides with core:decode, and we don't need it.
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +parse-key-args+
    +jump-8+ +jump-16+ +jump-24+
    +jump-if-8+ +jump-if-16+ +jump-if-24+
    +jump-if-supplied-8+ +jump-if-supplied-16+
    +check-arg-count<=+ +check-arg-count>=+ +check-arg-count=+
    +push-values+ +append-values+ +pop-values+
    +mv-call+ +mv-call-receive-one+ +mv-call-receive-fixed+
    +entry+
    +exit-8+ +exit-16+ +exit-24+
    +entry-close+
    +catch-8+ +catch-16+
    +throw+ +catch-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +progv+
    +fdefinition+
    +nil+
    +eq+
    +push+ +pop+
    +long+))

;;;

;; An annotation in the function.
(defstruct (annotation (:type vector))
  ;; The function containing this annotation.
  function
  ;; The index of this annotation in its function's annotations.
  index
  ;; The current (optimistic) position of this annotation in this function.
  position
  ;; The initial position of this annotaiton in this function.
  initial-position)

(defstruct (label (:include annotation) (:type vector)))

(defstruct (fixup (:include annotation)
                  (:type vector) :named
                  (:constructor make-fixup (label initial-size emitter resizer
                                            &aux (size initial-size))))
  ;; The label this fixup references.
  label
  ;; The current (optimistic) size of this fixup in bytes.
  size
  ;; The initial size of this fixup in bytes.
  initial-size
  ;; How to emit this fixup once sizes are resolved.
  emitter
  ;; How to resize this fixup. Returns the new size.
  resizer)

#+(or)
(defmethod print-object ((label label) stream)
  (print-unreadable-object (label stream :identity t)
    (format stream "LABEL :POSITION ~d" (annotation-position label))))

#+(or)
(defmethod print-object ((fixup fixup) stream)
  (print-unreadable-object (fixup stream :identity t)
    (format stream "FIXUP :POSITION ~d :SIZE ~d"
            (annotation-position fixup)
            (fixup-size fixup))))

;;; Optimistic positioning of ANNOTATION in its module.
(defun annotation-module-position (annotation)
  (+ (cfunction-position (annotation-function annotation))
     (annotation-position annotation)))

;;; The (module) displacement from this fixup to its label,
(defun fixup-delta (fixup)
  (- (annotation-module-position (fixup-label fixup))
     (annotation-module-position fixup)))

(defun emit-label (context label)
  (setf (label-position label) (length (context-assembly context)))
  (let ((function (context-function context)))
    (setf (label-function label) function)
    (setf (label-index label)
          (vector-push-extend label (cfunction-annotations function)))))

(defun assemble (context &rest values)
  (let ((assembly (context-assembly context)))
    (dolist (value values)
      (vector-push-extend value assembly))))

(defun assemble-into (code position &rest values)
  (do ((values values (rest values))
       (position position (1+ position)))
      ((null values))
    (setf (aref code position) (first values))))

;;; Write WORD of bytesize SIZE to VECTOR at POSITION.
(defun write-le-unsigned (vector word size position)
  (let ((end (+ position size)))
    (do ((position position (1+ position))
         (word word (ash word -8)))
        ((>= position end))
      (setf (aref vector position) (logand word #xff)))))

;;; Emit FIXUP into CONTEXT.
(defun emit-fixup (context fixup)
  (let* ((assembly (context-assembly context))
         (cfunction (context-function context))
         (position (length assembly)))
    (setf (fixup-function fixup) cfunction)
    (setf (fixup-initial-position fixup) position)
    (setf (fixup-position fixup) position)
    (setf (fixup-index fixup)
          (vector-push-extend fixup (cfunction-annotations cfunction)))
    (dotimes (i (fixup-initial-size fixup))
      (vector-push-extend 0 assembly))))

;;; Emit OPCODE and then a label reference.
(defun emit-control+label (context opcode8 opcode16 opcode24 label)
  (flet ((emitter (fixup position code)
           (let* ((size (fixup-size fixup))
                  (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
             (setf (aref code position)
                   (cond ((eql size 2) opcode8)
                         ((eql size 3) opcode16)
                         ((eql size 4) opcode24)
                         (t (error "Unknown size ~d" size))))
             (write-le-unsigned code offset (1- size) (1+ position))))
         (resizer (fixup)
           (let ((delta (fixup-delta fixup)))
             (cond ((typep delta '(signed-byte 8)) 2)
                   ((typep delta '(signed-byte 16)) 3)
                   ((typep delta '(signed-byte 24)) 4)
                   (t (error "???? PC offset too big ????"))))))
    (emit-fixup context (make-fixup label 2 #'emitter #'resizer))))

(defun emit-jump (context label)
  (emit-control+label context +jump-8+ +jump-16+ +jump-24+ label))
(defun emit-jump-if (context label)
  (emit-control+label context +jump-if-8+ +jump-if-16+ +jump-if-24+ label))
(defun emit-exit (context label)
  (emit-control+label context +exit-8+ +exit-16+ +exit-24+ label))
(defun emit-catch (context label)
  (emit-control+label context +catch-8+ +catch-16+ nil label))

(defun emit-jump-if-supplied (context index label)
  (flet ((emitter (fixup position code)
           (let* ((size (fixup-size fixup))
                  (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
             (setf (aref code position)
                   (cond
                     ((eql size 3) +jump-if-supplied-8+)
                     ((eql size 4) +jump-if-supplied-16+)
                     (t (error "Unknown size ~d" size))))
             (setf (aref code (1+ position)) index)
             (write-le-unsigned code offset (- size 2) (+ position 2))))
         (resizer (fixup)
           (let ((delta (fixup-delta fixup)))
             (cond ((typep delta '(signed-byte 8)) 3)
                   ((typep delta '(signed-byte 16)) 4)
                   (t (error "???? PC offset too big ????"))))))
    (emit-fixup context (make-fixup label 3 #'emitter #'resizer))))

;;; Different kinds of things can go in the variable namespace and they can
;;; all shadow each other, so we use this structure to disambiguate.
#-clasp
(defstruct (var-info (:constructor make-var-info (kind data)))
  (kind (error "kind required")
   :type (member :lexical :special :symbol-macro :constant))
  data)

#-clasp
(defstruct (lexical-info (:constructor make-lexical-info (frame-offset function)))
  frame-offset
  function
  (closed-over-p nil)
  (set-p nil))

#+clasp
(setf (fdefinition 'lexical-info-frame-offset)
      #'core:bytecode-cmp-lexical-var-info/frame-index
      (fdefinition 'lexical-info-function)
      #'core:bytecode-cmp-lexical-var-info/function
      (fdefinition 'lexical-info-closed-over-p)
      #'core:bytecode-cmp-lexical-var-info/closed-over-p
      (fdefinition '(setf lexical-info-closed-over-p))
      (lambda (new info)
        (core:bytecode-cmp-lexical-var-info/setf-closed-over-p info new))
      (fdefinition 'lexical-info-set-p)
      #'core:bytecode-cmp-lexical-var-info/set-p
      (fdefinition '(setf lexical-info-set-p))
      (lambda (new info)
        (core:bytecode-cmp-lexical-var-info/setf-set-p info new)))

;;; Does the variable with LEXICAL-INFO need a cell?
(defun indirect-lexical-p (lexical-info)
  (and (lexical-info-closed-over-p lexical-info)
       (lexical-info-set-p lexical-info)))

(defun make-lexical-var-info (frame-offset function)
  #+clasp
  (core:bytecode-cmp-lexical-var-info/make frame-offset function)
  #-clasp
  (make-var-info :lexical (make-lexical-info frame-offset function)))
(defun make-special-var-info ()
  #+clasp
  (core:bytecode-cmp-special-var-info/make)
  #-clasp
  (make-var-info :special nil))
(defun make-symbol-macro-var-info (expansion)
  #+clasp
  (core:bytecode-cmp-symbol-macro-var-info/make
   (lambda (form env) (declare (ignore form env)) expansion))
  #-clasp
  (make-var-info :symbol-macro expansion))
(defun make-constant-var-info (value)
  #+clasp
  (core:bytecode-cmp-constant-var-info/make value)
  #-clasp
  (make-var-info :constant value))

#+clasp
(defun var-info-kind (info)
  (cond ((null info) info)
        ((typep info 'core:bytecode-cmp-lexical-var-info) :lexical)
        ((typep info 'core:bytecode-cmp-special-var-info) :special)
        ((typep info 'core:bytecode-cmp-symbol-macro-var-info) :symbol-macro)
        ((typep info 'core:bytecode-cmp-constant-var-info) :constant)
        (t (error "Unknown info ~a" info))))

#+clasp
(defun var-info-data (info)
  (cond ((null info) info)
        ((typep info 'core:bytecode-cmp-lexical-var-info) info)
        ((typep info 'core:bytecode-cmp-special-var-info) 'nil)
        ((typep info 'core:bytecode-cmp-symbol-macro-var-info)
         (core:bytecode-cmp-symbol-macro-var-info/expander info))
        ((typep info 'core:bytecode-cmp-constant-var-info)
         (core:bytecode-cmp-constant-var-info/value info))
        (t (error "Unknown info ~a" info))))

#-clasp
(defstruct (fun-info (:constructor make-fun-info (kind data)))
  (kind (error "kind required")
   :type (member :global-function :global-macro
                 :local-function :local-macro))
  data)

(defun make-global-function-fun-info ()
  #+clasp
  (core:bytecode-cmp-global-fun-info/make)
  #-clasp
  (make-fun-info :global-function nil))
(defun make-global-macro-fun-info (expander)
  #+clasp
  (core:bytecode-cmp-global-macro-info/make expander)
  #-clasp
  (make-fun-info :global-macro expander))
(defun make-local-function-fun-info (fun-var)
  #+clasp
  (core:bytecode-cmp-local-fun-info/make fun-var)
  #-clasp
  (make-fun-info :local-function fun-var))
(defun make-local-macro-fun-info (expander)
  #+clasp
  (core:bytecode-cmp-local-macro-info/make expander)
  #-clasp
  (make-fun-info :local-macro expander))

(defun fun-info-kind (info)
  (cond ((null info) nil)
        ((typep info 'core:bytecode-cmp-global-fun-info) :global-function)
        ((typep info 'core:bytecode-cmp-local-fun-info) :local-function)
        ((typep info 'core:bytecode-cmp-global-macro-info) :global-macro)
        ((typep info 'core:bytecode-cmp-local-macro-info) :local-macro)
        (t (error "Unknown info ~a" info))))

(defun fun-info-data (info)
  (cond ((null info) nil)
        ((typep info 'core:bytecode-cmp-global-fun-info) nil)
        ((typep info 'core:bytecode-cmp-local-fun-info)
         (core:bytecode-cmp-local-fun-info/fun-var info))
        ((typep info 'core:bytecode-cmp-global-macro-info)
         (core:bytecode-cmp-global-macro-info/expander info))
        ((typep info 'core:bytecode-cmp-local-macro-info)
         (core:bytecode-cmp-local-macro-info/expander info))
        (t (error "Unknown info ~a" info))))

#-clasp
(defstruct (lexical-environment (:constructor make-null-lexical-environment)
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An alist of (var . var-info) in the current environment.
  (vars nil :type list)
  ;; An alist of (tag tag-dynenv . label) in the current environment.
  (tags nil :type list)
  ;; An alist of (block block-dynenv . label) in the current environment.
  (blocks nil :type list)
  ;; An alist of (fun . fun-var) in the current environment.
  (funs nil :type list)
  ;; The current end of the frame.
  (frame-end 0 :type integer))

#+clasp
(progn
  (setf (fdefinition 'vars) #'core:bytecode-cmp-env/vars
        (fdefinition 'funs) #'core:bytecode-cmp-env/funs
        (fdefinition 'tags) #'core:bytecode-cmp-env/tags
        (fdefinition 'blocks) #'core:bytecode-cmp-env/blocks
        (fdefinition 'frame-end) #'core:bytecode-cmp-env/frame-end))

#+clasp
(defun make-null-lexical-environment ()
  (core:bytecode-cmp-env/make nil nil nil nil 0))

(defun make-lexical-environment (parent &key (vars (vars parent))
                                          (tags (tags parent))
                                          (blocks (blocks parent))
                                          (frame-end (frame-end parent))
                                          (funs (funs parent)))
  #+clasp
  (core:bytecode-cmp-env/make vars tags blocks funs frame-end)
  #-clasp
  (%make-lexical-environment
   :vars vars :tags tags :blocks blocks :frame-end frame-end :funs funs))

;;; Bind each variable to a stack location, returning a new lexical
;;; environment. The max local count in the current function is also
;;; updated.
(defun bind-vars (vars env context)
  (let* ((frame-start (frame-end env))
         (var-count (length vars))
         (frame-end (+ frame-start var-count))
         (function (context-function context)))
    (setf (cfunction-nlocals function)
          (max (cfunction-nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (vars vars (rest vars))
         (new-vars (vars env)
                   (acons (first vars) (make-lexical-var-info index function) new-vars)))
        ((>= index frame-end)
         (make-lexical-environment env :vars new-vars :frame-end frame-end))
      (when (constantp (first vars))
        (error "Cannot bind constant value ~a!" (first vars))))))

;;; Get information about a variable.
;;; Returns two values.
;;; The first is :LEXICAL, :SPECIAL, :CONSTANT, :SYMBOL-MACRO, or NIL.
;;; If the variable is lexical, the first is :LEXICAL and the second is more info.
;;; If the variable is special, the first is :SPECIAL and the second is NIL.
;;; If the variable is a macro, the first is :SYMBOL-MACRO and the second is
;;; the expansion.
;;; If the variable is a constant, :CONSTANT and the value.
;;; If the first value is NIL, the variable is unknown, and the second
;;; value is NIL.
(defun var-info (symbol env)
  (let ((info (cdr (assoc symbol (vars env)))))
    (cond (info (values (var-info-kind info) (var-info-data info)))
          ((constantp symbol nil) (values :constant (symbol-value symbol)))
          ((ext:specialp symbol) (values :special nil)) ; globally special
          (t (values nil nil)))))

;;; Like the above. Check the struct for details.
(defun fun-info (name env)
  (let ((info (cdr (assoc name (funs env)))))
    (cond (info (values (fun-info-kind info) (fun-info-data info)))
          ((and (symbolp name) (macro-function name nil))
           (values :global-macro (macro-function name nil)))
          ((and (symbolp name) (special-operator-p name))
           (error "Tried to get FUN-INFO for special operator ~s - that's impossible" name))
          ((fboundp name) (values :global-function nil))
          (t (values nil nil)))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defstruct (cfunction (:constructor make-cfunction (cmodule &key name doc))
                      (:type vector) :named)
  cmodule
  ;; Bytecode vector for this function.
  (bytecode (make-array 0 :element-type '(unsigned-byte 8)
                          :fill-pointer 0 :adjustable t))
  ;; An ordered vector of annotations emitted in this function.
  (annotations (make-array 0 :fill-pointer 0 :adjustable t))
  (nlocals 0)
  (closed (make-array 0 :fill-pointer 0 :adjustable t))
  (entry-point (make-label))
  ;; The position of the start of this function in this module
  ;; (optimistic).
  position
  ;; How much to add to the bytecode vector length for increased fixup
  ;; sizes for the true length.
  (extra 0)
  ;; The index of this function in the containing module's function
  ;; vector.
  index
  ;; The runtime function, used during link.
  info
  ;; Stuff for the function description
  name doc)

(defstruct (cmodule (:constructor make-cmodule (literals))
                    (:type vector))
  (cfunctions (make-array 1 :fill-pointer 0 :adjustable t))
  literals)

;;; The context contains information about what the current form needs
;;; to know about what it is enclosed by.
(defstruct (context (:type vector)) receiving function)

(defun context-module (context)
  (cfunction-cmodule (context-function context)))

(defun context-assembly (context)
  (cfunction-bytecode (context-function context)))

(defun literal-index (literal context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (position literal literals)
        (vector-push-extend literal literals))))

(defun closure-index (info context)
  (let ((closed (cfunction-closed (context-function context))))
    (or (position info closed)
        (vector-push-extend info closed))))

(defun new-context (parent &key (receiving (context-receiving parent))
                                (function (context-function parent)))
  (make-context :receiving receiving :function function))

(defun bytecompile (lambda-expression
                    &optional (env (make-null-lexical-environment)))
  (check-type lambda-expression lambda-expression)
  (let* ((module (make-cmodule (make-array 0 :fill-pointer 0 :adjustable t)))
         (lambda-list (cadr lambda-expression))
         (body (cddr lambda-expression)))
    (link-function (compile-lambda lambda-list body env module))))

(defun compile-form (form env context)
  (cond ((symbolp form) (compile-symbol form env context))
        ((consp form) (compile-cons (car form) (cdr form) env context))
        (t (compile-literal form env context))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (cond ((null form) (assemble context +nil+))
          (t (assemble context +const+ (literal-index form context))))
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(flet ((maybe-emit (lexical-info opcode context)
         (flet ((emitter (fixup position code)
                  #+clasp-min (declare (ignore fixup))
                  #-clasp-min
                  (assert (= (fixup-size fixup) 1))
                  (setf (aref code position) opcode))
                (resizer (fixup)
                  (declare (ignore fixup))
                  (if (indirect-lexical-p lexical-info) 1 0)))
           (emit-fixup context
                       (make-fixup lexical-info 0 #'emitter #'resizer)))))
  (defun maybe-emit-make-cell (lexical-info context)
    (maybe-emit lexical-info +make-cell+ context))
  (defun maybe-emit-cell-ref (lexical-info context)
    (maybe-emit lexical-info +cell-ref+ context)))

;;; FIXME: This is probably a good candidate for a specialized
;;; instruction.
(defun maybe-emit-encage (lexical-info context)
  (let ((index (lexical-info-frame-offset lexical-info)))
    (flet ((emitter (fixup position code)
             #+clasp-min (declare (ignore fixup))
             #-clasp-min
             (assert (= (fixup-size fixup) 5))
             (assemble-into code position
                            +ref+ index +make-cell+ +set+ index))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-info) 5 0)))
      (emit-fixup context (make-fixup lexical-info 0 #'emitter #'resizer)))))

(defun emit-lexical-set (lexical-info context)
  (let ((index (lexical-info-frame-offset lexical-info)))
    (flet ((emitter (fixup position code)
             (if (= (fixup-size fixup) 3)
                 (assemble-into code position +ref+ index +cell-set+)
                 (assemble-into code position +set+ index)))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-info) 3 2)))
      (emit-fixup context (make-fixup lexical-info 2 #'emitter #'resizer)))))

(defun compile-symbol (form env context)
  (multiple-value-bind (kind data) (var-info form env)
    (cond ((eq kind :symbol-macro)
           (compile-form (funcall *macroexpand-hook* data form env)
                         env context))
          ;; A symbol macro could expand into something with arbitrary side
          ;; effects so we always have to compile that, but otherwise, if no
          ;; values are wanted, we want to not compile anything.
          ((eql (context-receiving context) 0))
          (t
           (cond
             ((eq kind :lexical)
              (cond ((eq (lexical-info-function data) (context-function context))
                     (assemble context +ref+ (lexical-info-frame-offset data)))
                    (t
                     (setf (lexical-info-closed-over-p data) t)
                     (assemble context +closure+ (closure-index data context))))
              (maybe-emit-cell-ref data context))
             ((eq kind :special) (assemble context +symbol-value+
                           (literal-index form context)))
             ((eq kind :constant) (return-from compile-symbol ; don't pop again.
                            (compile-literal data env context)))
             ((null kind)
              (warn "Unknown variable ~a: treating as special" form)
              (assemble context +symbol-value+
                        (literal-index form context)))
             (t (error "Unknown kind ~a" kind)))
           (when (eq (context-receiving context) t)
             (assemble context +pop+))))))

(defun compile-cons (head rest env context)
  (cond
    ((eq head 'progn) (compile-progn rest env context))
    ((eq head 'let) (compile-let (first rest) (rest rest) env context))
    ((eq head 'let*) (compile-let* (first rest) (rest rest) env context))
    ((eq head 'flet) (compile-flet (first rest) (rest rest) env context))
    ((eq head 'labels) (compile-labels (first rest) (rest rest) env context))
    ((eq head 'setq) (compile-setq rest env context))
    ((eq head 'if) (compile-if (first rest) (second rest) (third rest) env context))
    ((eq head 'function) (compile-function (first rest) env context))
    ((eq head 'tagbody) (compile-tagbody rest env context))
    ((eq head 'go) (compile-go (first rest) env context))
    ((eq head 'block) (compile-block (first rest) (rest rest) env context))
    ((eq head 'return-from) (compile-return-from (first rest) (second rest) env context))
    ((eq head 'catch) (compile-catch (first rest) (rest rest) env context))
    ((eq head 'throw) (compile-throw (first rest) (second rest) env context))
    ((eq head 'progv) (compile-progv (first rest) (second rest) (rest (rest rest)) env context))
    ((eq head 'quote) (compile-literal (first rest) env context))
    ((eq head 'symbol-macrolet)
     (compile-symbol-macrolet (first rest) (rest rest) env context))
    #+clasp
    ((eq head 'macrolet)
     (compile-macrolet (first rest) (rest rest) env context))
    ((eq head 'multiple-value-call)
     (compile-multiple-value-call (first rest) (rest rest) env context))
    ((eq head 'multiple-value-prog1)
     (compile-multiple-value-prog1 (first rest) (rest rest) env context))
    ((eq head 'locally) (compile-locally rest env context))
    ((eq head 'eval-when) (compile-eval-when (first rest) (rest rest) env context))
    ((eq head 'the) ; don't do anything.
     (compile-form (second rest) env context))
    (t ; function call or macro
     (multiple-value-bind (kind data) (fun-info head env)
       (cond
         ((member kind '(:global-macro :local-macro))
          (compile-form (funcall *macroexpand-hook* data (cons head rest) env)
                        env context))
         ((member kind '(:global-function :local-function nil))
          ;; unknown function warning handled by compile-function
          ;; note we do a double lookup, which is inefficient
          (compile-function head env (new-context context :receiving 1))
          (dolist (arg rest)
            (compile-form arg env (new-context context :receiving 1)))
          (let ((receiving (context-receiving context)))
            (cond ((eq receiving t) (assemble context +call+ (length rest)))
                  ((eql receiving 1)
                   (assemble context +call-receive-one+ (length rest)))
                  (t (assemble context
                               +call-receive-fixed+ (length rest) receiving)))))
         (t (error "Unknown kind ~a" kind)))))))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms)))
      ((null (rest forms))
       (compile-form (first forms) env context))
    (compile-form (first forms) env (new-context context :receiving 0))))

;;; Add VARS as specials in ENV.
(defun add-specials (vars env)
  (make-lexical-environment
   env
   :vars (append (mapcar (lambda (var)
                           (cons var (make-special-var-info)))
                         vars)
                 (vars env))))

(defun compile-locally (body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (compile-progn body (if specials (add-specials specials env) env) context)))

(defun compile-eval-when (situations body env context)
  (if (or (member 'cl:eval situations) (member :execute situations))
      (compile-progn body env context)
      (compile-literal nil env context)))

(defun parse-let (bindings env)
  (let ((lexical-bindings '())
        (special-bindings '()))
    (dolist (binding bindings)
      (if (symbolp binding)
          (if (eq (var-info binding env) :special)
              (push (cons binding nil) special-bindings)
              (push (cons binding nil) lexical-bindings))
          (if (eq (var-info (first binding) env) :special)
              (push binding special-bindings)
              (push binding lexical-bindings))))
    (values (nreverse lexical-bindings) (nreverse special-bindings))))

(defun compile-let (bindings body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let ((env (add-specials specials env))
          (lexical-count 0))
      (multiple-value-bind (lexical-bindings special-bindings)
          (parse-let bindings env)
        (let ((new-env (bind-vars (mapcar #'first lexical-bindings) env context))
              (special-count 0))
          (dolist (binding lexical-bindings)
            ;; Make sure we compile the forms in the old env.
            (compile-form (second binding) env (new-context context :receiving 1))
            (maybe-emit-make-cell (nth-value 1 (var-info (first binding) new-env)) context)
            (incf lexical-count))
          (when lexical-bindings
            ;; ... And use the end of the old env's frame.
            (assemble context +bind+ lexical-count (frame-end env)))
          (dolist (binding special-bindings)
            (compile-form (second binding) new-env (new-context context :receiving 1))
            (assemble context +special-bind+ (literal-index (first binding) context))
            (incf special-count))
          (compile-progn body new-env context)
          (dotimes (_ special-count)
            (assemble context +unbind+)))))))

(defun compile-let* (bindings body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let ((special-binding-count 0))
      (dolist (binding bindings)
        (let ((var (if (consp binding) (car binding) binding))
              (valf (if (and (consp binding) (consp (cdr binding)))
                        (cadr binding)
                        'nil)))
          (compile-form valf env (new-context context :receiving 1))
          (cond ((or (member var specials) (ext:specialp var))
                 (incf special-binding-count)
                 (setq env (make-lexical-environment
                            env
                            :vars (acons var (make-special-var-info)
                                         (vars env))))
                 (assemble context +special-bind+ (literal-index var context)))
                (t
                 (let ((frame-start (frame-end env)))
                   (setq env (bind-vars (list var) env context))
                   (maybe-emit-make-cell (nth-value 1 (var-info var env))
                                         context)
                   (assemble context +set+ frame-start))))))
      (compile-progn body
                     (if specials
                         ;; We do this to make sure special declarations get
                         ;; through even if this form doesn't bind them.
                         ;; This creates duplicate alist entries for anything
                         ;; that _is_ bound here, but that's not a big deal.
                         (add-specials specials env)
                         env)
                     context)
      (dotimes (_ special-binding-count)
        #-clasp
        (declare (ignore _))
        (assemble context +unbind+)))))

(defun compile-setq (pairs env context)
  (if (null pairs)
      (unless (eql (context-receiving context) 0)
        (assemble context +nil+))
      (do ((pairs pairs (cddr pairs)))
          ((endp pairs))
        (let ((var (car pairs))
              (valf (cadr pairs))
              (rest (cddr pairs)))
          (compile-setq-1 var valf env
                          (if rest
                              (new-context context :receiving 0)
                              context))))))

(defun compile-setq-1 (var valf env context)
  (multiple-value-bind (kind data) (var-info var env)
    (cond
      ((eq kind :symbol-macro)
       (compile-form `(setf ,data ,valf) env context))
      ((or (eq kind :special) (null kind))
       (when (null kind)
         (warn "Unknown variable ~a: treating as special" var))
       (compile-form valf env (new-context context :receiving 1))
       ;; If we need to return the new value, stick it into a new local
       ;; variable, do the set, then return the lexical variable.
       ;; We can't just read from the special, since some other thread may
       ;; alter it.
       (let ((index (frame-end env)))
         (unless (eql (context-receiving context) 0)
           (assemble context +set+ index +ref+ index)
           ;; called for effect, i.e. to keep frame size correct
           (bind-vars (list var) env context))
         (assemble context +symbol-value-set+ (literal-index var context))
         (unless (eql (context-receiving context) 0)
           (assemble context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+)))))
      ((eq kind :lexical)
       (let ((localp (eq (lexical-info-function data)
                         (context-function context)))
             (index (frame-end env)))
         (unless localp
           (setf (lexical-info-closed-over-p data) t))
         (setf (lexical-info-set-p data) t)
         (compile-form valf env (new-context context :receiving 1))
         ;; similar concerns to specials above.
         (unless (eql (context-receiving context) 0)
           (assemble context +set+ index +ref+ index)
           (bind-vars (list var) env context))
         (cond (localp
                (emit-lexical-set data context))
               ;; Don't emit a fixup if we already know we need a cell.
               (t
                (assemble context +closure+ (closure-index data context))
                (assemble context +cell-set+)))
         (unless (eql (context-receiving context) 0)
           (assemble context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+)))))
      (t (error "Unknown kind ~a" kind)))))

(defun compile-flet (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0)
        ;; HACK FIXME
        (frame-slot (frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             ,@(cddr definition))
                          env (new-context context :receiving 1))
        (push fun-var fun-vars)
        (push (cons name (make-local-function-fun-info
                          (make-lexical-var-info frame-slot
                                                 (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (assemble context +bind+ fun-count (frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (funs env)))))
      (compile-locally body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-count 0)
        (funs '())
        (fun-vars '())
        (closures '())
        (env env)
        (frame-start (frame-end env))
        (frame-slot (frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push fun-var fun-vars)
        (push (cons name (make-local-function-fun-info
                          (make-lexical-info frame-slot
                                             (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (let ((frame-slot (frame-end env))
          (env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (funs env)))))
      (dolist (definition definitions)
        (let* ((fun (compile-lambda (second definition)
                                    (rest (rest definition))
                                    env
                                    (context-module context)))
               (literal-index (literal-index fun context)))
          (cond ((zerop (length (cfunction-closed fun)))
                 (assemble context +const+ literal-index))
                (t
                 (push (cons fun frame-slot) closures)
                 (assemble context +make-uninitialized-closure+
                   literal-index))))
        (incf frame-slot))
      (assemble context +bind+ fun-count frame-start)
      (dolist (closure closures)
        (dotimes (i (length (cfunction-closed (car closure))))
          (reference-lexical-info (aref (cfunction-closed (car closure)) i)
                                  context))
        (assemble context +initialize-closure+ (cdr closure)))
      (compile-progn body env context))))

(defun compile-if (condition then else env context)
  (compile-form condition env (new-context context :receiving 1))
  (let ((then-label (make-label))
        (done-label (make-label)))
    (emit-jump-if context then-label)
    (compile-form else env context)
    (emit-jump context done-label)
    (emit-label context then-label)
    (compile-form then env context)
    (emit-label context done-label)))

;;; Push the immutable value or cell of lexical in CONTEXT.
(defun reference-lexical-info (info context)
  (if (eq (lexical-info-function info) (context-function context))
      (assemble context +ref+ (lexical-info-frame-offset info))
      (assemble context +closure+ (closure-index info context))))

(defun compile-function (fnameoid env context)
  (unless (eql (context-receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((cfunction (compile-lambda (cadr fnameoid) (cddr fnameoid)
                                          env (context-module context)))
               (closed (cfunction-closed cfunction)))
          (dotimes (i (length closed))
            (reference-lexical-info (aref closed i) context))
          (if (zerop (length closed))
              (assemble context +const+ (literal-index cfunction context))
              (assemble context +make-closure+ (literal-index cfunction context))))
        (multiple-value-bind (kind data) (fun-info fnameoid env)
          (cond
            ((member kind '(:global-function nil))
             (when (null kind) (warn "Unknown function ~a" fnameoid))
             (assemble context +fdefinition+ (literal-index fnameoid context)))
            ((member kind '(:local-function))
             (reference-lexical-info data context))
            (t (error "Unknown kind ~a" kind)))))
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

;;; Deal with lambda lists. Return the new environment resulting from
;;; binding these lambda vars.
;;; Optional/key handling is done in two steps:
;;;
;;; 1. Bind any supplied optional/key vars to the passed values.
;;;
;;; 2. Default any unsupplied optional/key values and set the
;;; corresponding suppliedp var for each optional/key.
(defun compile-lambda-list (lambda-list env context)
  (multiple-value-bind (required optionals rest key-flag keys aok-p aux)
      (core:process-lambda-list lambda-list 'function)
    (let* ((function (context-function context))
           (entry-point (cfunction-entry-point function))
           (min-count (car required))
           (optional-count (car optionals))
           (max-count (+ min-count optional-count))
           (key-count (car keys))
           (more-p (or rest key-flag))
           (env (bind-vars (cdr required) env context)))
      (emit-label context entry-point)
      ;; Check that a valid number of arguments have been
      ;; supplied to this function.
      (cond ((and required (= min-count max-count) (not more-p))
             (assemble context +check-arg-count=+ min-count))
            (t
             (when required
               (assemble context +check-arg-count>=+ min-count))
             (when (not more-p)
               (assemble context +check-arg-count<=+ max-count))))
      (unless (zerop min-count)
        (assemble context +bind-required-args+ min-count)
        (dolist (var (cdr required))
          (maybe-emit-encage (nth-value 1 (var-info var env)) context)))
      (unless (zerop optional-count)
        (assemble context +bind-optional-args+
          min-count
          optional-count)
        (let ((vars nil))
          (do ((opts (cdr optionals) (cdddr opts)))
              ((endp opts) (setq env (bind-vars (nreverse vars) env context)))
            (push (car opts) vars))))
      (when rest
        (assemble context +listify-rest-args+ max-count)
        (assemble context +set+ (frame-end env))
        (setq env (bind-vars (list rest) env context))
        (maybe-emit-encage (nth-value 1 (var-info rest env)) context))
      (when key-flag
        (let ((key-names nil))
          (do ((keys (cdr keys) (cddddr keys)))
              ((endp keys))
            (push (car keys) key-names))
          (setq key-names (nreverse key-names))
          (assemble context +parse-key-args+
            max-count
            (if aok-p (- key-count) key-count)
            (literal-index (first key-names) context)
            (frame-end env))
          (dolist (key-name (rest key-names))
            (literal-index key-name context)))
        (let ((keyvars nil))
          (do ((keys (cdr keys) (cddddr keys)))
              ((endp keys)
               (setq env (bind-vars (nreverse keyvars) env context)))
            (push (cadr keys) keyvars))))
      (unless (zerop optional-count)
        (do ((optionals (cdr optionals) (cdddr optionals))
             (optional-label (make-label) next-optional-label)
             (next-optional-label (make-label) (make-label)))
            ((null optionals)
             (emit-label context optional-label))
          (emit-label context optional-label)
          (let ((optional-var (car optionals))
                (defaulting-form (cadr optionals))
                (supplied-var (caddr optionals)))
            (setq env
                  (compile-optional/key-item optional-var defaulting-form supplied-var
                                             next-optional-label context env)))))
      (when key-flag
        (do ((keys (cdr keys) (cddddr keys))
             (key-label (make-label) next-key-label)
             (next-key-label (make-label) (make-label)))
            ((null keys)
             (emit-label context key-label))
          (emit-label context key-label)
          (let ((key-name (car keys)) (key-var (cadr keys))
                (defaulting-form (caddr keys)) (supplied-var (cadddr keys)))
            (declare (ignore key-name))
            (setq env
                  (compile-optional/key-item key-var defaulting-form supplied-var
                                             next-key-label context env)))))
      (values
       ;; convert from process-lambda-list's aux format (var val var val) to bindings.
       (let ((bindings nil))
         (do ((aux aux (cddr aux)))
             ((endp aux) (nreverse bindings))
           (push (list (car aux) (cadr aux)) bindings)))
       env))))

;;; Compile an optional/key item and return the resulting environment.
(defun compile-optional/key-item (var defaulting-form supplied-var next-label
                                  context env)
  (flet ((default (suppliedp info)
           (cond (suppliedp
                  (maybe-emit-encage info context))
                 (t
                  (compile-form defaulting-form env
                                (new-context context :receiving 1))
                  (maybe-emit-make-cell info context)
                  (assemble context +set+ (lexical-info-frame-offset info)))))
         (supply (suppliedp info)
           (if suppliedp
               (compile-literal t env (new-context context :receiving 1))
               (assemble context +nil+))
           (maybe-emit-make-cell info context)
           (assemble context +set+ (lexical-info-frame-offset info))))
    (let ((supplied-label (make-label))
          (var-info (nth-value 1 (var-info var env))))
      (multiple-value-bind (env supplied-var-info)
          (if supplied-var
              (let ((env (bind-vars (list supplied-var) env context)))
                (values env (nth-value 1 (var-info supplied-var env))))
              (values env nil))
        (emit-jump-if-supplied context (lexical-info-frame-offset var-info) supplied-label)
        (default nil var-info)
        (when supplied-var
          (supply nil supplied-var-info))
        (emit-jump context next-label)
        (emit-label context supplied-label)
        (default t var-info)
        (when supplied-var
          (supply t supplied-var-info))
        env))))

;;; Compile the lambda in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (lambda-list body env module)
  (multiple-value-bind (decls body docs)
      (core:process-declarations body t)
    (let* ((name (or (core:extract-lambda-name-from-declares decls)
                     `(lambda ,(lambda-list-for-name lambda-list))))
           (function (make-cfunction module :name name :doc docs))
           (context (make-context :receiving t :function function))
           (env (make-lexical-environment env :frame-end 0)))
      (setf (cfunction-index function)
            (vector-push-extend function (cmodule-cfunctions module)))
      (multiple-value-bind (aux-bindings env)
          (compile-lambda-list lambda-list env context)
        (compile-let* aux-bindings body env context))
      (assemble context +return+)
      function)))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let* ((new-tags (tags env))
         (tagbody-dynenv (gensym "TAG-DYNENV"))
         (env (bind-vars (list tagbody-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info tagbody-dynenv env))))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement dynenv-info (make-label))
              new-tags)))
    (assemble context +entry+)
    (let ((env (make-lexical-environment env :tags new-tags)))
      ;; Bind the dynamic environment. We don't need a cell as it is
      ;; not mutable.
      (assemble context +set+ (lexical-info-frame-offset dynenv-info))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cddr (assoc statement (tags env))))
            (compile-form statement env (new-context context :receiving 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (context-receiving context) 0)
    (assemble context +nil+)
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (tags env))))
    (if pair
        (destructuring-bind (dynenv-info . tag-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (bind-vars (list block-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info block-dynenv env)))
         (label (make-label)))
    (assemble context +entry+)
    ;; Bind the dynamic environment. We don't need a cell as it is
    ;; not mutable.
    (assemble context +set+ (lexical-info-frame-offset dynenv-info))
    (let ((env (make-lexical-environment
                env
                :blocks (acons name (cons dynenv-info label) (blocks env)))))
      ;; Force single values into multiple so that we can uniformly PUSH afterward.
      (compile-progn body env (new-context context :receiving t)))
    (emit-label context label)
    (assemble context +entry-close+)
    (when (eql (context-receiving context) 1)
      (assemble context +push+))))

(defun compile-return-from (name value env context)
  (compile-form value env (new-context context :receiving t))
  (let ((pair (assoc name (blocks env))))
    (if pair
        (destructuring-bind (dynenv-info . block-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context block-label))
        (error "The block ~a does not exist." name))))

(defun compile-catch (tag body env context)
  (compile-form tag env (new-context context :receiving 1))
  (let ((target (make-label)))
    (emit-catch context target)
    (compile-progn body env context)
    (assemble context +catch-close+)
    (emit-label context target)))

(defun compile-throw (tag result env context)
  (compile-form tag env (new-context context :receiving 1))
  (compile-form result env (new-context context :receiving t))
  (assemble context +throw+))

(defun compile-progv (symbols values body env context)
  (compile-form symbols env (new-context context :receiving 1))
  (compile-form values env (new-context context :receiving 1))
  (assemble context +progv+)
  (compile-progn body env context)
  (assemble context +unbind+))

(defun compile-symbol-macrolet (bindings body env context)
  (let ((smacros nil))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (compile-locally body (make-lexical-environment
                           env
                           :vars (append (nreverse smacros) (vars env)))
                     context)))

(defun lexenv-for-macrolet (env)
  ;; Macrolet expanders need to be compiled in the local compilation environment,
  ;; so that e.g. their bodies can use macros defined in outer macrolets.
  ;; At the same time, they obviously do not have access to any runtime
  ;; environment. Taking out all runtime information is one way to do this but
  ;; it's slightly not-nice in that if someone writes a macroexpander that does
  ;; try to use local runtime information may fail silently by using global info
  ;; instead. So: KLUDGE.
  (make-lexical-environment
   env
   :vars (let ((cpairs nil))
           (dolist (pair (vars env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (member (var-info-kind info) '(:constant :symbol-macro))
                 (push pair cpairs)))))
   :funs (let ((cpairs nil))
           (dolist (pair (funs env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (member (fun-info-kind info) '(:global-macro :local-macro))
                 (push pair cpairs)))))
   :tags nil :blocks nil :frame-end 0))

#+clasp
(defun compile-macrolet (bindings body env context)
  (let ((macros nil))
    (dolist (binding bindings)
      (let* ((name (car binding)) (lambda-list (cadr binding))
             (body (cddr binding))
             (eform (ext:parse-macro name lambda-list body env))
             (aenv (lexenv-for-macrolet env))
             (expander (bytecompile eform aenv))
             (info (make-local-macro-fun-info expander)))
        (push (cons name info) macros)))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (funs env)))
                     context)))

(defun compile-multiple-value-call (function-form forms env context)
  (compile-form function-form env (new-context context :receiving 1))
  (let ((first (first forms))
        (rest (rest forms)))
    (compile-form first env (new-context context :receiving t))
    (when rest
      (assemble context +push-values+)
      (dolist (form rest)
        (compile-form form env (new-context context :receiving t))
        (assemble context +append-values+))
      (assemble context +pop-values+)))
  (let ((receiving (context-receiving context)))
    (cond ((eq receiving t) (assemble context +mv-call+))
          ((eql receiving 1) (assemble context +mv-call-receive-one+))
          (t (assemble context +mv-call-receive-fixed+ receiving)))))

(defun compile-multiple-value-prog1 (first-form forms env context)
  (compile-form first-form env context)
  (unless (member (context-receiving context) '(0 1))
    (assemble context +push-values+))
  (dolist (form forms)
    (compile-form form env (new-context context :receiving 0)))
  (unless (member (context-receiving context) '(0 1))
    (assemble context +pop-values+)))

;;;; linkage

(defun unsigned (x size)
  (logand x (1- (ash 1 size))))

;;; Use the optimistic bytecode vector sizes to initialize the optimistic cfunction position.
(defun initialize-cfunction-positions (cmodule)
  (let ((position 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let ((function (aref (cmodule-cfunctions cmodule) i)))
        (setf (cfunction-position function) position)
        (incf position (length (cfunction-bytecode function)))))))

;;; Update the positions of all affected functions and annotations
;;; from the effect of increasing the size of FIXUP by INCREASE. The
;;; resizer has already updated the size of the the fixup.
(defun update-positions (fixup increase)
  (let ((function (fixup-function fixup)))
    ;; Update affected annotation positions in this function.
    (let ((annotations (cfunction-annotations function)))
      (do ((index (1+ (fixup-index fixup)) (1+ index)))
          ((= index (length annotations)))
        (let ((annotation (aref annotations index)))
          (incf (annotation-position annotation) increase))))
    ;; Increase the size of this function to account for fixup growth.
    (incf (cfunction-extra function) increase)
    ;; Update module offsets for affected functions.
    (let ((functions (cmodule-cfunctions (cfunction-cmodule function))))
      (do ((index (1+ (cfunction-index function)) (1+ index)))
          ((= index (length functions)))
        (let ((function (aref functions index)))
          (incf (cfunction-position function) increase))))))

;;; With all functions and annotations initialized with optimistic
;;; sizes, resize fixups until no more expansion is needed.
(defun resolve-fixup-sizes (cmodule)
  (loop
    (let ((changed-p nil)
          (functions (cmodule-cfunctions cmodule)))
      (dotimes (i (length functions))
        (dotimes (j (length (cfunction-annotations (aref functions i))))
          (let ((annotation (aref (cfunction-annotations (aref functions i)) j)))
            (when (fixup-p annotation)
              (let ((old-size (fixup-size annotation))
                    (new-size (funcall (fixup-resizer annotation) annotation)))
                (unless (= old-size new-size)
                  #+(or)
                  (assert (>= new-size old-size))
                  (setf (fixup-size annotation) new-size)
                  (setq changed-p t)
                  (update-positions annotation (- new-size old-size))))))))
      (unless changed-p
        (return)))))

;;; The size of the module bytecode vector.
(defun module-bytecode-size (cmodule)
  (let* ((cfunctions (cmodule-cfunctions cmodule))
         (last-cfunction (aref cfunctions (1- (length cfunctions)))))
    (+ (cfunction-position last-cfunction)
       (length (cfunction-bytecode last-cfunction))
       (cfunction-extra last-cfunction))))

;;; Create the bytecode module vector. We scan over the fixups in the
;;; module and copy segments of bytecode between fixup positions.
(defun create-module-bytecode (cmodule)
  (let ((bytecode (make-array (module-bytecode-size cmodule)
                              :element-type '(unsigned-byte 8)))
        (index 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let* ((function (aref (cmodule-cfunctions cmodule) i))
             (cfunction-bytecode (cfunction-bytecode function))
             (position 0))
        (dotimes (i (length (cfunction-annotations function)))
          (let ((annotation (aref (cfunction-annotations function) i)))
            (when (fixup-p annotation)
              (unless (zerop (fixup-size annotation))
                #+(or)
              (assert (= (fixup-size annotation)
                         (funcall (fixup-resizer annotation) annotation)))
              ;; Copy bytes in this segment.
              (let ((end (fixup-initial-position annotation)))
                (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
                (incf index (- end position))
                (setf position end))
              #+(or)
              (assert (= index (annotation-module-position annotation)))
              ;; Emit fixup.
              (funcall (fixup-emitter annotation)
                       annotation
                       index
                       bytecode)
              (incf position (fixup-initial-size annotation))
              (incf index (fixup-size annotation))))))
        ;; Copy any remaining bytes from this function to the module.
        (let ((end (length cfunction-bytecode)))
          (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
          (incf index (- end position)))))
    bytecode))

;;; Copied from codegen.lisp.
(defun lambda-list-for-name (raw-lambda-list)
  (multiple-value-bind (req opt rest keyflag keys aok-p)
      (core:process-lambda-list raw-lambda-list 'function)
    `(,@(rest req)
      ,@(unless (zerop (first opt)) '(&optional))
      ,@(let ((optnames nil))
          (do ((opts (rest opt) (cdddr opts)))
              ((null opts) (nreverse optnames))
            (push (first opts) optnames)))
      ,@(when rest `(&rest ,rest))
      ,@(when keyflag '(&key))
      ,@(let ((keykeys nil))
          (do ((key (rest keys) (cddddr key)))
              ((null key) keykeys)
            (push (first key) keykeys)))
      ,@(when aok-p '(&allow-other-keys)))))

;;; Run down the hierarchy and link the compile time representations
;;; of modules and functions together into runtime objects. Return the
;;; bytecode function corresponding to CFUNCTION.
(defun link-function (cfunction)
  (declare (optimize debug))
  (let ((cmodule (cfunction-cmodule cfunction)))
    (initialize-cfunction-positions cmodule)
    (resolve-fixup-sizes cmodule)
    (let* ((cmodule-literals (cmodule-literals cmodule))
           (literal-length (length cmodule-literals))
           (literals (make-array literal-length))
           (bytecode (create-module-bytecode cmodule))
           (bytecode-module
             #-clasp
             (vm::make-bytecode-module
              :bytecode bytecode
              :literals literals)
             #+clasp
             (core:bytecode-module/make)))
      ;; Create the real function objects.
      (dotimes (i (length (cmodule-cfunctions cmodule)))
        (let ((cfunction (aref (cmodule-cfunctions cmodule) i)))
          (setf (cfunction-info cfunction)
                #-clasp
                (vm::make-bytecode-function
                 bytecode-module
                 (cfunction-nlocals cfunction)
                 (length (cfunction-closed cfunction))
                 (annotation-module-position (cfunction-entry-point cfunction)))
                #+clasp
                (core:global-bytecode-entry-point/make
                 (core:function-description/make
                  :function-name (cfunction-name cfunction)
                  :docstring (cfunction-doc cfunction))
                 bytecode-module
                 (cfunction-nlocals cfunction)
                 0 0 0 0 nil 0 ; unused at the moment
                 (length (cfunction-closed cfunction))
                 (make-list 7 :initial-element
                            (annotation-module-position (cfunction-entry-point cfunction)))))))
      ;; Now replace the cfunctions in the cmodule literal vector with
      ;; real bytecode functions.
      (dotimes (index literal-length)
        (setf (aref literals index)
              (let ((literal (aref cmodule-literals index)))
                (if (cfunction-p literal)
                    (cfunction-info literal)
                    literal))))
      #+clasp
      (progn
        (core:bytecode-module/setf-literals bytecode-module literals)
        ;; Now just install the bytecode and Bob's your uncle.
        (core:bytecode-module/setf-bytecode bytecode-module bytecode))))
  (cfunction-info cfunction))

;;; --------------------------------------------------
;;;
;;; Generate C++ code for the VM bytecodes
;;;
;;; Generate an enum called vm_codes that sets the values
;;; for all of the vm bytecodes according to the order in
;;; which they are defined above in *codes*.
;;;

#+(or)
(defun c++ify (name)
  (flet ((submatch (substr remain)
           (let ((sublen (length substr)))
             (and (>= (length remain) sublen) (string= substr remain :start2 0 :end2 sublen)))))
    (with-output-to-string (sout)
      (loop for index below (length name)
            for remain = (subseq name index)
            for chr = (elt remain 0)
            do (cond
                 ((submatch "/=" remain)
                  (format sout "_NE_")
                  (incf index))
                 ((submatch ">=" remain)
                  (format sout "_GE_")
                  (incf index))
                 ((submatch "<=" remain)
                  (format sout "_LE_")
                  (incf index))
                 ((char= chr #\=) (format sout "_EQ_"))
                 ((char= chr #\<) (format sout "_LT_"))
                 ((char= chr #\>) (format sout "_GT_"))
                 ((char= chr #\-) (format sout "_"))
                 (t (format sout "~a" chr)))))))

#+(or)
(defun generate-header (&optional (file-name "virtualMachine.h"))
  (with-open-file (fout file-name :direction :output :if-exists :supersede)
    (write-string "#ifndef virtualMachine_H" fout) (terpri fout)
    (write-string "#define virtualMachine_H" fout) (terpri fout) (terpri fout)
    (let ((enums (loop for sym in *codes*
                       for index from 0
                       for trimmed-sym-name = (string-downcase (string-trim "+" (symbol-name sym)))
                       for sym-name = (format nil "vm_~a" (c++ify trimmed-sym-name))
                       collect (format nil "~a=~a" sym-name index))))
      (format fout "enum vm_codes {~%~{   ~a~^,~^~%~} };~%" enums))
    (terpri fout)
    (write-string "#endif /*guard */" fout) (terpri fout)))
