#+(or)
(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile))

(in-package #:cmp)

(setq *print-circle* t)

(defmacro logf (message &rest args)
  (declare (ignore message args))
  nil)
#+(or)
(progn
  (defvar *bclog* (progn
                    (format t "!~%!~%!   Opening /tmp/allcode.log - logging all bytecode compilation~%!~%!~%")
                    (open "/tmp/allcode.log" :direction :output :if-exists :supersede)))
  (defun log-function (cfunction compile-info bytecode)
    (format *bclog* "Name: ~s~%" (core:bytecode-cmp-function/name cfunction))
    (let ((*print-circle* t))
      (format *bclog* "Form: ~s~%" (car compile-info))
      (format *bclog* "Bytecode: ~s~%" bytecode)
      (finish-output *bclog*)))
  (defmacro logf (message &rest args)
    `(format *bclog* ,message ,@args)))

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

;;; Optimistic positioning of ANNOTATION in its module.
(defun annotation-module-position (annotation)
  (+ (core:bytecode-cmp-function/position (core:bytecode-cmp-annotation/function annotation))
     (core:bytecode-cmp-annotation/position annotation)))

;;; The (module) displacement from this fixup to its label,
(defun fixup-delta (fixup)
  (- (annotation-module-position (core:bytecode-cmp-fixup/label fixup))
     (annotation-module-position fixup)))

(defun emit-label (context label)
  (core:bytecode-cmp-annotation/setf-position
   label (length (context-assembly context)))
  (let ((function (core:bytecode-cmp-context/function context)))
    (core:bytecode-cmp-annotation/setf-function label function)
    (core:bytecode-cmp-annotation/setf-index
     label
     (vector-push-extend label (core:bytecode-cmp-function/annotations function)))))

(defun values-less-than-p (values max)
  (dolist (value values t)
    (unless (<= 0 value (1- max)) (return-from values-less-than-p nil))))

(defun assemble-maybe-long (context opcode &rest values)
  (apply #'core:bytecode-cmp-assemble-maybe-long context opcode values)
  #+(or)
  (let ((assembly (context-assembly context)))
    (cond ((values-less-than-p values #.(ash 1 8))
           (vector-push-extend opcode assembly)
           (dolist (value values)
             (vector-push-extend value assembly)))
          ((values-less-than-p values #.(ash 1 16))
           (vector-push-extend +long+ assembly)
           (vector-push-extend opcode assembly)
           (dolist (value values)
             (vector-push-extend (ldb (byte 8 0) value) assembly)
             (vector-push-extend (ldb (byte 8 8) value) assembly)))
          (t
           (error "Bytecode compiler limit reached: Indices too large! ~a" values)))))

(defun assemble (context opcode &rest values)
  (apply #'core:bytecode-cmp-assemble context opcode values)
  #+(or)
  (let ((assembly (context-assembly context)))
    (vector-push-extend opcode assembly)
    (dolist (value values)
      (vector-push-extend value assembly))))

(defun assemble-into (code position &rest values)
  (apply #'core:bytecode-cmp-assemble-into code position values)
  #+(or)
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
         (cfunction (core:bytecode-cmp-context/function context))
         (position (length assembly)))
    (core:bytecode-cmp-annotation/setf-function fixup cfunction)
    (core:bytecode-cmp-annotation/setf-initial-position fixup position)
    (core:bytecode-cmp-annotation/setf-position fixup position)
    (core:bytecode-cmp-annotation/setf-index
     fixup (vector-push-extend fixup (core:bytecode-cmp-function/annotations cfunction)))
    (dotimes (i (core:bytecode-cmp-fixup/initial-size fixup))
      (vector-push-extend 0 assembly))))

;;; Emit OPCODE and then a label reference.
(defun emit-control+label (context opcode8 opcode16 opcode24 label)
  (flet ((emitter (fixup position code)
           (let* ((size (core:bytecode-cmp-fixup/size fixup))
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
    (emit-fixup context (core:bytecode-cmp-fixup/make label 2 #'emitter #'resizer))))

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
           (let* ((size (core:bytecode-cmp-fixup/size fixup))
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
    (emit-fixup context (core:bytecode-cmp-fixup/make label 3 #'emitter #'resizer))))

(defun emit-const (context index)
  (if (> index 255)
      (assemble context
        +long+ +const+
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context +const+ index)))

(defun emit-fdefinition (context index)
  (if (> index 255)
      (assemble context
        +long+ +fdefinition+
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context +fdefinition+ index)))


(defun emit-parse-key-args (context max-count key-count key-names env aok-p)
  (let* ((keystart (literal-index (first key-names) context))
         (index (core:bytecode-cmp-env/frame-end env))
         (all (list max-count key-count keystart index)))
    (cond ((values-less-than-p all 127)
           (assemble context +parse-key-args+
                     max-count
                     (if aok-p (logior #x80 key-count) key-count)
                     keystart index))
          ((values-less-than-p all 32767)
           (assemble context +long+ +parse-key-args+
                     (ldb (byte 8 0) max-count) (ldb (byte 8 8) max-count)
                     (ldb (byte 8 0) key-count)
                     (if aok-p
                         (logior #x80 (ldb (byte 7 8) key-count))
                         (ldb (byte 7 8) key-count))
                     (ldb (byte 8 0) keystart) (ldb (byte 8 8) keystart)
                     (ldb (byte 8 0) index) (ldb (byte 8 8) index)))
          (t
           (error "Handle more than 32767 keyword parameters - you need ~s" key-count)))))

(defun emit-bind (context count offset)
  (cond ((= count 1)
         (assemble-maybe-long context +set+ offset))
        ((= count 0))
        ((and (< count #.(ash 1 8)) (< offset #.(ash 1 8)))
         (assemble context +bind+ count offset))
        ((and (< count #.(ash 1 16)) (< offset #.(ash 1 16)))
         (assemble context +long+ +bind+
                   (ldb (byte 8 0) count) (ldb (byte 8 8) count)
                   (ldb (byte 8 0) offset) (ldb (byte 8 8) offset)))
        (t (error "Too many lexicals: ~d ~d" count offset))))

(defun emit-call (context count)
  (let ((receiving (core:bytecode-cmp-context/receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble-maybe-long context +call+ count))
          ((eql receiving 1)
           (assemble-maybe-long context +call-receive-one+ count))
          (t (assemble-maybe-long context +call-receive-fixed+ count receiving)))))

(defun emit-mv-call (context)
  (let ((receiving (core:bytecode-cmp-context/receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble context +mv-call+))
          ((eql receiving 1)
           (assemble context +mv-call-receive-one+))
          (t (assemble-maybe-long context +mv-call-receive-fixed+ receiving)))))

(defun emit-special-bind (context symbol)
  (assemble-maybe-long context +special-bind+ (literal-index symbol context)))

(defun emit-unbind (context count)
  (dotimes (_ count) (assemble context +unbind+)))

(defun (setf core:bytecode-cmp-lexical-var-info/closed-over-p) (new info)
  (core:bytecode-cmp-lexical-var-info/setf-closed-over-p info new))
(defun (setf core:bytecode-cmp-lexical-var-info/set-p) (new info)
  (core:bytecode-cmp-lexical-var-info/setf-set-p info new))

;;; Does the variable with LEXICAL-INFO need a cell?
(defun indirect-lexical-p (lexical-info)
  (and (core:bytecode-cmp-lexical-var-info/closed-over-p lexical-info)
       (core:bytecode-cmp-lexical-var-info/set-p lexical-info)))

(defun make-symbol-macro-var-info (expansion)
  (core:bytecode-cmp-symbol-macro-var-info/make
   (lambda (form env) (declare (ignore form env)) expansion)))

(defun make-null-lexical-environment ()
  (core:bytecode-cmp-env/make nil nil nil nil 0))

(defun make-lexical-environment (parent &key (vars (core:bytecode-cmp-env/vars parent))
                                          (tags (core:bytecode-cmp-env/tags parent))
                                          (blocks (core:bytecode-cmp-env/blocks parent))
                                          (frame-end (core:bytecode-cmp-env/frame-end parent))
                                          (funs (core:bytecode-cmp-env/funs parent)))
  (core:bytecode-cmp-env/make vars tags blocks funs frame-end))

;;; Bind each variable to a stack location, returning a new lexical
;;; environment. The max local count in the current function is also
;;; updated.
(defun bind-vars (vars env context)
  (let* ((frame-start (core:bytecode-cmp-env/frame-end env))
         (var-count (length vars))
         (frame-end (+ frame-start var-count))
         (function (core:bytecode-cmp-context/function context)))
    (core:bytecode-cmp-function/setf-nlocals
     function
     (max (core:bytecode-cmp-function/nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (vars vars (rest vars))
         (new-vars (core:bytecode-cmp-env/vars env)
                   (acons (first vars) (core:bytecode-cmp-lexical-var-info/make index function) new-vars)))
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
;;; the expander.
;;; If the variable is a constant, :CONSTANT and the value.
;;; If the first value is NIL, the variable is unknown, and the second
;;; value is NIL.
(defun var-info (symbol env)
  (let ((info (cdr (assoc symbol (core:bytecode-cmp-env/vars env)))))
    (cond (info)
          ((ext:symbol-macro symbol)
           (core:bytecode-cmp-symbol-macro-var-info/make
            (ext:symbol-macro symbol)))
          ((constantp symbol nil)
           (core:bytecode-cmp-constant-var-info/make (symbol-value symbol)))
          ((ext:specialp symbol) ; globally special
           (core:bytecode-cmp-special-var-info/make))
          (t nil))))

;;; Like the above. Check the struct for details.
(defun fun-info (name env)
  (let ((info (cdr (assoc name (core:bytecode-cmp-env/funs env) :test 'equal))))
    (cond (info)
          ((and (symbolp name) (macro-function name nil))
           (core:bytecode-cmp-global-macro-info/make
            (macro-function name nil)))
          ((and (symbolp name) (special-operator-p name))
           (error "Tried to get FUN-INFO for special operator ~s - that's impossible" name))
          ((fboundp name) (core:bytecode-cmp-global-fun-info/make))
          (t (values nil nil)))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))
#+(or)
(defstruct (cfunction (:constructor make-cfunction (cmodule &key name doc))
                      (:type vector) :named)
  ;; Stuff for the function description
  name doc)

(defun context-module (context)
  (core:bytecode-cmp-function/module (core:bytecode-cmp-context/function context)))

(defun context-assembly (context)
  (core:bytecode-cmp-function/bytecode (core:bytecode-cmp-context/function context)))

(defun literal-index (literal context)
  (let ((literals (core:bytecode-cmp-module/literals (context-module context))))
    (or (position literal literals)
        (vector-push-extend literal literals))))

(defun closure-index (info context)
  (let ((closed (core:bytecode-cmp-function/closed (core:bytecode-cmp-context/function context))))
    (or (position info closed)
        (vector-push-extend info closed))))

(defun bytecompile (lambda-expression
                    &optional (env (make-null-lexical-environment)))
  (check-type lambda-expression lambda-expression)
  (logf "vvvvvvvv bytecompile ~%Form: ~s~%" lambda-expression)
  (let* ((module (core:bytecode-cmp-module/make))
         (lambda-list (cadr lambda-expression))
         (body (cddr lambda-expression)))
    (logf "-------- About to link~%")
    (multiple-value-prog1
        (link-function (compile-lambda lambda-list body env module) (cons lambda-expression env))
      (logf "^^^^^^^^^ Compile done~%"))))

(defun compile-form (form env context)
  (when *code-walker*
    (setq form (funcall sys:*code-walker* form env)))
  (cond ((symbolp form) (compile-symbol form env context))
        ((consp form) (compile-cons (car form) (cdr form) env context))
        (t (compile-literal form env context))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (core:bytecode-cmp-context/receiving context) 0)
    (cond ((null form) (assemble context +nil+))
          (t (emit-const context (literal-index form context))))
    (when (eql (core:bytecode-cmp-context/receiving context) t)
      (assemble context +pop+))))

(defun compile-load-time-value (form env context)
  (if *generate-compile-file-load-time-values*
      (error "Handle compile-file")
      (let ((value (eval form)))
        (compile-literal value env context))))

(flet ((maybe-emit (lexical-info opcode context)
         (flet ((emitter (fixup position code)
                  #+clasp-min (declare (ignore fixup))
                  #-clasp-min
                  (assert (= (core:bytecode-cmp-fixup/size fixup) 1))
                  (setf (aref code position) opcode))
                (resizer (fixup)
                  (declare (ignore fixup))
                  (if (indirect-lexical-p lexical-info) 1 0)))
           (emit-fixup context
                       (core:bytecode-cmp-fixup/make lexical-info 0 #'emitter #'resizer)))))
  (defun maybe-emit-make-cell (lexical-info context)
    (maybe-emit lexical-info +make-cell+ context))
  (defun maybe-emit-cell-ref (lexical-info context)
    (maybe-emit lexical-info +cell-ref+ context)))

;;; FIXME: This is probably a good candidate for a specialized
;;; instruction.
(defun maybe-emit-encage (lexical-info context)
  (let ((index (core:bytecode-cmp-lexical-var-info/frame-index lexical-info)))
    (flet ((emitter (fixup position code)
             (cond ((= (core:bytecode-cmp-fixup/size fixup) 5)
                    (assemble-into code position
                                   +ref+ index +make-cell+ +set+ index))
                   ((= (core:bytecode-cmp-fixup/size fixup) 9)
                    (let ((low (ldb (byte 8 0) index))
                          (high (ldb (byte 8 8) index)))
                      (assemble-into code position
                                     +long+ +ref+ low high +make-cell+ +long+ +set+ low high)))
                   (t (error "Unknown fixup size ~d" (core:bytecode-cmp-fixup/size fixup)))))
           (resizer (fixup)
             (declare (ignore fixup))
             (cond ((not (indirect-lexical-p lexical-info)) 0)
                   ((< index #.(ash 1 8)) 5)
                   ((< index #.(ash 1 16)) 9)
                   (t (error "Too many lexicals: ~d" index)))))
      (emit-fixup context (core:bytecode-cmp-fixup/make lexical-info 0 #'emitter #'resizer)))))

(defun emit-lexical-set (lexical-info context)
  (let ((index (core:bytecode-cmp-lexical-var-info/frame-index lexical-info)))
    (flet ((emitter (fixup position code)
             (let ((size (core:bytecode-cmp-fixup/size fixup)))
               (cond ((= size 2)
                      (assemble-into code position +set+ index))
                     ((= size 3)
                      (assemble-into code position +ref+ index +cell-set+))
                     ((= size 5)
                      (assemble-into code position
                                     +long+ +ref+
                                     (ldb (byte 8 0) index) (ldb (byte 8 8) index)
                                     +cell-set+))
                     (t (error "Unknown fixup size ~d" size)))))
           (resizer (fixup)
             (declare (ignore fixup))
             (cond ((not (indirect-lexical-p lexical-info)) 2)
                   ((< index #.(ash 1 8)) 3)
                   ((< index #.(ash 1 16)) 5)
                   (t (error "Too many lexicals: ~d" index)))))
      (emit-fixup context (core:bytecode-cmp-fixup/make lexical-info 2 #'emitter #'resizer)))))

(defun compile-symbol (form env context)
  (let ((info (var-info form env)))
    (cond ((typep info 'core:bytecode-cmp-symbol-macro-var-info)
           (let ((expander
                   (core:bytecode-cmp-symbol-macro-var-info/expander info)))
             (compile-form (funcall *macroexpand-hook* expander form env)
                           env context)))
          ;; A symbol macro could expand into something with arbitrary side
          ;; effects so we always have to compile that, but otherwise, if no
          ;; values are wanted, we want to not compile anything.
          ((eql (core:bytecode-cmp-context/receiving context) 0))
          (t
           (cond
             ((typep info 'core:bytecode-cmp-lexical-var-info)
              (cond ((eq (core:bytecode-cmp-lexical-var-info/function info)
                         (core:bytecode-cmp-context/function context))
                     (assemble-maybe-long
                      context +ref+
                      (core:bytecode-cmp-lexical-var-info/frame-index info)))
                    (t
                     (setf (core:bytecode-cmp-lexical-var-info/closed-over-p info) t)
                     (assemble-maybe-long context
                                          +closure+ (closure-index info context))))
              (maybe-emit-cell-ref info context))
             ((typep info 'core:bytecode-cmp-special-var-info)
              (assemble-maybe-long context +symbol-value+
                                   (literal-index form context)))
             ((typep info 'core:bytecode-cmp-constant-var-info)
              (return-from compile-symbol ; don't pop again.
                (compile-literal (core:bytecode-cmp-constant-var-info/value info)
                                 env context)))
             ((null info)
              (warn "Unknown variable ~a: treating as special" form)
              (assemble context +symbol-value+
                        (literal-index form context)))
             (t (error "BUG: Unknown info ~a" info)))
           (when (eq (core:bytecode-cmp-context/receiving context) t)
             (assemble context +pop+))))))

(defun compile-cons (head rest env context)
  (logf "compile-cons ~s~%" (list* head rest))
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
    ;; handled by macros
    #-clasp
    ((eq head 'catch) (compile-catch (first rest) (rest rest) env context))
    #-clasp
    ((eq head 'throw) (compile-throw (first rest) (second rest) env context))
    #-clasp
    ((eq head 'progv)
     (compile-progv (first rest) (second rest) (rest (rest rest)) env context))
    ((eq head 'quote) (compile-literal (first rest) env context))
    ((eq head 'load-time-value) (compile-load-time-value (first rest) env context))
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
     (let ((info (fun-info head env)))
       (cond
         ((typep info 'core:bytecode-cmp-global-macro-info)
          (let* ((expander
                   (core:bytecode-cmp-global-macro-info/expander info))
                 (expanded
                   (funcall *macroexpand-hook* expander (cons head rest) env)))
            (compile-form expanded env context)))
         ((typep info 'core:bytecode-cmp-local-macro-info)
          (let* ((expander
                   (core:bytecode-cmp-local-macro-info/expander info))
                 (expanded
                   (funcall *macroexpand-hook* expander (cons head rest) env)))
            (compile-form expanded env context)))
         ((typep info '(or core:bytecode-cmp-global-fun-info
                        core:bytecode-cmp-local-fun-info
                        null))
          ;; unknown function warning handled by compile-function
          ;; note we do a double lookup of the fun info,
          ;; which is inefficient in the compiler (generated code is ok)
          (compile-function head env (core:new-context context :receiving 1))
          (do ((args rest (rest args))
               (arg-count 0 (1+ arg-count)))
              ((endp args)
               (emit-call context arg-count))
            (compile-form (first args) env (core:new-context context :receiving 1))))
         (t (error "BUG: Unknown info ~a" info)))))))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms)))
      ((null (rest forms))
       (compile-form (first forms) env context))
    (compile-form (first forms) env (core:new-context context :receiving 0))))

;;; Add VARS as specials in ENV.
(defun add-specials (vars env)
  (make-lexical-environment
   env
   :vars (append (mapcar (lambda (var)
                           (cons var (core:bytecode-cmp-special-var-info/make)))
                         vars)
                 (core:bytecode-cmp-env/vars env))))

(defun compile-locally (body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (compile-progn body (if specials (add-specials specials env) env) context)))

(defun compile-eval-when (situations body env context)
  (if (or (member 'cl:eval situations) (member :execute situations))
      (compile-progn body env context)
      (compile-literal nil env context)))

(defun canonicalize-binding (binding)
  (if (consp binding)
      (values (first binding) (second binding))
      (values binding nil)))

(defun compile-let (bindings body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let ((lexical-binding-count 0)
          (special-binding-count 0)
          (post-binding-env (add-specials specials env)))
      (dolist (binding bindings)
        (multiple-value-bind (var valf) (canonicalize-binding binding)
          (compile-form valf env (core:new-context context :receiving 1))
          (cond ((or (member var specials)
                     (typep (var-info var env)
                            'core:bytecode-cmp-special-var-info))
                 (incf special-binding-count)
                 (emit-special-bind context var))
                (t
                 (setq post-binding-env
                       (bind-vars (list var) post-binding-env context))
                 (incf lexical-binding-count)
                 (maybe-emit-make-cell
                  (var-info var post-binding-env) context)))))
      (emit-bind context lexical-binding-count
                 (core:bytecode-cmp-env/frame-end env))
      (compile-progn body post-binding-env context)
      (emit-unbind context special-binding-count))))

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
          (compile-form valf env (core:new-context context :receiving 1))
          (cond ((or (member var specials) (ext:specialp var))
                 (incf special-binding-count)
                 (setq env (add-specials (list var) env))
                 (emit-special-bind context var))
                (t
                 (let ((frame-start (core:bytecode-cmp-env/frame-end env)))
                   (setq env (bind-vars (list var) env context))
                   (maybe-emit-make-cell (var-info var env) context)
                   (assemble-maybe-long context +set+ frame-start))))))
      (compile-progn body
                     (if specials
                         ;; We do this to make sure special declarations get
                         ;; through even if this form doesn't bind them.
                         ;; This creates duplicate alist entries for anything
                         ;; that _is_ bound here, but that's not a big deal.
                         (add-specials specials env)
                         env)
                     context)
      (emit-unbind context special-binding-count))))

(defun compile-setq (pairs env context)
  (if (null pairs)
      (unless (eql (core:bytecode-cmp-context/receiving context) 0)
        (assemble context +nil+))
      (do ((pairs pairs (cddr pairs)))
          ((endp pairs))
        (let ((var (car pairs))
              (valf (cadr pairs))
              (rest (cddr pairs)))
          (compile-setq-1 var valf env
                          (if rest
                              (core:new-context context :receiving 0)
                              context))))))

(defun compile-setq-1 (var valf env context)
  (let ((info (var-info var env)))
    (cond
      ((typep info 'core:bytecode-cmp-symbol-macro-var-info)
       (let ((expansion
               (funcall *macroexpand-hook*
                        (core:bytecode-cmp-symbol-macro-var-info/expander info)
                        var env)))
         (compile-form `(setf ,expansion ,valf) env context)))
      ((typep info '(or null core:bytecode-cmp-special-var-info))
       (when (null info)
         (warn "Unknown variable ~a: treating as special" var))
       (compile-form valf env (core:new-context context :receiving 1))
       ;; If we need to return the new value, stick it into a new local
       ;; variable, do the set, then return the lexical variable.
       ;; We can't just read from the special, since some other thread may
       ;; alter it.
       (let ((index (core:bytecode-cmp-env/frame-end env)))
         (unless (eql (core:bytecode-cmp-context/receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           ;; called for effect, i.e. to keep frame size correct
           (bind-vars (list var) env context))
         (assemble-maybe-long context +symbol-value-set+ (literal-index var context))
         (unless (eql (core:bytecode-cmp-context/receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (core:bytecode-cmp-context/receiving context) t)
             (assemble context +pop+)))))
      ((typep info 'core:bytecode-cmp-lexical-var-info)
       (let ((localp (eq (core:bytecode-cmp-lexical-var-info/function info)
                         (core:bytecode-cmp-context/function context)))
             (index (core:bytecode-cmp-env/frame-end env)))
         (unless localp
           (setf (core:bytecode-cmp-lexical-var-info/closed-over-p info) t))
         (setf (core:bytecode-cmp-lexical-var-info/set-p info) t)
         (compile-form valf env (core:new-context context :receiving 1))
         ;; similar concerns to specials above.
         (unless (eql (core:bytecode-cmp-context/receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           (bind-vars (list var) env context))
         (cond (localp
                (emit-lexical-set info context))
               ;; Don't emit a fixup if we already know we need a cell.
               (t
                (assemble-maybe-long context +closure+ (closure-index info context))
                (assemble context +cell-set+)))
         (unless (eql (core:bytecode-cmp-context/receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (core:bytecode-cmp-context/receiving context) t)
             (assemble context +pop+)))))
      (t (error "BUG: Unknown var info ~a" info)))))

(defun fun-name-block-name (fun-name)
  (if (symbolp fun-name)
      fun-name
      ;; setf name
      (second fun-name)))

(defun compile-flet (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0)
        ;; HACK FIXME
        (frame-slot (core:bytecode-cmp-env/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             (block ,(fun-name-block-name name)
                               (locally ,@(cddr definition))))
                          env (core:new-context context :receiving 1))
        (push fun-var fun-vars)
        (push (cons name (core:bytecode-cmp-local-fun-info/make
                          (core:bytecode-cmp-lexical-var-info/make
                           frame-slot
                           (core:bytecode-cmp-context/function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (emit-bind context fun-count (core:bytecode-cmp-env/frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (core:bytecode-cmp-env/funs env)))))
      (compile-locally body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-count 0)
        (funs '())
        (fun-vars '())
        (closures '())
        (env env)
        (frame-start (core:bytecode-cmp-env/frame-end env))
        (frame-slot (core:bytecode-cmp-env/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push fun-var fun-vars)
        (push (cons name (core:bytecode-cmp-local-fun-info/make
                          (core:bytecode-cmp-lexical-var-info/make
                           frame-slot
                           (core:bytecode-cmp-context/function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (let ((frame-slot (core:bytecode-cmp-env/frame-end env))
          (env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (core:bytecode-cmp-env/funs env)))))
      (dolist (definition definitions)
        (let* ((name (first definition))
               (fun (compile-lambda (second definition)
                                    `((block ,(fun-name-block-name name)
                                        (locally ,@(cddr definition))))
                                    env
                                    (context-module context)))
               (literal-index (literal-index fun context)))
          (cond ((zerop (length (core:bytecode-cmp-function/closed fun)))
                 (emit-const context literal-index))
                (t
                 (push (cons fun frame-slot) closures)
                 (assemble-maybe-long context
                                      +make-uninitialized-closure+ literal-index))))
        (incf frame-slot))
      (emit-bind context fun-count frame-start)
      (dolist (closure closures)
        (dotimes (i (length (core:bytecode-cmp-function/closed (car closure))))
          (reference-lexical-info (aref (core:bytecode-cmp-function/closed (car closure)) i)
                                  context))
        (assemble-maybe-long context +initialize-closure+ (cdr closure)))
      (compile-progn body env context))))

(defun compile-if (condition then else env context)
  (compile-form condition env (core:new-context context :receiving 1))
  (let ((then-label (core:bytecode-cmp-label/make))
        (done-label (core:bytecode-cmp-label/make)))
    (emit-jump-if context then-label)
    (compile-form else env context)
    (emit-jump context done-label)
    (emit-label context then-label)
    (compile-form then env context)
    (emit-label context done-label)))

;;; Push the immutable value or cell of lexical in CONTEXT.
(defun reference-lexical-info (info context)
  (if (eq (core:bytecode-cmp-lexical-var-info/function info)
          (core:bytecode-cmp-context/function context))
      (assemble-maybe-long context +ref+
                           (core:bytecode-cmp-lexical-var-info/frame-index info))
      (assemble-maybe-long context +closure+ (closure-index info context))))

(defun compile-function (fnameoid env context)
  (unless (eql (core:bytecode-cmp-context/receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((cfunction (compile-lambda (cadr fnameoid) (cddr fnameoid)
                                          env (context-module context)))
               (closed (core:bytecode-cmp-function/closed cfunction)))
          (dotimes (i (length closed))
            (reference-lexical-info (aref closed i) context))
          (if (zerop (length closed))
              (emit-const context (literal-index cfunction context))
              (assemble-maybe-long context +make-closure+
                                   (literal-index cfunction context))))
        (let ((info (fun-info fnameoid env)))
          (cond
            ((typep info '(or core:bytecode-cmp-global-fun-info null))
             #-(or clasp-min aclasp bclasp)
             (when (null info) (warn "Unknown function ~a" fnameoid))
             (emit-fdefinition context (literal-index fnameoid context)))
            ((typep info 'core:bytecode-cmp-local-fun-info)
             (reference-lexical-info
              (core:bytecode-cmp-local-fun-info/fun-var info) context))
            (t (error "BUG: Unknown fun info ~a" info)))))
    (when (eql (core:bytecode-cmp-context/receiving context) t)
      (assemble context +pop+))))

;;; (list (car list) (car (FUNC list)) (car (FUNC (FUNC list))) ...)
(defun collect-by (func list)
  (let ((col nil))
    (do ((list list (funcall func list)))
        ((endp list) (nreverse col))
      (push (car list) col))))

(defun compile-with-lambda-list (lambda-list body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body t)
    (declare (ignore docs decls))
    (multiple-value-bind (required optionals rest key-flag keys aok-p aux)
        (core:process-lambda-list lambda-list 'function)
      (let* ((function (core:bytecode-cmp-context/function context))
             (entry-point (core:bytecode-cmp-function/entry-point function))
             (min-count (first required))
             (optional-count (first optionals))
             (max-count (+ min-count optional-count))
             (key-count (first keys))
             (more-p (or rest key-flag))
             (new-env (bind-vars (cdr required) env context))
             (special-binding-count 0)
             ;; An alist from optional and key variables to their local indices.
             ;; This is needed so that we can properly mark any that are special as
             ;; such while leaving them temporarily "lexically" bound during
             ;; argument parsing.
             (opt-key-indices nil))
        (emit-label context entry-point)
        ;; Generate argument count check.
        (cond ((and (> min-count 0) (= min-count max-count) (not more-p))
               (assemble-maybe-long context +check-arg-count=+ min-count))
              (t
               (when (> min-count 0)
                 (assemble-maybe-long context +check-arg-count>=+ min-count))
               (unless more-p
                 (assemble-maybe-long context +check-arg-count<=+ max-count))))
        (unless (zerop min-count)
          ;; Bind the required arguments.
          (assemble-maybe-long context +bind-required-args+ min-count)
          (dolist (var (cdr required))
            ;; We account for special declarations in outer environments/globally
            ;; by checking the original environment - not our new one - for info.
            (cond ((or (member var specials)
                       (typep (var-info var env)
                              'core:bytecode-cmp-special-var-info))
                   (let ((info (var-info var new-env)))
                     (assemble-maybe-long
                      context +ref+
                      (core:bytecode-cmp-lexical-var-info/frame-index info)))
                   (emit-special-bind context var))
                  (t
                   (maybe-emit-encage (var-info var new-env) context))))
          (setq new-env (add-specials (intersection specials (cdr required)) new-env)))
        (unless (zerop optional-count)
          ;; Generate code to bind the provided optional args; unprovided args will
          ;; be initialized with the unbound marker.
          (assemble-maybe-long context +bind-optional-args+ min-count optional-count)
          (let ((optvars (collect-by #'cdddr (cdr optionals))))
            ;; Mark the locations of each optional. Note that we do this even if
            ;; the variable will be specially bound.
            (setq new-env (bind-vars optvars new-env context))
            ;; Add everything to opt-key-indices.
            (dolist (var optvars)
              (let ((info (var-info var new-env)))
                (push (cons var (core:bytecode-cmp-lexical-var-info/frame-index info))
                      opt-key-indices)))
            ;; Re-mark anything that's special in the outer context as such, so that
            ;; default initforms properly treat them as special.
            (let ((specials (remove-if-not
                             (lambda (sym)
                               (typep (var-info sym env)
                                      'core:bytecode-cmp-special-var-info))
                             optvars)))
              (when specials
                (setq new-env (add-specials specials new-env))))))
        (when key-flag
          ;; Generate code to parse the key args. As with optionals, we don't do
          ;; defaulting yet.
          (let ((key-names (collect-by #'cddddr (cdr keys))))
            (emit-parse-key-args context max-count key-count key-names new-env aok-p)
            ;; emit-parse-key-args establishes the first key in the literals.
            ;; now do the rest.
            (dolist (key-name (rest key-names))
              (literal-index key-name context)))
          (let ((keyvars (collect-by #'cddddr (cddr keys))))
            (setq new-env (bind-vars keyvars new-env context))
            (dolist (var keyvars)
              (let ((info (var-info var new-env)))
                (push (cons var (core:bytecode-cmp-lexical-var-info/frame-index info))
                      opt-key-indices)))
            (let ((specials (remove-if-not
                             (lambda (sym)
                               (typep (var-info sym env)
                                      'core:bytecode-cmp-special-var-info))
                             keyvars)))
              (when specials
                (setq new-env (add-specials specials new-env))))))
        ;; Generate defaulting code for optional args, and special-bind them
        ;; if necessary.
        (unless (zerop optional-count)
          (do ((optionals (cdr optionals) (cdddr optionals))
               (optional-label (core:bytecode-cmp-label/make) next-optional-label)
               (next-optional-label (core:bytecode-cmp-label/make) (core:bytecode-cmp-label/make)))
              ((endp optionals) (emit-label context optional-label))
            (emit-label context optional-label)
            (let* ((optional-var (car optionals))
                   (defaulting-form (cadr optionals)) (supplied-var (caddr optionals))
                   (optional-special-p
                     (or (member optional-var specials)
                         (typep (var-info optional-var env)
                                'core:bytecode-cmp-special-var-info)))
                   (index (cdr (assoc optional-var opt-key-indices)))
                   (supplied-special-p
                     (and supplied-var
                          (or (member supplied-var specials)
                              (typep (var-info supplied-var env)
                                     'core:bytecode-cmp-special-var-info)))))
              (setq new-env
                    (compile-optional/key-item optional-var defaulting-form index
                                               supplied-var next-optional-label
                                               optional-special-p supplied-special-p
                                               context new-env))
              (when optional-special-p (incf special-binding-count))
              (when supplied-special-p (incf special-binding-count)))))
        ;; &rest
        (when rest
          (assemble-maybe-long context +listify-rest-args+ max-count)
          (assemble-maybe-long context +set+ (frame-end new-env))
          (setq new-env (bind-vars (list rest) new-env context))
          (cond ((or (member rest specials)
                     (typep (var-info rest env)
                            'core:bytecode-cmp-special-var-info))
                 (assemble-maybe-long
                  +ref+ (var-info rest new-env) context)
                 (emit-special-bind context rest)
                 (incf special-binding-count 1)
                 (setq new-env (add-specials (list rest) new-env)))
                (t (maybe-emit-encage (var-info rest new-env) context))))
        ;; Generate defaulting code for key args, and special-bind them if necessary.
        (when key-flag
          (do ((keys (cdr keys) (cddddr keys))
               (key-label (core:bytecode-cmp-label/make) next-key-label)
               (next-key-label (core:bytecode-cmp-label/make) (core:bytecode-cmp-label/make)))
              ((endp keys) (emit-label context key-label))
            (emit-label context key-label)
            (let* ((key-var (cadr keys)) (defaulting-form (caddr keys))
                   (index (cdr (assoc key-var opt-key-indices)))
                   (supplied-var (cadddr keys))
                   (key-special-p
                     (or (member key-var specials)
                         (typep (var-info key-var env)
                                'core:bytecode-cmp-special-var-info)))
                   (supplied-special-p
                     (and supplied-var
                          (or (member supplied-var specials)
                              (typep (var-info key-var env)
                                     'core:bytecode-cmp-special-var-info)))))
              (setq new-env
                    (compile-optional/key-item key-var defaulting-form index
                                               supplied-var next-key-label
                                               key-special-p supplied-special-p
                                               context new-env))
              (when key-special-p (incf special-binding-count))
              (when supplied-special-p (incf special-binding-count)))))
        ;; Generate aux and the body as a let*.
        ;; We have to convert from process-lambda-list's aux format
        ;; (var val var val) to let* bindings.
        ;; We repeat the special declarations so that let* will know the auxs
        ;; are special, and so that any free special declarations are processed.
        (let ((bindings nil))
          (do ((aux aux (cddr aux)))
              ((endp aux)
               (compile-let* (nreverse bindings)
                             `((declare (special ,@specials)) ,@body)
                             new-env context))
            (push (list (car aux) (cadr aux)) bindings)))
        ;; Finally, clean up any special bindings.
        (emit-unbind context special-binding-count)))))

;;; Compile an optional/key item and return the resulting environment.
(defun compile-optional/key-item (var defaulting-form var-index supplied-var next-label
                                  var-specialp supplied-specialp context env)
  (flet ((default (suppliedp specialp var info)
           (cond (suppliedp
                  (cond (specialp
                         (assemble-maybe-long context +ref+ var-index)
                         (emit-special-bind context var))
                        (t
                         (maybe-emit-encage info context))))
                 (t
                  (compile-form defaulting-form env
                                (core:new-context context :receiving 1))
                  (cond (specialp
                         (emit-special-bind context var))
                        (t
                         (maybe-emit-make-cell info context)
                         (assemble-maybe-long context +set+ var-index))))))
         (supply (suppliedp specialp var info)
           (if suppliedp
               (compile-literal t env (core:new-context context :receiving 1))
               (assemble context +nil+))
           (cond (specialp
                  (emit-special-bind context var))
                 (t
                  (maybe-emit-make-cell info context)
                  (assemble-maybe-long
                   context +set+
                   (core:bytecode-cmp-lexical-var-info/frame-index info))))))
    (let ((supplied-label (core:bytecode-cmp-label/make))
          (var-info (var-info var env)))
      (when supplied-var
        (setq env (bind-vars (list supplied-var) env context)))
      (let ((supplied-info (var-info supplied-var env)))
        (emit-jump-if-supplied context var-index supplied-label)
        (default nil var-specialp var var-info)
        (when supplied-var
          (supply nil supplied-specialp supplied-var supplied-info))
        (emit-jump context next-label)
        (emit-label context supplied-label)
        (default t var-specialp var var-info)
        (when supplied-var
          (supply t supplied-specialp supplied-var supplied-info))
        (when var-specialp
          (setq env (add-specials (list var) env)))
        (when supplied-specialp
          (setq env (add-specials (list supplied-var) env)))
        env))))

;;; Compile the lambda in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (lambda-list body env module)
  (multiple-value-bind (decls sub-body docs)
      (core:process-declarations body t)
    ;; we pass the original body w/declarations to compile-with-lambda-list
    ;; so that it can do its own special handling.
    (declare (ignore sub-body))
    (let* ((name (or (core:extract-lambda-name-from-declares decls)
                     `(lambda ,(lambda-list-for-name lambda-list))))
           (function (core:bytecode-cmp-function/make module name docs))
           (context (core:bytecode-cmp-context/make t function))
           (env (make-lexical-environment env :frame-end 0)))
      (core:bytecode-cmp-function/setf-index
       function
       (vector-push-extend function
                           (core:bytecode-cmp-module/cfunctions module)))
      (compile-with-lambda-list lambda-list body env context)
      (assemble context +return+)
      function)))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let* ((new-tags (core:bytecode-cmp-env/tags env))
         (tagbody-dynenv (gensym "TAG-DYNENV"))
         (env (bind-vars (list tagbody-dynenv) env context))
         (dynenv-info (var-info tagbody-dynenv env)))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement dynenv-info (core:bytecode-cmp-label/make))
              new-tags)))
    (let ((env (make-lexical-environment env :tags new-tags)))
      ;; Bind the dynamic environment.
      (assemble context +entry+ (core:bytecode-cmp-lexical-var-info/frame-index dynenv-info))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cddr (assoc statement (core:bytecode-cmp-env/tags env))))
            (compile-form statement env (core:new-context context :receiving 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (core:bytecode-cmp-context/receiving context) 0)
    (assemble context +nil+)
    (when (eql (core:bytecode-cmp-context/receiving context) t)
      (assemble context +pop+))))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (core:bytecode-cmp-env/tags env))))
    (if pair
        (destructuring-bind (dynenv-info . tag-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (bind-vars (list block-dynenv) env context))
         (dynenv-info (var-info block-dynenv env))
         (label (core:bytecode-cmp-label/make))
         (normal-label (core:bytecode-cmp-label/make)))
    ;; Bind the dynamic environment.
    (assemble context +entry+ (core:bytecode-cmp-lexical-var-info/frame-index dynenv-info))
    (let ((env (make-lexical-environment
                env
                :blocks (acons name (cons dynenv-info label) (core:bytecode-cmp-env/blocks env)))))
      ;; Force single values into multiple so that we can uniformly PUSH afterward.
      (compile-progn body env context))
    (when (eql (core:bytecode-cmp-context/receiving context) 1)
      (emit-jump context normal-label))
    (emit-label context label)
    ;; When we need 1 value, we have to make sure that the
    ;; "exceptional" case pushes a single value onto the stack.
    (when (eql (core:bytecode-cmp-context/receiving context) 1)
      (assemble context +push+)
      (emit-label context normal-label))
    (assemble context +entry-close+)))

(defun compile-return-from (name value env context)
  (compile-form value env (core:new-context context :receiving t))
  (let ((pair (assoc name (core:bytecode-cmp-env/blocks env))))
    (if pair
        (destructuring-bind (dynenv-info . block-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context block-label))
        (error "The block ~a does not exist." name))))

(defun compile-catch (tag body env context)
  (compile-form tag env (core:new-context context :receiving 1))
  (let ((target (core:bytecode-cmp-label/make)))
    (emit-catch context target)
    (compile-progn body env context)
    (assemble context +catch-close+)
    (emit-label context target)))

(defun compile-throw (tag result env context)
  (compile-form tag env (core:new-context context :receiving 1))
  (compile-form result env (core:new-context context :receiving t))
  (assemble context +throw+))

(defun compile-progv (symbols values body env context)
  (compile-form symbols env (core:new-context context :receiving 1))
  (compile-form values env (core:new-context context :receiving 1))
  (assemble context +progv+)
  (compile-progn body env context)
  (emit-unbind context 1))

(defun compile-symbol-macrolet (bindings body env context)
  (let ((smacros nil))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (compile-locally body (make-lexical-environment
                           env
                           :vars (append (nreverse smacros) (core:bytecode-cmp-env/vars env)))
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
           (dolist (pair (core:bytecode-cmp-env/vars env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (typep info '(or core:bytecode-cmp-constant-var-info
                                   core:bytecode-cmp-symbol-macro-var-info))
                 (push pair cpairs)))))
   :funs (let ((cpairs nil))
           (dolist (pair (core:bytecode-cmp-env/funs env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (typep info '(or core:bytecode-cmp-global-macro-info
                                   core:bytecode-cmp-local-macro-info))
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
             (info (core:bytecode-cmp-local-macro-info/make expander)))
        (push (cons name info) macros)))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (core:bytecode-cmp-env/funs env)))
                     context)))

(defun compile-multiple-value-call (function-form forms env context)
  (compile-form function-form env (core:new-context context :receiving 1))
  (let ((first (first forms))
        (rest (rest forms)))
    (compile-form first env (core:new-context context :receiving t))
    (when rest
      (assemble context +push-values+)
      (dolist (form rest)
        (compile-form form env (core:new-context context :receiving t))
        (assemble context +append-values+))
      (assemble context +pop-values+)))
  (emit-mv-call context))

(defun compile-multiple-value-prog1 (first-form forms env context)
  (compile-form first-form env context)
  (unless (member (core:bytecode-cmp-context/receiving context) '(0 1))
    (assemble context +push-values+))
  (dolist (form forms)
    (compile-form form env (core:new-context context :receiving 0)))
  (unless (member (core:bytecode-cmp-context/receiving context) '(0 1))
    (assemble context +pop-values+)))

;;;; linkage

(defun unsigned (x size)
  (logand x (1- (ash 1 size))))

;;; Use the optimistic bytecode vector sizes to initialize the optimistic cfunction position.
(defun initialize-cfunction-positions (cmodule)
  (let ((position 0))
    (dotimes (i (length (core:bytecode-cmp-module/cfunctions cmodule)))
      (let ((function (aref (core:bytecode-cmp-module/cfunctions cmodule) i)))
        (core:bytecode-cmp-function/setf-position function position)
        (incf position (length (core:bytecode-cmp-function/bytecode function)))))))

;;; Update the positions of all affected functions and annotations
;;; from the effect of increasing the size of FIXUP by INCREASE. The
;;; resizer has already updated the size of the the fixup.
(defun update-positions (fixup increase)
  (let ((function (core:bytecode-cmp-annotation/function fixup)))
    ;; Update affected annotation positions in this function.
    (let ((annotations (core:bytecode-cmp-function/annotations function)))
      (do ((index (1+ (core:bytecode-cmp-annotation/index fixup)) (1+ index)))
          ((= index (length annotations)))
        (let* ((annotation (aref annotations index))
               (pos (core:bytecode-cmp-annotation/position annotation)))
          (core:bytecode-cmp-annotation/setf-position annotation (+ pos increase)))))
    ;; Increase the size of this function to account for fixup growth.
    (core:bytecode-cmp-function/setf-extra
     function
     (+ (core:bytecode-cmp-function/extra function) increase))
    ;; Update module offsets for affected functions.
    (let ((functions (core:bytecode-cmp-module/cfunctions (core:bytecode-cmp-function/module function))))
      (do ((index (1+ (core:bytecode-cmp-function/index function)) (1+ index)))
          ((= index (length functions)))
        (let ((function (aref functions index)))
          (core:bytecode-cmp-function/setf-position
           function
           (+ (core:bytecode-cmp-function/position function) increase)))))))

;;; With all functions and annotations initialized with optimistic
;;; sizes, resize fixups until no more expansion is needed.
(defun resolve-fixup-sizes (cmodule)
  (loop
    (let ((changed-p nil)
          (functions (core:bytecode-cmp-module/cfunctions cmodule)))
      (dotimes (i (length functions))
        (dotimes (j (length (core:bytecode-cmp-function/annotations (aref functions i))))
          (let ((annotation (aref (core:bytecode-cmp-function/annotations (aref functions i)) j)))
            (when (typep annotation 'core:bytecode-cmp-fixup)
              (let ((old-size (core:bytecode-cmp-fixup/size annotation))
                    (new-size (funcall (core:bytecode-cmp-fixup/resizer annotation) annotation)))
                (unless (= old-size new-size)
                  #+(or)
                  (assert (>= new-size old-size))
                  (core:bytecode-cmp-fixup/setf-size annotation new-size)
                  (setq changed-p t)
                  (update-positions annotation (- new-size old-size))))))))
      (unless changed-p
        (return)))))

;;; The size of the module bytecode vector.
(defun module-bytecode-size (cmodule)
  (let* ((cfunctions (core:bytecode-cmp-module/cfunctions cmodule))
         (last-cfunction (aref cfunctions (1- (length cfunctions)))))
    (+ (core:bytecode-cmp-function/position last-cfunction)
       (length (core:bytecode-cmp-function/bytecode last-cfunction))
       (core:bytecode-cmp-function/extra last-cfunction))))

;;; Create the bytecode module vector. We scan over the fixups in the
;;; module and copy segments of bytecode between fixup positions.
(defun create-module-bytecode (cmodule)
  (let ((bytecode (make-array (module-bytecode-size cmodule)
                              :element-type '(unsigned-byte 8)))
        (index 0))
    (dotimes (i (length (core:bytecode-cmp-module/cfunctions cmodule)))
      (let* ((function (aref (core:bytecode-cmp-module/cfunctions cmodule) i))
             (cfunction-bytecode (core:bytecode-cmp-function/bytecode function))
             (position 0))
        (dotimes (i (length (core:bytecode-cmp-function/annotations function)))
          (let ((annotation (aref (core:bytecode-cmp-function/annotations function) i)))
            (when (typep annotation 'core:bytecode-cmp-fixup)
              (unless (zerop (core:bytecode-cmp-fixup/size annotation))
                #+(or)
              (assert (= (fixup-size annotation)
                         (funcall (fixup-resizer annotation) annotation)))
              ;; Copy bytes in this segment.
              (let ((end (core:bytecode-cmp-annotation/initial-position annotation)))
                (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
                (incf index (- end position))
                (setf position end))
              #+(or)
              (assert (= index (annotation-module-position annotation)))
              ;; Emit fixup.
              (funcall (core:bytecode-cmp-fixup/emitter annotation)
                       annotation
                       index
                       bytecode)
              (incf position (core:bytecode-cmp-fixup/initial-size annotation))
              (incf index (core:bytecode-cmp-fixup/size annotation))))))
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
(defun link-function (cfunction compile-info)
  (declare (optimize debug))
  (let ((cmodule (core:bytecode-cmp-function/module cfunction)))
    (initialize-cfunction-positions cmodule)
    (resolve-fixup-sizes cmodule)
    (let* ((cmodule-literals (core:bytecode-cmp-module/literals cmodule))
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
      (dotimes (i (length (core:bytecode-cmp-module/cfunctions cmodule)))
        (let ((cfunction (aref (core:bytecode-cmp-module/cfunctions cmodule) i)))
          (core:bytecode-cmp-function/setf-info
           cfunction
           (core:global-bytecode-entry-point/make
            (core:function-description/make
             :function-name (core:bytecode-cmp-function/name cfunction)
             :docstring (core:bytecode-cmp-function/doc cfunction))
            bytecode-module
            (core:bytecode-cmp-function/nlocals cfunction)
            0 0 0 0 nil 0          ; unused at the moment
            (length (core:bytecode-cmp-function/closed cfunction))
            (make-list 7 :initial-element
                       (annotation-module-position (core:bytecode-cmp-function/entry-point cfunction)))))))
      ;; Now replace the cfunctions in the cmodule literal vector with
      ;; real bytecode functions.
      (dotimes (index literal-length)
        (setf (aref literals index)
              (let ((literal (aref cmodule-literals index)))
                (if (typep literal 'core:bytecode-cmp-function)
                    (core:bytecode-cmp-function/info literal)
                    literal))))
      #+clasp
      (progn
        (core:bytecode-module/setf-literals bytecode-module literals)
        ;; Now just install the bytecode and Bob's your uncle.
        (core:bytecode-module/setf-bytecode bytecode-module bytecode)
        (core:bytecode-module/setf-compile-info bytecode-module compile-info))
      #+(or)(log-function cfunction compile-info bytecode)))
  (core:bytecode-cmp-function/info cfunction))

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
