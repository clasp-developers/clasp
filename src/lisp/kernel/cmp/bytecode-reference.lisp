
(in-package :cmpref)


(defmacro logf (message &rest args)
  (declare (ignore message args))
  nil)
#+(or)
(progn
  (defvar *bclog* (progn
                    (format t "!~%!~%!   Opening /tmp/allcode.log - logging all bytecode compilation~%!~%!~%")
                    (open "/tmp/allcode.log" :direction :output :if-exists :supersede)))
  (defun log-function (cfunction compile-info bytecode)
    (format *bclog* "Name: ~s~%" (cfunction-name cfunction))
    (let ((*print-circle* t))
      (format *bclog* "Form: ~s~%" (car compile-info))
      (format *bclog* "Bytecode: ~s~%" bytecode)
      (finish-output *bclog*)))
  (defmacro logf (message &rest args)
    `(format *bclog* ,message ,@args)))



;;; FIXME: New package

(let ((rev-codes nil)
      (forms nil))
  (macrolet ((new-instr (name code &optional arguments long-arguments)
               `(progn
                  (push (list ,name ,code ,arguments ,long-arguments) rev-codes)
                  (let ((sym (intern (format nil "+~a+" ,(string-upcase name))))
                        (cd ,code))
                    (eval `(defconstant ,sym ,cd))
                    ))))
    (new-instr "ref" 0 '(1) '(2))
    (new-instr "const" 1 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "closure" 2 '(1) '(2))
    (new-instr "call" 3 '(1) '(2))
    (new-instr "call-receive-one" 4 '(1) '(2))
    (new-instr "call-receive-fixed" 5 '(1 1) '(2 2))
    (new-instr "bind" 6 '(1 1) '(2 2))
    (new-instr "set" 7 '(1) '(2))
    (new-instr "make-cell" 8)
    (new-instr "cell-ref" 9)
    (new-instr "cell-set" 10)
    (new-instr "make-closure" 11 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "make-uninitialized-closure" 12 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "initialize-closure" 13 '(1) '(2))
    (new-instr "return" 14)
    (new-instr "bind-required-args" 15 '(1) '(2))
    (new-instr "bind-optional-args" 16 '(1 1) '(2 2))
    (new-instr "listify-rest-args" 17 '(1) '(2))
    (new-instr "vaslistify-rest-args" 18 '(1))
    (new-instr "parse-key-args" 19 '(1 1 (keys-arg 1) 1) '(2 2 (keys-arg 2) 2))
    (new-instr "jump-8" 20 '((label-arg 1)))
    (new-instr "jump-16" 21 '((label-arg 2)))
    (new-instr "jump-24" 22 '((label-arg 3)))
    (new-instr "jump-if-8" 23 '((label-arg 1)))
    (new-instr "jump-if-16" 24 '((label-arg 2)))
    (new-instr "jump-if-24" 25 '((label-arg 3)))
    (new-instr "jump-if-supplied-8" 26 '(1 (label-arg 1)))
    (new-instr "jump-if-supplied-16" 27 '(1 (label-arg 2)))
    (new-instr "check-arg-count-LE" 28 '(1) '(2))
    (new-instr "check-arg-count-GE" 29 '(1) '(2))
    (new-instr "check-arg-count-EQ" 30 '(1) '(2))
    (new-instr "push-values" 31)
    (new-instr "append-values" 32)
    (new-instr "pop-values" 33)
    (new-instr "mv-call" 34)
    (new-instr "mv-call-receive-one" 35)
    (new-instr "mv-call-receive-fixed" 36 '(1) '(2))
    (new-instr "entry" 37 '(1))
    (new-instr "exit-8" 38 '((label-arg 1)))
    (new-instr "exit-16" 39 '((label-arg 2)))
    (new-instr "exit-24" 40 '((label-arg 3)))
    (new-instr "entry-close" 41)
    (new-instr "catch-8" 42)
    (new-instr "catch-16" 43)
    (new-instr "throw" 44)
    (new-instr "catch-close" 45)
    (new-instr "special-bind" 46 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "symbol-value" 47 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "symbol-value-set" 48 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "unbind" 49)
    (new-instr "progv" 50)
    (new-instr "fdefinition" 51 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "nil" 52)
    (new-instr "eq" 53)
    (new-instr "push" 54)
    (new-instr "pop" 55)
    (new-instr "long" 56)
    (defparameter *full-codes* (nreverse rev-codes))
    (defparameter *codes* (mapcar #'first (nreverse rev-codes)))
    (eval `(progn ,@forms))
    (defun decode-instr (code)
      code)
    ))

#+(or)
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                (defun decode-instr (code)
                  code)
                (defun encode-instr (code)
                  (nth code '(,@names)))                )))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +vaslistify-rest-args+ +parse-key-args+
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

(defun values-less-than-p (values max)
  (dolist (value values t)
    (unless (<= 0 value (1- max)) (return-from values-less-than-p nil))))

(defun assemble-maybe-long (context opcode &rest values)
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
  (unless (values-less-than-p values #.(ash 1 8))
    (error "Bad value in assemble ~s" values))
  (let ((assembly (context-assembly context)))
    (vector-push-extend opcode assembly)
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
  (let* ((keystart (new-literal-index (first key-names) context))
         (index (cmp:lexenv/frame-end env))
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
  (let ((receiving (context-receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble-maybe-long context +call+ count))
          ((eql receiving 1)
           (assemble-maybe-long context +call-receive-one+ count))
          (t (assemble-maybe-long context +call-receive-fixed+ count receiving)))))

(defun emit-mv-call (context)
  (let ((receiving (context-receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble context +mv-call+))
          ((eql receiving 1)
           (assemble context +mv-call-receive-one+))
          (t (assemble-maybe-long context +mv-call-receive-fixed+ receiving)))))

(defun emit-special-bind (context symbol)
  (assemble-maybe-long context +special-bind+ (literal-index symbol context)))

(defun emit-unbind (context count)
  (dotimes (_ count) (assemble context +unbind+)))

(defun (setf cmp:lexical-var-info/closed-over-p) (new info)
  (cmp:lexical-var-info/setf-closed-over-p info new))
(defun (setf cmp:lexical-var-info/set-p) (new info)
  (cmp:lexical-var-info/setf-set-p info new))

;;; Does the variable with LEXICAL-INFO need a cell?
(defun indirect-lexical-p (lexical-info)
  (and (cmp:lexical-var-info/closed-over-p lexical-info)
       (cmp:lexical-var-info/set-p lexical-info)))

(defun make-symbol-macro-var-info (expansion)
  (cmp:symbol-macro-var-info/make
   (lambda (form env) (declare (ignore form env)) expansion)))

(defun var-info-kind (info)
  (cond ((null info) info)
        ((typep info 'cmp:lexical-var-info) :lexical)
        ((typep info 'cmp:special-var-info) :special)
        ((typep info 'cmp:symbol-macro-var-info) :symbol-macro)
        ((typep info 'cmp:constant-var-info) :constant)
        (t (error "Unknown info ~a" info))))

(defun var-info-data (info)
  (cond ((null info) info)
        ((typep info 'cmp:lexical-var-info) info)
        ((typep info 'cmp:special-var-info) 'nil)
        ((typep info 'cmp:symbol-macro-var-info)
         (cmp:symbol-macro-var-info/expander info))
        ((typep info 'cmp:constant-var-info)
         (cmp:constant-var-info/value info))
        (t (error "Unknown info ~a" info))))

(defun fun-info-kind (info)
  (cond ((null info) nil)
        ((typep info 'cmp:global-fun-info) :global-function)
        ((typep info 'cmp:local-fun-info) :local-function)
        ((typep info 'cmp:global-macro-info) :global-macro)
        ((typep info 'cmp:local-macro-info) :local-macro)
        (t (error "Unknown info ~a" info))))

(defun fun-info-data (info)
  (cond ((null info) nil)
        ((typep info 'cmp:global-fun-info) nil)
        ((typep info 'cmp:local-fun-info)
         (cmp:local-fun-info/fun-var info))
        ((typep info 'cmp:global-macro-info)
         (cmp:global-macro-info/expander info))
        ((typep info 'cmp:local-macro-info)
         (cmp:local-macro-info/expander info))
        (t (error "Unknown info ~a" info))))

(defun make-null-lexical-environment ()
  (cmp:lexenv/make nil nil nil nil nil 0))

(defun make-lexical-environment (parent &key (vars (cmp:lexenv/vars parent))
                                          (tags (cmp:lexenv/tags parent))
                                          (blocks (cmp:lexenv/blocks parent))
                                          (frame-end (cmp:lexenv/frame-end parent))
                                          (funs (cmp:lexenv/funs parent)))
  (cmp:lexenv/make vars tags blocks funs nil frame-end))

;;; Bind each variable to a stack location, returning a new lexical
;;; environment. The max local count in the current function is also
;;; updated.
(defun bind-vars (vars env context)
  (let* ((frame-start (cmp:lexenv/frame-end env))
         (var-count (length vars))
         (frame-end (+ frame-start var-count))
         (function (context-function context)))
    (setf (cfunction-nlocals function)
          (max (cfunction-nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (vars vars (rest vars))
         (new-vars (cmp:lexenv/vars env)
                   (acons (first vars) (cmp:lexical-var-info/make index function) new-vars)))
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
  (let ((info (cdr (assoc symbol (cmp:lexenv/vars env)))))
    (cond (info (values (var-info-kind info) (var-info-data info)))
          ((ext:symbol-macro symbol) (values :symbol-macro (ext:symbol-macro symbol)))
          ((constantp symbol nil) (values :constant (symbol-value symbol)))
          ((ext:specialp symbol) (values :special nil)) ; globally special
          (t (values nil nil)))))

;;; Like the above. Check the struct for details.
(defun fun-info (name env)
  (let ((info (cdr (assoc name (cmp:lexenv/funs env) :test 'equal))))
    (cond (info (values (fun-info-kind info) (fun-info-data info)))
          ((and (symbolp name) (macro-function name nil))
           (values :global-macro (macro-function name nil)))
          ((and (symbolp name) (special-operator-p name))
           (error "Tried to get FUN-INFO for special operator ~s - that's impossible" name))
          ((fboundp name)
           (values :global-function (compiler-macro-function name)))
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

;;; Force a literal into the end of the literals even if it's already
;;; there. This is used in keyword argument parsing.
(defun new-literal-index (literal context)
  (vector-push-extend literal (cmodule-literals (context-module context))))

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
  (logf "vvvvvvvv bytecompile ~%Form: ~s~%" lambda-expression)
  (let* ((module (make-cmodule (make-array 0 :fill-pointer 0 :adjustable t)))
         (lambda-list (cadr lambda-expression))
         (body (cddr lambda-expression)))
    (logf "-------- About to link~%")
    (multiple-value-prog1
        (link-function (compile-lambda lambda-list body env module) (cons lambda-expression env))
      (logf "^^^^^^^^^ Compile done~%"))))

(export 'bytecompile)

(defun compile-form (form env context)
  (cond ((symbolp form) (compile-symbol form env context))
        ((consp form) (compile-cons (car form) (cdr form) env context))
        (t (compile-literal form env context))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (cond ((null form) (assemble context +nil+))
          (t (emit-const context (literal-index form context))))
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(defun compile-load-time-value (form env context)
  (if cmp::*generate-compile-file-load-time-values*
      (error "Handle compile-file")
      (let ((value (eval form)))
        (compile-literal value env context))))

(flet ((maybe-emit (lexical-info opcode context)
         (flet ((emitter (fixup position code)
                  (declare (ignorable fixup))
                  #+(or)
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
  (let ((index (cmp:lexical-var-info/frame-index lexical-info)))
    (flet ((emitter (fixup position code)
             (cond ((= (fixup-size fixup) 5)
                    (assemble-into code position
                                   +ref+ index +make-cell+ +set+ index))
                   ((= (fixup-size fixup) 9)
                    (let ((low (ldb (byte 8 0) index))
                          (high (ldb (byte 8 8) index)))
                      (assemble-into code position
                                     +long+ +ref+ low high +make-cell+ +long+ +set+ low high)))
                   (t (error "Unknown fixup size ~d" (fixup-size fixup)))))
           (resizer (fixup)
             (declare (ignore fixup))
             (cond ((not (indirect-lexical-p lexical-info)) 0)
                   ((< index #.(ash 1 8)) 5)
                   ((< index #.(ash 1 16)) 9)
                   (t (error "Too many lexicals: ~d" index)))))
      (emit-fixup context (make-fixup lexical-info 0 #'emitter #'resizer)))))

(defun emit-lexical-set (lexical-info context)
  (let ((index (cmp:lexical-var-info/frame-index lexical-info)))
    (flet ((emitter (fixup position code)
             (let ((size (fixup-size fixup)))
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
              (cond ((eq (cmp:lexical-var-info/cfunction data)
                         (context-function context))
                     (assemble-maybe-long
                      context +ref+
                      (cmp:lexical-var-info/frame-index data)))
                    (t
                     (setf (cmp:lexical-var-info/closed-over-p data) t)
                     (assemble-maybe-long context
                                          +closure+ (closure-index data context))))
              (maybe-emit-cell-ref data context))
             ((eq kind :special) (assemble-maybe-long context +symbol-value+
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
    ((eq head 'cleavir-primop::funcall)
     (compile-funcall (second rest) env context))
    (t ; function call or macro
     (cond
       ((symbolp head)
        (multiple-value-bind (kind data) (fun-info head env)
          (cond
            ((member kind '(:global-macro :local-macro))
             (compile-form (funcall *macroexpand-hook* data (cons head rest) env)
                           env context))
            ((member kind '(:global-function :local-function nil))
             ;; Try a compiler macroexpansion
             (when (and (eq kind ':global-function) data)
               (let* ((form (cons head rest))
                      (expansion (funcall *macroexpand-hook* data form env)))
                 (unless (eq form expansion)
                   (return-from compile-cons
                     (compile-form expansion env context)))))
             ;; unknown function warning handled by compile-function
             ;; note we do a double lookup, which is inefficient
             (compile-function head env (new-context context :receiving 1))
             (do ((args rest (rest args))
                  (arg-count 0 (1+ arg-count)))
                 ((endp args)
                  (emit-call context arg-count))
               (compile-form (first args) env (new-context context :receiving 1))))
            (t (error "Unknown kind ~a" kind)))))
       ((and (consp head) (eq (car head) 'cl:lambda))
        ;; lambda form
        (compile-function head env (new-context context :receiving 1))
        (do ((args rest (rest args))
             (arg-count 0 (1+ arg-count)))
            ((endp args) (emit-call context arg-count))
          (compile-form (first args) env (new-context context :receiving 1))))
       (t (error "Illegal form: ~s" (cons head rest)))))))

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
                           (cons var (cmp:special-var-info/make (ext:specialp var))))
                         vars)
                 (cmp:lexenv/vars env))))

(defun compile-locally (body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (compile-progn body (if specials (add-specials specials env) env) context)))

(defun compile-funcall (callee args env context)
  (compile-form callee env (context/sub context 1))
  (compile-call args env context))

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
          (compile-form valf env (new-context context :receiving 1))
          (cond ((or (member var specials)
                     (eq (var-info var env) :special))
                 (incf special-binding-count)
                 (emit-special-bind context var))
                (t
                 (setq post-binding-env
                       (bind-vars (list var) post-binding-env context))
                 (incf lexical-binding-count)
                 (maybe-emit-make-cell
                  (nth-value 1 (var-info var post-binding-env)) context)))))
      (emit-bind context lexical-binding-count
                 (cmp:lexenv/frame-end env))
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
          (compile-form valf env (new-context context :receiving 1))
          (cond ((or (member var specials) (ext:specialp var))
                 (incf special-binding-count)
                 (setq env (add-specials (list var) env))
                 (emit-special-bind context var))
                (t
                 (let ((frame-start (cmp:lexenv/frame-end env)))
                   (setq env (bind-vars (list var) env context))
                   (maybe-emit-make-cell (nth-value 1 (var-info var env))
                                         context)
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
       (let ((index (cmp:lexenv/frame-end env)))
         (unless (eql (context-receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           ;; called for effect, i.e. to keep frame size correct
           (bind-vars (list var) env context))
         (assemble-maybe-long context +symbol-value-set+ (literal-index var context))
         (unless (eql (context-receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+)))))
      ((eq kind :lexical)
       (let ((localp (eq (cmp:lexical-var-info/cfunction data)
                         (context-function context)))
             (index (cmp:lexenv/frame-end env)))
         (unless localp
           (setf (cmp:lexical-var-info/closed-over-p data) t))
         (setf (cmp:lexical-var-info/set-p data) t)
         (compile-form valf env (new-context context :receiving 1))
         ;; similar concerns to specials above.
         (unless (eql (context-receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           (bind-vars (list var) env context))
         (cond (localp
                (emit-lexical-set data context))
               ;; Don't emit a fixup if we already know we need a cell.
               (t
                (assemble-maybe-long context +closure+ (closure-index data context))
                (assemble context +cell-set+)))
         (unless (eql (context-receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+)))))
      (t (error "Unknown kind ~a" kind)))))

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
        (frame-slot (cmp:lexenv/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             (block ,(fun-name-block-name name)
                               (locally ,@(cddr definition))))
                          env (new-context context :receiving 1))
        (push fun-var fun-vars)
        (push (cons name (cmp:local-fun-info/make
                          (cmp:lexical-var-info/make frame-slot
                                                 (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (emit-bind context fun-count (cmp:lexenv/frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (cmp:lexenv/funs env)))))
      (compile-locally body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-count 0)
        (funs '())
        (fun-vars '())
        (closures '())
        (env env)
        (frame-start (cmp:lexenv/frame-end env))
        (frame-slot (cmp:lexenv/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push fun-var fun-vars)
        (push (cons name (cmp:local-fun-info/make
                          (cmp:lexical-var-info/make frame-slot
                                                 (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (let ((frame-slot (cmp:lexenv/frame-end env))
          (env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (cmp:lexenv/funs env)))))
      (dolist (definition definitions)
        (let* ((name (first definition))
               (fun (compile-lambda (second definition)
                                    `((block ,(fun-name-block-name name)
                                        (locally ,@(cddr definition))))
                                    env
                                    (context-module context)))
               (literal-index (literal-index fun context)))
          (cond ((zerop (length (cfunction-closed fun)))
                 (emit-const context literal-index))
                (t
                 (push (cons fun frame-slot) closures)
                 (assemble-maybe-long context
                                      +make-uninitialized-closure+ literal-index))))
        (incf frame-slot))
      (emit-bind context fun-count frame-start)
      (dolist (closure closures)
        (dotimes (i (length (cfunction-closed (car closure))))
          (reference-lexical-info (aref (cfunction-closed (car closure)) i)
                                  context))
        (assemble-maybe-long context +initialize-closure+ (cdr closure)))
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
  (if (eq (cmp:lexical-var-info/cfunction info) (context-function context))
      (assemble-maybe-long context +ref+
                           (cmp:lexical-var-info/frame-index info))
      (assemble-maybe-long context +closure+ (closure-index info context))))

(defun compile-function (fnameoid env context)
  (unless (eql (context-receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((cfunction (compile-lambda (cadr fnameoid) (cddr fnameoid)
                                          env (context-module context)))
               (closed (cfunction-closed cfunction)))
          (dotimes (i (length closed))
            (reference-lexical-info (aref closed i) context))
          (if (zerop (length closed))
              (emit-const context (literal-index cfunction context))
              (assemble-maybe-long context +make-closure+
                                   (literal-index cfunction context))))
        (multiple-value-bind (kind data) (fun-info fnameoid env)
          (cond
            ((member kind '(:global-function nil))
             #-(or clasp-min aclasp bclasp)(when (null kind) (warn "Unknown function ~a" fnameoid))
             ;;(assemble context +fdefinition+ (literal-index fnameoid context))
             (emit-fdefinition context (literal-index fnameoid context))
             )
            ((member kind '(:local-function))
             (reference-lexical-info data context))
            (t (error "Unknown kind ~a" kind)))))
    (when (eql (context-receiving context) t)
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
    (multiple-value-bind (required optionals rest key-flag keys aok-p aux
                          varest-p)
        (core:process-lambda-list lambda-list 'function)
      (let* ((function (context-function context))
             (entry-point (cfunction-entry-point function))
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
               (assemble-maybe-long context +check-arg-count-eq+ min-count))
              (t
               (when (> min-count 0)
                 (assemble-maybe-long context +check-arg-count-ge+ min-count))
               (unless more-p
                 (assemble-maybe-long context +check-arg-count-le+ max-count))))
        (unless (zerop min-count)
          ;; Bind the required arguments.
          (assemble-maybe-long context +bind-required-args+ min-count)
          (dolist (var (cdr required))
            ;; We account for special declarations in outer environments/globally
            ;; by checking the original environment - not our new one - for info.
            (cond ((or (member var specials) (eq :special (var-info var env)))
                   (let ((info (nth-value 1 (var-info var new-env))))
                     (assemble-maybe-long
                      context +ref+
                      (cmp:lexical-var-info/frame-index info)))
                   (emit-special-bind context var))
                  (t
                   (maybe-emit-encage (nth-value 1 (var-info var new-env)) context))))
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
              (let ((info (nth-value 1 (var-info var new-env))))
                (push (cons var (cmp:lexical-var-info/frame-index info))
                      opt-key-indices)))
            ;; Re-mark anything that's special in the outer context as such, so that
            ;; default initforms properly treat them as special.
            (let ((specials (remove-if-not (lambda (sym)
                                             (eq :special (var-info sym env)))
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
              (new-literal-index key-name context)))
          (let ((keyvars (collect-by #'cddddr (cddr keys))))
            (setq new-env (bind-vars keyvars new-env context))
            (dolist (var keyvars)
              (let ((info (nth-value 1 (var-info var new-env))))
                (push (cons var (cmp:lexical-var-info/frame-index info))
                      opt-key-indices)))
            (let ((specials (remove-if-not (lambda (sym)
                                             (eq :special (var-info sym env)))
                                           keyvars)))
              (when specials
                (setq new-env (add-specials specials new-env))))))
        ;; Generate defaulting code for optional args, and special-bind them
        ;; if necessary.
        (unless (zerop optional-count)
          (do ((optionals (cdr optionals) (cdddr optionals))
               (optional-label (make-label) next-optional-label)
               (next-optional-label (make-label) (make-label)))
              ((endp optionals) (emit-label context optional-label))
            (emit-label context optional-label)
            (let* ((optional-var (car optionals))
                   (defaulting-form (cadr optionals)) (supplied-var (caddr optionals))
                   (optional-special-p (or (member optional-var specials)
                                           (eq :special (var-info optional-var env))))
                   (index (cdr (assoc optional-var opt-key-indices)))
                   (supplied-special-p (and supplied-var
                                            (or (member supplied-var specials)
                                                (eq :special (var-info supplied-var env))))))
              (setq new-env
                    (compile-optional/key-item optional-var defaulting-form index
                                               supplied-var next-optional-label
                                               optional-special-p supplied-special-p
                                               context new-env))
              (when optional-special-p (incf special-binding-count))
              (when supplied-special-p (incf special-binding-count)))))
        ;; &rest
        (when rest
          (if varest-p
              (assemble-maybe-long context +vaslistify-rest-args+ max-count)
              (assemble-maybe-long context +listify-rest-args+ max-count))
          (assemble-maybe-long context +set+ (cmp:lexenv/frame-end new-env))
          (setq new-env (bind-vars (list rest) new-env context))
          (cond ((or (member rest specials)
                     (eq :special (var-info rest env)))
                 (assemble-maybe-long
                  +ref+ (nth-value 1 (var-info rest new-env)) context)
                 (emit-special-bind context rest)
                 (incf special-binding-count 1)
                 (setq new-env (add-specials (list rest) new-env)))
                (t (maybe-emit-encage (nth-value 1 (var-info rest new-env)) context))))
        ;; Generate defaulting code for key args, and special-bind them if necessary.
        (when key-flag
          (do ((keys (cdr keys) (cddddr keys))
               (key-label (make-label) next-key-label)
               (next-key-label (make-label) (make-label)))
              ((endp keys) (emit-label context key-label))
            (emit-label context key-label)
            (let* ((key-var (cadr keys)) (defaulting-form (caddr keys))
                   (index (cdr (assoc key-var opt-key-indices)))
                   (supplied-var (cadddr keys))
                   (key-special-p (or (member key-var specials)
                                      (eq :special (var-info key-var env))))
                   (supplied-special-p (and supplied-var
                                            (or (member supplied-var specials)
                                                (eq :special (var-info key-var env))))))
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
                                (new-context context :receiving 1))
                  (cond (specialp
                         (emit-special-bind context var))
                        (t
                         (maybe-emit-make-cell info context)
                         (assemble-maybe-long context +set+ var-index))))))
         (supply (suppliedp specialp var info)
           (if suppliedp
               (compile-literal t env (new-context context :receiving 1))
               (assemble context +nil+))
           (cond (specialp
                  (emit-special-bind context var))
                 (t
                  (maybe-emit-make-cell info context)
                  (assemble-maybe-long
                   context +set+
                   (cmp:lexical-var-info/frame-index info))))))
    (let ((supplied-label (make-label))
          (var-info (nth-value 1 (var-info var env))))
      (when supplied-var
        (setq env (bind-vars (list supplied-var) env context)))
      (let ((supplied-info (nth-value 1 (var-info supplied-var env))))
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
           (function (make-cfunction module :name name :doc docs))
           (context (make-context :receiving t :function function))
           (env (make-lexical-environment env :frame-end 0)))
      (setf (cfunction-index function)
            (vector-push-extend function (cmodule-cfunctions module)))
      (compile-with-lambda-list lambda-list body env context)
      (assemble context +return+)
      function)))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let* ((new-tags (cmp:lexenv/tags env))
         (tagbody-dynenv (gensym "TAG-DYNENV"))
         (env (bind-vars (list tagbody-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info tagbody-dynenv env))))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement dynenv-info (make-label))
              new-tags)))
    (let ((env (make-lexical-environment env :tags new-tags)))
      ;; Bind the dynamic environment.
      (assemble context +entry+ (cmp:lexical-var-info/frame-index dynenv-info))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cddr (assoc statement (cmp:lexenv/tags env))))
            (compile-form statement env (new-context context :receiving 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (context-receiving context) 0)
    (assemble context +nil+)
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (cmp:lexenv/tags env))))
    (if pair
        (destructuring-bind (dynenv-info . tag-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (bind-vars (list block-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info block-dynenv env)))
         (label (make-label))
         (normal-label (make-label)))
    ;; Bind the dynamic environment.
    (assemble context +entry+ (cmp:lexical-var-info/frame-index dynenv-info))
    (let ((env (make-lexical-environment
                env
                :blocks (acons name (cons dynenv-info label) (cmp:lexenv/blocks env)))))
      ;; Force single values into multiple so that we can uniformly PUSH afterward.
      (compile-progn body env context))
    (when (eql (context-receiving context) 1)
      (emit-jump context normal-label))
    (emit-label context label)
    ;; When we need 1 value, we have to make sure that the
    ;; "exceptional" case pushes a single value onto the stack.
    (when (eql (context-receiving context) 1)
      (assemble context +push+)
      (emit-label context normal-label))
    (assemble context +entry-close+)))

(defun compile-return-from (name value env context)
  (compile-form value env (new-context context :receiving t))
  (let ((pair (assoc name (cmp:lexenv/blocks env))))
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
  (emit-unbind context 1))

(defun compile-symbol-macrolet (bindings body env context)
  (let ((smacros nil))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (compile-locally body (make-lexical-environment
                           env
                           :vars (append (nreverse smacros) (cmp:lexenv/vars env)))
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
           (dolist (pair (cmp:lexenv/vars env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (member (var-info-kind info) '(:constant :symbol-macro))
                 (push pair cpairs)))))
   :funs (let ((cpairs nil))
           (dolist (pair (cmp:lexenv/funs env) (nreverse cpairs))
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
             (info (cmp:local-macro-info/make expander)))
        (push (cons name info) macros)))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (cmp:lexenv/funs env)))
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
  (emit-mv-call context))

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
(defun link-function (cfunction compile-info)
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
                 0 0 0 0 nil 0          ; unused at the moment
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
        (core:bytecode-module/setf-bytecode bytecode-module bytecode)
        (core:bytecode-module/setf-compile-info bytecode-module compile-info))
      #+(or)(log-function cfunction compile-info bytecode)))
  (cfunction-info cfunction))



;;; ------------------------------------------------------------
;;;
;;; disassembler
;;;
;;;



(defun early-mask-field (size position integer)
  (logand (ash (lognot (ash -1 size))
	       position)
	  integer))

(defun dis-signed (x size)
  (logior x (- (early-mask-field 1 (1- size) x))))

-(defun disassemble-instruction (bytecode &key (ip 0))
  (declare (optimize (debug 3)))
  (flet ((fixed (n) (let ((bytes nil))
                      (dotimes (i n)
                        (push (aref bytecode (incf ip)) bytes))
                      (nreverse bytes))) ;;(loop repeat n collect (aref bytecode (incf ip))))
         (this-signed (n)
           (dis-signed (do* ((ncur n (1- n))
                             (s 0 (+ 8 s))
                             (sum 0))
                            ((= ncur 0) sum)
                         (setq sum (+ sum (ash (aref bytecode (incf ip)) s))))
                       (* n 8))))
    (macrolet ((dfixed (n)
                 `(prog1 (list* op (fixed ,n)) (incf ip)))
               (dsigned (n)
                 `(prog1 (list op (this-signed ,n)) (incf ip))))
      (let ((op (decode-instr (aref bytecode ip))))
        (block test
          (if (member op (list +make-cell+ +cell-ref+ +cell-set+
                               +return+
                               +entry-close+ +unbind+
                               +catch-close+
                               +throw+
                               +progv+
                               +nil+ +eq+
                               +push-values+ +pop-values+
                               +append-values+
                               +mv-call+
                               +mv-call-receive-one+
                               +pop+
                               +push+
                               +long+))
              (return-from test (dfixed 0)))
          (if (member op (list +ref+ +const+ +closure+
                               +listify-rest-args+
                               +vaslistify-rest-args+
                               +call+ +call-receive-one+
                               +set+ +make-closure+ +make-uninitialized-closure+ +initialize-closure+
                               +check-arg-count-eq+ +check-arg-count-le+ +check-arg-count-ge+
                               +bind-required-args+
                               +special-bind+ +symbol-value+ +symbol-value-set+
                               +entry+
                               +fdefinition+ +mv-call-receive-fixed+))
              (return-from test (dfixed 1)))
          (if (member op (list +jump-8+ +jump-if-8+ +exit-8+ +catch-8+))
              (return-from test (dsigned 1)))
          (if (member op (list +jump-16+ +jump-if-16+ +exit-16+ +catch-16+)) (return-from test (dsigned 2)))
          (if (member op (list +jump-24+ +jump-if-24+ +exit-24+)) (return-from test (dsigned 3)))
          (if (member op (list +jump-if-supplied-8+))
              (return-from test (prog1 (list op (aref bytecode (incf ip)) (this-signed 1)) (incf ip))))
          (if (member op (list +jump-if-supplied-16+))
              (return-from test (prog1 (list op (aref bytecode (incf ip)) (this-signed 2)) (incf ip))))
          (if (member op (list +call-receive-fixed+ +bind+ +bind-optional-args+))
              (return-from test (dfixed 2)))
          (if (member op (list +parse-key-args+)) (return-from test (dfixed 4)))
          (error "Illegal op ~a" op)
          )))))

(defun instruction-length (name)
  (if (member name (load-time-value (list +make-cell+ +cell-ref+ +cell-set+
                                          +return+
                                          +entry-close+ +unbind+
                                          +catch-close+ +throw+ +progv+
                                          +nil+ +eq+ +push-values+ +pop-values+
                                          +append-values+
                                          +mv-call+ +mv-call-receive-one+
                                          +pop+ +push+ +long+)))
      1
      (if (member name (list +ref+ +const+ +closure+
                             +listify-rest-args+
                             +vaslistify-rest-args+
                             +call+ +call-receive-one+
                             +set+ +make-closure+
                             +make-uninitialized-closure+ +initialize-closure+
                             +check-arg-count-eq+ +check-arg-count-le+ +check-arg-count-ge+
                             +bind-required-args+
                             +special-bind+ +symbol-value+ +symbol-value-set+
                             +entry+ +catch-8+
                             +jump-if-8+ +jump-8+
                             +exit-8+
                             +fdefinition+ +mv-call-receive-fixed+))
          2
          (if (member name (list +call-receive-fixed+ +bind+ +bind-optional-args+
                                 +jump-16+ +jump-if-16+ +exit-16+ +catch-16+
                                 +jump-if-supplied-8+))
              3
              (if (member name (list +jump-24+ +jump-if-24+ +exit-24+
                                     +jump-if-supplied-16+))
                  4
                  (if (member name (list +parse-key-args+))
                      5
                      (error "Unknown length for instruction ~a" name)))))))

(defun %disassemble-bytecode (bytecode)
  ;; First pass: Go through the bytecode storing all labels
  ;; (i.e. positions that are jumped to)
  (let* ((length (length bytecode))
         (labels
             (flet ((this-signed (ip n)
                      (dis-signed (do* ((i 0 (1+ i))
                                        (s 0 (+ 8 s))
                                        (sum 0))
                                       ((= i n) (progn
                                                  sum))
                                    (setq sum (+ sum (ash (aref bytecode (+ ip i)) s))))
                                  (* n 8))))
               (let ((ip 0)
                     (result nil)
                     op)
                 (loop (setq op (decode-instr (aref bytecode ip)))
                       (if (member op (list +jump-8+ +jump-if-8+ +exit-8+ +catch-8+))
                           (push (+ ip (this-signed (+ ip 1) 1)) result))
                       (if (member op (list +jump-16+ +jump-if-16+
                                        +exit-16+ +catch-16+))
                           (push (+ ip (this-signed (+ ip 1) 2)) result))
                       (if (member op (list +jump-24+ +jump-if-24+
                                        +exit-24+))
                           (push (+ ip (this-signed (+ ip 1) 3)) result))
                       (if (member op (list +jump-if-supplied-8+))
                           (push (+ ip (this-signed (+ ip 2) 1)) result))
                       (if (member op (list +jump-if-supplied-16+))
                           (push (+ ip (this-signed (+ ip 2) 2)) result))
                       (incf ip (instruction-length op))
                       (if (>= ip length) (return (nreverse result))))))))
    ;; now actually output, putting in the labels appropriately
    (let ((ip 0)
          (result nil)
          op)
      (loop (setq op (decode-instr (aref bytecode ip)))
            (if (position ip labels)
                (push (write-to-string (position ip labels)) result))
            (push (labels ((fixed (n &optional (offset 1))
                             (let ((result nil))
                               (dotimes (i n)
                                 (push (aref bytecode (+ ip i offset)) result))
                               (nreverse result)))
                           (label-aux (n offset)
                             (dis-signed
                              (do* ((i 0 (1+ i))
                                    (s 0 (+ 8 s))
                                    (byte (aref bytecode (+ ip i offset)) (aref bytecode (+ ip i offset)))
                                    (sum 0))
                                   ((= i n) (return sum))
                                (setq sum (+ sum (ash byte s))))
                              (* 8 n)))
                           (label (n &optional (offset 1))
                             (let ((pos (+ ip (label-aux n offset))))
                               (or (write-to-string (position pos labels))
                                   (error "bug: no label for ~d" pos)))))
                    (cons
                     ip
                     (block test
                       (if (member op (list +make-cell+ +cell-ref+ +cell-set+ +return+
                                            +entry-close+ +unbind+ +catch-close+
                                            +throw+ +progv+ +nil+ +eq+ +push-values+
                                            +pop-values+ +append-values+ +mv-call+
                                            +mv-call-receive-one+ +pop+ +push+ +long+))
                           (return-from test (list op)))
                       (if (member op (list +ref+ +const+ +closure+ +listify-rest-args+
                                            +vaslistify-rest-args+ +call+
                                            +call-receive-one+ +set+ +make-closure+
                                            +make-uninitialized-closure+ +initialize-closure+
                                            +check-arg-count-eq+ +check-arg-count-le+
                                            +check-arg-count-ge+ +bind-required-args+
                                            +special-bind+ +symbol-value+ +symbol-value-set+
                                            +entry+ +catch-8+
                                            +fdefinition+ +mv-call-receive-fixed+))
                           (return-from test (list* op (fixed 1))))
                       (if (member op (list +call-receive-fixed+ +bind+ +bind-optional-args+))
                           (return-from test (list* op (fixed 2))))
                       (if (member op (list +parse-key-args+))
                           (return-from test (list* op (fixed 4))))
                       (if (member op (list +jump-8+ +jump-if-8+ +exit-8+ +catch-8+))
                           (return-from test (list op (label 1))))
                       (if (member op (list +jump-16+ +jump-if-16+ +exit-16+ +catch-16+))
                           (return-from test (list op (label 2))))
                       (if (member op (list +jump-24+ +jump-if-24+ +exit-24+))
                           (return-from test (list op (label 3))))
                       (if (member op (list +jump-if-supplied-8+))
                           (return-from test `(,op ,@(fixed 1) ,(label 1 2))))
                       (if (member op (list +jump-if-supplied-16+))
                           (return-from test `(,op ,@(fixed 1) ,(label 2 2))))
                       (error "Illegal op ~a" op))))
                  result)
            (incf ip (instruction-length op))
            (if (>= ip length) (return (nreverse result)))))))

(defun disassemble-bytecode (bytecode &key (address 0))
  (let ((dis (%disassemble-bytecode bytecode)))
    (flet ((textify-operand-or-label (thing)
             (if (integerp thing)
                 (write-to-string thing)
                 (concatenate 'string "L" (string thing)))))
      (format t "~&---module---~%")
      (dolist (item dis)
        (if (consp item)
            (format t "~&0x~8,'0x:  ~a~{ ~a~}~%"
                    (+ address (car item))
                    (string-trim "+" (symbol-name (cadr item)))
                    (mapcar #'textify-operand-or-label (cddr item)))
            (if (or (stringp item) (symbolp item))
                (format t "~&; ~a:~%" (textify-operand-or-label item))
                (error "Illegal item ~a" item))))))
  (values))

;;; --------------------------------------------------
;;;
;;; Generate C++ code for the VM bytecodes
;;;
;;; Generate an enum called vm_codes that sets the values
;;; for all of the vm bytecodes according to the order in
;;; which they are defined above in *codes*.
;;;

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


;;; --------------------------------------------------------------
;;;
;;; Generate Python code for the VM bytecodes
;;;
;;;

(defun pythonify-arguments (args)
  (declare (optimize (debug 3)))
  (loop for arg in args
        when (integerp arg)
          collect (format nil "~d" arg)
        when (consp arg)
          collect (let* ((fn-name (string-downcase (car arg)))
                         (fn-underscore-name (substitute #\- #\_ fn-name))
                         (num-arg (second arg)))
                    (format nil "~a(~d)" fn-underscore-name num-arg))))

(defun generate-python-bytecode-table (&optional (file-name "virtualMachine.py"))
  (with-open-file (fout file-name :direction :output :if-exists :supersede)
    (loop for full-code in *full-codes*
          do (destructuring-bind (name code arguments long-arguments)
                 full-code
               (let ((python-arguments (pythonify-arguments arguments))
                     (python-long-arguments (pythonify-arguments long-arguments)))
                 (format fout "new_instr( ~s, ~d, [~{ ~a~^, ~}], [~{ ~a~^, ~}] )~%"
                         name
                         code
                         python-arguments
                         python-long-arguments))))))
