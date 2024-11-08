(in-package #:clasp-cleavir)

;;;; Variables and accessors used in the translation process.
;;;; In a separate file because translate-bir and landing-pad-bir both need
;;;; these and circular dependencies are bad.

(defvar *tags*)
(defvar *datum-values*)
(defvar *constant-values*)
(defvar *dynenv-storage*)
(defvar *unwind-ids*)
(defvar *function-info*)
(defvar *enclose-initializers*)

;; This is an alist from BIR:FUNCTIONs to closure environments
;; (i.e. lists of lexicals). It is used by the BTB compiler to
;; ensure that clasp-cleavir lays out a simple fun that can use
;; an existing closure layout.
(defvar *fixed-closures* nil)

;;; In CSTs and stuff the origin is (spi . spi). Use the head.
(defun origin-spi (origin)
  (if (consp origin) (car origin) origin))

(defun ensure-origin (origin &optional (num 999905))
  (or origin
      (core:make-source-pos-info :filepos num :lineno num :column num)))

(defun delay-initializer (initializer-thunk)
  (push initializer-thunk *enclose-initializers*))

(defun force-initializers ()
  (loop (unless *enclose-initializers* (return))
        (funcall (pop *enclose-initializers*))))

(defun iblock-tag (iblock)
  (or (gethash iblock *tags*)
      (error "BUG: No tag for iblock: ~a" iblock)))

(defun datum-name-as-string (datum)
  ;; We need to write out setf names as well as symbols, in a simple way.
  ;; "simple" means no pretty printer, for a start.
  ;; Using SYMBOL-NAME like this is about 25x faster than using write-to-string,
  ;; and this function is called rather a lot so it's nice to make it faster.
  (let ((name (bir:name datum)))
    (typecase name
      (null "")
      (symbol (symbol-name name))
      (t (write-to-string name
                          :escape nil
                          :readably nil
                          :pretty nil)))))

;;; This function is used for names for debug information, so we want them to be
;;; a little bit more complete.
(defun full-datum-name-as-string (datum)
  (let ((*package* (find-package "KEYWORD")))
    (write-to-string datum :escape t :readably nil :pretty nil)))

(defgeneric vrtype->llvm (vrtype))
(defmethod vrtype->llvm ((vrtype (eql :object))) cmp:%t*%)
(defmethod vrtype->llvm ((vrtype (eql :boolean))) cmp:%i1%)
(defmethod vrtype->llvm ((vrtype (eql :vaslist))) cmp:%vaslist%)
;; These all pretty much have to match element-type->llvm-type in cmp/cmpintrinsics.
(defmethod vrtype->llvm ((vrtype (eql :single-float))) cmp:%float%)
(defmethod vrtype->llvm ((vrtype (eql :double-float))) cmp:%double%)
(defmethod vrtype->llvm ((vrtype (eql :base-char))) cmp:%i8%)
(defmethod vrtype->llvm ((vrtype (eql :character))) cmp:%i32%)
(defmethod vrtype->llvm ((vrtype (eql :fixnum))) cmp:%fixnum%)
(defmethod vrtype->llvm ((vrtype (eql :utfixnum))) cmp:%fixnum%)

(defun bind-variable (var)
  (if (bir:immutablep var)
      ;; Since immutable vars are just LLVM Values, they will be initialized
      ;; by their single VARIABLE-OUT call.
      nil
      (setf (gethash var *datum-values*)
            (ecase (bir:extent var)
              ((:local :dynamic)
               ;; just an alloca
               (let* ((name (datum-name-as-string var))
                      #+(or)
                      (fname (full-datum-name-as-string var))
                      (rtype (cc-bmir:rtype var)))
                 (if (null rtype)
                     ;; Variable is unused: bind nothing
                     ()
                     ;; More usual case
                     (let* ((vrtype
                              (cond ((listp rtype)
                                     (first (cc-bmir:rtype var)))
                                    (t (error "BUG: Bad rtype ~a" rtype))))
                            (alloca (cmp:alloca (vrtype->llvm vrtype) 1 name))
                            #+(or)
                            (spi (origin-spi (bir:origin var))))
                       ;; set up debug info
                       ;; Disable for now - FIXME and get it working
                       #+(or)(cmp:dbg-variable-alloca alloca fname spi)
                       ;; return
                       alloca))))
              ((:indefinite)
               ;; make a cell
               (%intrinsic-invoke-if-landing-pad-or-call
                "cc_makeCell" nil (datum-name-as-string var)))))))

;; Return either the value or cell of a closed over variable depending
;; on whether it is immutable so we can close over the memory location
;; and implement the the cell indirection properly when the variable
;; is mutable and closed over.
;; We also allow a "variable" to be NIL. This indicates that the closure
;; has a slot unused by the code; this is used when we BTB compile
;; a bytecode function. In this case we just put in a nil.
;; (Not undef, since it still ought to be a valid object.)
(defun variable-as-argument (variable)
  (if variable
      (let ((value/cell (or (gethash variable *datum-values*)
                            (error "BUG: Variable or cell missing: ~a" variable))))
        (if (or (typep variable 'bir:come-from)
                (bir:immutablep variable))
            value/cell
            (ecase (bir:extent variable)
              (:indefinite value/cell)
              (:dynamic (cmp:irc-bit-cast value/cell cmp:%t*%))
              (:local
               (error "Should not be passing the local variable ~a as an environment argument." variable)))))
      (%nil)))

(defun in (datum)
  (check-type datum (or bir:phi bir:ssa))
  (multiple-value-bind (dat presentp) (gethash datum *datum-values*)
    (if presentp
        dat
        (error "BUG: No variable for datum: ~a defined by ~a"
               datum (bir:definitions datum)))))

;;; The C standard says that, after setjmp returns subsequently, the state of
;;; local variables is indeterminate unless they are of volatile type.
;;; Practically speaking, this means that without the loads being marked
;;; volatile, LLVM will optimize them away if the binding function gives it no
;;; reason not to; for example (let ((x 0)) ... x) could be optimized into
;;; simply returning a literal 0, even if the "..." calls a function that
;;; modifies x and then returns nonlocally.
;;; I don't believe we need to mark the stores volatile, or to mark the atomic
;;; loads volatile, since LLVM won't really optimize those away.
;;; But I'm not sure.
;;; Non-volatility seems to be the cause of #1183.
(defun needs-volatile-loads-p (function)
  (cleavir-set:some (lambda (inst) (not (null (bir:unwinds inst))))
                    (bir:come-froms function)))

(defun variable-in (variable)
  (check-type variable bir:variable)
  (if (bir:immutablep variable)
      (multiple-value-bind (dat presentp) (gethash variable *datum-values*)
        (if presentp
            dat
            (error "BUG: Variable missing: ~a" variable)))
      ;; NOTE: In the future when we need type loads for LLVM, the rtype will
      ;; have the type information.
      (if (null (cc-bmir:rtype variable))
          nil
          (ecase (bir:extent variable)
            (:local
             (let* ((alloca (or (gethash variable *datum-values*)
                                (error "BUG: Variable missing: ~a" variable)))
                    (rtype (first (cc-bmir:rtype variable)))
                    (alloca-type (vrtype->llvm rtype)))
               (cmp:irc-typed-load alloca-type alloca)))
            (:dynamic
             (let ((alloca (or (gethash variable *datum-values*)
                               (error "BUG: DX cell missing: ~a" variable)))
                   (volatile (needs-volatile-loads-p
                              (bir:function (bir:binder variable)))))
               (cmp:irc-t*-load (cmp:irc-bit-cast alloca cmp:%t**%) "" volatile)))
            (:indefinite
             (let ((cell (or (gethash variable *datum-values*)
                             (error "BUG: Cell missing: ~a" variable)))
                   (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
               (cmp:irc-t*-load-atomic (cmp::gen-memref-address cell offset))))))))

(defun out (value datum)
  (check-type datum bir:ssa)
  (assert (not (nth-value 1 (gethash datum *datum-values*)))
          ()
          "Double OUT for ~a: Old value ~a, new value ~a"
          datum (gethash datum *datum-values*) value)
  (setf (gethash datum *datum-values*) value))

(defun phi-out (value datum llvm-block)
  (check-type datum bir:phi)
  (let ((rt (cc-bmir:rtype datum)))
    (cond ((or (member rt '(:multiple-values :vaslist))
               (and (listp rt) (= (length rt) 1)))
           ;; datum is a T_mv or a vaslist or a single value
           (llvm-sys:add-incoming (in datum) value llvm-block))
          ((listp rt)
           ;; Datum is a list of llvm data, and (in datum) is a list of phis.
           (loop for phi in (in datum)
                 for val in value
                 do (llvm-sys:add-incoming phi val llvm-block)))
          (t (error "BUG: Bad rtype ~a" rt)))))

(defun variable-out (value variable)
  (check-type variable bir:variable)
  (if (bir:immutablep variable)
      (prog1 (setf (gethash variable *datum-values*) value)
        ;; FIXME - this doesn't work yet
        #+(or)(cmp:dbg-variable-value
               value (full-datum-name-as-string variable)
               (origin-spi (bir:origin variable))))
      (if (null (cc-bmir:rtype variable))
          value
          ;; NOTE: For typed loads in the future, use the rtype
          (ecase (bir:extent variable)
            (:local
             (let ((alloca (or (gethash variable *datum-values*)
                               (error "BUG: Variable missing: ~a" variable))))
               (cmp:irc-store value alloca)))
            (:dynamic
             (let ((alloca (or (gethash variable *datum-values*)
                               (error "BUG: DX cell missing: ~a" variable))))
               (cmp:irc-store value (cmp:irc-bit-cast alloca cmp:%t**%))))
            (:indefinite
             (let ((cell (or (gethash variable *datum-values*)
                             (error "BUG: Cell missing: ~a" variable)))
                   (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
               (cmp:irc-store-atomic
                value
                (cmp::gen-memref-address cell offset))))))))

(defun dynenv-storage (dynenv)
  (check-type dynenv bir:dynamic-environment)
  (multiple-value-bind (value presentp) (gethash dynenv *dynenv-storage*)
    (if presentp
        value
        (error "BUG: Missing dynenv storage for ~a" dynenv))))

(defun (setf dynenv-storage) (new dynenv)
  (setf (gethash dynenv *dynenv-storage*) new))

(defun get-destination-id (iblock)
  (or (gethash iblock *unwind-ids*)
      (error "Missing unwind ID for ~a" iblock)))

;; Does this iblock have nonlocal entrances?
(defun has-entrances-p (iblock)
  (not (cleavir-set:empty-set-p (bir:entrances iblock))))

(defun find-llvm-function-info (function)
  (or (gethash function *function-info*)
      (error "Missing llvm function info for BIR function ~a." function)))

;;; Binding and unbinding special variables
(defun bind-special (cellv value)
  (let* ((bde-cons-mem (cmp:alloca-i8 cmp:+cons-size+ :alignment cmp:+alignment+
                                      :label "binding-dynenv-cons-mem"))
         (bde-mem (cmp:alloca-i8 cmp:+binding-dynenv-size+
                                 :alignment cmp:+alignment+
                                 :label "binding-dynenv-mem"))
         (old-de-stack (%intrinsic-call "cc_get_dynenv_stack" nil))
         (ind (%intrinsic-call "cc_getCellTLIndex" (list cellv)))
         (old (%intrinsic-call "cc_specialBind" (list ind value))))
    (%intrinsic-call "cc_initializeAndPushBindingDynenv"
                     (list bde-mem bde-cons-mem cellv old))
    (values ind old old-de-stack)))

(defun unbind-special (index old-value old-de-stack)
  (%intrinsic-call "cc_specialUnbind" (list index old-value))
  (%intrinsic-call "cc_set_dynenv_stack" (list old-de-stack)))

(defun gen-call-cleanup (uwprotect-inst)
  (let (;; KLUDGE: In order to reenable interrupts when throwing
        ;; an exception out, we use a fake constant bind to generate
        ;; a landing pad. This only needs its class identity,
        ;; dynenv-storage, and parent to be handled right by
        ;; landing-pad.lisp.
        ;; I can't think of a cleaner way to do this.
        (bind (make-instance 'bir:constant-bind
                :iblock (bir:iblock uwprotect-inst))))
    (multiple-value-bind (ind old old-destack)
        (bind-special (literal:constants-table-value
                       (literal:reference-variable-cell
                        'core:*interrupts-enabled*)
                       :literal-name "*INTERRUPTS-ENABLED*")
                      (%nil))
      (setf (dynenv-storage bind) (list ind old old-destack))
      (cmp:with-landing-pad (maybe-entry-landing-pad bind *tags*)
        (closure-call-or-invoke (in (first (bir:inputs uwprotect-inst))) nil))
      (unbind-special ind old old-destack))))
