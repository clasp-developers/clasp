(cl:in-package #:clasp-cleavir)


(defvar *debug-cleavir* nil
  "controls if graphs are generated as forms are being compiled.")
(defvar *debug-cleavir-literals* nil
  "controls if cleavir debugging is carried out on literal compilation. 
when this is t a lot of graphs will be generated.")

;;;
;;; the first argument to this function is an instruction that has a
;;; single successor.  whether a go is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; the inputs are forms to be evaluated.  the outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction return-value inputs outputs abi current-function-info))

(defgeneric translate-branch-instruction (instruction return-value inputs outputs successors abi current-function-info))



(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defun datum-name-as-string (datum)
  ;; We need to write out setf names as well as symbols, in a simple way.
  ;; "simple" means no pretty printer, for a start.
  (write-to-string (cleavir-ir:name datum)
                   :escape nil
                   :readably nil
                   :pretty nil))

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      (let* ((value (cleavir-ir:value datum)))
        (%literal-ref value t))
      (let ((var (gethash datum *vars*)))
	(when (null var)
          (typecase datum
            (cleavir-ir:values-location) ; do nothing - we don't actually use them
            (cleavir-ir:immediate-input (setf var (%i64 (cleavir-ir:value datum))))
            ;; names may be (setf foo), so use write-to-string and not just string
            (cc-mir:typed-lexical-location
             (setf var (alloca (cc-mir:lexical-location-type datum) 1 (datum-name-as-string datum))))
            (cleavir-ir:lexical-location (setf var (alloca-t* (datum-name-as-string datum))))
            (t (error "add support to translate datum: ~a~%" datum)))
	  (setf (gethash datum *vars*) var))
	var)))

(defun translate-lambda-list-item (item)
  (cond ((symbolp item)
	 item)
	((consp item)
	 (ecase (length item)
	   (2 (list (translate-datum (first item))
		    nil
		    (translate-datum (second item))))
	   (3 (list (list (first item)
			  (translate-datum (second item)))
		    nil
		    (translate-datum (third item))))))
	(t
	 (translate-datum item))))

(defun translate-lambda-list (lambda-list)
  (mapcar #'translate-lambda-list-item lambda-list))

(defun layout-basic-block (basic-block return-value abi current-function-info)
  (destructuring-bind (first last owner) basic-block
    (cc-dbg-when *debug-log*
			 (format *debug-log* "- - - -  begin layout-basic-block  owner: ~a~%" (cc-mir:describe-mir owner))
			 (loop for instruction = first
			    then (first (cleavir-ir:successors instruction))
			    until (eq instruction last)
			    do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction))))
    (loop for instruction = first
       then (first (cleavir-ir:successors instruction))
       for inputs = (cleavir-ir:inputs instruction)
       for input-vars = (mapcar #'translate-datum inputs)
       for outputs = (cleavir-ir:outputs instruction)
       for output-vars = (mapcar #'translate-datum outputs)
       until (eq instruction last)
#||       do (cc-dbg-when *debug-log*
		       (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
||#
       do (translate-simple-instruction
           instruction return-value input-vars output-vars abi current-function-info))
    (let* ((inputs (cleavir-ir:inputs last))
	   (input-vars (mapcar #'translate-datum inputs))
	   (outputs (cleavir-ir:outputs last))
	   (output-vars (mapcar #'translate-datum outputs))
	   (successors (cleavir-ir:successors last))
	   (successor-tags (loop for successor in successors
			      collect (gethash successor *tags*))))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))
      (cond ((= (length successors) 1)
             (translate-simple-instruction
              last return-value input-vars output-vars abi current-function-info)
             (if (typep last 'cleavir-ir:unwind-instruction)
                 (cmp:irc-unreachable)
                 (progn
                   (cmp:irc-low-level-trace :flow)
                   (cmp:irc-br (gethash (first successors) *tags*)))))
            (t (translate-branch-instruction
                last return-value input-vars output-vars successor-tags abi current-function-info)))
      (cc-dbg-when *debug-log*
		   #+stealth-gids(format *debug-log* "- - - -  END layout-basic-block  owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner) basic-block)
                   #-stealth-gids(format *debug-log* "- - - -  END layout-basic-block  owner: ~a   -->  ~a~%" (cleavir-ir-gml::label owner) basic-block)))))

(defun get-or-create-lambda-name (instr)
  (if (typep instr 'clasp-cleavir-hir:named-enter-instruction)
      (clasp-cleavir-hir:lambda-name instr)
      'TOP-LEVEL))


(defun layout-procedure* (the-function body-irbuilder
                          body-block
                          first-basic-block
                          rest-basic-blocks
                          function-info
                          lambda-name
                          initial-instruction abi &key (linkage 'llvm-sys:internal-linkage))
  (let ((return-value (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                        (alloca-return_type))))
    (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
      ;; in case of a non-local exit, zero out the number of returned values
      (with-return-values (return-values return-value abi)
        (%store (%size_t 0) (number-of-return-values return-values))))
    ;; Setup the landing pads
    (cmp:with-irbuilder (body-irbuilder)
      (generate-needed-landing-pads the-function function-info return-value *tags* abi)
      (cmp:with-dbg-function ("unused-with-dbg-function-name"
                              :linkage-name (llvm-sys:get-name the-function)
                              :function the-function
                              :function-type cmp:%fn-prototype%
                              :form *form*)
        (cmp:with-dbg-lexical-block (*form*)
          (cmp:dbg-set-current-source-pos-for-irbuilder cmp:*irbuilder-function-alloca* cmp:*current-form-lineno*)
          (cmp:dbg-set-current-source-pos-for-irbuilder body-irbuilder cmp:*current-form-lineno*)
          (cmp:irc-low-level-trace :arguments)
          (cmp:irc-set-insert-point-basic-block body-block body-irbuilder )
          (cmp:with-landing-pad (or (landing-pad-for-unwind function-info) (landing-pad-for-cleanup function-info))
            (cmp:irc-begin-block body-block)
            (layout-basic-block first-basic-block return-value abi function-info)
            (loop for block in rest-basic-blocks
               for instruction = (first block)
               do #+(or)(format t "laying out basic block: ~a~%" block)
                  #+(or)(format t "inserting basic block for instruction: ~a~%" instruction)
                  (cmp:irc-begin-block (gethash instruction *tags*))
                  (layout-basic-block block return-value abi function-info)))
          ;; finish up by jumping from the entry block to the body block
          (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
            (cmp:irc-low-level-trace :flow)
            (cmp:irc-br body-block))
          (cc-dbg-when *debug-log* (format *debug-log* "----------end layout-procedure ~a~%" (llvm-sys:get-name the-function)))
          (values the-function lambda-name))))))

(defvar *forms*)
(defvar *map-enter-to-function-info* nil)

(defvar *top-level-procedure*)

(defun layout-procedure (initial-instruction abi &key (linkage 'llvm-sys:internal-linkage))
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((clasp-cleavir-ast-to-hir:*landing-pad* nil)
         (current-function-info-landing-pad (gethash initial-instruction *map-enter-to-function-info*))
         (current-function-info (if current-function-info-landing-pad
                                    current-function-info-landing-pad
                                    (make-instance 'function-info :unwind-target nil)))
         (procedure-has-debug-on #+(or) t (and (null *top-level-procedure*) (policy-anywhere-p initial-instruction 'maintain-shadow-stack)))
         (*top-level-procedure* nil)
         (needs-cleanup (or (unwind-target current-function-info) procedure-has-debug-on))
         ;; Gather the basic blocks of this procedure in basic-blocks
         (basic-blocks (remove initial-instruction *basic-blocks* :test-not #'eq :key #'third))
         ;; Hypothesis: This finds the first basic block
         (first-basic-block (find initial-instruction basic-blocks :test #'eq :key #'first))
         ;; This gathers the rest of the basic blocks
         (rest-basic-blocks (remove first-basic-block basic-blocks :test #'eq))
         (lambda-name (get-or-create-lambda-name initial-instruction)))
    (setf (enter-instruction current-function-info) initial-instruction)
    (setf (debug-on current-function-info) procedure-has-debug-on)
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((main-fn-name lambda-name) ;;(format nil "cl->~a" lambda-name))
           (cmp:*current-function-name* (cmp:jit-function-name main-fn-name))
           (cmp:*gv-current-function-name* (cmp:module-make-global-string cmp:*current-function-name* "fn-name"))
           (the-function (cmp:irc-function-create
                          cmp:%fn-prototype%
                          linkage
                          (cmp:jit-function-name main-fn-name) ;cmp:*current-function-name*)
                          cmp:*the-module*))
           (cmp:*current-function* the-function)
           (entry-block (cmp:irc-basic-block-create "entry" the-function))
           (*current-function-entry-basic-block* entry-block)
           (*function-current-multiple-value-array-address* nil)
           (cmp:*irbuilder-function-alloca* (llvm-sys:make-irbuilder cmp:*llvm-context*))
           (body-irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*))
           (body-block (cmp:irc-basic-block-create "body")))
      (llvm-sys:set-personality-fn the-function (cmp:irc-personality-function))
      (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-uwtable)
      (cc-dbg-when *debug-log*
                   (format *debug-log* "------------ begin layout-procedure ~a~%" (llvm-sys:get-name the-function))
                   (format *debug-log* "   basic-blocks for procedure~%")
                   (dolist (bb basic-blocks)
                     (destructuring-bind (first last owner) bb
                       #+stealth-gids(format *debug-log* "basic-block owner: ~a:~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner))
                       #-stealth-gids(format *debug-log* "basic-block owner: ~a~%" (cleavir-ir-gml::label owner))
                       (loop for instruction = first
                          then (first (cleavir-ir:successors instruction))
                          until (eq instruction last)
                          do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
                       (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))))
      (let ((args (llvm-sys:get-argument-list the-function)))
        (mapc #'(lambda (arg argname) (llvm-sys:set-name arg argname))
              (llvm-sys:get-argument-list the-function) cmp:+fn-prototype-argument-names+))
      ;; create a basic-block for every remaining tag
      (loop for block in rest-basic-blocks
         for instruction = (first block)
         do (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag")))
      (cmp:irc-set-insert-point-basic-block entry-block cmp:*irbuilder-function-alloca*)
;;; Set up the calling convention here by calling cclasp-setup-calling-convention using the
;;; first instruction and the function args
;;; From translate-simple-instruction on the enter-instruction
;;;               (fn-args (llvm-sys:get-argument-list cmp:*current-function*))
;;;               (calling-convention (cclasp-setup-calling-convention fn-args lambda-list NIL #|DEBUG-ON|#)))
;;;      Store this info in the function-info
      (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*) ;; body-irbuilder)
        (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
               (lambda-list (cleavir-ir:lambda-list initial-instruction))
               (calling-convention (cmp:cclasp-setup-calling-convention fn-args lambda-list procedure-has-debug-on #|DEBUG-ON|#)))
          (setf (calling-convention current-function-info) calling-convention)))
      (when needs-cleanup
        ;; This function will need a landing pad - so alloca exn.slot and ehselector.slot
        (setf (exn.slot current-function-info) (alloca-i8* "exn.slot")
              (ehselector.slot current-function-info) (alloca-i32 "ehselector.slot"))
        (when (debug-on current-function-info)
          ;; We want a backtrace frame for this function 
          (setf (on-entry-for-cleanup current-function-info)
                (lambda (function-info)
                  (let ((cc (calling-convention function-info)))
                    (%intrinsic-call "cc_push_InvocationHistoryFrame"
                                     (list (cmp:calling-convention-closure cc)
                                           (cmp:calling-convention-invocation-history-frame* cc)
                                           (cmp:calling-convention-va-list* cc)
                                           (cmp:irc-load (cmp:calling-convention-remaining-nargs* cc))))))
                (on-exit-for-cleanup current-function-info)
                (lambda (function-info)
                  (let ((cc (calling-convention function-info)))
                    (%intrinsic-call "cc_pop_InvocationHistoryFrame"
                                     (list (cmp:calling-convention-closure cc)
                                           (cmp:calling-convention-invocation-history-frame* cc)))))))
        (when (unwind-target current-function-info)
          (setf (on-entry-for-unwind current-function-info)
                (lambda (function-info)
                  (let ((cc (calling-convention function-info))
                        (enter-instruction (enter-instruction function-info))
                        (result (%intrinsic-call "cc_pushLandingPadFrame" nil)))
                    (%store result (translate-datum (clasp-cleavir-hir:frame-holder enter-instruction))))))
          (setf (on-exit-for-unwind current-function-info)
                (lambda (function-info)
                  (let ((cc (calling-convention function-info))
                        (enter-instruction (enter-instruction function-info)))
                    (%intrinsic-call "cc_popLandingPadFrame"
                                     (list (%load (translate-datum (clasp-cleavir-hir:frame-holder enter-instruction))))))))))
      (layout-procedure* the-function
                         body-irbuilder
                         body-block
                         first-basic-block
                         rest-basic-blocks
                         current-function-info
                         lambda-name
                         initial-instruction abi :linkage linkage))))

(defun translate (initial-instruction map-enter-to-function-info &optional (abi *abi-x86-64*) &key (linkage 'llvm-sys:internal-linkage))
  (let* (#+use-ownerships(ownerships
			  (cleavir-hir-transformations:compute-ownerships initial-instruction)))
    (let* (#+use-ownerships
           (*ownerships* ownerships)
           (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
           (*tags* (make-hash-table :test #'eq))
           (*vars* (make-hash-table :test #'eq))
           (*map-enter-to-function-info* map-enter-to-function-info)
           (*top-level-procedure* t)
           )
      #+use-ownerships(setf *debug-ownerships* *ownerships*)
      (setf *debug-basic-blocks* *basic-blocks*
	    *debug-tags* *tags*
	    *debug-vars* *vars*)
      (cc-dbg-when *debug-log*
                   (let ((mir-pathname (make-pathname :name (format nil "mir~a" (incf *debug-log-index*)) :type "gml" :defaults (pathname *debug-log*))))
                     (format *debug-log* "About to write mir to ~a~%" (namestring mir-pathname))
                     (finish-output *debug-log*)
                     (multiple-value-bind (instruction-ids datum-ids)
                         (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname)))
                     (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname)))
                   (let ((mir-pathname (make-pathname :name (format nil "mir~a" (incf *debug-log-index*)) :type "dot" :defaults (pathname *debug-log*))))
                     (cleavir-ir-graphviz:draw-flowchart initial-instruction (namestring mir-pathname))
                     (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname))))
      (multiple-value-bind (function lambda-name)
          (layout-procedure initial-instruction abi :linkage linkage)
        (cmp::cmp-log-compile-file-dump-module cmp:*the-module* "after-translate")
        (values function lambda-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instr cleavir-ir:enter-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (when (unwind-target function-info)
    (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
      (funcall (on-entry-for-unwind function-info) function-info)))
  (let* ((lambda-list (cleavir-ir:lambda-list instr))
         (closed-env-dest (first outputs))
         (calling-convention (calling-convention function-info)))
    (%store (cmp:calling-convention-closure calling-convention) closed-env-dest)
    (let* ((static-environment-output (first (cleavir-ir:outputs instr)))
           (args (cdr (cleavir-ir:outputs instr))))
      #++(progn
           (format t "    outputs: ~s~%" args)
           (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) args))
           (format t "lambda-list: ~a~%" lambda-list))
      (and (on-entry-for-cleanup function-info) (funcall (on-entry-for-cleanup function-info) function-info))
      (and (on-entry-for-unwind function-info) (funcall (on-entry-for-unwind function-info) function-info))
      (cmp:with-landing-pad (or (landing-pad-for-cleanup function-info) nil)
        (cmp:compile-lambda-list-code lambda-list args calling-convention
                                      :translate-datum #'translate-datum)))))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:bind-va-list-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (let* ((lambda-list                   (cleavir-ir:lambda-list instr))
         (vaslist                       (car inputs))
         (src-remaining-nargs*          (%intrinsic-call "cc_vaslist_remaining_nargs_address" (list (%load vaslist))))
         (src-va_list*                  (%intrinsic-call "cc_vaslist_va_list_address" (list (%load vaslist)) "vaslist_address"))
         (local-remaining-nargs*        (alloca-size_t "local-remaining-nargs"))
         (local-va_list*                (alloca-va_list "local-va_list"))
         (_                             (%store (%load src-remaining-nargs*) local-remaining-nargs*))
         (_                             (%intrinsic-call "llvm.va_copy" (list (%pointer-cast local-va_list* cmp:%i8*%)
                                                                              (%pointer-cast src-va_list* cmp:%i8*%))))
         (callconv                      (cmp:make-calling-convention-impl :nargs (%load src-remaining-nargs*)
                                                                          :va-list* local-va_list*
                                                                          :remaining-nargs* local-remaining-nargs*)))
    #++(progn
         (format t "           outputs: ~s~%" outputs)
         (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) outputs))
         (format t "       lambda-list: ~a~%" lambda-list))
    (cmp:with-landing-pad (landing-pad-for-cleanup function-info)
      (cmp:compile-lambda-list-code lambda-list outputs callconv
                                    :translate-datum (lambda (datum)
;;                                                       (core:bformat *debug-io* "datum: %s\n" datum)
                                                       (if (typep datum 'cleavir-ir:lexical-location)
                                                           (translate-datum datum)
                                                           datum))))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) return-value inputs outputs abi function-info)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) return-value inputs outputs abi function-info)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (cmp:irc-low-level-trace :flow)
  (let ((input (first inputs))
        (output (first outputs)))
    (cond
      ((typep input 'llvm-sys:constant-int)
       (let ((val (%inttoptr input cmp:%t*%)))
         (%store val output)))
      (t
       (let ((load (%load input)))
         (%store load output))))))

(defun safe-llvm-name (obj)
  "Generate a name for the object that can be used as a variable label in llvm"
  (cond
    ((stringp obj) obj)
    ((symbolp obj) (string obj))
    ((listp obj) "list")
    ((arrayp obj) "array")
    (t "object")))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) return-value inputs outputs abi function-info)
  (let ((idx (first inputs)))
    (cmp:irc-low-level-trace :flow)
    (let* ((label (safe-llvm-name (clasp-cleavir-hir:precalc-value-instruction-original-object instruction)))
           (value (%load (%gep-variable (cmp:ltv-global) (list (%size_t 0) idx) label))))
      (%store value (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; Write the first return value into the result
  (with-return-values (return-values return-value abi)
    (%store (%size_t (length inputs)) (number-of-return-values return-values))
    (dotimes (i (length inputs))
      (%store (%load (elt inputs i)) (return-value-elt return-values i)))
    #+(or)(cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-values) (first outputs))
    ))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; Create a basic block for each output
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    #+(or)(cmp:irc-intrinsic "cc_loadThreadLocalMultipleValues" (sret-arg return-vals) (first inputs))
    (let* ((blocks (let (b) (dotimes (i (1+ (length outputs))) (push (cmp:irc-basic-block-create (format nil "mvn~a-" i)) b)) (nreverse b)))
	   (final-block (cmp:irc-basic-block-create "mvn-final"))
	   (switch (cmp:irc-switch (%load (number-of-return-values return-vals)) (car (last blocks)) (length blocks))))
      (dotimes (n (length blocks))
	(let ((block (elt blocks n)))
	  (cmp:irc-begin-block block)
	  (llvm-sys:add-case switch (%size_t n) block)
	  (dotimes (i (length outputs))
	    (if (< i n)
		(%store (%load (return-value-elt return-vals i)) (elt outputs i))
		(%store (%nil) (elt outputs i))))
          (cmp:irc-low-level-trace :flow)
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:multiple-value-foreign-call-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (cmp:irc-low-level-trace :flow)
  (check-type (clasp-cleavir-hir:function-name instruction) string)
  (let ((call (clasp-cleavir:unsafe-multiple-value-foreign-call (clasp-cleavir-hir:function-name instruction) return-value inputs abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction multiple-value-foreign-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (cmp:irc-low-level-trace :flow)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (let ((call (clasp-cleavir:unsafe-foreign-call :call (clasp-cleavir-hir:foreign-types instruction) (clasp-cleavir-hir:function-name instruction) (car outputs) inputs abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction foreign-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-pointer-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (cmp:irc-low-level-trace :flow)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (let ((call (clasp-cleavir:unsafe-foreign-call-pointer :call (clasp-cleavir-hir:foreign-types instruction) (%load (car inputs)) (car outputs) (cdr inputs) abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction foreign-call-pointer-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  #+(or)(progn
          (format t "--------------- translate-simple-instruction funcall-instruction~%")
          #+stealth-gids(format t "       funcall:~a~%" (clasp-cleavir:instruction-gid instruction))
          (format t "         return value: ~a~%" return-value)
          (format t "           arg inputs: ~a~%" inputs)
          (format t "          arg outputs: ~a~%" outputs)
          (format t "      instance inputs: ~a~%" (cleavir-ir:inputs instruction))
          (format t "     instance outputs: ~a~%" (cleavir-ir:outputs instruction))
          (format t "  instance successors: ~a~%" (cleavir-ir:successors instruction))
          )
  (cmp:irc-low-level-trace :flow)
  (let ((call (closure-call-or-invoke (first inputs) return-value (cdr inputs) abi)))
    #+(or)(progn
            (format t "generated call --> ~a~%" call))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction funcall-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))





(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
;;; FIXME - this should use cmp::*current-unwind-landing-pad-dest*
  (cmp:irc-low-level-trace :flow)
  (let ((call (closure-call-or-invoke (first inputs) return-value (cdr inputs) abi)))
    (cc-dbg-when *debug-log*
                 (format *debug-log* "    translate-simple-instruction invoke-instruction: ~a~%" (cc-mir:describe-mir instruction))
                 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:indexed-unwind-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    ;; Save whatever is in return-vals in the multiple-value array
    (%intrinsic-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
    (cmp:irc-low-level-trace :cclasp-eh)
    (evaluate-cleanup-code function-info)
    (%intrinsic-call "cc_unwind" (list (%load (first inputs)) (%size_t (clasp-cleavir-hir:jump-id instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((result (%intrinsic-call "cc_makeCell" nil)))
    (%store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (%intrinsic-call "cc_writeCell" (list cell val))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) return-value inputs outputs abi function-info)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
  (cmp:irc-low-level-trace :flow)
    (let ((result (%intrinsic-call "cc_readCell" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((env (%load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (%intrinsic-call "cc_fetch" (list env idx))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value inputs outputs abi function-info)
  ;; How do we figure out if we should use safe or unsafe version
  (cmp:irc-low-level-trace :flow)
  (let ((cell (%load (first inputs) "func-name")))
    ;;    (format t "translate-simple-instruction (first inputs) = ~a ~%" (first inputs))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_safe_fdefinition" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value inputs outputs abi function-info)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (%intrinsic-call "debugMessage" (list msg))))
	

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (%load (first inputs) "setf-func-name")))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_safe_setfdefinition" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (%load (first inputs) "sym-name")))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_safe_symbol_value" (list sym))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (%load (first inputs) "sym-name"))
	(val (%load (second inputs) "value")))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_setSymbolValue" (list sym val))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function lambda-name)
        (layout-procedure enter-instruction abi)
      (let* ((loaded-inputs (mapcar (lambda (x) (%load x "cell")) inputs))
             (ltv-lambda-name (%literal-value lambda-name (format nil "lambda-name->~a" lambda-name)))
             (dx-p (cleavir-ir:dynamic-extent-p instruction))
             (result
               (if dx-p
                   (%intrinsic-call
                    "cc_stack_enclose"
                    (list* (alloca-i8 (core:closure-with-slots-size (length inputs)) "stack-allocated-closure")
                           ltv-lambda-name
                           enclosed-function
                           cmp:*gv-source-file-info-handle*
                           (cmp:irc-size_t-*current-source-pos-info*-filepos)
                           (cmp:irc-size_t-*current-source-pos-info*-lineno)
                           (cmp:irc-size_t-*current-source-pos-info*-column)
                           (%size_t (length inputs))
                           loaded-inputs)
                    (format nil "closure->~a" lambda-name))
                   (%intrinsic-call
                    "cc_enclose"
                    (list* ltv-lambda-name
                           enclosed-function
                           cmp:*gv-source-file-info-handle*
                           (cmp:irc-size_t-*current-source-pos-info*-filepos)
                           (cmp:irc-size_t-*current-source-pos-info*-lineno)
                           (cmp:irc-size_t-*current-source-pos-info*-column)
                           (%size_t (length inputs))
                           loaded-inputs)
                    (format nil "closure->~a" lambda-name)))))
        (cc-dbg-when *debug-log*
                     (format *debug-log* "~:[cc_enclose~;cc_stack_enclose~] with ~a cells~%"
                             dx-p (length inputs))
                     (format *debug-log* "    inputs: ~a~%" inputs))
        (%store result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
 ;   (%intrinsic-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
    ;; NOTE: (NOT A FIXME)  This instruction is explicitly for calls.
    (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call "cc_call_multipleValueOneFormCallWithRet0" 
				     (list (%load (first inputs)) (%load return-value)))))
      (%store call-result return-value)
      (cc-dbg-when *debug-log*
                   (format *debug-log* "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
                           (cc-mir:describe-mir instruction))
                   (format *debug-log* "     instruction --> ~a~%" call-result))
      )))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-multiple-value-call-instruction) return-value inputs outputs abi function-info)
  (cmp:irc-low-level-trace :flow)
  (let* ((lpad (clasp-cleavir::landing-pad instruction)))
    (with-return-values (return-vals return-value abi)
      ;;(%intrinsic-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
      (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call "cc_call_multipleValueOneFormCallWithRet0" 
                                                                   (list (%load (first inputs)) (%load return-value))
                                                           "mvofc")))
        (%store call-result return-value)
	(cc-dbg-when *debug-log*
                     (format *debug-log* "    translate-simple-instruction invoke-multiple-value-call-instruction: ~a~%" 
                             (cc-mir:describe-mir instruction))
                     (format *debug-log* "     instruction --> ~a~%" call-result))))))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((array (%load array)) (index (%load index))
         (type (llvm-sys:type-get-pointer-to (cmp::simple-vector-llvm-type element-type)))
         (cast (%bit-cast array type))
         (var-offset (%ptrtoint index fixnum-type))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged fixnum")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; 1 gets us to the "data" slot of the struct.
    ;; untagged is the actual offset.
    (%gep-variable cast (list (%i32 0) (%i32 1) untagged) "aref")))

(defun translate-bit-aref (output array index)
  (let* ((array (%load array)) (index (%load index))
         (var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (bit (%intrinsic-call "cc_simpleBitVectorAref" (list array untagged) "bit-aref"))
         (tagged-bit (%shl bit cmp::+fixnum-shift+ :nuw t :label "tagged-bit"))
         ;; inttoptr intrinsically zexts, according to docs.
         (fixnum (%inttoptr tagged-bit cmp:%t*% "bit-aref-result")))
    (%store fixnum output)))

(defun translate-bit-aset (value array index)
  (let* ((value (%load value)) (array (%load array)) (index (%load index))
         (var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (offset (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (uint-value (%ptrtoint value cmp::%uint%))
         (untagged-value (%lshr uint-value cmp::+fixnum-shift+ :exact t :label "untagged-value")))
    ;; Note: We cannot label void calls, because then they'll get a variable
    (%intrinsic-call "cc_simpleBitVectorAset" (list array offset untagged-value))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aref-instruction) return-value inputs outputs abi function-info)
  (let ((et (cleavir-ir:element-type instruction)))
    (if (eq et 'bit) ; have to special case due to the layout.
        (translate-bit-aref (first outputs) (first inputs) (second inputs))
        (%store (%load (gen-vector-effective-address (first inputs) (second inputs)
                                                     (cleavir-ir:element-type instruction)
                                                     (%default-int-type abi)))
                (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aset-instruction) return-value inputs outputs abi function-info)
  (let ((et (cleavir-ir:element-type instruction)))
    (if (eq et 'bit) ; ditto above
        (translate-bit-aset (third inputs) (first inputs) (second inputs))
        (%store (%load (third inputs))
                (gen-vector-effective-address (first inputs) (second inputs)
                                              (cleavir-ir:element-type instruction)
                                              (%default-int-type abi))))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::vector-length-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info))
  (let* ((tptr (%load (first inputs)))
         (ui-offset (%uintptr_t (- cmp::+simple-vector._length-offset+ cmp:+general-tag+)))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp:%t**%))
           (read-val (%ptrtoint (%load ptr) (%default-int-type abi)))
           ;; now we just make it a fixnum.
           (fixnum (%shl read-val cmp::+fixnum-shift+ :nuw t :label "tag fixnum")))
      (%store (%inttoptr fixnum cmp:%t*%) (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displacement-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%intrinsic-call "cc_realArrayDisplacement"
                           (list (%load (first inputs))))
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displaced-index-offset-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_realArrayDisplacedIndexOffset"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-total-size-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayTotalSize"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-rank-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayRank"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-dimension-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayDimension"
                             (list (%load (first inputs))
                                   (%lshr (%ptrtoint (%load (second inputs)) (%default-int-type abi))
                                          cmp::+fixnum-shift+ 
                                          :exact t :label "untagged fixnum")))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value inputs outputs abi function-info)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%))
         (ui-offset (%bit-cast offset cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp::%t**%))
           (read-val (%load ptr)))
      (%store read-val (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value inputs outputs abi function-info)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%))
         (ui-offset (%bit-cast offset cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (dest (%inttoptr uiptr cmp::%t**% "memset2-dest"))
           (val (%load (third inputs) "memset2-val")))
      (%store val dest))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:box-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((intrinsic
          (ecase (cleavir-ir:element-type instruction)
            ((base-char) "to_object_claspChar")
            ((character) "to_object_claspCharacter")
            ((ext:byte8) "to_object_uint8")
            ((ext:integer8) "to_object_int8")
            ((ext:byte16) "to_object_uint16")
            ((ext:integer16) "to_object_int16")
            ((ext:byte32) "to_object_uint32")
            ((ext:integer32) "to_object_int32")
            ((fixnum) "to_object_fixnum")
            ((ext:byte64) "to_object_uint64")
            ((ext:integer64) "to_object_int64")
            ((single-float) "to_object_float")
            ((double-float) "to_object_double"))))
    (%store
     (%intrinsic-call intrinsic (list (%load (first inputs))))
     (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unbox-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((intrinsic
          (ecase (cleavir-ir:element-type instruction)
            ((base-char) "from_object_claspChar")
            ((character) "from_object_claspCharacter")
            ((ext:byte8) "from_object_uint8")
            ((ext:integer8) "from_object_int8")
            ((ext:byte16) "from_object_uint16")
            ((ext:integer16) "from_object_int16")
            ((ext:byte32) "from_object_uint32")
            ((ext:integer32) "from_object_int32")
            ((fixnum) "from_object_fixnum")
            ((ext:byte64) "from_object_uint64")
            ((ext:integer64) "from_object_int64")
            ((single-float) "from_object_float")
            ((double-float) "from_object_double"))))
    (%store
     (%intrinsic-call intrinsic (list (%load (first inputs))))
     (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-values-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:dynamic-allocation-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info))
  (cmp:irc-unreachable))

#+(or)
(progn
  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:tailcall-instruction) return-value inputs outputs abi function-info)
    (declare (ignore outputs))
    `(return (funcall ,(first inputs) ,@(rest inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:car-instruction) return-value inputs outputs abi function-info)
    `(setq ,(first outputs)
	   (car ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:cdr-instruction) return-value inputs outputs abi function-info)
    `(setq ,(first outputs)
	   (cdr ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplaca-instruction) return-value inputs outputs abi function-info)
    (declare (ignore outputs))
    `(rplaca ,(first inputs) ,(second inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplacd-instruction) return-value inputs outputs abi function-info)
    (declare (ignore outputs))
    `(rplacd ,(first inputs) ,(second inputs))))

;;; Floating point arithmetic


(defmacro define-fp-binop (instruction-class-name op)
  `(defmethod translate-simple-instruction
       ((instruction ,instruction-class-name) return-value inputs outputs abi function-info)
     (declare (ignore return-value abi function-info))
     (%store (,op (%load (first inputs)) (%load (second inputs)))
             (first outputs))))

;;; As it happens, we do the same IR generation for singles and doubles.
;;; This is because the LLVM value descriptors have associated types,
;;; so fadd of doubles is different from fadd of singles.
;;; The generated boxes and unboxes are where actual types are more critical.

(define-fp-binop cleavir-ir:float-add-instruction %fadd)
(define-fp-binop cleavir-ir:float-sub-instruction %fsub)
(define-fp-binop cleavir-ir:float-mul-instruction %fmul)
(define-fp-binop cleavir-ir:float-div-instruction %fdiv)


;;; Arithmetic conversion

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:coerce-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((input (%load (first inputs))) (output (first outputs)))
    (%store
     (ecase (cleavir-ir:from-type instruction)
       ((single-float)
        (ecase (cleavir-ir:to-type instruction)
          ((double-float)
           (%fpext input cmp::%double%)))))
     output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) return-value inputs outputs successors abi function-info)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-low-level-trace :flow)
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:consp-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+tag-mask+) "tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value inputs outputs successors abi function-info)
  (let* ((value     (%load (first inputs)))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                          (%uintptr_t cmp:+immediate-mask+) "character-tag-only"))
         (cmp (%icmp-eq tag (%uintptr_t cmp:+character-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value inputs outputs successors abi function-info)
  (let* ((value     (%load (first inputs)))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                    (%uintptr_t cmp:+immediate-mask+) "single-float-tag-only"))
         (cmp       (%icmp-eq tag (%uintptr_t cmp:+single-float-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:headerq-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore return-value outputs abi function-info))
  (cmp::compile-header-check
   (cc-mir:header-value-min-max instruction)
   (%load (first inputs)) (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  (evaluate-cleanup-code function-info)
  (%ret (%load return-value)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-no-return-instruction) return-value inputs outputs successors (abi abi-x86-64) function-info)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  ;; If there is cleanup to be done then the invoke will have a landing pad
  ;;    and that will do the cleanup
  (closure-call-or-invoke (first inputs) return-value (cdr inputs) abi)
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore return-value inputs output successors abi function-info))
  (cmp:irc-unreachable))


(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:throw-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  ;; Do the cleanup explicitly
  (evaluate-cleanup-code function-info)
  (%intrinsic-call "cc_throw" (list (%load (first inputs)) (%load (second inputs))))
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:landing-pad-return-instruction) return-value inputs outputs successors abi function-info)
  (cmp:irc-low-level-trace :cclasp-eh)
  ;; FIXME: Remove the landing-pad-return-instruction
  ;; I don't call cc_popLandingPadFrame explicitly anymore
  ;;(%intrinsic-call "cc_popLandingPadFrame" (list (cmp:irc-load (car (last inputs)))))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum comparison instructions

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:%t*%) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:%t*%) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-slt x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-sle x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-eq x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Float comparison instructions

;;; Thanks to LLVM semantics, we translate these identically for all
;;; float types. Types are more important for un/boxing.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-less-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-olt x y)))
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-not-greater-instruction)
     return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-ole x y)))
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-equal-instruction)
     return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-oeq x y)))
    (%cond-br cmp (first successors) (second successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.
;;;
;;; This will compile a top-level form (doing all tlf processing)
;;; into the current *module*

;; All enclosed functions need to be finalized
(defvar *functions-to-finalize*)

(defparameter *enable-type-inference* t)

(defun my-hir-transformations (init-instr system env)
  ;; FIXME: Per Cleavir rules, we shouldn't need the environment at this point.
  ;; We do anyway because of the possibility that a load-time-value input is introduced
  ;; in HIR that needs the environment to compile, e.g. the form is a constant variable,
  ;; or an object needing a make-load-form.
  ;; That shouldn't actually happen, but it's a little ambiguous in Cleavir right now.
  (quick-draw-hir init-instr "hir-before-transformations")
  ;; required by most of the below
  (cleavir-hir-transformations:process-captured-variables init-instr)
  (setf *ct-process-captured-variables* (compiler-timer-elapsed))
  (quick-draw-hir init-instr "hir-after-pcv") 
  (clasp-cleavir:optimize-stack-enclose init-instr) ; see FIXME at definition
  (setf *ct-optimize-stack-enclose* (compiler-timer-elapsed))
  (cleavir-kildall-type-inference:thes->typeqs init-instr clasp-cleavir:*clasp-env*)
  (quick-draw-hir init-instr "hir-after-thes-typeqs")
  (setf *ct-thes->typeqs* (compiler-timer-elapsed))

  ;;; See comment in policy.lisp. tl;dr these analyses are slow.
  #-disable-type-inference
  (let ((do-dx (policy-anywhere-p init-instr 'do-dx-analysis))
        (do-ty (policy-anywhere-p init-instr 'do-type-inference)))
    (when (or do-dx do-ty)
      (let ((liveness (cleavir-liveness:liveness init-instr)))
        (setf *ct-liveness* (compiler-timer-elapsed))
        ;; DX analysis
        (when do-dx
          (cleavir-escape:mark-dynamic-extent init-instr :liveness liveness)
          (setf *ct-mark-dynamic-extent* (compiler-timer-elapsed)))
        ;; Type inference
        (when do-ty
          (cleavir-kildall-type-inference:infer-types init-instr clasp-cleavir:*clasp-env*
                                                      :liveness liveness :prune t
                                                      :draw (quick-hir-pathname "hir-before-prune-ti"))
          (quick-draw-hir init-instr "hir-after-ti")
          (setf *ct-infer-types* (compiler-timer-elapsed))))))

  ;; delete the-instruction and the-values-instruction
  (cleavir-kildall-type-inference:delete-the init-instr)
  (setf *ct-delete-the* (compiler-timer-elapsed))
  (quick-draw-hir init-instr "hir-after-delete-the")
  (cc-hir-to-mir:reduce-typeqs init-instr)
  (setf *ct-eliminate-typeq* (compiler-timer-elapsed))
  (quick-draw-hir init-instr "hir-after-eliminate-typeq")
  (clasp-cleavir::eliminate-load-time-value-inputs init-instr system env)
  (quick-draw-hir init-instr "hir-after-eliminate-load-time-value-inputs")
  (setf *ct-eliminate-load-time-value-inputs* (compiler-timer-elapsed)))

(defun compile-form-to-mir (FORM &optional (ENV *clasp-env*))
  "Compile a form down to MIR and return it.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (setf *ct-start* (compiler-timer-elapsed))
  (let* ((clasp-system *clasp-system*)
	 (ast (let ((ast (cleavir-generate-ast:generate-ast FORM ENV clasp-system)))
                (cc-dbg-when
                 *debug-log*
                 (let ((ast-pathname (make-pathname :name (format nil "ast~a" (incf *debug-log-index*))
                                                    :type "dot" :defaults (pathname *debug-log*))))
                   (cleavir-ast-graphviz:draw-ast ast ast-pathname)
                   (core:bformat *debug-io* "Just dumped the ast to %s\n" ast-pathname)
                   #+(or)(multiple-value-bind (instruction-ids datum-ids)
                             (cleavir-ir-gml:draw-flowchart initial-instruction (namestring ast-pathname)))
                   (format *debug-log* "Wrote ast to: ~a~%" (namestring ast-pathname))))
                (setf *ct-generate-ast* (compiler-timer-elapsed))
                ast))
	 (hoisted-ast (prog1 (clasp-cleavir-ast:hoist-load-time-value ast env)
                        (setf *ct-hoist-ast* (compiler-timer-elapsed))))
	 (hir (prog1 (cleavir-ast-to-hir:compile-toplevel hoisted-ast)
                (setf *ct-generate-hir* (compiler-timer-elapsed)))))
    (let ((map-enter-to-function-info (prog1 (clasp-cleavir:convert-funcalls hir)
                                        (setf *ct-convert-funcalls* (compiler-timer-elapsed)))))
      (my-hir-transformations hir clasp-system env)
      (quick-draw-hir hir "hir-pre-mir")
      (cleavir-ir:hir-to-mir hir clasp-system nil nil)
      #+stealth-gids(cc-mir:assign-mir-instruction-datum-ids hir)
      (clasp-cleavir:finalize-unwind-and-landing-pad-instructions hir map-enter-to-function-info)
      (setf *ct-finalize-unwind-and-landing-pad-instructions* (compiler-timer-elapsed))
      (quick-draw-hir hir "mir")
      (values hir map-enter-to-function-info))))
                                                

(defun compile-lambda-form-to-llvm-function (lambda-form)
  "Compile a lambda-form into an llvm-function and return
that llvm function. This works like compile-lambda-function in bclasp."
  (multiple-value-bind (mir map-enter-to-function-info)
      (compile-form-to-mir lambda-form)
    (let ((function-enter-instruction
           (block first-function
             (cleavir-ir:map-instructions-with-owner
              (lambda (instruction owner)
                (when (and (eq mir owner)
                           (typep instruction 'cleavir-ir:enclose-instruction))
                  (return-from first-function (cleavir-ir:code instruction))))
              mir))))
      (or function-enter-instruction (error "Could not find enter-instruction for enclosed function in ~a" lambda-form))
      (translate function-enter-instruction map-enter-to-function-info))))

#++
(defun find-first-enclosed-enter-instruction (mir)
  (block first-function
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (and (eq mir owner)
                  (typep i 'cleavir-ir:enclose-instruction))
         (return-from first-function (cleavir-ir:code instruction)))))))
   
(defparameter *debug-final-gml* nil)
(defparameter *debug-final-next-id* 0)


(defun cclasp-compile-to-module-with-run-time-table (form env pathname &key (linkage 'llvm-sys:internal-linkage))
  (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
    (let (function lambda-name)
      (multiple-value-bind (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
          (literal:with-rtv
              (multiple-value-bind (mir map-enter-to-landing-pad)
                  (compile-form-to-mir form env)
                (multiple-value-setq (function lambda-name)
                  (translate mir map-enter-to-landing-pad *abi-x86-64* :linkage linkage))))
        (values function lambda-name ordered-raw-constants-list constants-table startup-fn shutdown-fn)))))


;; Set this to T to watch cclasp-compile* run
(defvar *cleavir-compile-verbose* nil)
(export '*cleavir-compile-verbose*)
(in-package :clasp-cleavir)
(defun cclasp-compile* (name form env pathname &key (linkage 'llvm-sys:internal-linkage))
  (when *cleavir-compile-verbose*
    (format *trace-output* "Cleavir compiling t1expr: ~s~%" form)
    (format *trace-output* "          in environment: ~s~%" env ))
  (let ((cleavir-generate-ast:*compiler* 'cl:compile))
    (handler-bind
        ((cleavir-env:no-variable-info
          (lambda (condition)
;;;	  (declare (ignore condition))
            #+verbose-compiler(warn "Condition: ~a" condition)
            (invoke-restart 'cleavir-generate-ast::consider-special)))
         (cleavir-env:no-function-info
          (lambda (condition)
;;;	  (declare (ignore condition))
            #+verbose-compiler(warn "Condition: ~a" condition)
            (invoke-restart 'cleavir-generate-ast::consider-global))))
      (multiple-value-bind (fn lambda-name ordered-raw-constants-list constants-table startup-fn shutdown-fn)
          (cclasp-compile-to-module-with-run-time-table form env pathname :linkage linkage)
        (or fn (error "There was no function returned by compile-lambda-function"))
        (cmp:cmp-log "fn --> %s\n" fn)
        ;;(cmp:cmp-log-dump-module cmp:*the-module*)
        (cmp:quick-module-dump cmp:*the-module* "cclasp-compile-module-pre-optimize")
        (let* ((setup-function (cmp:jit-add-module-return-function cmp:*the-module* fn startup-fn shutdown-fn ordered-raw-constants-list)))
          (let ((enclosed-function (funcall setup-function)))
            (values enclosed-function)))))))

(defun compile-form (form &optional (env *clasp-env*))
  (handler-bind
      ((cleavir-env:no-variable-info
        (lambda (condition)
;;;	  (declare (ignore condition))
          #+verbose-compiler(warn "Condition: ~a" condition)
          (cmp:compiler-warning-undefined-global-variable (cleavir-environment:name condition))
          (invoke-restart 'cleavir-generate-ast::consider-special)))
       (cleavir-env:no-function-info
        (lambda (condition)
;;;	  (declare (ignore condition))
          #+verbose-compiler(warn "Condition: ~a" condition)
          (cmp:register-global-function-ref (cleavir-environment:name condition))
          (invoke-restart 'cleavir-generate-ast::consider-global))))
    (multiple-value-bind (mir map-enter-to-landing-pad)
        (compile-form-to-mir form env)
      (multiple-value-prog1 (translate mir map-enter-to-landing-pad *abi-x86-64*)
        (setf *ct-translate* (compiler-timer-elapsed))))))

(defun cleavir-compile-file-form (form &optional (env *clasp-env*))
  (let ((cleavir-generate-ast:*compiler* 'cl:compile-file)
        (core:*use-cleavir-compiler* t))
    (literal:with-top-level-form
     (cmp:dbg-set-current-source-pos form)
      (if cmp::*debug-compile-file*
          (compiler-time (compile-form form env))
          (compile-form form env)))))

(defun cclasp-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym)))
    (loop
      (let ((eof (peek-char t source-sin nil eof-value)))
        (unless (eq eof eof-value)
          (let ((pos (core:input-stream-source-pos-info source-sin)))
            (setq *current-form-lineno* (core:source-file-pos-lineno pos)))))
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let ((form (read source-sin nil eof-value)))
        (when cmp:*debug-compile-file* (bformat t "compile-file: cf%d -> %s\n" (incf cmp:*debug-compile-file-counter*) form))
        (if (eq form eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form form))
              (cleavir-compile-file-form form environment)))))))

(defun cclasp-compile-in-env (name form &optional env)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (if cmp::*debug-compile-file*
        (progn
          (format t "cclasp-compile-in-env -> ~s~%" form)
          (compiler-time (cmp:compile-in-env name form env #'cclasp-compile* 'llvm-sys:external-linkage)))
        (cmp:compile-in-env name form env #'cclasp-compile* 'llvm-sys:external-linkage))))
        
(defun cleavir-compile (name form &key (debug *debug-cleavir*))
  (let ((cmp:*compile-debug-dump-module* debug)
	(*debug-cleavir* debug))
    (cclasp-compile-in-env name form nil)))

(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((*debug-log-index* 0)
	(cleavir-generate-ast:*compiler* 'cl:compile-file)
        (cmp:*cleavir-compile-file-hook* 'cleavir-compile-file-form)
        (core:*use-cleavir-compiler* t))
    (apply #'cmp::compile-file given-input-pathname args)))

(defmacro with-debug-compile-file ((log-file &key debug-log-on) &rest body)
  `(with-open-file (clasp-cleavir::*debug-log* ,log-file :direction :output)
     (let ((clasp-cleavir::*debug-log-on* ,debug-log-on))
       ,@body)))
(export 'with-debug-compile-file)


(defmacro open-debug-log (log-file &key (debug-compile-file t) debug-compile)
  `(eval-when (:compile-toplevel :execute)
     (core:bformat t "Turning on compiler debug logging\n")
     (setq *debug-log* (open (ensure-directories-exist ,log-file) :direction :output))
     (setq *debug-log-on* t)
     (setq cmp::*debug-compiler* t)
     (setq cmp::*debug-compile-file* ,debug-compile-file)
     (setq cmp::*compile-file-debug-dump-module* ,debug-compile-file)
     (setq cmp::*debug-compile* ,debug-compile)
     (setq cmp::*compile-debug-dump-module* ,debug-compile)
     ))

(defmacro close-debug-log ()
  `(eval-when (:compile-toplevel :execute)
     (core:bformat t "Turning off compiler debug logging\n")
     (setq *debug-log-on* nil)
     (close *debug-log*)
     (setq cmp::*debug-compiler* nil)
     (setq *debug-log* nil)
     (setq cmp::*compile-file-debug-dump-module* nil)
     (setq cmp::*debug-compile-file* nil)
     (setq cmp::*debug-compile* nil)
     (setq cmp::*compile-debug-dump-module* nil)
     ))

(export '(open-debug-log close-debug-log))
  
