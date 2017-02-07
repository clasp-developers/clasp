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
(defgeneric translate-simple-instruction (instruction return-value inputs outputs abi))

(defgeneric translate-branch-instruction (instruction return-value inputs outputs successors abi))



(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defvar *entry-irbuilder*)

(setf (fdefinition 'cleavir-primop:call-with-variable-bound) 
            (fdefinition 'core:call-with-variable-bound))

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      (let* ((value (cleavir-ir:value datum)))
        (%literal-ref value t))
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (cond
	    ;; do nothing for values-location - there is only one
	    ((typep datum 'cleavir-ir:values-location)
	     #+(or)(setf var (alloca-mv-struct (string (gensym "v")))) )
	    ((typep datum 'cleavir-ir:immediate-input)
	     (setf var (%i64 (cleavir-ir:value datum))))
	    ((typep datum 'cleavir-ir:dynamic-lexical-location)
	     (setf var (alloca-t* (string (cleavir-ir:name datum)))))
	    ((typep datum 'cleavir-ir:static-lexical-location)
	     (setf var (alloca-t* (string (cleavir-ir:name datum)))))
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

(defun layout-basic-block (basic-block return-value abi)
  (destructuring-bind (first last owner) basic-block
    (declare (ignore owner))
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
       collect (translate-simple-instruction
		instruction return-value input-vars output-vars abi))
    (let* ((inputs (cleavir-ir:inputs last))
	   (input-vars (mapcar #'translate-datum inputs))
	   (outputs (cleavir-ir:outputs last))
	   (output-vars (mapcar #'translate-datum outputs))
	   (successors (cleavir-ir:successors last))
	   (successor-tags (loop for successor in successors
			      collect (gethash successor *tags*))))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))
      (if (= (length successors) 1)
	  (list (translate-simple-instruction
		 last return-value input-vars output-vars abi)
		(if (typep (second basic-block) 'cleavir-ir:unwind-instruction)
		    (cmp:irc-unreachable)
                    (progn
                      (cmp:irc-low-level-trace :flow)
                      (cmp:irc-br (gethash (first successors) *tags*)))))
	  (list (translate-branch-instruction
		 last return-value input-vars output-vars successor-tags abi)))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "- - - -  END layout-basic-block  owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner) basic-block))
      )))

(defun get-or-create-lambda-name (instr)
  (if (typep instr 'clasp-cleavir-hir:named-enter-instruction)
      (clasp-cleavir-hir:lambda-name instr)
      'TOP-LEVEL))

(defun layout-procedure (initial-instruction abi)
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((clasp-cleavir-ast-to-hir:*landing-pad* nil)
         return-value
	 (basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 ;; Hypothesis: This finds the first basic block
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 ;; This gathers the rest of the basic blocks
	 (rest (remove first basic-blocks :test #'eq))
	 (lambda-name (get-or-create-lambda-name initial-instruction)))
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((main-fn-name lambda-name) ;;(format nil "cl->~a" lambda-name))
	   (cmp:*current-function-name* (cmp:jit-function-name main-fn-name))
	   (cmp:*gv-current-function-name* (cmp:jit-make-global-string cmp:*current-function-name* "fn-name"))
	   (fn (cmp:irc-function-create
		cmp:+fn-prototype+
		'llvm-sys:internal-linkage
		(cmp:jit-function-name main-fn-name) ;cmp:*current-function-name*)
		cmp:*the-module*))
	   (cmp:*current-function* fn)
	   (entry-block (cmp:irc-basic-block-create "entry" fn))
	   (*current-function-entry-basic-block* entry-block)
	   (*function-current-multiple-value-array-address* nil)
	   (*entry-irbuilder* (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-block (cmp:irc-basic-block-create "body")))
      (llvm-sys:set-personality-fn fn (cmp:irc-personality-function))
      (llvm-sys:add-fn-attr fn 'llvm-sys:attribute-uwtable)
      (push fn cmp:*all-functions-for-one-compile*)
      (cc-dbg-when *debug-log*
		   (format *debug-log* "------------ begin layout-procedure ~a~%" (llvm-sys:get-name fn))
		   (format *debug-log* "   basic-blocks for procedure~%")
		   (dolist (bb basic-blocks)
		     (destructuring-bind (first last owner) bb
		       (format *debug-log* "basic-block owner: ~a:~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner))
		       (loop for instruction = first
			  then (first (cleavir-ir:successors instruction))
			  until (eq instruction last)
			  do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
		       (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))))
      (let ((args (llvm-sys:get-argument-list fn)))
	(mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
		(llvm-sys:get-argument-list fn) cmp:+fn-prototype-argument-names+)
	;; set the first argument attribute to be sret
	#+(or)(if args
                  (let ((attribute-set (llvm-sys:attribute-set-get cmp:*llvm-context* 1 (list 'llvm-sys:attribute-struct-ret))))
                    (llvm-sys:add-attr (first args) attribute-set))))
      ;; create a basic-block for every remaining tag
      (loop for block in rest
	 for instruction = (first block)
	 do (progn
	      #+(or)(format t "creating basic block for rest instruction: ~a~%" instruction)
	      (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag"))))
      (llvm-sys:set-insert-point-basic-block *entry-irbuilder* entry-block)
      ;; hypothesis: bind variables for every var owned by this
      ;; initial instruction and that are not outputs of the initial
      ;; instruction (passed arguments)  i think i should use passed arguments
      ;; to create allocas for them.
      (cmp:with-irbuilder (*entry-irbuilder*)
        ;; make sure no landing pads are used from outer functions
        (cmp:with-landing-pad nil 
          (setq return-value (alloca-return_type))
          ;; in case of a non-local exit, zero out the number of returned
          ;; values
          (with-return-values (return-values return-value abi)
            (%store (%size_t 0) (number-of-return-values return-values)))
          (cmp:with-dbg-function ("repl-fix"
                                  :linkage-name (llvm-sys:get-name fn)
                                  :function fn
                                  :function-type cmp:+fn-prototype+
                                  :form *form*)
            (cmp:with-dbg-lexical-block (*form*)
              (cmp:dbg-set-current-source-pos-for-irbuilder *form* *entry-irbuilder*)
              (cmp:dbg-set-current-source-pos-for-irbuilder *form* body-irbuilder)
              (cmp:irc-low-level-trace :arguments)
              #+use-ownerships(loop for var being each hash-key of *ownerships*
                                 using (hash-value owner)
                                 when (and (typep var '(or
                                                        cleavir-ir:lexical-location
                                                        cleavir-ir:values-location))
                                           (eq owner initial-instruction)
                                           #+(or)(not (member var (cleavir-ir:outputs
                                                                   initial-instruction))))
                                 collect (translate-datum var))))))
      (cmp:with-dbg-function ("unused-with-dbg-function-name"
                              :linkage-name (llvm-sys:get-name fn)
                              :function fn
                              :function-type cmp:+fn-prototype+
                              :form *form*)
        (cmp:with-dbg-lexical-block (*form*)
          ;;          (cmp:dbg-set-current-source-pos *form*)
          (llvm-sys:set-insert-point-basic-block body-irbuilder body-block)
          (cmp:with-irbuilder (body-irbuilder)
            (cmp:irc-begin-block body-block)
            (layout-basic-block first return-value abi)
            (loop for block in rest
               for instruction = (first block)
               do (progn
                    #+(or)(format t "laying out basic block: ~a~%" block)
                    #+(or)(format t "inserting basic block for instruction: ~a~%" instruction)
                    (cmp:irc-begin-block (gethash instruction *tags*))
                    (layout-basic-block block return-value abi))))
          ;; finish up by jumping from the entry block to the body block
          (cmp:with-irbuilder (*entry-irbuilder*)
            (cmp:irc-low-level-trace :flow)
            (cmp:irc-br body-block))
          (cc-dbg-when *debug-log*
                       (format *debug-log* "----------end layout-procedure ~a~%" (llvm-sys:get-name fn)))
          (values fn lambda-name))))))

(defvar *forms*)
(defvar *map-enter-to-landing-pad* nil)

(defun translate (initial-instruction map-enter-to-landing-pad &optional (abi *abi-x86-64*))
  (let* (#+use-ownerships(ownerships
			  (cleavir-hir-transformations:compute-ownerships initial-instruction)))
    (let* (#+use-ownerships
           (*ownerships* ownerships)
           (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
           (*tags* (make-hash-table :test #'eq))
           (*vars* (make-hash-table :test #'eq))
           (*map-enter-to-landing-pad* map-enter-to-landing-pad)
           )
      #+use-ownerships(setf *debug-ownerships* *ownerships*)
      (setf *debug-basic-blocks* *basic-blocks*
	    *debug-tags* *tags*
	    *debug-vars* *vars*)
      (cc-dbg-when 
       *debug-log*
       (let ((mir-pathname (make-pathname :name (format nil "mir~a" *debug-log-index*) :type "gml" :defaults (pathname *debug-log*))))
	 (multiple-value-bind (instruction-ids datum-ids)
	     (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname)))
	 (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname)))
       (let ((mir-pathname (make-pathname :name (format nil "mir~a" *debug-log-index*) :type "dot" :defaults (pathname *debug-log*))))
	 (cleavir-ir-graphviz:draw-flowchart initial-instruction (namestring mir-pathname))
	 (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname))))
      (multiple-value-bind (function lambda-name)
          (layout-procedure initial-instruction abi)
        (values function lambda-name)
        #+(or)(let ((forms (cleavir-ir:forms initial-instruction)))
                ;; Generate the run-all code here
                (cmp:irc-verify-module-safe cmp:*the-module*)
;;; figure out what to return
                (values function lambda-name forms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instr cleavir-ir:enter-instruction) return-value inputs outputs (abi abi-x86-64))
  (let ((landing-pad (gethash instr *map-enter-to-landing-pad*)))
    (when landing-pad
      (let ((exn.slot (alloca-i8* "exn.slot"))
	    (ehselector.slot (alloca-i32 "ehselector.slot")))
	(setf (basic-block landing-pad)
	      (clasp-cleavir:create-landing-pad exn.slot
                                                ehselector.slot
                                                instr
                                                return-value
                                                landing-pad
                                                *tags*
                                                abi)))
      (cmp:with-irbuilder (*entry-irbuilder*)
        (cmp:irc-low-level-trace :cclasp-eh)
	(let ((result (cmp:irc-create-call "cc_pushLandingPadFrame" nil)))
	  (cmp:irc-store result (translate-datum (clasp-cleavir-hir:frame-holder instr)))))))
  (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
         (closed-env-dest (first outputs)))
    (multiple-value-bind (closed-env-arg calling-convention)
        (cmp:parse-function-arguments fn-args)
      (llvm-sys:create-store cmp:*irbuilder* closed-env-arg closed-env-dest nil)
      #+(or)(progn
              (format t "translate-simple-instruction for enter-instruction~%")
              (format t " fn-args: ~a~%" fn-args))
      (let* ((lambda-list (cleavir-ir:lambda-list instr))
             (static-environment-output (first (cleavir-ir:outputs instr)))
             (args (cdr (cleavir-ir:outputs instr))))
        #+(or)(progn
                (format t "    outputs: ~s~%" args)
                (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) args))
                (format t "lambda-list: ~a~%" lambda-list))
        (compile-lambda-list-code lambda-list args calling-convention)))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) return-value inputs outputs abi)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) return-value inputs outputs abi)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (cmp:irc-low-level-trace :flow)
  (let ((input (first inputs))
        (output (first outputs)))
    (cond
      ((typep input 'llvm-sys:constant-int)
       (let ((val (%inttoptr input cmp:+t*+)))
         (%store val output)))
      (t
       (let ((load (%load input)))
         (%store load output))))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) return-value inputs outputs abi)
  (let ((idx (first inputs)))
    (cmp:irc-low-level-trace :flow)
    (let* ((label (format nil "~s" (clasp-cleavir-hir:precalc-value-instruction-original-object instruction)))
           (value (cmp::irc-smart-ptr-extract (cmp:irc-load (cmp::irc-gep (cmp:ltv-global) (list (%size_t 0) idx) label)))))
      (llvm-sys:create-store cmp:*irbuilder* value (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value inputs outputs (abi abi-x86-64))
  ;; Write the first return value into the result
  (with-return-values (return-values return-value abi)
    (%store (%size_t (length inputs)) (number-of-return-values return-values))
    (dotimes (i (length inputs))
      (%store (cmp:irc-load (elt inputs i)) (return-value-elt return-values i)))
    #+(or)(cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-values) (first outputs))
    ))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs (abi abi-x86-64))
  ;; Create a basic block for each output
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    #+(or)(cmp:irc-intrinsic "cc_loadThreadLocalMultipleValues" (sret-arg return-vals) (first inputs))
    (let* ((blocks (let (b) (dotimes (i (1+ (length outputs))) (push (cmp:irc-basic-block-create (format nil "mvn~a-" i)) b)) (nreverse b)))
	   (final-block (cmp:irc-basic-block-create "mvn-final"))
	   (switch (cmp:irc-switch (cmp:irc-load (number-of-return-values return-vals)) (car (last blocks)) (length blocks))))
      (dotimes (n (length blocks))
	(let ((block (elt blocks n)))
	  (cmp:irc-begin-block block)
	  (llvm-sys:add-case switch (%size_t n) block)
	  (dotimes (i (length outputs))
	    (if (< i n)
		(%store (cmp:irc-load (return-value-elt return-vals i)) (elt outputs i))
		(%store (%nil) (elt outputs i))))
          (cmp:irc-low-level-trace :flow)
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:multiple-value-foreign-call-instruction) return-value inputs outputs (abi abi-x86-64))
  (cmp:irc-low-level-trace :flow)
  (let ((call (clasp-cleavir:unsafe-multiple-value-foreign-call :call (clasp-cleavir-hir:function-name instruction) return-value inputs abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction multiple-value-foreign-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-instruction) return-value inputs outputs (abi abi-x86-64))
  (cmp:irc-low-level-trace :flow)
  (let ((call (clasp-cleavir:unsafe-foreign-call :call (clasp-cleavir-hir:function-name instruction) (car outputs) inputs abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction foreign-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-pointer-instruction) return-value inputs outputs (abi abi-x86-64))
  (cmp:irc-low-level-trace :flow)
  (let ((call (clasp-cleavir:unsafe-foreign-call-pointer :call (%load (car inputs)) (car outputs) (cdr inputs) abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction foreign-call-pointer-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs (abi abi-x86-64))
  #+(or)(progn
          (format t "--------------- translate-simple-instruction funcall-instruction~%")
          (format t "       funcall:~a~%" (clasp-cleavir:instruction-gid instruction))
          (format t "         return value: ~a~%" return-value)
          (format t "           arg inputs: ~a~%" inputs)
          (format t "          arg outputs: ~a~%" outputs)
          (format t "      instance inputs: ~a~%" (cleavir-ir:inputs instruction))
          (format t "     instance outputs: ~a~%" (cleavir-ir:outputs instruction))
          (format t "  instance successors: ~a~%" (cleavir-ir:successors instruction))
          )
  (cmp:irc-low-level-trace :flow)
  (let ((call (closure-call :call "cc_call" (first inputs) return-value (cdr inputs) abi)))
    #+(or)(progn
            (format t "generated call --> ~a~%" call))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction funcall-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))





(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-instruction) return-value inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let* ((lpad (clasp-cleavir::landing-pad instruction)))
    (or (basic-block lpad) (error "There must be a basic-block defined for the landing-pad"))
    (cmp:irc-low-level-trace :flow)
    (let ((call (closure-call :invoke "cc_call" (first inputs) return-value (cdr inputs) abi :landing-pad (basic-block lpad))))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "    translate-simple-instruction invoke-instruction: ~a~%" (cc-mir:describe-mir instruction))
		   (format *debug-log* "     instruction --> ~a~%" call)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value inputs outputs abi)
  (%nil))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:indexed-unwind-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    ;; Save whatever is in return-vals in the multiple-value array
    (cmp:irc-create-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
    (cmp:irc-low-level-trace :cclasp-eh)
    (cmp:irc-create-call "cc_unwind" (list (cmp::irc-load (first inputs)) (%size_t (clasp-cleavir-hir:jump-id instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((result (cmp:irc-create-call "cc_makeCell" nil)))
    (%store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (cmp:irc-create-call "cc_writeCell" (list cell val))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) return-value inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
  (cmp:irc-low-level-trace :flow)
    (let ((result (cmp:irc-create-call "cc_readCell" (list cell))))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((env (cmp:irc-load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (cmp:irc-create-call "cc_fetch" (list env idx))))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value inputs outputs abi)
  ;; How do we figure out if we should use safe or unsafe version
  (cmp:irc-low-level-trace :flow)
  (let ((cell (cmp:irc-load (first inputs) "func-name")))
    ;;    (format t "translate-simple-instruction (first inputs) = ~a ~%" (first inputs))
    (let ((result (cmp:irc-create-call "cc_safe_fdefinition" (list cell) "func")))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value inputs outputs abi)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (cmp:irc-create-call "debugMessage" (list msg))))
	

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (cmp:irc-load (first inputs) "setf-func-name")))
    (let ((result (cmp:irc-create-call "cc_safe_setfdefinition" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (cmp:irc-load (first inputs) "sym-name")))
    (let ((result (cmp:irc-create-call "cc_safe_symbol_value" (list sym))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (cmp:irc-load (first inputs) "sym-name"))
	(val (cmp:irc-load (second inputs) "value")))
    (cmp:irc-create-call "cc_setSymbolValue" (list sym val))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) return-value inputs outputs abi)
  (declare (ignore inputs))
  (cmp:irc-low-level-trace :flow)
  #||
  (format t "translate-simple-instruction::enclose-instruction~%")
  (format t "         *current-source-pos-info*: ~a~%" core:*current-source-pos-info*)
||#
  (let* ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function lambda-name)
	(layout-procedure enter-instruction abi)
      (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
	     (ltv-lambda-name (%literal-value lambda-name (format nil "lambda-name->~a" lambda-name)))
	     (result (cmp:irc-create-call
                      "cc_enclose"
                      (list* ltv-lambda-name
                             enclosed-function
                             cmp:*gv-source-file-info-handle*
                             (cmp:irc-size_t-*current-source-pos-info*-filepos)
                             (cmp:irc-size_t-*current-source-pos-info*-lineno)
                             (cmp:irc-size_t-*current-source-pos-info*-column)
                             (%size_t (length inputs))
                             loaded-inputs)
                      (format nil "closure->~a" lambda-name))))
	(cc-dbg-when *debug-log*
		     (format *debug-log* "cc_enclose with ~a cells~%" (length inputs))
		     (format *debug-log* "    inputs: ~a~%" inputs))
	(%store result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction cc-mir:stack-enclose-instruction) return-value inputs outputs abi)
  (declare (ignore inputs))
  (cmp:irc-low-level-trace :flow)
  (let* ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function lambda-name)
        (layout-procedure enter-instruction abi)
      (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
             (stack-allocated-closure-space (alloca-i8 (core:closure-with-slots-size (length inputs)) "stack-allocated-closure"))
             (ptr-to-sacs
              (llvm-sys:create-bit-cast cmp:*irbuilder* stack-allocated-closure-space cmp:+i8*+ "closure-ptr"))
             (ltv-lambda-name (%literal-value lambda-name (format nil "lambda-name->~a" lambda-name)))
             (result
              (let ()
                (cmp:irc-create-call
                 "cc_stack_enclose"
                 (list* ptr-to-sacs
                        ltv-lambda-name
                        enclosed-function
                        cmp:*gv-source-file-info-handle*
                        (cmp:irc-size_t-*current-source-pos-info*-filepos)
                        (cmp:irc-size_t-*current-source-pos-info*-lineno)
                        (cmp:irc-size_t-*current-source-pos-info*-column)
                        (%size_t (length inputs))
                        loaded-inputs)
                 (format nil "closure->~a" lambda-name)))
               #+(or) (progn
                        (cmp:irc-intrinsic-args
                         "cc_enclose"
                         (list* ltv-lambda-name
                                enclosed-function
                                cmp:*gv-source-file-info-handle*
                                (cmp:irc-size_t-*current-source-pos-info*-filepos)
                                (cmp:irc-size_t-*current-source-pos-info*-lineno)
                                (cmp:irc-size_t-*current-source-pos-info*-column)
                                (%size_t (length inputs))
                                loaded-inputs)
                         (format nil "closure->~a" lambda-name)))
               ))
        (cc-dbg-when *debug-log*
                     (format *debug-log* "cc_enclose with ~a cells~%" (length inputs))
                     (format *debug-log* "    inputs: ~a~%" inputs))
        (%store result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    (cmp:irc-create-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
    (let ((call-result (cmp:irc-create-call "cc_call_multipleValueOneFormCall" 
				     (list (cmp:irc-load (first inputs))))))
      (%store call-result return-value)
      (cc-dbg-when 
       *debug-log*
       (format *debug-log* "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
	       (cc-mir:describe-mir instruction))
       (format *debug-log* "     instruction --> ~a~%" call-result))
      )))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-multiple-value-call-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let* ((lpad (clasp-cleavir::landing-pad instruction)))
    (with-return-values (return-vals return-value abi)
      (cmp:irc-create-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
      (let ((call-result (cmp:irc-create-invoke "cc_call_multipleValueOneFormCall" 
					 (list (cmp:irc-load (first inputs)))
					 (basic-block lpad))))
        (%store call-result return-value)
	(cc-dbg-when 
	 *debug-log*
	 (format *debug-log* "    translate-simple-instruction invoke-multiple-value-call-instruction: ~a~%" 
		 (cc-mir:describe-mir instruction))
	 (format *debug-log* "     instruction --> ~a~%" call-result))))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value inputs outputs abi)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:+uintptr_t+))
         (ui-offset (%bit-cast offset cmp:+uintptr_t+)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp::+t**+))
           (read-val (%load ptr)))
      (%store read-val (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value inputs outputs abi)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:+uintptr_t+))
         (ui-offset (%bit-cast offset cmp:+uintptr_t+)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (dest (%inttoptr uiptr cmp::+t**+ "memset2-dest"))
           (val (%load (third inputs) "memset2-val")))
      (%store val dest))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) return-value inputs outputs abi)
  (declare (ignore outputs))
  #+(or) (warn "What should I do with the-instruction")
  )
#||  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
(error 'type-error
       :expected-type ',(cleavir-ir:value-type instruction)
       :datum ,(first inputs))))
||#




#+(or)
(progn
  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:tailcall-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(return (funcall ,(first inputs) ,@(rest inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:the-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
       (error 'type-error
	      :expected-type ',(cleavir-ir:value-type instruction)
	      :datum ,(first inputs))))


  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:car-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (car ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:cdr-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (cdr ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplaca-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(rplaca ,(first inputs) ,(second inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplacd-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(rplacd ,(first inputs) ,(second inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:t-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:bit-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unsigned-byte-8-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:short-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:single-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:double-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:long-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:t-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:bit-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unsigned-byte-8-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:short-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:single-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:double-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:long-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs)
    (let ((temp (gensym)))
      `(let ((,temp ,(first inputs)))
	 (declare (ignorable ,temp))
	 ,@(loop for output in outputs
	      collect `(setf ,output (pop ,temp))))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unwind-instruction) return-value inputs outputs)
    (gensym))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) return-value inputs outputs successors abi)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-low-level-trace :flow)
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:consp-instruction) return-value inputs outputs successors abi)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::+i32+) (%i32 cmp:+tag-mask+) "tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value inputs outputs successors abi)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::+i32+) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value inputs outputs successors abi)
  (let* ((tag (%and (%ptrtoint value cmp:+uintptr_t+)
                    (%uintptr_t cmp:+immediate-mask+) "character-tag-only"))
         (cmp (%icmp-eq tag (%uintptr_t cmp:+character-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value inputs outputs successors abi)
  (let* ((tag (%and (%ptrtoint value cmp:+uintptr_t+)
                    (%uintptr_t cmp:+immediate-mask+) "single-float-tag-only"))
         (cmp (%icmp-eq tag (%uintptr_t cmp:+single-float-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) return-value inputs outputs successors abi)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  (llvm-sys:create-ret cmp:*irbuilder* (%load return-value)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-no-return-instruction) return-value inputs outputs successors (abi abi-x86-64))
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  (closure-call :call "cc_call" (first inputs) return-value (cdr inputs) abi)
  (cmp:irc-unreachable))


(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:throw-instruction) return-value inputs outputs successors abi)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  #+(or)(with-return-values (return-vals return-value abi)
          (cmp:irc-intrinsic "cc_saveMultipleValue0" return-value #|(sret-arg return-vals)|#)
          (cmp:irc-intrinsic "cc_throw" (cmp:irc-load (first inputs))))
  (cmp:irc-create-call "cc_throw" (list (%load (first inputs)) (%load (second inputs))))
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:landing-pad-return-instruction) return-value inputs outputs successors abi)
  (cmp:irc-low-level-trace :cclasp-eh)
  (cmp:irc-create-call "cc_popLandingPadFrame" (list (cmp:irc-load (car (last inputs)))))
  (call-next-method))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:+t*+) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:+t*+) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-slt x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-sle x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-eq x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

;;; When the FUNCALL-INSTRUCTION is the last instruction of a basic
;;; block, it is because there is a call to a function that will never
;;; return, such as ERROR, and the instruction then has no successors
;;; (which is why it is at the end of the basic block).
;;;
;;; We therefore must provide a method on TRANSLATE-BRANCH-INSTRUCTION
;;; (in addition to the method on TRANSLATE-SIMPLE-INSTRUCTION)
;;; specialized to FUNCALL-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs successors abi)
  (declare (ignore outputs successors))
  `(funcall ,(first inputs) ,@(rest inputs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.
;;;
;;; This will compile a top-level form (doing all tlf processing)
;;; into the current *module*

;; All enclosed functions need to be finalized
(defvar *functions-to-finalize*)


(defun describe-form (form)
  (when (consp form)
    (cond
      ((eq 'core:fset (car form))
       (let* ((name (cadr (cadr form)))
	      (is-macro (cadddr form))
	      (header (if is-macro
			  "defmacro"
			  "defun")))
	 (format t ";    ~a ~a~%" header name)))
      ((eq 'core:*make-constant (car form))
       (let* ((name (cadr (cadr form))))
	 (format t ";    *make-constant ~a~%" name)))
      ((eq 'cl:defun (car form))
       (format t ";    defun ~a~%" (cadr form)))
      ((eq 'cl:defgeneric (car form))
       (format t ";    defgeneric ~a~%" (cadr form)))
      ((eq 'cl:defstruct (car form))
       (format t ";    defstruct ~a~%" (cadr form)))
      ((eq 'cl:defclass (car form))
       (format t ";    defclass ~a~%" (cadr form)))
      ((eq 'cl:defmethod (car form))
       (format t ";    defmethod ~a~%" (cadr form)))
      ((eq 'cl:defmacro (car form))
       (format t ";    defun ~a~%" (cadr form)))
      ((eq 'cl:defconstant (car form))
       (format t ";    defconstant ~a~%" (cadr form)))
      ((eq 'cl:defvar (car form))
       (format t ";    defvar ~a~%" (cadr form)))
      ((eq 'cl:defparameter (car form))
       (format t ";    defparameter ~a~%" (cadr form)))
      #+(or)((eq 'cl:eval-when (car form))
             (format t ";    eval-when ~a ~a~%" (cadr form) (caddr form)))
      (t ()))))

;;;
;;; Turn type inference off for now - it slows the build down
;;;
(defparameter *enable-type-inference* nil)

(defun my-hir-transformations (init-instr implementation processor os)
  (cleavir-typed-transforms:thes->typeqs init-instr)
  (quick-draw-hir init-instr "hir-after-thes-typeqs")
-  (when *enable-type-inference*
    ;; Conditionally use type inference.
    (handler-case
        (let ((types (cleavir-type-inference:infer-types init-instr)))
          (when *debug-cleavir*
            (let ((cleavir-ir-graphviz::*types* types))
              (quick-draw-hir init-instr "hir-before-prune-ti")))
          ;; prune paths with redundant typeqs
          (cleavir-typed-transforms:prune-typeqs init-instr types)
          (when *debug-cleavir*
            (let ((cleavir-ir-graphviz::*types* types))
              (quick-draw-hir init-instr "hir-after-ti"))))
      (error (c)
        nil
        #+(or)(warn "Cannot infer types in ~s: ~s" init-instr c))))
  ;; delete the-instruction and the-values-instruction
  (cleavir-typed-transforms:delete-the init-instr)
  (quick-draw-hir init-instr "hir-after-delete-the")
  ;; convert (typeq x fixnum) -> (fixnump x)
  ;;         (typeq x cons) -> (consp x)
  ;;         (typeq x YYY) -> (typep x 'YYY)
  (cleavir-hir-transformations:eliminate-typeq init-instr)
  (quick-draw-hir init-instr "hir-after-eliminate-typeq")
  (clasp-cleavir::eliminate-load-time-value-inputs init-instr *clasp-system*)
  (quick-draw-hir init-instr "hir-after-eliminate-load-time-value-inputs")
  ;; The following breaks code when inlining takes place
  ;;  (cleavir-hir-transformations:eliminate-superfluous-temporaries init-instr)
  (cleavir-hir-transformations:process-captured-variables init-instr)
  (quick-draw-hir init-instr "hir-after-pcv"))

(defun compile-form-to-mir (FORM &optional (ENV *clasp-env*))
  "Compile a form down to MIR and return it.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (let* ((clasp-system *clasp-system*)
	 (ast (let ((a (cleavir-generate-ast:generate-ast FORM ENV clasp-system)))
		(when *debug-cleavir* (draw-ast a))
		a))
	 (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast)))
    (let ((map-enter-to-landing-pad (clasp-cleavir:convert-funcalls hir)))
      (my-hir-transformations hir clasp-system nil nil)
      (quick-draw-hir hir "hir-pre-mir")
      (cleavir-ir:hir-to-mir hir clasp-system nil nil)
      (clasp-cleavir:optimize-stack-enclose hir)
      (cc-mir:assign-mir-instruction-datum-ids hir)
      (clasp-cleavir:finalize-unwind-and-landing-pad-instructions hir map-enter-to-landing-pad)
      (quick-draw-hir hir "mir")
      (values hir map-enter-to-landing-pad))))

(defun compile-lambda-form-to-llvm-function (lambda-form)
  "Compile a lambda-form into an llvm-function and return
that llvm function. This works like compile-lambda-function in bclasp."
  (multiple-value-bind (mir map-enter-to-landing-pad)
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
      (translate function-enter-instruction map-enter-to-landing-pad))))

(defun find-first-enclosed-enter-instruction (mir)
  (block first-function
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (and (eq mir owner)
                  (typep i 'cleavir-ir:enclose-instruction))
         (return-from first-function (cleavir-ir:code instruction)))))))
   
(defparameter *debug-final-gml* nil)
(defparameter *debug-final-next-id* 0)


(defun cclasp-compile-to-module-with-run-time-table (form env pathname)
  (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
    (let (function lambda-name)
      (multiple-value-bind (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
          (literal:with-rtv
              (multiple-value-bind (mir map-enter-to-landing-pad)
                  (compile-form-to-mir form env)
                (multiple-value-setq (function lambda-name)
                  (translate mir map-enter-to-landing-pad *abi-x86-64*))))
        (values function lambda-name ordered-raw-constants-list constants-table startup-fn shutdown-fn)))))


;; Set this to T to watch cclasp-compile* run
(defvar *cleavir-compile-verbose* nil)
(export '*cleavir-compile-verbose*)
(in-package :clasp-cleavir)
(defun cclasp-compile* (name form env pathname)
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
          (cclasp-compile-to-module-with-run-time-table form env pathname)
        (or fn (error "There was no function returned by compile-lambda-function"))
        (cmp:cmp-log "fn --> %s\n" fn)
        (cmp:cmp-log-dump cmp:*the-module*)
        (cmp:link-intrinsics-module cmp:*the-module*)
        (cmp:quick-module-dump cmp:*the-module* "cclasp-compile-module-pre-optimize")
        (cmp:cmp-log "About to test and maybe set up the *run-time-execution-engine*\n")
        ;; SETUP THE *run-time-execution-engine* here for the first time
        ;; using the current module in *the-module*
        ;; At this point the *the-module* will become invalid because
        ;; the execution-engine will take ownership of it
        (if (not cmp:*run-time-execution-engine*)
            (setq cmp:*run-time-execution-engine* (cmp:create-run-time-execution-engine cmp:*the-module*))
            (llvm-sys:add-module cmp:*run-time-execution-engine* cmp:*the-module*))
        ;; At this point the Module in *the-module* is invalid because the
        ;; execution-engine owns it
        (cmp:cmp-log "The execution-engine now owns the module\n")
        (setq cmp:*the-module* nil)
        (cmp:cmp-log "About to finalize-engine with fn %s\n" fn)
        (let* ((fn-name (llvm-sys:get-name fn)) ;; this is the name of the function - a string
               (setup-function
                (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
                 cmp:*run-time-execution-engine*
                 lambda-name	        ; main fn name
                 fn			; llvm-fn
                 nil			; environment
                 "repl-fn.txt"
                 4321
                 5678
                 startup-fn
                 shutdown-fn
                 ordered-raw-constants-list)))
          (let ((enclosed-function (funcall setup-function)))
            ;;(format t "*all-functions-for-one-compile* -> ~s~%" cmp:*all-functions-for-one-compile*)
            ;;(cmp:set-associated-funcs enclosed-function cmp:*all-functions-for-one-compile*)
            (values enclosed-function)))))))

(defun compile-form (form)
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
    (when *compile-print* (describe-form form))
    (multiple-value-bind (mir map-enter-to-landing-pad)
        (compile-form-to-mir form *clasp-env*)
      (translate mir map-enter-to-landing-pad *abi-x86-64*))))

(defun cleavir-compile-file-form (form)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile-file)
        (core:*use-cleavir-compiler* t))
    (literal:with-top-level-form (compile-form form))))

(defun cclasp-compile-in-env (name form &optional env)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t)
	(cmp:*all-functions-for-one-compile* nil))
    (cmp:compile-in-env name form env #'cclasp-compile*)))
        
	
(defun cleavir-compile (name form &key (debug *debug-cleavir*))
  (let ((cmp:*compile-debug-dump-module* debug)
	(*debug-cleavir* debug))
    (cclasp-compile-in-env name form nil)))

(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((*debug-log-index* 0)
	(cleavir-generate-ast:*compiler* 'cl:compile-file)
        (cmp:*cleavir-compile-file-hook* 'cleavir-compile-file-form)
        (core:*use-cleavir-compiler* t))
    (apply #'cmp::compile-file* #'cleavir-compile-file-form given-input-pathname args)))

(defmacro with-debug-compile-file ((log-file &key debug-log-on) &rest body)
  `(with-open-file (clasp-cleavir::*debug-log* ,log-file :direction :output)
     (let ((clasp-cleavir::*debug-log-on* ,debug-log-on))
       ,@body)))
(export 'with-debug-compile-file)


(defmacro open-debug-log (log-file)
  `(eval-when (:compile-toplevel)
     (setq *debug-log* ,log-file :direction :output)
     (setq *debug-log-on* t)))

(defmacro close-debug-log ()
  `(eval-when (:compile-toplevel)
     (setq *debug-log-on* nil)
     (close *debug-log*)
     (setq *debug-log* nil)))
(export '(open-debug-log close-debug-log))
  
