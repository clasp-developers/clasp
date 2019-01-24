(cl:in-package #:clasp-cleavir)

(defvar *debug-cleavir* nil
  "controls if graphs are generated as forms are being compiled.")
(defvar *debug-cleavir-literals* nil
  "controls if cleavir debugging is carried out on literal compilation. 
when this is t a lot of graphs will be generated.")


;;;
;;; Set the source-position for an instruction
;;;

(defun get-or-register-file-metadata (fileid)
  (let ((file-metadata (gethash fileid *llvm-metadata*)))
    (unless file-metadata
      (let* ((sfi (core:source-file-info fileid))
             (pathname (core:source-file-info-pathname sfi))
             (metadata (cmp:make-file-metadata pathname)))
        (setf file-metadata metadata)
        (setf (gethash fileid *llvm-metadata*) file-metadata)))
    file-metadata))


(defun setup-function-scope-metadata (name function-info
                                      &key function llvm-function-name llvm-function llvm-function-type)
  (let* ((instruction (enter-instruction function-info))
         (source-pos-info (instruction-source-pos-info instruction))
         (fileid (core:source-pos-info-file-handle source-pos-info))
         (lineno (core:source-pos-info-lineno source-pos-info))
         (file-metadata (get-or-register-file-metadata fileid))
         (function-metadata (cmp:make-function-metadata :file-metadata file-metadata
                                                        :linkage-name llvm-function-name
                                                        :function-type llvm-function-type
                                                        :lineno lineno)))
    (llvm-sys:set-subprogram function function-metadata)
    (setf (metadata function-info) function-metadata)
    function-metadata))

(defun set-instruction-source-position (origin function-metadata)
  (when cmp:*dbg-generate-dwarf*
    (if origin
        (let ((source-pos-info (if (consp origin) (car origin) origin)))
          (llvm-sys:set-current-debug-location-to-line-column-scope
           cmp:*irbuilder*
           (core:source-pos-info-lineno source-pos-info)
           (1+ (core:source-pos-info-column source-pos-info))
           function-metadata))
        (llvm-sys:clear-current-debug-location cmp:*irbuilder*))))


(defvar *current-source-position* nil)
(defvar *current-function-metadata* nil)

(defun do-debug-info-source-position (origin function-metadata body-lambda)
  (unwind-protect
       (let ((*current-source-position* origin)
             (*current-function-metadata* function-metadata))
         (set-instruction-source-position origin function-metadata)
         (funcall body-lambda))
    (set-instruction-source-position *current-source-position* *current-function-metadata*)))

(defmacro with-debug-info-source-position (origin function-metadata &body body)
  `(do-debug-info-source-position ,origin ,function-metadata (lambda () ,@body)))

(defmacro with-debug-info-disabled (&body body)
  `(do-debug-info-source-position nil nil (lambda () ,@body)))

;;;
;;; the first argument to this function is an instruction that has a
;;; single successor.  whether a go is required at the end of this
;;; function is determined by the code layout algorithm.  
(defgeneric translate-simple-instruction
    (instruction return-value abi current-function-info))

(defmethod translate-simple-instruction :around
    (instruction return-value abi current-function-info)
  (with-debug-info-source-position (cleavir-ir:origin instruction) (metadata current-function-info)
    (call-next-method)))

(defgeneric translate-branch-instruction
    (instruction return-value successors abi current-function-info))

(defmethod translate-branch-instruction :around
    (instruction return-value successors abi current-function-info)
  (with-debug-info-source-position (cleavir-ir:origin instruction) (metadata current-function-info)
    (call-next-method)))


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

(defun make-datum-alloca (datum)
  (etypecase datum
    (cc-mir:typed-lexical-location
     (alloca (cc-mir:lexical-location-type datum) 1 (datum-name-as-string datum)))
    (cleavir-ir:lexical-location
     (alloca-t* (datum-name-as-string datum)))))

(defun datum-alloca (datum)
  (or (gethash datum *vars*)
      (setf (gethash datum *vars*) (make-datum-alloca datum))))

(defun ssablep (location)
  (let ((defs (cleavir-ir:defining-instructions location)))
    (= (length defs) 1)))

(defun in (datum &optional (label ""))
  (etypecase datum
    (cleavir-ir:immediate-input
     (cmp:irc-int-to-ptr (%i64 (cleavir-ir:value datum)) cmp:%t*%))
    (cleavir-ir:lexical-location
     (let ((existing (gethash datum *vars*)))
       (if (null existing)
           (error "BUG: Input ~a not previously defined" datum)
           (if (ssablep datum)
               existing
               (%load existing label)))))))

(defun out (value datum &optional (label ""))
  (if (ssablep datum)
      (progn
        (unless (null (gethash datum *vars*))
          (error "BUG: SSAable output ~a previously defined" datum))
        (setf (gethash datum *vars*) value))
      (%store value (datum-alloca datum) label)))

(defun layout-basic-block (basic-block return-value abi current-function-info)
  (with-accessors ((first cleavir-basic-blocks:first-instruction)
                   (last cleavir-basic-blocks:last-instruction)
                   (owner cleavir-basic-blocks:owner))
      basic-block
    (cc-dbg-when *debug-log*
                         (format *debug-log* "- - - -  begin layout-basic-block  owner: ~a~%" (cc-mir:describe-mir owner))
                         (loop for instruction = first
                            then (first (cleavir-ir:successors instruction))
                            until (eq instruction last)
                            do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction))))
    (loop for instruction = first
            then (first (cleavir-ir:successors instruction))
          if (eq instruction last)
            ;; finish off the block
            do (let* ((successors (cleavir-ir:successors instruction))
                      (successor-tags (loop for successor in successors
                                            collect (gethash successor *tags*))))
                 (cc-dbg-when *Debug-log* (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))
                 (cond ((= (length successors) 1)
                        ;; one successor: we have to do branching ourselves.
                        (translate-simple-instruction
                         instruction return-value abi current-function-info)
                        (cmp:irc-br (first successor-tags)))
                       (t ; 0 or 2 or more successors: it handles branching.
                        (translate-branch-instruction
                         instruction return-value successor-tags
                         abi current-function-info))))
               (loop-finish)
          else
            do (translate-simple-instruction
                instruction return-value abi current-function-info))
    (cc-dbg-when *debug-log*
                 #+stealth-gids(format *debug-log* "- - - -  END layout-basic-block  owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner) basic-block)
                 #-stealth-gids(format *debug-log* "- - - -  END layout-basic-block  owner: ~a   -->  ~a~%" (cleavir-ir-gml::label owner) basic-block))))

(defun get-or-create-lambda-name (instr)
  (if (typep instr 'clasp-cleavir-hir:named-enter-instruction)
      (clasp-cleavir-hir:lambda-name instr)
      'TOP-LEVEL))


(defun instruction-source-pos-info (instruction)
  "Return a source-pos-info object for the instruction"
  (let ((origin (cleavir-ir:origin instruction)))
    (cond (origin (if (consp origin) (car origin) origin))
          (core:*current-source-pos-info*)
          (t (core:make-source-pos-info "no-source-info-available" 0 0 0)))))

(defun layout-procedure* (the-function body-irbuilder
                                       body-block
                                       first-basic-block
                                       rest-basic-blocks
                                       function-info
                                       initial-instruction abi &key (linkage 'llvm-sys:internal-linkage))
  (with-debug-info-disabled
   (let ((return-value (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                                           (alloca-return_type))))
     (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                         ;; in case of a non-local exit, zero out the number of returned values
                         (with-return-values (return-values return-value abi)
                                             (%store (%size_t 0) (number-of-return-values return-values))))
     (cmp:with-irbuilder
      (body-irbuilder)
      (cmp:with-dbg-lexical-block
       (:lineno (core:source-pos-info-lineno (instruction-source-pos-info (cleavir-basic-blocks:first-instruction first-basic-block))))
       (cmp:irc-set-insert-point-basic-block body-block body-irbuilder)
       (with-catch-pad-prep
        (cmp:irc-begin-block body-block)
        (layout-basic-block first-basic-block return-value abi function-info)
        (loop for block in rest-basic-blocks
              for instruction = (cleavir-basic-blocks:first-instruction block)
              do (cmp:irc-begin-block (gethash instruction *tags*))
              (layout-basic-block block return-value abi function-info)))
       ;; finish up by jumping from the entry block to the body block
       (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                           (cmp:irc-br body-block))
       (cc-dbg-when *debug-log* (format *debug-log* "----------end layout-procedure ~a~%"
                                        (llvm-sys:get-name the-function)))
       the-function)))))

;;; Returns all basic blocks with the given owner.
;;; They are sorted so that a block never appears before one of its dominators, for SSA reasons.
;;; (I think both breadth and depth first orderings do this? Here it's depth for simplicity.)
(defun function-basic-blocks (enter)
  (let (ret)
    (labels ((aux (block)
               (push block ret)
               (loop for succ in (cleavir-basic-blocks:successors block)
                     unless (member succ ret)
                       do (aux succ))))
      (aux (find enter *basic-blocks* :key #'cleavir-basic-blocks:first-instruction))
      (nreverse ret))))

(defun log-layout-procedure (the-function basic-blocks)
  (format *debug-log* "------------ begin layout-procedure ~a~%" (llvm-sys:get-name the-function))
  (format *debug-log* "   basic-blocks for procedure~%")
  (dolist (bb basic-blocks)
    (with-accessors ((first cleavir-basic-blocks:first-instruction)
                     (last cleavir-basic-blocks:last-instruction)
                     (owner cleavir-basic-blocks:owner))
        bb
      #+stealth-gids(format *debug-log* "basic-block owner: ~a:~a~%"
                            (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner))
      #-stealth-gids(format *debug-log* "basic-block owner: ~a~%" (cleavir-ir-gml::label owner))
      (loop for instruction = first
              then (first (cleavir-ir:successors instruction))
            until (eq instruction last)
            do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
      (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))))

(defvar *forms*)
(defvar *map-enter-to-function-info* nil)

(defun calculate-function-info (enter llvm-function-name)
  (let* ((origin (cleavir-ir:origin enter))
         (source-pos-info (if (consp origin) (car origin) origin))
         (lineno 0)
         (column 0)
         (filepos 0))
    (when (and source-pos-info (typep source-pos-info 'core:source-pos-info))
      (setf lineno (core:source-pos-info-lineno source-pos-info)
            column (1+ (core:source-pos-info-column source-pos-info))
            filepos (core:source-pos-info-filepos source-pos-info)))
    (cond
      ((typep enter 'clasp-cleavir-hir:named-enter-instruction)
       (cmp:make-function-info :function-name llvm-function-name
                               :lambda-list (clasp-cleavir-hir:original-lambda-list enter)
                               :docstring (clasp-cleavir-hir:docstring enter)
                               :declares nil
                               :form nil
                               :lineno lineno
                               :column column
                               :filepos filepos))
      ((typep enter 'cleavir-ir:enter-instruction)
       (cmp:make-function-info :function-name llvm-function-name
                               :lambda-list nil
                               :docstring nil
                               :declares nil
                               :form nil
                               :lineno lineno
                               :column column
                               :filepos filepos))
      (t (error "layout-procedure enter is not a known type of enter-instruction - it is a ~a - handle it" enter)))))

(defun layout-procedure (enter lambda-name abi &key (linkage 'llvm-sys:internal-linkage) ignore-arguments)
  (let* ((function-info (gethash enter *map-enter-to-function-info*))
         ;; Gather the basic blocks of this procedure in basic-blocks
         (basic-blocks (function-basic-blocks enter))
         ;; The basic block control starts in.
         (first-basic-block (first basic-blocks))
         ;; This gathers the rest of the basic blocks
         (rest-basic-blocks (rest basic-blocks))
         (cmp:*current-function-name* (cmp:jit-function-name lambda-name))
         (cmp:*gv-current-function-name*
          (cmp:module-make-global-string cmp:*current-function-name* "fn-name"))
         (llvm-function-type cmp:%fn-prototype%)
         (llvm-function-name cmp:*current-function-name*))
    (multiple-value-bind
     (the-function function-description)
     (cmp:irc-cclasp-function-create
      llvm-function-type
      linkage
      llvm-function-name
      cmp:*the-module*
      (calculate-function-info enter lambda-name))
     (let* ((cmp:*current-function* the-function)
            (cmp:*current-function-description* function-description)
            (entry-block (cmp:irc-basic-block-create "entry" the-function))
            (*function-current-multiple-value-array-address* nil)
            (cmp:*irbuilder-function-alloca* (llvm-sys:make-irbuilder cmp:*llvm-context*))
            (body-irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*))
            (body-block (cmp:irc-basic-block-create "body"))
            (metadata (setup-function-scope-metadata lambda-name
                                                     function-info
                                                     :function the-function
                                                     :llvm-function-name llvm-function-name
                                                     :llvm-function the-function
                                                     :llvm-function-type llvm-function-type))
            (cmp:*dbg-current-function* metadata) ; layout-procedure* uses with-dbg-lexical-block - so set cmp:*dbg-current-function*
            (cmp:*dbg-current-scope* cmp:*dbg-current-function*)) ; and cmp:*dbg-current-scope*
       (llvm-sys:set-personality-fn the-function (cmp:irc-personality-function))
       (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-uwtable)
       (cc-dbg-when *debug-log* (log-layout-procedure the-function basic-blocks))
       (let ((args (llvm-sys:get-argument-list the-function)))
         (mapc #'(lambda (arg argname) (llvm-sys:set-name arg argname))
               (llvm-sys:get-argument-list the-function) cmp:+fn-prototype-argument-names+))
       ;; create a basic-block for every remaining tag
       (loop for block in rest-basic-blocks
             for instruction = (cleavir-basic-blocks:first-instruction block)
             do (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag")))
       (cmp:irc-set-insert-point-basic-block entry-block cmp:*irbuilder-function-alloca*)
       ;; Generate code to get the arguments into registers.
       ;; (Actual lambda list stuff is covered by ENTER-INSTRUCTION.)
       (with-debug-info-disabled
        (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
                            (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
                                   (lambda-list (cleavir-ir:lambda-list enter))
                                   (calling-convention (cmp:setup-calling-convention
                                                        fn-args
                                                        :debug-on (and (null ignore-arguments) (debug-on function-info))
                                                        :lambda-list (clasp-cleavir-hir::original-lambda-list enter)
                                                        :cleavir-lambda-list lambda-list
                                                        :rest-alloc (clasp-cleavir-hir::rest-alloc enter)
                                                        :ignore-arguments ignore-arguments)))
                              (setf (calling-convention function-info) calling-convention))))
       (layout-procedure* the-function
                          body-irbuilder
                          body-block
                          first-basic-block
                          rest-basic-blocks
                          function-info
                          enter abi :linkage linkage)))))

;; A hash table of enter instructions to llvm functions.
;; This is used to avoid recompiling ENTERs, which may be
;; multiply accessible in the HIR.
;; We assume that the ABI and linkage will not change.
(defvar *compiled-enters*)
(defun memoized-layout-procedure (enter lambda-name abi &key (linkage 'llvm-sys:internal-linkage) ignore-arguments)
  (or (gethash enter *compiled-enters*)
      (setf (gethash enter *compiled-enters*)
            (layout-procedure enter lambda-name abi :linkage linkage :ignore-arguments ignore-arguments))))

(defun log-translate (initial-instruction)
  (let ((mir-pathname (make-pathname :name (format nil "mir~a" (incf *debug-log-index*))
                                     :type "gml" :defaults (pathname *debug-log*))))
    (format *debug-log* "About to write mir to ~a~%" (namestring mir-pathname))
    (finish-output *debug-log*)
    (multiple-value-bind (instruction-ids datum-ids)
        (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname)))
    (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname)))
  (let ((mir-pathname (make-pathname :name (format nil "mir~a" (incf *debug-log-index*))
                                     :type "dot" :defaults (pathname *debug-log*))))
    (cleavir-ir-graphviz:draw-flowchart initial-instruction (namestring mir-pathname))
    (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname))))

;;; This is "dumb" in that it can only find data that are used without ever being
;;; assigned to. It is also possible for bad HIR to have control paths that lead
;;; to a datum being used without being assigned to, even though in some other
;;; control paths it is assigned to; this function doesn't find that, and with the
;;; structure of CATCH/UNWIND it would be difficult to do so.
(defun check-for-uninitialized-inputs-dumb (initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((uninitialized nil))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for datum in (append (cleavir-ir:inputs instruction)
                                  (cleavir-ir:outputs instruction))
             when (and (typep datum '(or cleavir-ir:lexical-location
                                      cleavir-ir:values-location))
                       (null (cleavir-ir:defining-instructions datum))
                       (not (null (cleavir-ir:using-instructions datum))))
               do (pushnew datum uninitialized)))
     initial-instruction)
    uninitialized))

(defun translate (initial-instruction map-enter-to-function-info go-indices
                  &key (abi *abi-x86-64*) (linkage 'llvm-sys:internal-linkage)
                    ignore-arguments)
  #+(or)
  (let ((uninitialized (check-for-uninitialized-inputs-dumb initial-instruction)))
    (unless (null uninitialized)
      (error "Uninitialized inputs: ~a" uninitialized)))
  (let* ((*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
         (*tags* (make-hash-table :test #'eq))
         (*vars* (make-hash-table :test #'eq))
         (*compiled-enters* (make-hash-table :test #'eq))
         (*instruction-go-indices* go-indices)
         (*map-enter-to-function-info* map-enter-to-function-info)
         ;; FIXME: Probably don't return this as a value - it's a property of the ENTER.
         (lambda-name (get-or-create-lambda-name initial-instruction)))
    (cc-dbg-when *debug-log* (log-translate initial-instruction))
    (let ((function
            (memoized-layout-procedure initial-instruction lambda-name abi :linkage linkage :ignore-arguments ignore-arguments)))
      (cmp::cmp-log-compile-file-dump-module cmp:*the-module* "after-translate")
      (setf *ct-translate* (compiler-timer-elapsed))
      (values function lambda-name))))

(defun my-hir-transformations (init-instr system env)
  ;; FIXME: Per Cleavir rules, we shouldn't need the environment at this point.
  ;; We do anyway because of the possibility that a load-time-value input is introduced
  ;; in HIR that needs the environment to compile, e.g. the form is a constant variable,
  ;; or an object needing a make-load-form.
  ;; That shouldn't actually happen, but it's a little ambiguous in Cleavir right now.
  (quick-draw-hir init-instr "hir-before-transformations")
  #+cst
  (cleavir-partial-inlining:do-inlining init-instr)
  #+cst
  (quick-draw-hir init-instr "hir-after-inlining")
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
  #+(or)
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

  (cleavir-hir-transformations:eliminate-catches init-instr)
  ;; delete the-instruction and the-values-instruction
  (cleavir-kildall-type-inference:delete-the init-instr)
  (setf *ct-delete-the* (compiler-timer-elapsed))
  (quick-draw-hir init-instr "hir-after-delete-the")
  (cc-hir-to-mir:reduce-typeqs init-instr)
  (setf *ct-eliminate-typeq* (compiler-timer-elapsed))
  (quick-draw-hir init-instr "hir-after-eliminate-typeq")
  (clasp-cleavir::eliminate-load-time-value-inputs init-instr system env)
  (quick-draw-hir init-instr "hir-after-eliminate-load-time-value-inputs")
  (setf *ct-eliminate-load-time-value-inputs* (compiler-timer-elapsed))
  #+(or)
  (cleavir-remove-useless-instructions:remove-useless-instructions init-instr)
  #+(or)
  (quick-draw-hir init-instr "hir-after-remove-useless-instructions"))

;; Used by both CST-to-AST and Generate-AST versions.
(defun log-cst-to-ast (ast)
  (let ((ast-pathname (make-pathname :name (format nil "ast~a" (incf *debug-log-index*))
                                     :type "dot" :defaults (pathname *debug-log*))))
    (cleavir-ast-graphviz:draw-ast ast ast-pathname)
    (core:bformat *debug-io* "Just dumped the ast to %s%N" ast-pathname)
    #+(or)(multiple-value-bind (instruction-ids datum-ids)
              (cleavir-ir-gml:draw-flowchart initial-instruction (namestring ast-pathname)))
    (format *debug-log* "Wrote ast to: ~a~%" (namestring ast-pathname))))

(defvar *interactive-debug* nil)

#+cst
(defun cst->ast (cst &optional (env *clasp-env*))
  "Compile a cst into an AST and return it.
Does not hoist.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (handler-bind
      ((cleavir-env:no-variable-info
         (lambda (condition)
           #+verbose-compiler(warn "Condition: ~a" condition)
           (cmp:compiler-warning-undefined-global-variable (cleavir-environment:name condition))
           (invoke-restart 'cleavir-cst-to-ast:consider-special)))
       (cleavir-env:no-function-info
         (lambda (condition)
           #+verbose-compiler(warn "Condition: ~a" condition)
           (cmp:register-global-function-ref (cleavir-environment:name condition))
           (invoke-restart 'cleavir-cst-to-ast:consider-global))))
    (let* ((ast (cleavir-cst-to-ast:cst-to-ast cst env *clasp-system*)))
      (when *interactive-debug* (draw-ast ast))
      (cc-dbg-when *debug-log* (log-cst-to-ast ast))
      (setf *ct-generate-ast* (compiler-timer-elapsed))
      ast)))

#-cst
(defun generate-ast (form &optional (env *clasp-env*))
  "Compile a form into an AST and return it.
Does not hoist."
  (handler-bind
      ((cleavir-env:no-variable-info
         (lambda (condition)
           (cmp:compiler-warning-undefined-global-variable (cleavir-environment:name condition))
           (invoke-restart 'cleavir-generate-ast:consider-special)))
       (cleavir-env:no-function-info
         (lambda (condition)
           #+verbose-compiler(warn "Condition: ~a" condition)
           (cmp:register-global-function-ref (cleavir-environment:name condition))
           (invoke-restart 'cleavir-generate-ast:consider-global))))
    (let* ((ast (cleavir-generate-ast:generate-ast form env *clasp-system*)))
      (when *interactive-debug* (draw-ast ast))
      (cc-dbg-when *debug-log* (log-cst-to-ast ast))
      (setf *ct-generate-ast* (compiler-timer-elapsed))
      ast)))

(defun hoist-ast (ast &optional (env *clasp-env*))
  (prog1
      (clasp-cleavir-ast:hoist-load-time-value ast env)
    (setf *ct-hoist-ast* (compiler-timer-elapsed))))

(defun ast->hir (ast)
  "Compile an AST down to HIR and return it."
  (prog1 (cleavir-ast-to-hir:compile-toplevel ast)
    (setf *ct-generate-hir* (compiler-timer-elapsed))))

(defun hir->mir (hir &optional (env *clasp-env*))
  "Perform HIR transformations, then compile down to MIR. Returns function-info-map as second value,
and go-indices as third."
  ;; Note: We should not have an env parameter. It is only required due to
  ;; how types work at the moment, and will be eliminated as soon as practical.
  (let ((system *clasp-system*))
    ;;(cleavir-ir-graphviz:draw-flowchart hir "/tmp/hir.dot")
    (my-hir-transformations hir system env)
    (quick-draw-hir hir "hir-pre-mir")
    (let ((function-info-map (make-function-info-map hir))
          (*instruction-go-indices* (make-go-indices)))
      (lower-catches function-info-map)
      (cleavir-hir-to-mir:hir-to-mir hir system nil nil)
      #+stealth-gids(cc-mir:assign-mir-instruction-datum-ids hir)
      (quick-draw-hir hir "mir")
      (when *interactive-debug*
        (draw-hir hir)
        (format t "Press enter to continue: ")
        (finish-output)
        (read-line))
      (values hir function-info-map *instruction-go-indices*))))

;;; Convenience. AST must have been hoisted already.
(defun translate-hoisted-ast (ast &key (abi *abi-x86-64*) (linkage 'llvm-sys:internal-linkage)
                            (env *clasp-env*) ignore-arguments)
  (let ((hir (ast->hir ast)))
    (multiple-value-bind (mir function-info-map go-indices)
        (hir->mir hir env)
      (translate mir function-info-map go-indices
                 :abi abi :linkage linkage :ignore-arguments ignore-arguments))))


(defun translate-ast (ast &key (abi *abi-x86-64*) (linkage 'llvm-sys:internal-linkage)
                            (env *clasp-env*) ignore-arguments)
  (let ((hoisted-ast (hoist-ast ast env)))
    (translate-hoisted-ast hoisted-ast
                           :abi abi :linkage linkage :env env
                           :ignore-arguments ignore-arguments)))

(defun translate-lambda-expression-to-llvm-function (lambda-expression)
  "Compile a lambda expression into an llvm-function and return it.
This works like compile-lambda-function in bclasp."
  (let* (#+cst
         (cst (cst:cst-from-expression lambda-expression))
         #+cst
         (ast (cst->ast cst))
         #-cst
         (ast (generate-ast lambda-expression))
         (hir (ast->hir (hoist-ast ast))))
    (multiple-value-bind (mir function-info-map go-indices)
        (hir->mir hir)
      (let ((function-enter-instruction
              (block first-function
                (cleavir-ir:map-local-instructions
                 (lambda (instruction)
                   (when (typep instruction 'cleavir-ir:enclose-instruction)
                     (return-from first-function (cleavir-ir:code instruction))))
                 mir))))
        (unless function-enter-instruction
          (error "Could not find enter-instruction for enclosed function in ~a"
                 lambda-expression))
        (translate function-enter-instruction function-info-map go-indices)))))

(defparameter *debug-final-gml* nil)
(defparameter *debug-final-next-id* 0)

;;; Clasp cleavir entry point for CL:COMPILE.

;; Set this to T to watch cclasp-compile* run
(defvar *cleavir-compile-verbose* nil)
(export '*cleavir-compile-verbose*)
(defun cclasp-compile* (name form env pathname &key (linkage 'llvm-sys:internal-linkage))
  (when *cleavir-compile-verbose*
    (format *trace-output* "Cleavir compiling t1expr: ~s~%" form)
    (format *trace-output* "          in environment: ~s~%" env ))
  (setf *ct-start* (compiler-timer-elapsed))
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (*llvm-metadata* (make-hash-table :test 'eql))
         #+cst
         (cst (cst:cst-from-expression form))
         #+cst
         (ast (cst->ast cst env))
         #-cst
         (ast (generate-ast form env))
         function lambda-name
         ordered-raw-constants-list constants-table startup-fn shutdown-fn)
    (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
      (multiple-value-setq (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
        (literal:with-rtv
            (let* ((ast (hoist-ast ast env))
                   (hir (ast->hir ast)))
              (multiple-value-bind (mir function-info-map go-indices)
                  (hir->mir hir env)
                (multiple-value-setq (function lambda-name)
                  (translate mir function-info-map go-indices
                             :abi *abi-x86-64* :linkage linkage)))))))
    (unless function
      (error "There was no function returned by translate-ast"))
    (cmp:cmp-log "fn --> %s%N" fn)
    (cmp:quick-module-dump cmp:*the-module* "cclasp-compile-module-pre-optimize")
    (let ((setup-function
            (cmp:jit-add-module-return-function
             cmp:*the-module*
             function startup-fn shutdown-fn ordered-raw-constants-list)))
      (funcall setup-function))))

(defun compile-form (form &optional (env *clasp-env*))
  (setf *ct-start* (compiler-timer-elapsed))
  #+cst
  (let* ((cst (cst:cst-from-expression form))
         (ast (cst->ast cst env)))
    (translate-ast ast :env env))
  #-cst
  (let ((ast (generate-ast form env)))
    (translate-ast ast :env env)))


(defun compile-ltv-form (form &optional (env *clasp-env*))
  (setf *ct-start* (compiler-timer-elapsed))
  #+cst
  (let* ((cst (cst:cst-from-expression form))
         (ast (cst->ast cst env)))
    (translate-ast ast :env env :ignore-arguments t :linkage cmp:*default-linkage*))
  #-cst
  (let ((ast (generate-ast form env)))
    (translate-ast ast :env env :ignore-arguments t :linkage cmp:*default-linkage*)))

#+cst
(defun cleavir-compile-file-cst (cst &optional (env *clasp-env*))
  (literal:with-top-level-form
      (if cmp::*debug-compile-file*
          (compiler-time (let ((ast (cst->ast cst env)))
                           (translate-ast ast :env env :linkage cmp:*default-linkage*)))
          (let ((ast (cst->ast cst env)))
            (translate-ast ast :env env :linkage cmp:*default-linkage*)))))

#-cst
(defun cleavir-compile-file-form (form &optional (env *clasp-env*))
  (literal:with-top-level-form
      (if cmp:*debug-compile-file*
          (compiler-time (let ((ast (generate-ast form env)))
                           (translate-ast ast :env env cmp:*default-linkage*)))
          (let ((ast (generate-ast form env)))
            (translate-ast ast :env env :linkage cmp:*default-linkage*)))))

(defclass clasp-cst-client (eclector.concrete-syntax-tree:cst-client) ())

(defvar *cst-client* (make-instance 'clasp-cst-client))

(defmethod eclector.parse-result:source-position
    ((client clasp-cst-client) stream)
  (core:input-stream-source-pos-info stream))

(defun cclasp-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym))
        (eclector.reader:*client* *cst-client*)
        (read-function 'eclector.concrete-syntax-tree:cst-read)
        (*llvm-metadata* (make-hash-table :test 'eql))
        (cleavir-generate-ast:*compiler* 'cl:compile-file)
        (core:*use-cleavir-compiler* t))
    (loop
      ;; Required to update the source pos info. FIXME!?
      (peek-char t source-sin nil)
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let* ((core:*current-source-pos-info* (core:input-stream-source-pos-info source-sin))
             #+cst
             (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value))
             #-cst
             (form (read source-sin nil eof-value)))
        #+cst
        (if (eq cst eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form (cst:raw cst)))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (cleavir-compile-file-cst cst environment))))
        #-cst
        (if (eq form eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form form))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (cleavir-compile-file-form form))))))))

(defun cclasp-compile-in-env (name form &optional env)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (if cmp::*debug-compile-file*
        (compiler-time
         (cmp:compile-in-env name form env #'cclasp-compile* cmp:*default-compile-linkage*))
        (cmp:compile-in-env name form env #'cclasp-compile* cmp:*default-compile-linkage*))))
        
(defun cleavir-compile (name form &key (debug *debug-cleavir*))
  (let ((cmp:*compile-debug-dump-module* debug)
	(*debug-cleavir* debug))
    (cclasp-compile-in-env name form nil)))

(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((*debug-log-index* 0)
        (cmp:*cleavir-compile-file-hook* 'cclasp-loop-read-and-compile-file-forms))
    (apply #'cmp::compile-file given-input-pathname args)))


(defmacro with-debug-compile-file ((log-file &key debug-log-on) &rest body)
  `(with-open-file (clasp-cleavir::*debug-log* ,log-file :direction :output)
     (let ((clasp-cleavir::*debug-log-on* ,debug-log-on))
       ,@body)))
(export 'with-debug-compile-file)


(defmacro open-debug-log (log-file &key (debug-compile-file t) debug-compile)
  `(eval-when (:compile-toplevel :execute)
     (core:bformat t "Turning on compiler debug logging%N")
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
     (core:bformat t "Turning off compiler debug logging%N")
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
  
