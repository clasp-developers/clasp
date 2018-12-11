;;;; Satiate relevant Cleavir functions so that
;;;; startup and the first compile go a little faster.
;;;; (This file is thus essentially optional.)

(in-package #:clasp-cleavir)

;;; Initialization stuff.
;;; Note that we'll include some abstract classes that are never actually instantiated.
;;; Just putting them in the call history is no problem, though.
(eval-when (:load-toplevel)
  (macrolet ((frob ()
               `(clos:satiate-initialization
                 ;; KLUDGE: We incorporate all the classes satiated in clos/satiation.lsp
                 ;; here so that the discriminating function still includes them etc.
                 'standard-generic-function
                 'standard-method
                 'standard-class
                 'structure-class
                 'clos:standard-reader-method
                 'clos:standard-writer-method
                 'clos:standard-direct-slot-definition
                 'clos:standard-effective-slot-definition
                 'clos:eql-specializer
                 'clos:method-combination
                 'clos:funcallable-standard-class
                 ;; OK on to the real shit.
                 ,@(rest (clos:subclasses* (find-class 'cleavir-ast:ast)))
                 ,@(rest (clos:subclasses* (find-class 'cleavir-ir:instruction)))
                 ,@(rest (clos:subclasses* (find-class 'cleavir-ir:datum)))
                 ,@(rest (clos:subclasses* (find-class 'cleavir-env::entry)))
                 'cleavir-ast-to-hir:context
                 'cleavir-env:lexical-variable-info
                 'cleavir-env:special-variable-info
                 'cleavir-env:constant-variable-info
                 'cleavir-env:symbol-macro-info
                 'cleavir-env:local-function-info
                 'cleavir-env:global-function-info
                 'cleavir-env:local-macro-info
                 'cleavir-env:special-operator-info
                 'cleavir-env:block-info
                 'cleavir-env:tag-info
                 'cleavir-env:optimize-info)))
    (frob)))

;;; Functions go here (TODO)

;;; cleavir-env
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-env:name
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:macro) '(cleavir-env:tag)
                '(cleavir-env:lexical-variable) '(cleavir-env:special-variable)
                '(cleavir-env:variable-ignore) '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info)
                '(cleavir-env:global-macro-info) '(cleavir-env:special-variable-info)
                '(cleavir-env:lexical-variable-info))
  (clos:satiate #'cleavir-env:policy '(cleavir-env:optimize-info))
  (clos:satiate #'cleavir-env:ast '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:expander
                '(cleavir-env:local-macro-info) '(cleavir-env:global-macro-info) '(cleavir-env:macro))
  (clos:satiate #'cleavir-env:compiler-macro
                '(cleavir-env:global-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:identity
                '(cleavir-env:local-function-info) '(cleavir-env:block-info) '(cleavir-env:tag-info)
                '(cleavir-env:lexical-variable-info) '(cleavir-env:lexical-variable)
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:tag))
  (clos:satiate #'cleavir-env:inline '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:type
                '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:lexical-variable-info)
                '(cleavir-env:global-function-info) '(cleavir-env:special-variable-info))
  (clos:satiate #'cleavir-env:ignore
                '(cleavir-env:variable-ignore)
                '(cleavir-env:lexical-variable-info)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (macrolet ((for-entries (name &rest remaining-args)
               (let* ((entries (list* 'null 'clasp-global-environment
                                      (rest (clos:subclasses* (find-class 'cleavir-env::entry)))))
                      (tail (if remaining-args
                                (loop for things in remaining-args
                                      nconcing (mapcar (lambda (entry) `'(,entry ,@things)) entries))
                                (mapcar (lambda (entry) `'(,entry)) entries))))
                 `(clos:satiate #',name ,@tail))))
    (for-entries cleavir-env:block-info (symbol))
    ;; i include conses for (setf x) functions, but the specializer profile makes it irrelevant
    (for-entries cleavir-env:function-info (symbol) (cons))
    (for-entries cleavir-env:optimize-info)
    (for-entries cleavir-env:tag-info (symbol))
    (for-entries cleavir-env:variable-info (symbol))
    (for-entries cleavir-env:global-environment)
    (for-entries cleavir-env:declarations)
    (for-entries cleavir-env:compile-time)
    (for-entries cleavir-env:optimize-qualities)
    (for-entries cleavir-env:add-block (symbol))
    ;;(for-entries cleavir-env:add-function-dynamic-extent (symbol) (cons))
    ;;(for-entries cleavir-env:add-function-ignore (symbol symbol) (cons symbol) (symbol null) (cons null))
    ;;(for-entries cleavir-env:add-function-type (symbol cons) (cons cons) (symbol symbol) (cons symbol))
    ;;add-inline, add-inline-expansion
    (for-entries cleavir-env:add-lexical-variable (symbol))
    (for-entries cleavir-env:add-local-function (symbol))
    (for-entries cleavir-env:add-local-macro (symbol core:closure-with-slots))
    (for-entries cleavir-env:add-local-symbol-macro (symbol symbol cons)) ; not sure history is correct
    ;;add-optimize
    (for-entries cleavir-env:add-special-variable (symbol))
    (for-entries cleavir-env:add-tag (symbol))
    ;;add-variable-dynamic-extent
    (for-entries cleavir-env:add-variable-ignore (symbol symbol))
    (for-entries cleavir-env:add-variable-type (symbol symbol) (symbol cons))))

;;; cleavir-compilation-policy
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-compilation-policy:compute-policy
                '(cons clasp-global-environment))
  (clos:satiate #'cleavir-compilation-policy:policy-qualities
                '(clasp-global-environment))
  (clos:satiate #'cleavir-compilation-policy:normalize-optimize
                '(cons clasp-global-environment))
  (clos:satiate #'cleavir-compilation-policy:compute-policy-quality
                '((eql cleavir-kildall-type-inference:insert-type-checks) cons clasp-global-environment)
                '((eql cleavir-escape:trust-dynamic-extent) cons clasp-global-environment)
                '((eql maintain-shadow-stack) cons clasp-global-environment)
                '((eql do-type-inference) cons clasp-global-environment)
                '((eql do-dx-analysis) cons clasp-global-environment)))

;;; cleavir-ast
(eval-when (:load-toplevel)
  (macrolet ((satiate-for-all (name)
               `(clos:satiate #',name ,@(loop for class
                                                in (rest (clos:subclasses* (find-class 'cleavir-ast:ast)))
                                              collect `'(,class)))))
    (satiate-for-all cleavir-ast:origin)
    (satiate-for-all cleavir-ast:policy))
  (macrolet ((satiate-reader (name)
               `(clos:satiate #',name
                              ,@(loop for method in (clos:generic-function-methods (fdefinition name))
                                      when (null (method-qualifiers method))
                                        collect `'(,(first (clos:method-specializers method))))))
             (satiate-readers (&rest names)
               `(progn ,@(loop for name in names collect `(satiate-reader ,name)))))
    (satiate-readers
     ;; universe
     cleavir-ast:children ; Note: should probably be in satiate-for-all, but scope-ast needs a method
     ;; general-purpose-asts
     cleavir-ast:value
     cleavir-ast:name
     cleavir-ast:symbol-ast
     cleavir-ast:value-ast
     cleavir-ast:name-ast
     cleavir-ast:callee-ast cleavir-ast:argument-asts
     cleavir-ast:lambda-list cleavir-ast:body-ast
     cleavir-ast:forms
     cleavir-ast:form-asts
     cleavir-ast:block-ast cleavir-ast:form-ast
     cleavir-ast:lhs-ast
     cleavir-ast:lhs-asts
     cleavir-ast:tag-ast
     cleavir-ast:required-types cleavir-ast:optional-types cleavir-ast:rest-type
     cleavir-ast:type-specifier cleavir-ast:type-specifier-ast
     cleavir-ast:form cleavir-ast:read-only-p
     cleavir-ast:test-ast cleavir-ast:then-ast cleavir-ast:else-ast
     cleavir-ast:function-form-ast
     cleavir-ast:first-form-ast
     cleavir-ast:symbol
     cleavir-ast:arg1-ast cleavir-ast:arg2-ast
     ;; cons-related-asts
     cleavir-ast:cons-ast cleavir-ast:object-ast
     ;; fixnum-related-asts
     cleavir-ast:variable-ast
     ;; scope-related-asts (NOTE: should probably delete this file anyway)
     cleavir-ast:child-ast
     ;; simple-float-related-asts
     cleavir-ast:subtype
     cleavir-ast:from-type cleavir-ast:to-type cleavir-ast:arg-ast
     ;; standard-object-related-asts (NOTE: as of now, unused in clasp)
     cleavir-ast:slot-number-ast)))

;;; cleavir-io
(eval-when (:load-toplevel)
  (macrolet ((satiate-save-info ()
               `(clos:satiate #'cleavir-io:save-info
                              ,@(loop for method
                                        in (clos:generic-function-methods #'cleavir-io:save-info)
                                      when (equal (method-qualifiers method) '(append))
                                        collect `'(,(first (clos:method-specializers method)))))))
    (satiate-save-info)))

;;; cleavir-generate-ast
#-cst
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-generate-ast:convert-global-function
                '(cleavir-env:global-function-info null clasp-64bit))
  (clos:satiate #'cleavir-generate-ast:convert-special-variable
                '(cleavir-env:special-variable-info null clasp-64bit))
  ;; convert-constant-to-immediate is a bit difficult, since it can be called with any object.
  (clos:satiate #'cleavir-generate-ast:convert-code
                ;; the ENV class should be irrelevant due to the specializer profile.
                '(null cons cleavir-env:special-variable clasp-64bit))
  (clos:satiate #'cleavir-generate-ast:convert-setq-special-variable
                ;; the FORM-AST class should be irrelevant due to the specializer profile.
                ;; (Really, everything is - there's only one, unspecialized, method.)
                '(cleavir-env:special-variable-info symbol cleavir-ast:call-ast null clasp-64bit))
  #+(or) ;; FIXME: move the shit in inline-prep out.
  (clos:satiate #'cleavir-generate-ast:convert
                ;; first two classes irrelevant again.
                '(cons cleavir-env:special-variable clasp-64bit))
  (clos:satiate #'cleavir-generate-ast:convert-special-binding
                ;; again, specprofile is NIL NIL NIL NIL NIL
                '(symbol cleavir-ast:lexical-ast cleavir-ast:progn-ast cleavir-env:variable-ignore clasp-64bit))
  (clos:satiate #'cleavir-generate-ast:convert-function
                ;; T NIL NIL
                '(cleavir-env:local-function-info cleavir-env:function clasp-64bit)
                '(cleavir-env:global-function-info cleavir-env:function clasp-64bit))
  (clos:satiate #'cleavir-generate-ast:raw '(null) '(symbol) '(cons))
  ;; convert-special is probably the most involved.
  (macrolet ((satiate-special ()
               (let* ((special-operators
                        ;; Just grab them from the function itself, rather than maintain another damn list
                        (loop for method
                                in (clos:generic-function-methods #'cleavir-generate-ast:convert-special)
                              for specs = (clos:method-specializers method)
                              for espec = (first specs)
                              when (typep espec 'clos:eql-specializer)
                                collect espec))
                      ;; ENTRY itself is abstract, so we can exclude it (with REST)
                      (entries (rest (clos:subclasses* (find-class 'cleavir-env::entry))))
                      (lists (loop for sp in special-operators
                                   nconcing (loop for entry in entries
                                                  collect `'(,sp cons ,entry clasp-64bit)))))
                 `(clos:satiate #'cleavir-generate-ast:convert-special ,@lists))))
    (satiate-special))
  ;; check-special-form-syntax is rather involved as well, though.
  (macrolet ((satiate-check ()
               (let ((special-operators
                       (loop for method
                               in (clos:generic-function-methods #'cleavir-generate-ast:check-special-form-syntax)
                             for specs = (clos:method-specializers method)
                             for espec = (first specs)
                             when (typep espec 'clos:eql-specializer)
                               collect espec)))
                 `(clos:satiate #'cleavir-generate-ast:check-special-form-syntax
                                ,@(loop for sp in special-operators
                                        collect `'(,sp cons))))))
    (satiate-check)))

;;; cleavir-ast-to-hir
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-ast-to-hir:compile-function '(clasp-cleavir-ast:named-function-ast))
  (clos:satiate #'cleavir-ast-to-hir:invocation '(cleavir-ast-to-hir:context))
  (macrolet ((satiate-compile-ast ()
               (let* ((methods (clos:generic-function-methods #'cleavir-ast-to-hir:compile-ast))
                      (tail
                        (loop for method in methods
                              when (null (method-qualifiers method)) ; primary on ly
                                collect `'(,(first (clos:method-specializers method))
                                           cleavir-ast-to-hir:context))))
                 `(clos:satiate #'cleavir-ast-to-hir:compile-ast ,@tail))))
    (satiate-compile-ast)))

;;; cleavir-ir
(eval-when (:load-toplevel)
  (macrolet ((satiate-readers ()
               (let* ((instructions (rest (clos:subclasses* (find-class 'cleavir-ir:instruction))))
                      (tail (loop for i in instructions collect `'(,i))))
                 `(progn
                    (clos:satiate #'cleavir-ir:origin ,@tail)
                    (clos:satiate #'cleavir-ir:predecessors ,@tail)
                    (clos:satiate #'cleavir-ir:successors ,@tail)
                    (clos:satiate #'cleavir-ir:inputs ,@tail)
                    (clos:satiate #'cleavir-ir:outputs ,@tail)
                    (clos:satiate #'cleavir-ir:policy ,@tail))))
             (satiate-writers ()
               (let* ((instructions (rest (clos:subclasses* (find-class 'cleavir-ir:instruction))))
                      (tail (loop for i in instructions
                                  collect `'(cons ,i)
                                  collect `'(null ,i))))
                 `(progn
                    (clos:satiate #'(setf cleavir-ir:predecessors) ,@tail)
                    (clos:satiate #'(setf cleavir-ir:successors) ,@tail)
                    (clos:satiate #'(setf cleavir-ir:inputs) ,@tail)
                    (clos:satiate #'(setf cleavir-ir:outputs) ,@tail))))
             (satiate-subst ()
               (let* ((instructions (rest (clos:subclasses* (find-class 'cleavir-ir:instruction))))
                      (tail (loop for i in instructions
                                  collect `'(cleavir-ir:lexical-location cleavir-ir:lexical-location ,i))))
                 `(progn
                    (clos:satiate #'cleavir-ir:substitute-input ,@tail)
                    (clos:satiate #'cleavir-ir:substitute-output ,@tail)))))
    (satiate-readers)
    (satiate-writers)
    (satiate-subst)))

;;; cleavir-hir-to-mir
(eval-when (:load-toplevel)
  (macrolet ((satiate-specialize ()
               (let ((classes (rest (clos:subclasses* (find-class 'cleavir-ir:instruction)))))
                 `(clos:satiate #'cleavir-hir-to-mir:specialize
                                ,@(loop for c in classes
                                        collect `'(,c clasp-cleavir::clasp-64bit null null))))))
    (satiate-specialize)))

;;; clasp-cleavir
(eval-when (:load-toplevel)
  (macrolet ((satiate-simple ()
               (let* ((methods (clos:generic-function-methods #'translate-simple-instruction))
                      ;; note: includes INSTRUCTION, but i think that's harmless
                      (classes (loop for method in methods
                                     when (null (method-qualifiers method))
                                       collect (first (clos:method-specializers method)))))
                 `(clos:satiate
                   #'translate-simple-instruction
                   ,@(loop for c in classes
                           collect `'(,c llvm-sys:alloca-inst
                                      clasp-cleavir::abi-x86-64 clasp-cleavir::function-info)))))
             (satiate-branch (&rest groups)
               ;; We use a fixed list so we can treat the third argument specially.
               ;; It's the list of successors, and whether it's a cons or null depends
               ;; on the instruction class.
               `(clos:satiate #'clasp-cleavir::translate-branch-instruction
                              ,@(loop for (instruction list) in groups
                                      collect `'(,instruction llvm-sys:alloca-inst ,list
                                                 clasp-cleavir::abi-x86-64
                                                 clasp-cleavir::function-info)))))
    (satiate-simple)
    (satiate-branch
     (cleavir-ir:eq-instruction cons)
     (cleavir-ir:consp-instruction cons)
     (cleavir-ir:fixnump-instruction cons)
     (cc-mir:characterp-instruction cons)
     (cc-mir:single-float-p-instruction cons)
     (cc-mir:headerq-instruction cons)
     (cleavir-ir:unwind-instruction null)
     (cc-mir:assign-catch-instruction cons)
     (cleavir-ir:return-instruction null)
     (cleavir-ir:funcall-no-return-instruction null)
     (cleavir-ir:unreachable-instruction null)
     (clasp-cleavir-hir:throw-instruction null)
     (cleavir-ir:fixnum-add-instruction cons)
     (cleavir-ir:fixnum-sub-instruction cons)
     (cleavir-ir:fixnum-less-instruction cons)
     (cleavir-ir:fixnum-not-greater-instruction cons)
     (cleavir-ir:fixnum-equal-instruction cons)
     (cleavir-ir:float-less-instruction cons)
     (cleavir-ir:float-not-greater-instruction cons)
     (cleavir-ir:float-equal-instruction cons))))
;  (clos:satiate #'cclasp-eval-with-env '(cons null)
