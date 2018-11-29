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
                 ;; AST initialization - speeds up loading inline.lisp.
                 ,@(clos:subclasses* (find-class 'cleavir-ast:ast))
                 ;; the remainder only speeds up the first compile
                 ,@(clos:subclasses* (find-class 'cleavir-ir:instruction))
                 ,@(clos:subclasses* (find-class 'cleavir-env::entry))
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
  (macrolet ((for-entries (name)
               `(clos:satiate #',name '(null) '(clasp-cleavir:clasp-global-environment)
                              ,@(mapcar (lambda (class)
                                          `'(,class))
                                        (clos:subclasses* (find-class 'cleavir-env::entry))))))
    (for-entries cleavir-env:global-environment)
    (for-entries cleavir-env:declarations)
    (for-entries cleavir-env:compile-time)))

;;; cleavir-ast
(eval-when (:load-toplevel)
  (macrolet ((satiate-for-all (name)
               `(clos:satiate #',name ,@(loop for class
                                                in (rest (clos:subclasses* (find-class 'cleavir-ast:ast)))
                                              collect `'(,class)))))
    (satiate-for-all cleavir-ast:origin)
    (satiate-for-all cleavir-ast:policy))
  (macrolet ((satiate-children ()
               `(clos:satiate #'cleavir-ast:children
                              ,@(loop for method in (clos:generic-function-methods #'cleavir-ast:children)
                                      when (null (method-qualifiers method))
                                        collect `'(,(first (clos:method-specializers method)))))))
    (satiate-children)))

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
                                                  collect `'(,sp cons ,entry clasp-cleavir::clasp-64bit)))))
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
               (let* ((methods (clos:generic-function-methods
                                #'clasp-cleavir::translate-simple-instruction))
                      ;; note: includes INSTRUCTION, but i think that's harmless
                      (classes (loop for method in methods
                                     when (null (method-qualifiers method))
                                       collect (first (clos:method-specializers method)))))
                 `(clos:satiate
                   #'clasp-cleavir::translate-simple-instruction
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
