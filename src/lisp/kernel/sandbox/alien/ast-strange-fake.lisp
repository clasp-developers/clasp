(defpackage #:fake-ast
  (:use #:cl)
  (:export #:ast->form))

(in-package #:fake-ast)

(defvar *magic-variables*)
;;  "A list of variables set by the AST but not bound, as from lambda list processing.")
(defvar *lexical-to-sym* nil
  "A hash table mapping lexical ASTs to their names in the returned code.")
(defvar *blocks*)
(defvar *tags*)
;;  "An alist of block ASTs to their names in the produced code."

(defun gen-name (ast)
  (gensym (symbol-name (cleavir-ast:name ast))))

(defun fake-variable (ast &optional setq)
  (check-type ast cleavir-ast:lexical-ast)
  (let ((name (gethash ast *lexical-to-sym*)))
    (cond (name)
          (setq (setf (gethash ast *lexical-to-sym*) (gen-name ast)))
          (t (error "Bad AST: Unknown lexical-ast ~a" ast)))))

(defun ast->form (ast)
  (let ((*magic-variables* nil)
        (*lexical-to-sym* (make-hash-table :test #'eq))
        (*blocks* nil))
    (fake ast)))

(defun register-new-lexical (ast)
  (check-type ast cleavir-ast:lexical-ast)
  (if (gethash ast *lexical-to-sym*)
      (error "Bad AST: Lexical bound twice? ~a" ast)
      (setf (gethash ast *lexical-to-sym*) (gen-name ast))))

(defun fake-lambda-list (c-lambda-list)
  (loop for thing in c-lambda-list
        when (symbolp thing) ; lambda list keyword
          collect thing
        else when (and (listp thing) (= (length thing) 2)) ; optional
               collect (list (register-new-lexical (first thing))
                             nil ; not used
                             (register-new-lexical (second thing)))
        else when (and (listp thing) (= (length thing) 3)) ; key
               collect (list (list (first thing)
                                   (register-new-lexical (second thing)))
                             nil ; not used
                             (register-new-lexical (third thing)))
        else ; required or key or rest
        collect (register-new-lexical thing)))

(defgeneric fake (ast))

(defmethod fake ((ast cleavir-ast:function-ast))
  (let ((*magic-variables* nil)
        (lambda-list (fake-lambda-list (cleavir-ast:lambda-list ast))))
    ;; faking will fill up *magic-variables*.
    (let ((body (fake (cleavir-ast:body-ast ast))))
      `(lambda (,@lambda-list &aux ,@*magic-variables*)
         ,body))))

(defmethod fake ((ast cleavir-ast:setq-ast))
  `(setq ,(fake-variable (cleavir-ast:lhs-ast ast) t)
         ,(fake (cleavir-ast:value-ast ast))))

(defmethod fake ((ast cleavir-ast:lexical-ast))
  (fake-variable ast))

(defmethod fake ((ast cleavir-ast:constant-ast))
  `',(cleavir-ast:value ast))

;;; LTVs should be hoisted.

(defmethod fake ((ast cleavir-ast:progn-ast))
  `(progn ,@(mapcar #'fake (cleavir-ast:form-asts ast))))

(defmethod fake ((ast cleavir-ast:symbol-value-ast))
  `(symbol-value ,(fake (cleavir-ast:symbol-ast ast))))

(defmethod fake ((ast cleavir-ast:set-symbol-value-ast))
  `(set ,(fake (cleavir-ast:symbol-ast ast)) ,(fake (cleavir-ast:value-ast ast))))

;; no fdefinition ast - use sicl enviroments, so linking is your own deal

(defmethod fake ((ast cleavir-ast:call-ast))
  `(funcall ,(fake (cleavir-ast:callee-ast ast))
            ,@(mapcar #'fake (cleavir-ast:argument-asts ast))))

(defmethod fake ((ast cleavir-ast:block-ast))
  (let ((name (gensym "BLOCK")))
    (push (cons ast name) *blocks*)
    `(block ,name ,(fake (cleavir-ast:body-ast ast)))))

(defmethod fake ((ast cleavir-ast:return-from-ast))
  (let* ((block (cleavir-ast:block-ast ast))
         (pair (assoc block *blocks*)))
    (if pair
        `(return-from ,(cdr pair) ,(fake (cleavir-ast:form-ast ast)))
        ;; error sucks, but shouldn't happen in practice anyway,
        ;; as the AST was generated and cannot validly result in this
        (error "Return from unknown block"))))

(defmethod fake ((ast cleavir-ast:tagbody-ast))
  ;; collect all the tags before doing conversions. one can jump forward, after all.
  (let* ((items (cleavir-ast:item-asts ast))
         (new-tags (loop for item in items
                         when (typep item 'cleavir-ast:tag-ast)
                           collect (cons item (gen-name item))))
         (*tags* (append new-tags *tags*)))
    `(tagbody
        ,@(loop for item in items
                collect (if (typep item 'cleavir-ast:tag-ast)
                            (cdr (assoc item new-tags))
                            ;; just to make super sure the host doesn't interpret symbols as tags.
                            ;; unlikely to actually come up.
                            `(progn ,(fake item)))))))

(defmethod fake ((ast cleavir-ast:go-ast))
  (let* ((tag (cleavir-ast:tag-ast ast))
         (pair (assoc tag *tags*)))
    (unless pair (error "Bad AST: Unknown tag ~a" tag))
    `(go ,(cdr pair))))

(defmethod fake ((ast cleavir-ast:the-ast))
  ;; could do the type, but inefficiency is okay here
  (fake (cleavir-ast:form-ast ast)))

(defmethod fake ((ast cleavir-ast:typeq-ast))
  `(typep ,(fake (cleavir-ast:form-ast ast))
          ',(cleavir-ast:type-specifier ast)))

(defmethod fake ((ast cleavir-ast:if-ast))
  `(if ,(fake (cleavir-ast:test-ast ast)) ; we assume it's a valid boolean.
       ,(fake (cleavir-ast:then-ast ast))
       ,(fake (cleavir-ast:else-ast ast))))

(defmethod fake ((ast cleavir-ast:multiple-value-call-ast))
  `(multiple-value-call ,(fake (cleavir-ast:function-form-ast ast))
     ,@(mapcar #'fake (cleavir-ast:form-asts ast))))

(defmethod fake ((ast cleavir-ast:values-ast))
  `(values ,@(mapcar #'fake (cleavir-ast:argument-asts ast))))

(defmethod fake ((ast cleavir-ast:multiple-value-prog1-ast))
  `(multiple-value-prog1
       ,(fake (cleavir-ast:first-form-ast ast))
     ,@(mapcar #'fake (cleavir-ast:form-asts ast))))

(defmethod fake ((ast cleavir-ast:dynamic-allocation-ast))
  ;; Nope.
  (fake (cleavir-ast:form-ast ast)))

(defmethod fake ((ast cleavir-ast:unreachable-ast))
  `(error "Unreachable"))

(defmethod fake ((ast cleavir-ast:eq-ast))
  `(eq ,(fake (cleavir-ast:arg1-ast ast))
       ,(fake (cleavir-ast:arg2-ast ast))))
