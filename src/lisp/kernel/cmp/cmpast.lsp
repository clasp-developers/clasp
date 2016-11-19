;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleavir-like AST generation
;;;
;;; A lot of this code is copied and translated from Robert Strandh's
;;; Cleavir compiler.


(in-package :cmp)



(defparameter *subforms-are-top-level-p* t)
(defparameter *current-form-is-top-level-p* t)

(defstruct (if-ast (:type vector) :named) test-ast then-ast else-ast)
(defstruct (block-ast (:type vector) :named) body-ast)
(defstruct (return-from-ast (:type vector) :named) block-ast form-ast)
(defstruct (load-time-value-ast (:type vector) :named) form read-only-p)
(defstruct (progn-ast (:type vector) :named) form-asts)
(defstruct (tag-ast (:type vector) :named) name)
(defstruct (go-ast (:type vector) :named) tag-ast)
(defstruct (tagbody-ast (:type vector) :named) item-asts)
(defstruct (lexical-ast (:type vector) :named) name)
(defstruct (setq-ast (:type vector) :named) lhs-ast value-ast)



#+(or)(defun raw (thing) thing)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun destructure-variables (tree form)
    (let ((bindings '()))
      (labels ((traverse (tree form)
                 (cond ((null tree)
                        nil)
                       ((symbolp tree)
                        (push `(,tree ,form) bindings))
                       ((not (consp tree))
                        (error 'expectetree-but-found
                               :found tree))
                       (t
                        (let ((temp (gensym)))
                          (push `(,temp ,form) bindings)
                          (traverse (first tree) `(first ,temp))
                          (traverse (rest tree) `(rest ,temp)))))))
        (traverse tree form)
        (reverse bindings)))))

(defmacro db (tree form &body body)
  (let ((form-var (gensym)))
    `(let* ((,form-var ,form)
	    ,@(destructure-variables tree form-var))
       ,@body)))

(defun convert-literal (form env)
  (make-load-time-value-ast :form `(quote ,form)
                            :read-only-p t))

(defun convert-constant (form env)
  (make-load-time-value-ast :form `(quote ,form)
                            :read-only-p t))


(defun ensure-args (form &key (min 0) max)
  (let ((rest (cdr form)))
    (when (< (length rest) min)
      (error "insufficient arguments for ~a" form))
    (when (and max (> (length rest) max))
      (error "too many arguments for ~a" form))))

(defun convert-if (form env)
  (ensure-args form :min 1 :max 3)
  (db (if test then else) form
      (make-if-ast :test-ast (convert test env)
                   :then-ast (convert then env)
                   :else-ast (convert else env))))

  

(defun convert-block (form env)
  (ensure-args form :min 2)
  (db (block name . body) form
      (or (symbolp name)
          (compiler-error "Block must have a symbol as second argument: ~a" form))
      (let* ((ast (make-block-ast :body-ast nil))
             (new-env (clcenv:add-block env name ast)))
        (setf (block-ast-body-ast ast) (process-progn (convert-sequence body new-env))))))

(defun convert-return-from (form env)
  (ensure-args form :min 2 :max 3)
  (db (return-from block-name . rest) form
      (declare (ignore return-from))
      (let ((info (clcenv:block-info env block-name))
            (value-form (if (null rest) nil (first rest))))
        (make-return-from-ast :block-ast (clcenv:info-identity info)
                              :form-ast (convert value-form env)))))
             
(defun convert-sequence (forms env)
  (mapcar (lambda (form)
            (convert form env))
          forms))

(defun process-progn (asts &optional form)
  (if (null asts)
      (make-load-time-value-ast :form 'nil :read-only-p t)
      (make-progn-ast :form-asts asts)))

(defun convert-progn (form env)
  (process-progn (convert-sequence (cdr form) env) form))


;;;----------------------------------------------------------------------
;;;
;;; convert-let
;;;

(defun binding-init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

(defun binding-init-forms (bindings)
  (mapcar #'binding-init-form bindings))

(defun temp-asts-from-bindings (bindings)
  (loop repeat (length bindings)
	collect (make-lexical-ast :name (gensym))))

(defun binding-var (binding)
  (if (symbolp binding)
      binding
      (first binding)))

(defun binding-vars (bindings)
  (mapcar #'binding-var bindings))


(defun make-let-init-asts (bindings temp-asts env)
  (let (result)
    (do* ((init-form-cur (binding-init-forms bindings) (cdr init-form-cur))
          (init-form (car init-form-cur) (car init-form-cur))
          (converted (convert init-form env) (convert init-form env))
          (temp-asts-cur temp-asts (cdr temp-asts-cur))
          (temp-ast (car temp-asts-cur) (car temp-asts-cur)))
         ((null (cdr init-form-cur)) (reverse result))
      (format t "init-form-cur -> ~a~%" init-form-cur)
      (push (make-setq-ast :lhs-ast temp-ast :value-ast converted) result))))

(defun convert-let (form env)
  (db (let bindings . body ) form
      (declare (ignore let))
      (multiple-value-bind (declarations forms)
          (separate-ordinary-body body)
        (let* ((canonical-dspecs (canonicalize-declarations declarations))
               (variables (binding-vars bindings))
               (temp-asts (temp-asts-from-bindings bindings))
               (init-asts (make-let-init-asts bindings temp-asts env)))
          (multiple-value-bind (idspecs rdspecs) (itemize-declaration-specifiers
                                                  (listify variables)
                                                  canonical-dspecs)
            (process-progn
             (append init-asts
                     (list (process-remaining-let-bindings
                            (pair-items variables temp-asts)
                            idspecs
                            rdspecs
                            forms
                            env)))))))))



;;;----------------------------------------------------------------------
;;;
;;; convert-tagbody
;;;

(defun convert-tagbody (form env)
  (db (tagbody . items) form
      (let ((tag-asts (let (ta)
                        (dolist (sitem items)
                          (let ((item sitem))
                            (when (or (symbolp item)
                                      (integerp item))
                              (push (make-tag-ast :name item) ta))))
                        (nreverse ta)))
            (new-env env))
        (dolist (ast tag-asts)
          (setf new-env (clcenv:add-tag new-env (tag-ast-name ast) ast)))
        (let ((item-asts (mapcar (lambda (item)
                                   (if (or (symbolp item)
                                           (integerp item))
                                       (pop tag-asts)
                                       (convert item new-env)))
                                 items)))
          (process-progn
           (list (make-tagbody-ast :item-asts item-asts)
                 (convert-constant nil env)))))))

(defun convert-go (form env)
  (db (go tag) form
      (declare (ignore go))
      (let ((info (clcenv:tag-info env tag)))
        (make-go-ast :tag-ast (clcenv:info-identity info)))))

(defun convert-special-operator (form env)
  (let ((head (first form)))
    (multiple-value-bind (funcs found)
        (gethash head *special-operator-dispatch*)
      (if found
          (funcall (first funcs) form env)
          (compiler-error "Add support for special operator ~a" head)))))

(defun convert (form env)
  (let ((*current-form-is-top-level-p *subforms-are-top-level-p*)
        (*subforms-are-top-level-p* nil))
    (progn
      (when *code-walker*
        (setf form (funcall *code-walker* form env)))
      (if (atom form)
          (if (symbolp form)
              (let ((info (clcenv:variable-info env form)))
                (convert-form form info env))
              (convert-literal form env))
          (let ((head (car form))
                (rest (cdr form)))
            (cond
              ((treat-as-special-operator-p head)
               (convert-special-operator form env))
              ((and head (consp head) (eq (car head) 'cl:lambda))
               (convert `(funcall ,head ,@rest) env))
              ((and head (symbolp head))
               (convert-application form env))
              (t (error "Handle convert of ~a" form))))))))

(defun generate-ast (form env)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil))
    (convert form env)))

(defmacro with-preserved-toplevel-ness (&body body)
  `(progn (setf *subforms-are-top-level-p* *current-form-is-top-level-p*)
          ,@body))

