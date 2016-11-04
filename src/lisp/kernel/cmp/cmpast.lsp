(in-package :cmp)


(defstruct source-location)

(defun dfirst (x)
  (cond
    ((typep x 'source-location)
     (error "Handle source-location"))
    ((consp x)
     (first x))
    (t
     (error "Handle dfirst"))))

(defun drest (x)
  (cond
    ((typep x 'source-location)
     (error "Handle source-location"))
    ((consp x)
     (rest x))
    (t
     (error "Handle drest"))))


(defparameter *subforms-are-top-level-p* t)
(defparameter *current-form-is-top-level-p* t)

(defstruct ast origin)

(defstruct (if-ast (:include ast)) test-ast then-ast else-ast)

(defstruct (block-ast (:include ast)) body-ast)

(defstruct (return-from-ast (:include ast)) block-ast form-ast)

(defstruct (load-time-value-ast (:include ast)) form read-only-p)

(defstruct (progn-ast (:include ast)) form-asts)

(defstruct (tag-ast (:include ast)) name)

(defstruct (go-ast (:include ast)) tag-ast)

(defstruct (tagbody-ast (:include ast)) item-asts)

(defun raw (thing)
  thing)

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
                          (traverse (first tree) `(dfirst ,temp))
                          (traverse (rest tree) `(drest ,temp)))))))
        (traverse tree form)
        (reverse bindings)))))

(defmacro db (source-info-var tree form &body body)
  (let ((form-var (gensym)))
    `(let* ((,form-var ,form)
	    (,source-info-var (if (typep ,form-var 'source-location)
				  (location ,form-var)
				  nil))
	    ,@(destructure-variables tree form-var))
       (declare (ignorable ,source-info-var))
       ,@body)))

(defun convert-literal (form env)
  (make-load-time-value-ast :origin form
                            :form `(quote ,form)
                            :read-only-p t))

(defun convert-constant (form env)
  (make-load-time-value-ast :origin form
                            :form `(quote ,form)
                            :read-only-p t))


(defun ensure-args (form &key (min 0) max)
  (let ((rest (cdr form)))
    (when (< (length rest) min)
      (error "insufficient arguments for ~a" form))
    (when (and max (> (length rest) max))
      (error "too many arguments for ~a" form))))

(defun convert-if (form env)
  (ensure-args form :min 1 :max 3)
  (db origin (if test then else) form
      (make-if-ast :origin origin
                   :test-ast (convert test env)
                   :then-ast (convert then env)
                   :else-ast (convert else env))))

  

(defun convert-block (form env)
  (ensure-args form :min 2)
  (db origin (block name . body) form
      (or (symbolp name)
          (compiler-error "Block must have a symbol as second argument: ~a" form))
      (let* ((ast (make-block-ast :origin origin :body-ast nil))
             (new-env (clcenv:add-block env name ast)))
        (setf (block-ast-body-ast ast) (process-progn (convert-sequence body new-env))))))

(defun convert-return-from (form env)
  (ensure-args form :min 2 :max 3)
  (db origin (return-from block-name . rest) form
      (declare (ignore return-from))
      (let ((info (clcenv:block-info env block-name))
            (value-form (if (null rest) nil (first rest))))
        (make-return-from-ast :origin origin
                              :block-ast (clcenv:info-identity info)
                              :form-ast (convert value-form env)))))
             
(defun convert-sequence (forms env)
  (mapcar (lambda (form)
            (convert form env))
          forms))

(defun process-progn (asts &optional form)
  (if (null asts)
      (make-load-time-value-ast :form 'nil :read-only-p t)
      (make-progn-ast :form-asts asts :origin form)))

(defun convert-progn (form env)
  (process-progn (convert-sequence (cdr form) env) form))


(defun convert-tagbody (form env)
  (db origin (tagbody . items) form
      (let ((tag-asts (let (ta)
                        (dolist (sitem (raw items))
                          (let ((item (raw sitem)))
                            (when (or (symbolp item)
                                      (integerp item))
                              (push (make-tag-ast :name item :origin origin) ta))))
                        (nreverse ta)))
            (new-env env))
        (dolist (ast tag-asts)
          (setf new-env (clcenv:add-tag new-env (tag-ast-name ast) ast)))
        (let ((item-asts (mapcar (lambda (item)
                                   (if (or (symbolp (raw item))
                                           (integerp (raw item)))
                                       (pop tag-asts)
                                       (convert item new-env)))
                                 (raw items))))
          (process-progn
           (list (make-tagbody-ast :item-asts item-asts
                                   :origin origin)
                 (convert-constant nil env)))))))

(defun convert-go (form env)
  (db origin (go tag) form
      (declare (ignore go))
      (let ((info (clcenv:tag-info env (raw tag))))
        (make-go-ast :tag-ast (clcenv:info-identity info)
                     :origin origin))))

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
              (let ((info (variable-info env form)))
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


(generate-ast '(block foo 1 (if 1 2 3) 4 5 (tagbody (go a) a)) nil)
