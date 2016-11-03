(in-package :cmp)



(defparameter *subforms-are-top-level-p* t)
(defparameter *current-form-is-top-level-p* t)

(defstruct ast origin)

(defstruct (if-ast (:include ast)) test-ast then-ast else-ast)

(defstruct (block-ast (:include ast)) body-ast)

(defstruct (return-from-ast (:include ast)) block-ast form-ast)

(defstruct (load-time-value-ast (:include ast)) form read-only-p)

(defstruct (progn-ast (:include ast)) form-asts)




(defun convert-literal (form env)
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
  (let ((rest (cdr form)))
    (make-if-ast :origin form
                 :test-ast (convert (first rest) env)
                 :then-ast (convert (second rest) env)
                 :else-ast (convert (third rest) env))))

  

(defun convert-block (form env)
  (ensure-args form :min 2)
  (let ((block-symbol (second form)))
    (or (symbolp block-symbol)
        (compiler-error "Block must have a symbol as second argument: ~a" form))
    (destructuring-bind (block name . body ) form
      (let* ((ast (make-block-ast :origin form :body-ast nil))
             (new-env (clcenv:add-block env name ast)))
        (setf (block-ast-body-ast ast) (process-progn (convert-sequence body new-env)))))))

(defun convert-return-from (form env)
  (ensure-args form :min 2 :max 3)
  (destructuring-bind (return-from block-name . rest) form
    (declare (ignore return-from))
    (let ((info (clcenv:block-info env block-name))
          (value-form (if (null rest) nil (first rest))))
      (make-return-from-ast :origin form
                            :block-ast (clcenv:identity info)
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


(generate-ast '(block foo 1 (if 1 2 3) 4 5) nil)
