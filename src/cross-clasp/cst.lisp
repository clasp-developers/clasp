(in-package #:cross-clasp.clasp.concrete-syntax-tree)

;;;; Give environment-friendly definitions of some CST macros.

(defun transform (sourcef form)
  (typecase form
    ;; We can use CL:LIST instead of building a CST because TRANSFORM always
    ;; returns a form used as a non-final argument to %APPEND.
    (cl:atom `(cl:list (%quote ,sourcef ',form)))
    ((cl:cons (eql unquote)) `(cl:list ,(cl:second form)))
    ((cl:cons (eql unquote-splicing)) (cl:second form))
    (t `(cl:list ,(appender sourcef form)))))

(defun transform-compound (sourcef object)
  (labels ((rec (object)
             (typecase object
               ((cl:cons t cl:atom) ; (a . b)
                (cl:list (transform sourcef (cl:car object))
                         `(%quote ,sourcef ',(cl:cdr object))))
               ((cl:cons t (cl:cons (eql unquote))) ; (a . ,b)
                (cl:list (transform sourcef (cl:car object))
                         (cl:second (cl:cdr object))))
               ((cl:cons t (cl:cons (eql unquote-splicing))) ; (a . ,@b)
                (error "unquote-splicing-in-dotted-list"))
               (t (cl:list* (transform sourcef (cl:car object))
                            (rec (cl:cdr object)))))))
    (rec object)))

(defun appender (sourcef argument)
  ;; We could do some optimization here - transforming to a %LIST*, etc.
  `(%append ,sourcef ,@(transform-compound sourcef argument)))

(defun transform-qq-argument (sourcef argument)
  (if (cl:atom argument)
      `(%quote ,sourcef ',argument)
      (case (cl:car argument)
        ((unquote) (cl:second argument))
        ((unquote-splicing) (error "unquote-splicing-at-top"))
        (t (appender sourcef argument)))))

(defmacro quasiquote (sourcef argument)
  (let ((gsource (gensym "SOURCE")))
    `(let ((,gsource ,sourcef))
       ,(transform-qq-argument gsource argument))))

(defun destructure-variables (tree form)
  (let ((bindings '())
        (body-forms '()))
    (labels ((traverse (sub-tree sub-form)
               (cond ((cl:null sub-tree)
                      (push `(%null-or-lose ,sub-form ,form ',tree)
                            body-forms))
                     ((symbolp sub-tree)
                      (push `(,sub-tree ,sub-form) bindings))
                     ((not (cl:consp sub-tree))
                      (error "expectetree-but-found ~a" sub-tree))
                     (t
                      (let ((temp (gensym)))
                        (push `(,temp ,sub-form) bindings)
                        (traverse (cl:first sub-tree)
                                  `(%first-or-lose ,temp ,form ',tree))
                        (traverse (cl:rest sub-tree)
                                  `(%rest-or-lose ,temp ,form ',tree)))))))
      (traverse tree form))
    (values (reverse bindings) (nreverse body-forms))))

(defmacro db (source-var tree form &body body)
  ;; We use the DUMMY-VAR hack so we can execute BODY-FORMS after
  ;; BINDINGS but before BODY without messing with BODY's
  ;; declarations.
  (let ((form-var (gensym)) (dummy-var (gensym)))
    (multiple-value-bind (bindings body-forms)
        (destructure-variables tree form-var)
      `(let* ((,form-var ,form)
              (,source-var (source ,form-var))
              ,@bindings
              (,dummy-var ,@body-forms))
         (declare (ignorable ,source-var ,dummy-var))
         ,@body))))
