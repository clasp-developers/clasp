(in-package #:core)

;;;; This file defines a MACROEXPAND-ALL function.
;;;; MACROEXPAND-ALL is like MACROEXPAND, but it expands macros in subforms as well.
;;;; This is sometimes useful for debugging. Because it is basically just for
;;;; debugging, this function may not be entirely robust. It does go through some
;;;; effort to try to recreate the environments passed to macro functions just as
;;;; they would look during compilation, but there may be bugs.

;;;; Implementation notes:
;;;; Clasp deliberately avoids having its own special forms. As of this writing,
;;;; there are two, and one of those has a macroexpansion anyway. This makes
;;;; macroexpanding subforms a little easier, but as you can see, nontrivial.
;;;; We go through the CLTL2 accessors whenever possible - which is, happily, just
;;;; about everywhere - in an effort to future proof this against changes in
;;;; how environments work.
;;;; One part that's pretty ugly is declaration handling. This code has no provision
;;;; for understanding nonstandard declarations. Of course, those only really matter
;;;; if you're macroexpanding something and a macro function grovels through the
;;;; environment for them, which is rather rare.

(defgeneric macroexpand-cons (operator form environment))

(defun ext:macroexpand-all (form &optional env)
  (typecase form
    (symbol
     (multiple-value-bind (expansion expandedp) (macroexpand-1 form env)
       (if expandedp
           (values (ext:macroexpand-all expansion env) expandedp)
           (values form nil))))
    (cons (macroexpand-cons (car form) form env))
    (t (values form nil))))

(defmethod macroexpand-cons ((op (eql 'quote)) form env)
  (declare (ignore env))
  (values form nil))

(defun macroexpand-setq-1 (var valf env)
  (multiple-value-bind (vare exp?) (macroexpand-1 var env)
    (if exp?
        (values (ext:macroexpand-all `(setf ,vare ,valf) env) exp?)
        (multiple-value-bind (vale exp?) (ext:macroexpand-all valf env)
          (values `(setq ,var ,vale) exp?)))))

(defmethod macroexpand-cons ((op (eql 'setq)) form env)
  (let ((lform (length form)))
    (if (oddp lform)
        (if (= lform 3)
            (macroexpand-setq-1 (second form) (third form) env)
            (loop with expandedp = nil
                  for (var val) on (cdr form) by #'cddr
                  collect (multiple-value-bind (exp exp?)
                              (macroexpand-setq-1 var val env)
                            (setq expandedp (or expandedp exp?))
                            exp)
                    into subs
                  finally (return (values `(progn ,@subs) expandedp))))
        (error "Malformed SETQ"))))

(defmethod macroexpand-cons ((op (eql 'if)) form env)
  (destructuring-bind (condition then &optional (else nil elsep)) (cdr form)
    (multiple-value-bind (condition c?) (ext:macroexpand-all condition env)
      (multiple-value-bind (then t?) (ext:macroexpand-all then env)
        (if elsep
            (multiple-value-bind (else e?) (ext:macroexpand-all else env)
              (values `(if ,condition ,then ,else) (or c? t? e?)))
            (values `(if ,condition ,then) (or c? t?)))))))

(defmethod macroexpand-cons ((op (eql 'tagbody)) form env)
  (let* ((tags (remove-if-not (lambda (thing)
                                (or (symbolp thing) (integerp thing))) ; go tag
                              (cdr form)))
         (nenv (clasp-cltl2:augment-environment env :tag tags)))
    (loop with expandedp = nil
          for thing in (cdr form)
          if (or (symbolp thing) (integerp thing))
            collect thing into subs
          else
            collect (multiple-value-bind (sub ?) (ext:macroexpand-all thing nenv)
                      (setq expandedp (or expandedp ?))
                      sub)
              into subs
          finally (return (values `(tagbody ,@subs) expandedp)))))

(defmethod macroexpand-cons ((op (eql 'go)) form env)
  (declare (ignore env))
  (values form nil))

;;; Return a list of the macroexpanded forms, and a boolean indicating
;;; whether any expansion took place.
(defun macroexpand-seq (forms env)
  (loop with any-expanded-p = nil
        for form in forms
        collect (multiple-value-bind (expansion expandedp)
                    (ext:macroexpand-all form env)
                  (setq any-expanded-p (or any-expanded-p expandedp))
                  expansion)
          into expansions
        finally (return (values expansions any-expanded-p))))

(defmethod macroexpand-cons ((op (eql 'progn)) form env)
  (multiple-value-bind (subs expandedp) (macroexpand-seq (cdr form) env)
    (values `(progn ,@subs) expandedp)))

(defmethod macroexpand-cons ((op (eql 'block)) form env)
  (destructuring-bind (blockname . body) (rest form)
    (multiple-value-bind (ebody expandedp)
        (macroexpand-seq body
                         (clasp-cltl2:augment-environment env
                                                          :block (list blockname)))
      (values `(block ,blockname ,@ebody) expandedp))))

(defmethod macroexpand-cons ((op (eql 'return-from)) form env)
  (destructuring-bind (blockname &optional (subform nil subformp)) (rest form)
    (if subformp
        (multiple-value-bind (se exp) (ext:macroexpand-all subform env)
          (values `(return-from ,blockname ,se) exp))
        (values form nil))))

(defmethod macroexpand-cons ((op (eql 'catch)) form env)
  (multiple-value-bind (s expandedp) (macroexpand-seq (rest form) env)
    (values `(catch ,@s) expandedp)))

(defmethod macroexpand-cons ((op (eql 'throw)) form env)
  (destructuring-bind (tag sub) (rest form)
    (multiple-value-bind (etag eexp) (ext:macroexpand-all tag env)
      (multiple-value-bind (esub sexp) (ext:macroexpand-all sub env)
        (values `(throw ,etag ,esub) (or eexp sexp))))))

(defmethod macroexpand-cons ((op (eql 'progv)) form env)
  (multiple-value-bind (eforms eexp) (macroexpand-seq (rest form) env)
    (values `(progv ,@eforms) eexp)))

(defmethod macroexpand-cons ((op (eql 'unwind-protect)) form env)
  (multiple-value-bind (eforms eexp) (macroexpand-seq (rest form) env)
    (values `(unwind-protect ,@eforms) eexp)))

(defmethod macroexpand-cons ((op (eql 'load-time-value)) form env)
  (declare (ignore env))
  (values form nil))

(defmethod macroexpand-cons ((op (eql 'multiple-value-call)) form env)
  (multiple-value-bind (exps expandedp) (macroexpand-seq (cdr form) env)
    (values `(multiple-value-call ,@exps) expandedp)))

(defmethod macroexpand-cons ((op (eql 'multiple-value-prog1)) form env)
  (multiple-value-bind (exps expandedp) (macroexpand-seq (cdr form) env)
    (values `(multiple-value-prog1 ,@exps) expandedp)))

(defmethod macroexpand-cons ((op (eql 'the)) form env)
  (destructuring-bind (type sub) (cdr form)
    (multiple-value-bind (expansion expandedp) (ext:macroexpand-all sub env)
      (values `(the ,type ,expansion) expandedp))))

(defmethod macroexpand-cons ((op (eql 'eval-when)) form env)
  (destructuring-bind (situations . body) (cdr form)
    (multiple-value-bind (ebody expandedp) (macroexpand-seq body env)
      (values `(eval-when ,situations ,@ebody) expandedp))))

(defmethod macroexpand-cons ((op (eql 'locally)) form env)
  (multiple-value-bind (decls body) (core:process-declarations (rest form))
    (let ((nenv
            (clasp-cltl2:augment-environment env :declare decls)))
      (multiple-value-bind (ebody expandedp)
          (macroexpand-seq body nenv)
        (values `(locally (declare ,@decls) ,@ebody) expandedp)))))

(defmethod macroexpand-cons ((op (eql 'let)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (vars valfs)
        (loop for binding in bindings
              if (symbolp binding)
                collect binding into vars
                and collect nil into valfs
              else if (consp binding)
                     collect (car binding) into vars
                     and collect (second binding) into valfs
              else do (error "Malformed LET binding: ~s" binding)
              finally (return (values vars valfs)))
      (multiple-value-bind (evalfs valexp) (macroexpand-seq valfs env)
        (multiple-value-bind (decls body) (core:process-declarations body)
          (let ((nenv (clasp-cltl2:augment-environment env
                                                       :variable vars
                                                       :declare decls)))
            (multiple-value-bind (ebody bodyexp) (macroexpand-seq body nenv)
              (values `(let (,@(mapcar #'list vars evalfs))
                         (declare ,@decls)
                         ,@ebody)
                      (or bodyexp valexp)))))))))

;;; KLUDGE: Should be extensible or at least nicer.
(defun decls-applying-to-variable (decls var)
  (loop for thing in decls
        when (and (consp thing)
                  (case (car thing)
                    ((dynamic-extent ignorable ignore special)
                     (member var (cdr thing)))
                    ((type) (member var (cddr thing)))
                    ((ftype inline notinline optimize)) ; only for functions
                    (otherwise ; probably a type decl
                     (member var (cdr thing)))))
          collect thing))

(defmethod macroexpand-cons ((op (eql 'let*)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (decls body) (core:process-declarations body)
      (let* ((bindexp nil)
             (bindings
               (loop for binding in bindings
                     for var = (if (consp binding) (car binding) binding)
                     for valf = (if (consp binding) (second binding) 'nil)
                     for evalf = (multiple-value-bind (sub exp)
                                     (ext:macroexpand-all valf env)
                                   (when exp (setq bindexp t))
                                   sub)
                     collect `(,var ,evalf)
                     do (setf env (clasp-cltl2:augment-environment
                                   env
                                   :declare (decls-applying-to-variable decls var)
                                   :variable (list var)))))
             ;; this will redundantly apply bound declarations, but who cares
             (env (clasp-cltl2:augment-environment env :declare decls)))
        (multiple-value-bind (ebody ebodyexp) (macroexpand-seq body env)
          (values `(let* (,@bindings) (declare ,@decls) ,@ebody)
                  (or ebodyexp bindexp)))))))

(defmethod macroexpand-cons ((op (eql 'symbol-macrolet)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (decls body) (core:process-declarations body)
      (multiple-value-bind (ebody expandedp)
          (macroexpand-seq body (clasp-cltl2:augment-environment
                                 env :symbol-macro bindings :declare decls))
        (values `(locally (declare ,@decls) ,@ebody) expandedp)))))

(defmethod macroexpand-cons ((op (eql 'macrolet)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (decls body) (core:process-declarations body)
      (let* ((mbinds
               (loop for (name lambda-list . mbody) in bindings
                     for mlambda = (ext:parse-macro name lambda-list mbody env)
                     for expander = (clasp-cltl2:enclose mlambda env)
                     collect (list name expander)))
             (nenv
               (clasp-cltl2:augment-environment env :macro mbinds :declare decls)))
        (multiple-value-bind (ebody expandedp)
            (macroexpand-seq body nenv)
          (values `(locally (declare ,@decls) ,@ebody) expandedp))))))

(defun macroexpand-lambda (lambda-list body env)
  (multiple-value-bind (decls body doc) (core:process-declarations body)
    (multiple-value-bind (req opt rest keyflag keys aokp aux varestp)
        (core:process-lambda-list lambda-list 'function)
      (let* ((expandedp nil)
             (lambda-list
               `(,@(progn
                     (setf env (clasp-cltl2:augment-environment
                                env
                                :variable (cdr req)
                                :declare (loop for r in (cdr req)
                                               appending (decls-applying-to-variable decls r))))
                     (cdr req))
                 ,@(when (plusp (car opt)) '(&optional))
                 ,@(loop for (var default -p) on (cdr opt) by #'cdddr
                         for def = (multiple-value-bind (def ?)
                                       (ext:macroexpand-all default env)
                                     (when ? (setq expandedp t))
                                     def)
                         for vdecls = (decls-applying-to-variable decls var)
                         for pdecls = (when -p
                                        (decls-applying-to-variable decls -p))
                         collect (cond (-p `(,var ,def ,-p))
                                       (def `(,var ,def))
                                       (t var))
                         do (setf env (clasp-cltl2:augment-environment
                                       env
                                       :declare (append vdecls pdecls)
                                       :variable (if -p (list var -p) (list var)))))
                 ,@(when rest
                     (setf env (clasp-cltl2:augment-environment
                                env
                                :declare (decls-applying-to-variable decls rest)
                                :variable (list rest)))
                     (list (if varestp 'core:&va-rest '&rest) rest))
                 ,@(when keyflag `(&key))
                 ,@(loop for (key var default -p) on (cdr keys) by #'cddddr
                         for def = (multiple-value-bind (def ?)
                                       (ext:macroexpand-all default env)
                                     (when ? (setq expandedp t))
                                     def)
                         for vdecls = (decls-applying-to-variable decls var)
                         for pdecls = (when -p
                                        (decls-applying-to-variable decls -p))
                         for keyspec = (if (and (keywordp key) (string= key var))
                                           var
                                           (list key var))
                         collect (cond (-p `(,keyspec ,def ,-p))
                                       (def `(,keyspec ,def))
                                       (t (list keyspec)))
                         do (setf env (clasp-cltl2:augment-environment
                                       env
                                       :declare (append vdecls pdecls)
                                       :variable (if -p (list var -p) (list var)))))
                 ,@(when aokp `(&allow-other-keys))
                 ,@(unless (null aux) '(&aux))
                 ,@(loop for (var default) on aux by #'cddr
                         for def = (multiple-value-bind (def ?)
                                       (ext:macroexpand-all default env)
                                     (when ? (setq expandedp t))
                                     def)
                         for adecls = (decls-applying-to-variable decls var)
                         collect (cond (def `(,var ,def)) (t var))
                         do (setf env (clasp-cltl2:augment-environment
                                       env
                                       :declare adecls
                                       :variable (list var))))))
             ;; redundant declarations again, but again who cares
             (nenv (clasp-cltl2:augment-environment env :declare decls)))
        (multiple-value-bind (ebody bodyexp) (macroexpand-seq body nenv)
          (values `(lambda ,lambda-list
                     ,@(when doc (list doc))
                     (declare ,@decls)
                     ,@ebody)
                  (or expandedp bodyexp)))))))

(defun macroexpand-lexpr (lexpr env)
  (destructuring-bind (lambda-list . body) (rest lexpr)
    (macroexpand-lambda lambda-list body env)))

(defmethod macroexpand-cons ((op (eql 'function)) form env)
  (destructuring-bind (fnameoid) (rest form)
    (if (and (consp fnameoid) (eq (car fnameoid) 'lambda))
        (multiple-value-bind (lambda expandedp)
            (macroexpand-lexpr fnameoid env)
          (values `(function ,lambda) expandedp))
        (values form nil))))

(defmethod macroexpand-cons ((op (eql 'flet)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (decls body) (core:process-declarations body)
      (let* ((bexp nil)
             (bindings
               (loop for (name lambda-list . fbody) in bindings
                     for elambda = (multiple-value-bind (lambda exp)
                                       (macroexpand-lambda lambda-list fbody env)
                                     (when exp (setq bexp t))
                                     lambda)
                     collect (list* name (rest elambda))))
             (nenv (clasp-cltl2:augment-environment env
                                                    :function (mapcar #'car bindings)
                                                    :declare decls)))
        (multiple-value-bind (ebody eexp) (macroexpand-seq body nenv)
          (values `(flet (,@bindings) (declare ,@decls) ,@ebody) (or eexp bexp)))))))

;;; Declarations applying to function bodies in LABELS.
;;; Defined in the CLHS page for LABELS as inline, notinline, and ftype.
(defun decls-applying-to-labels-bodies (decls fnames)
  (loop for thing in decls
        for i = (and (consp thing)
                     (case (car thing)
                       ((inline notinline) (intersection (cdr thing) fnames))
                       ((ftype) (intersection (cddr thing) fnames))))
        when i
          collect (cons (car thing) i)))

(defmethod macroexpand-cons ((op (eql 'labels)) form env)
  (destructuring-bind (bindings . body) (rest form)
    (multiple-value-bind (decls body) (core:process-declarations body)
      (let* ((fnames (mapcar #'car bindings))
             (nenv (clasp-cltl2:augment-environment
                    env :function fnames
                        :declare (decls-applying-to-labels-bodies decls fnames)))
             (bexp nil)
             (bindings
               (loop for (name lambda-list . fbody) in bindings
                     for elambda = (multiple-value-bind (lambda exp)
                                       (macroexpand-lambda lambda-list fbody nenv)
                                     (when exp (setq bexp t))
                                     lambda)
                     collect (list* name (rest elambda))))
             (nenv (clasp-cltl2:augment-environment nenv :declare decls)))
        (multiple-value-bind (ebody eexp) (macroexpand-seq body nenv)
          (values `(labels (,@bindings) (declare ,@decls) ,@ebody)
                  (or eexp bexp)))))))

(defmethod macroexpand-cons ((op (eql 'core:foreign-call-pointer)) form env)
  (destructuring-bind (signature . args) (rest form)
    (multiple-value-bind (eargs eexp) (macroexpand-seq args env)
      (values `(core:foreign-call-pointer ,signature ,@eargs) eexp))))

(defmethod macroexpand-cons ((op (eql 'core::primop)) form env)
  (destructuring-bind (prim . args) (rest form)
    (multiple-value-bind (eargs eexp) (macroexpand-seq args env)
      (values `(,op ,prim ,@eargs) eexp))))

(defmethod macroexpand-cons ((op cons) form env)
  ;; lambda form
  (unless (eq (car op) 'lambda)
    (error "Malformed form: ~s" form))
  (multiple-value-bind (lambda lexp) (macroexpand-lexpr op env)
    (multiple-value-bind (aforms aexp) (macroexpand-seq (cdr form) env)
      (values `(,lambda ,@aforms) (or lexp aexp)))))

(defmethod macroexpand-cons ((op symbol) form env)
  ;; unknown operator, so try macroexpanding, or else it's a function call.
  (multiple-value-bind (expansion expandedp) (macroexpand form env)
    (if expandedp
        (values (ext:macroexpand-all expansion env) expandedp)
        (multiple-value-bind (args expandedp) (macroexpand-seq (cdr form) env)
          (values `(,op ,@args) expandedp)))))

(defmethod macroexpand-cons (op form env)
  (declare (ignore op env))
  (error "Malformed form: ~s" form))
