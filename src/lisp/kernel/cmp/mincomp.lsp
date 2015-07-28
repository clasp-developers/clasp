
(in-package :cmp)

(defmacro mincmp-log (fmt &rest args)
  nil)

(defparameter *ast-node-children* (make-hash-table :test #'eq))
(defparameter *ast-node-dispatchers* nil)

(defmacro define-ast (name codegen vars &optional children)
  (let ((predicate-gs (gensym)))
    `(progn
       (defstruct (,name (:type vector) :named) ,@vars env form)
       (setf (gethash ',name *ast-node-children*) ',(mapcar (lambda (x) (intern (bformat nil "%s-%s" (string name) (string x)))) children))
       (let ((,predicate-gs (intern (bformat nil "%s-P" (string ',name)))))
         (push (list ,predicate-gs ',codegen) *ast-node-dispatchers*)))))

(defun ast-name (node) (aref node 0))
(define-ast constant-ast codegen-constant (constant) ())
(define-ast variable-get-ast codegen-variable-get (symbol classified) ())
(define-ast variable-set-ast codegen-variable-set (symbol classified) ())
(define-ast progn-ast codegen-progn (forms) (forms))
(define-ast call-ast codegen-call (function arguments) (arguments))
(define-ast if-ast codegen-if (cond then else) (cond then else))
(define-ast tagbody-ast codegen-tagbody (tags) (tags))
(define-ast tag-ast codegen-tag (name index block code) (code))
(define-ast dynamic-go-ast codegen-dynamic-go (tag depth index target-env))
(define-ast local-go-ast codegen-lexical-go (tag index target-env))
(define-ast let/let*-ast codegen-let/let* (operator new-env lambda-list-handler variables expressions code) (expressions code))
(define-ast function-ast codegen-function (name-or-lambda) ())
(define-ast setq-ast codegen-setq (variable expression) (variable expression))
(define-ast block-ast codegen-block (symbol forms) (forms))
(define-ast return-from-ast codegen-return-from (symbol inter-function block-env return-form) (return-form))
(define-ast multiple-value-call-ast codegen-multiple-value-call (function forms) (function forms))
(define-ast multiple-value-prog1-ast codegen-multiple-value-prog1 (form1 rest-forms) (form1 rest-forms))

(defun analyze-variable-get (node)
  (let ((sym (variable-get-ast-symbol node))
        (env (variable-get-ast-env node)))
    (bformat t "analyze-variable-get>>Analyze the one variable: %s\n" (variable-get-ast-symbol node))
    (let ((classified (irc-classify-variable env sym)))
      (setf (variable-get-ast-classified node) classified))))

(defun analyze-variable-set (node)
  (progn
  (bformat t "analyze-variable-set node: %s\n" node)
  (let* ((symbol (variable-set-ast-symbol node))
         (env (variable-set-ast-env node))
         (classified (irc-classify-variable env symbol)))
    (setf (variable-set-ast-classified node) classified))))

(defun analyze-variables (node)
  (cond
    ((variable-get-ast-p node)
     (analyze-variable-get node))
    ((variable-set-ast-p node)
     (analyze-variable-set node))
    (t    
     (let* ((struct-name (ast-name node))
            (child-accessors (gethash struct-name *ast-node-children*)))
       (bformat t "analyze-variables name: %s\n" struct-name)
       (bformat t "analyze-variables accessors: %s\n" child-accessors)
       (dolist (ca child-accessors)
         (let ((child-ren (funcall ca node)))
           (cond
             ((null child-ren))
             ((atom child-ren) (analyze-variables child-ren))
             ((consp child-ren) (dolist (c child-ren)
                                  (analyze-variables c)))
             (t (error "Illegal child-ren: ~a" child-ren)))))))))

(defun check-arguments (name min-args max-args form)
  (let ((form-length (length form)))
    (if (and (>= form-length min-args) (<= form-length max-args))
        nil
        (compiler-error
         (list* name form)
         "Wrong number of arguments for special operator ~a expected [~a,~a]"
         name min-args max-args))))


(defun mincomp-multiple-value-call (form env)
  (let ((function (mincomp (cadr form) env))
        (forms (mapcar (lambda (f) (mincomp f env)) (cddr form))))
    (make-multiple-value-call-ast :function function
                                  :forms forms
                                  :env env
                                  :form form)))

(defun mincomp-multiple-value-prog1 (form env)
  (let ((form1 (mincomp (cadr form) env))
        (rest-forms (mapcar (lambda (f) (mincomp f env)) (cddr form))))
    (make-multiple-value-prog1-ast :form1 form1
                                   :rest-forms rest-forms
                                   :env env
                                   :form form)))

(defun mincomp-block (form env)
  "codegen-block using the try macro"
  (let* ((rest (cdr form))
         (block-symbol (car rest))
         (body (cdr rest)))
    (let* ((block-env (make-block-environment block-symbol env)))
      (make-block-ast :symbol block-symbol
                      :forms (mincomp `(progn ,@body) block-env)
                      :env block-env
                      :form form))))

(defun mincomp-return-from (form env)
  (let* ((rest (cdr form))
         (block-symbol (car rest))
         (return-form (cadr rest)))
    (multiple-value-bind (recognizes-block-symbol inter-function block-env)
	(classify-return-from-symbol env block-symbol)
      (unless recognizes-block-symbol
        (error "Unrecognized block symbol ~a" block-symbol))
      (make-return-from-ast :symbol block-symbol
                            :inter-function inter-function
                            :block-env block-env
                            :return-form (mincomp return-form env)
                            :env env
                            :form form))))

(defun mincomp-let/let* (form env)
  (let ((operator (car form))
        (assignments (cadr form))
        (body (cddr form)))
    (multiple-value-bind (variables expressions)
        (separate-pair-list assignments)
      (multiple-value-bind (declares code docstring specials )
	    (process-declarations body t)
	  (cmp-log "About to create lambda-list-handler\n")
	  (dbg-set-current-debug-location-here)
	  (let* ((lambda-list-handler (make-lambda-list-handler variables declares 'core::function))
		 (new-env (make-value-environment-for-number-of-entries (number-of-lexical-variables lambda-list-handler) env))
                 (evaluate-env (if (eq operator 'let) env new-env)))
            (make-let/let*-ast :operator operator
                               :new-env new-env
                               :lambda-list-handler lambda-list-handler
                               :variables variables
                               :expressions (mapcar (lambda (e) (mincomp e evaluate-env)) expressions)
                               :code (mincomp `(progn ,@code) new-env)
                               :env env
                               :form form))))))

(defun mincomp-let (form env)
  (mincomp-let/let* form env))

(defun mincomp-let* (form env)
  (mincomp-let/let* form env))


(defun mincomp-function-def (operator-symbol function-env functions parent-env closure-env)
  "Create a closure for each of the function bodies in the flet/labels and put the closures into the activation frame in (result-af). (env) is the parent environment of the (result-af) value frame"
  (let ((result-af (irc-renv function-env)))
    (dbg-set-current-debug-location-here)
    (irc-intrinsic "makeFunctionFrame" result-af (jit-constant-i32 (length functions)) (irc-renv parent-env))
    ;;    )
    (cmp-log "About to generate code for args\n")
    (do* ((cur functions (cdr cur)))
	 ((endp cur) nil)
      (let* ((fn (car cur))
	     (fn-name (car fn))
	     #+(or)(fn-lambda `(ext::lambda-block ,fn-name ,@(cdr fn)))
	     (fn-lambda-list (cadr fn))
	     (fn-raw-body (cddr fn))
	     (fn-lambda (generate-lambda-block fn-name fn-lambda-list fn-raw-body))
	     (fn-classified (classify-function-lookup function-env fn-name))
	     (fn-index (or (cadddr fn-classified) (error "Could not find lexical function ~a" fn-name)))
	     (target (irc-intrinsic "functionFrameReference" result-af (jit-constant-i32 fn-index)
			       (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))

(defun mincomp-flet/labels (form env)
  (let ((operator (car form))
        (function-sexps (cadr form))
        (body (cddr form)))
    (let ((function-env (make-function-value-environment (length function-sexps) env)))
      (multiple-value-bind (declares code docstring specials)
	  (process-declarations body nil) ;; don't expect docstring
	(let ((evaluate-env (cond
			      ((eq operator-symbol 'flet) env)
			      ((eq operator-symbol 'labels) function-env)
			      (t (error "flet/labels doesn't understand operator symbol[~a]" operator-symbol))))
	      traceid
              (functions (mapcar (fe) (mincomp-function-def fe evaluate-env)) function-sexps))
          (make-flet/labels-ast :operator operator
                                :new-env function-env
                                :functions functions
                                :code (mincomp `(progn ,@code) function-env)
                                :env env
                                :form form))))))

(defun mincomp-flet (form env)
  (codegen-flet/labels form env))

(defun mincomp-labels (form env)
  (codegen-flet/labels form env))






(defun mincomp-function (form env)
  "Return IR code for a function or closure"
  (let ((name-or-lambda (cadr form)))
    (make-function-ast :name-or-lambda name-or-lambda
                       :env env
                       :form form)))

(defun mincomp-setq (form env)
  "Carry out setq for a collection of pairs"
  (let ((assignments (cdr form))
        variables expressions)
    (unless (evenp (length assignments))
      (compiler-error (cdr form) "Wrong number of arguments for setq"))
    (multiple-value-bind (variables expressions)
        (do* ((cur (cdr form) (cddr cur))
              (var (car cur) (car cur))
              (exp (cadr cur) (cadr cur))
              (vars nil)
              (exps nil))
             ((null cur) (values (nreverse vars) (nreverse exps)))
          (unless (symbolp var)
            (compiler-error var "setq target needs to be a symbol"))
          (push var vars)
          (push exp exps))
      (bformat t "variables = %s\n" variables)
      (bformat t "expressions = %s\n" expressions)
      (let ((varlen (length variables)))
        (cond
          ((= varlen 0)
           (mincomp nil env))
          ((= varlen 1)
           (bformat t "varlen 1\n")
           (let* ((var (car variables))
                  (exp (car expressions))
                  (expanded (macroexpand var env)))
             (if (eq expanded var)
                 ;; The variable didn't expand so use setq
                 (make-setq-ast :variable (make-variable-set-ast
                                           :symbol var
                                           :env env
                                           :form form)
                                :expression (mincomp exp env)
                                :env env
                                :form form)
                 (mincomp `(progn (setf ,expanded ,exp)) env))))
          (t
           (mincomp `(progn
                       ,@(mapcar (lambda (v e) `(setq ,v ,e)) 
                                 variables expressions))
                    env)))))))

(defun mincomp-if (form env)
  "See Kaleidoscope example for if/then/else"
  (let ((icond (cadr form))
	(ithen (caddr form))
	(ielse (cadddr form)))
    (check-arguments 'if 3 4 form)
    (make-if-ast :cond (mincomp icond env)
                 :then (mincomp ithen env)
                 :else (mincomp ielse env)
                 :env env
                 :form form)))

(defun extract-section (begin end tagbody-env tag-next)
  "Extract a section of a list from begin up to but not including end"
  (let (result)
    (do* ((cur begin (cdr cur)))
         ((eq cur end))
      (push (car cur) result))
    (when tag-next (push `(go ,tag-next) result))
    (nreverse result)))

(defun tagbody.enumerate-tags (code tagbody-env)
  (let (result (index 0))
    ;; Create a tag for each symbol and at first associate the rest of the code
    ;; with that tag
    (mapl #'(lambda (x)
              (mincmp-log "first x: %s\n" x)
	      (if (and (car x) (symbolp (car x)))
		  (progn
                    (setq result (cons (make-tag-ast :name (car x)
                                                     :index index 
                                                     :code x
                                                     :env tagbody-env) result))
                    (when tagbody-env (add-tag tagbody-env (car x) result))
                    (setq index (+ 1 index)))))
          code)
    (setf result (nreverse result))
    ;; Now extract the code that belongs to each section
    (mapl (lambda (cur)
            (let* ((tag-cur (car cur))
                   (tag-next (cadr cur))
                   (code-cur (tag-ast-code tag-cur))
                   (code-next (when tag-next (tag-ast-code tag-next)))
                   (next-tag-name (when tag-next (tag-ast-name tag-next))))
              (setf (tag-ast-code tag-cur) (mincomp-progn `(progn ,@(extract-section (cdr code-cur) code-next tagbody-env next-tag-name)) tagbody-env))))
          result)
    result))

(defun mincomp-tagbody (form env)
  "Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  ;; stick a dummy tag at the head if there isn't one
  (let ((rest (cdr form)))
    (unless (and (car rest) (symbolp (car rest))) (push (gensym) rest))
    (let* ((tagbody-env (make-tagbody-environment env))
           (enumerated-tags (tagbody.enumerate-tags rest tagbody-env)))
      (mincmp-log "enumerated-tags: %s\n" enumerated-tags)
      ;; If the GO spec.ops. are in the same function
      ;; we could use simple cleanup and branches for TAGBODY/GO
      ;; so save the function
      (setf-metadata tagbody-env 'tagbody-function *current-function*)
      (make-tagbody-ast :tags enumerated-tags :env tagbody-env :form form))))

(defun mincomp-go (form env)
  (let* ((tag (car (cdr form)))
	 (classified-tag (classify-tag env tag)))
    (cond
      ((and classified-tag (eq (car classified-tag) 'dynamic-go))
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag)))
         (make-dynamic-go-ast :tag tag :depth depth :index index :env env)))
      ((and classified-tag (eq (car classified-tag) 'local-go))
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag))
	     (tagbody-env (cadddr classified-tag)))
	 (cmp-log "Target tagbody environment: %s  tag: %s\n" tagbody-env tag)
         (make-local-go-ast :tag tag :index index :target-env tagbody-env :env env :form form)))
      (t (error "go to unknown classified tag ~a ~a" tag classified-tag)))))



(defun mincomp-progn (form env)
  "Evaluate forms discarding results but keep last one"
  (cmp-log "mincomp-progn %s\n" form)
  (make-progn-ast :forms (mapcar #'(lambda (f) (mincomp f env)) (cdr form)) :env env :form form))

(defun mincomp-call (form evaluate-env)
  (mincmp-log "mincomp-call form: %s\n" form)
  (make-call-ast :function (car form)
                 :arguments (mapcar (lambda (arg) (mincomp arg evaluate-env)) (cdr form))
                 :env evaluate-env
                 :form form))

(defun mincomp-application (form env)
  "A compiler macro function, macro function or a regular function"
  (mincmp-log "mincomp-application form: %s\n" form)
  (cond
    ;; A compiler macro
    ((and ;;(symbolp (car form))
      (not (core:lexical-function (car form) env))
      (not (core:lexical-macro-function (car form) env))
      (not (core:declared-global-notinline-p (car form)))
      (let ((expansion (core:compiler-macroexpand form env)))
        (if (eq expansion form)
            nil
            (mincomp expansion env)))))
    ;; A regular macro
    ((and (symbolp (car form))
          (not (core:lexical-function (car form) env))
          (macro-function (car form) env))
     (bformat t "regular macro\n")
     (multiple-value-bind (expansion expanded-p)
         (macroexpand form env)
       (mincomp expansion env)))
    ;; It's a regular function call
    (t (mincomp-call form env))))

(defun mincomp-special-operator (form env)
  (let* ((functions (gethash (car form) *special-operator-dispatch* 'nil))
         (function (car functions)))
    (if function
	(funcall function form env)
	form))) ;; later convert this to an error once all special operators are handled

(defun mincomp-symbol-value (symbol env)
  (if (keywordp symbol)
      symbol
      (let ((expanded (macroexpand symbol env)))
	(if (eq expanded symbol)
	    ;; The symbol is unchanged, look up its value
	    (make-variable-get-ast :symbol symbol :env env)
	    ;; The symbol was a symbol-macro - evaluate it
	    (mincom expanded env)))))

(defun mincomp (form env)
  (declare (optimize (debug 3)))
  (let* ((*current-form* form)
         (*current-env* env))
    ;; If a *code-walker* is defined then invoke the code-walker
    ;; with the current form and environment
    (cmp-log "mincomp form: %s\n" form)
    ;;
    ;; If a *code-walker* is defined then invoke the code-walker
    ;; with the current form and environment
    (when *code-walker*
      (setq form (funcall *code-walker* form env)))
    (if (atom form)
        (if (symbolp form)
            (mincomp-symbol-value form env)
            (progn
              (cmp-log "make-constant :constant %s\n" form)
              (make-constant-ast :constant form :env env)))
        (let ((head (car form))
              (rest (cdr form)))
          (cond
            ((treat-as-special-operator-p head)
             (mincomp-special-operator form env))
            ((and head (symbolp head))
             (mincomp-application form env))
            (t (error "Handle mincomp of cons: ~a" form)))))))
