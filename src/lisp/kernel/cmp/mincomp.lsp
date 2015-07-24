
(in-package :cmp)

(defmacro mincmp-log (fmt &rest args)
  nil)

(defvar *ast-node-children* (make-hash-table :test #'eq))

(defstruct (ast (:type vector)) env form)
(defmacro define-ast (name vars &optional children)
  `(progn
     (defstruct (,name (:type vector) (:include ast) :named) ,@vars)
     (setf (gethash ',name *ast-node-children*) ',(mapcar (lambda (x) (intern (bformat nil "%s-%s" (string name) (string x)))) children))))

(define-ast constant-ast (constant) ())
(define-ast variable-ast (symbol classified) ())
(define-ast progn-ast (forms) (forms))
(define-ast call-ast (function arguments) (arguments))
(define-ast if-ast (cond then else) (cond then else))
(define-ast tagbody-ast (tags) (tags))
(define-ast tag-ast (name index block code) (code))
(define-ast dynamic-go-ast (tag depth index target-env))
(define-ast local-go-ast (tag index target-env))
(define-ast let-ast (variables expressions bindings body) (bindings body))
(define-ast let*-ast (variables expressions bindings body) (bindings body))

(defun analyze-one-variable (node)
  (bformat t "Analyze the one variable: %s\n" (node-symbol node)))

(defun analyze-variables (node)
  (if (variable-ast-p node)
      (analyze-variable node)
      (let ((child-accessors (gethash (aref node 0) *ast-node-children*)))
        (dolist (ca child-accessors)
          (analyze-variables (funcall ca node))))))









(defun check-arguments (name min-args max-args form)
  (let ((form-length (length form)))
    (if (and (>= form-length min-args) (<= form-length max-args))
        nil
        (compiler-error (list* name form) "Wrong number of arguments for special operator ~a expected [~a,~a]" name min-args max-args))))


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
		 (new-env (irc-new-unbound-value-environment-of-size
			   env
			   :number-of-arguments (number-of-lexical-variables lambda-list-handler) ;; lambda-list-handler lambda-list-handler
			   :label (symbol-name operator))))
            (cond
              ((eq operator 'let) (make-let-ast :variables variables :expressions (mapcar (lambda (e) (mincomp e env)) expressions) :env new-env :form form))
              ((eq operator 'let*) (make-let*-ast :variables variables :expressions (mapcar (lambda (e) (mincomp e new-env)) expressions) :env new-env :form form))
              (t (error "let/let* doesn't understand operator symbol[~a]" operator-symbol))))))))

(defun mincomp-let (form env)
  (mincomp-let/let* form env))

(defun mincomp-let* (form env)
  (mincomp-let/let* form env))





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
    (let* ((tagbody-env (irc-new-tagbody-environment env))
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
	    (make-variable-ast :symbol symbol :env env)
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
