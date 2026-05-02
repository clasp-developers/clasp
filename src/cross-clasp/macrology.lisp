(in-package #:cross-clasp.clasp.core)

(defmacro %pprint-logical-block
    ((stream-symbol object
      &key (prefix "" prefix-p) (per-line-prefix "" per-line-prefix-p) (suffix "" suffix-p))
     &body body)
  (inravina:expand-logical-block 'cross-clasp.clasp.incless-intrinsic:*client* stream-symbol
                                 object prefix prefix-p per-line-prefix per-line-prefix-p
                                 suffix suffix-p 'pprint-exit-if-list-exhausted 'pprint-pop nil
                                 body))

(defmacro with-unique-names (symbols &body body)
  `(let* ,(mapcar (lambda (symbol)
                    (let* ((symbol-name (symbol-name symbol))
                           (stem symbol-name))
                      `(,symbol (gensym ,stem))))
                  symbols)
     ,@body))

(defmacro with-clean-symbols (symbols &body body)
  "Rewrites the given forms replacing the given symbols with uninterned
ones, which is useful for creating hygienic macros."
  `(progn ,@(sublis (mapcar #'(lambda (s) (cons s (make-symbol (symbol-name s))))
			    symbols)
		    body)))

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*

Create a Let* which evaluates each Value-Expression, binding a
temporary variable to the result, and wrapping the Let* around the
result of the evaluation of Body.  Within the body, each Var is bound
to the corresponding temporary variable.

Bare symbols in `specs' are equivalent to:

  (symbol symbol)

Example:

  (defmacro cons1 (x)
    (once-only (x) `(cons ,x ,x)))
  (let ((y 0))
    (cons1 (incf y)))
  ; => (1 . 1)
"
  (labels ((frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   (cond ((atom spec)
                          (setf spec (list spec spec)))
                         ((/= (length spec) 2)
                          (error "Malformed Once-Only binding spec: ~S." spec)))
                   (let ((name (first spec))
                         (exp-temp (gensym)))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "OO-")))
                        (list 'let (list (list ,name ,exp-temp))
                              ,(frob (rest specs) body))
                        #+(or) ; can't use host quasiquote
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))
    (frob specs body)))

(defmacro defconstant-eqx (name form test &rest rest)
  `(defconstant ,name
     (let ((value ,form))
       (cond ((not (boundp ',name)) value)
             ((,test (symbol-value ',name) value)
              (symbol-value ',name))
             (t value))) ; probably error
     ,@rest))

(defmacro defconstant-equal (name form &rest rest)
  `(defconstant-eqx ,name ,form equal ,@rest))

(defun process-declarations (body &optional docstringp)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation docstringp)
    (let* ((decls (mapcan (lambda (L) (copy-list (cdr L))) decls))
           (specials
             (loop for thing in decls
                   when (and (consp thing)
                             (eq (car thing) 'special))
                     append (cdr thing))))
      (values decls body doc specials))))

(defun find-declarations (body &optional (docp t))
  (multiple-value-bind (decls body doc)
      (process-declarations body docp)
    (values (if decls `((declare ,@decls)) nil)
           body doc)))

(defun dm-too-many-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-many))

(defun dm-too-few-arguments (current-form vl macro-name)
  (error 'destructure-wrong-number-of-arguments
         :macro-name macro-name :lambda-list vl :arguments current-form
         :problem :too-few))

(defun function-block-name (fname)
  (etypecase fname
    (symbol fname)
    ((cons (eql setf) (cons symbol null)) (second fname))))

(defun process-lambda-list (lambda-list context)
  (ecase context
    ((function)
     (multiple-value-bind (required optional rest keys aokp aux keyp)
         (alexandria:parse-ordinary-lambda-list lambda-list)
       (values (list* (length required) required)
               (list* (length optional)
                      (loop for (var def -p) in optional
                            collect var collect def collect -p))
               rest keyp
               (list* (length keys)
                      (loop for ((var key) def -p) in keys
                            collect var collect key collect def collect -p))
               aokp
               (loop for (var def) in aux collect var collect def)
               ;; varest-p
               nil)))))

;;; So that parsed macros/whatever can be used in the host w/o complaint.
(declaim (declaration lambda-name lambda-list))

(defun cross-clasp.clasp.ext:parse-macro (name lambda-list body &optional env)
  (ecclesia:parse-macro-using-canonicalization name lambda-list body env
                                               `((lambda-name (macro-function ,name))
                                                 (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-compiler-macro
    (name lambda-list body &optional env)
  (ecclesia:parse-compiler-macro-using-canonicalization
   name lambda-list body env
   `((lambda-name (compiler-macro-function ,name))
     (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-deftype (name lambda-list body &optional env)
  (ecclesia:parse-deftype name lambda-list body env
                          `((lambda-name (cross-clasp.clasp.ext:type-expander ,name))
                            (lambda-list ,@lambda-list))))

(defun cross-clasp.clasp.ext:parse-define-setf-expander
    (name lambda-list body &optional env)
  (ecclesia:parse-macro name lambda-list body env
                        `((lambda-name (cross-clasp.clasp.ext:setf-expander ,name))
                          (lambda-list ,@lambda-list))))

(defmacro while (condition &body body) `(loop while ,condition do (progn ,@body)))
(defmacro until (condition &body body) `(loop until ,condition do (progn ,@body)))

(defmacro %defun (&whole whole name lambda-list &body body)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation t :whole whole)
    `(progn
       (eval-when (:compile-toplevel)
         (cross-clasp.clasp.cmp::register-global-function-def 'defun ',name))
       (setf (fdefinition ',name)
             (lambda ,lambda-list
               (declare (lambda-name ,name))
               ,@decls
               ,@(when doc (list doc))
               (block ,(function-block-name name) ,@body)))
       ',name)))

;;; We avoid clobbering build macros while building Clasp itself, because that
;;; makes it easier to deal with bootstrapping weirdness.
;;; But when we build libraries we need the full Clasp macros.
;;; So, during build we store macro functions in this variable, and then load
;;; them once Clasp is complete enough.
(defvar *delayed-macros* (make-hash-table))

(defun delay-macro (name expander)
  (setf (gethash name *delayed-macros*) expander))

(defun reset-delayed-macros () (clrhash *delayed-macros*))

(defmacro %defmacro (name lambda-list &body body &environment env)
  (let ((lexpr (cross-clasp.clasp.ext:parse-macro name lambda-list body env)))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((expander #',lexpr))
           (if (macro-function ',name)
               (delay-macro ',name expander)
               (setf (macro-function ',name) expander))))
       (eval-when (:load-toplevel :execute)
         (setf (macro-function ',name) #',lexpr))
       ',name)))

(defmacro %define-compiler-macro (name lambda-list &body body &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compiler-macro-function ',name)
           #',(cross-clasp.clasp.ext:parse-compiler-macro name lambda-list body env))
     ',name))

(defmacro %deftype (name lambda-list &body body &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (cross-clasp.clasp.ext:type-expander ',name)
           #',(cross-clasp.clasp.ext:parse-deftype name lambda-list body env))
     ',name))

(defmacro %define-setf-expander (name lambda-list &body body &environment env)
  (let ((lexpr (cross-clasp.clasp.ext:parse-define-setf-expander
                name lambda-list body env)))
    `(progn
       (eval-when (:compile-toplevel)
         (unless (cross-clasp.clasp.ext:setf-expander ',name)
           (setf (cross-clasp.clasp.ext:setf-expander ',name) #',lexpr)))
       (eval-when (:load-toplevel :execute)
         (setf (cross-clasp.clasp.ext:setf-expander ',name) #',lexpr))
       ',name)))

;;; Dummy functions, since we don't need to record source locations
;;; during build.
(defun variable-source-info (var)
  (declare (ignore var))
  nil)
(defun (setf variable-source-info) (info var)
  (declare (ignore var))
  info)

(defmacro %defvar (name &optional (value nil valuep) doc)
  `(progn
     (declaim (special ,name))
     ,@(when valuep
         `((unless (boundp ',name)
             (setf (symbol-value ',name) ,value))))
     ,@(when (cross-clasp.clasp.ext:current-source-location)
         `((setf (variable-source-info ',name)
                 ',(cross-clasp.clasp.ext:current-source-location))))
     ,@(when doc
         `((cross-clasp.clasp.ext:annotate ',name 'documentation 'variable ,doc)))
     ',name))

(defmacro %defparameter (name value &optional doc)
  `(progn
     (declaim (special ,name))
     (setf (symbol-value ',name) ,value)
     ,@(when (cross-clasp.clasp.ext:current-source-location)
         `((setf (variable-source-info ',name)
                 ',(cross-clasp.clasp.ext:current-source-location))))
     ,@(when doc
         `((cross-clasp.clasp.ext:annotate ',name 'documentation 'variable ,doc)))
     ',name))

;;; These are in common-macros, but they use their own condition classes &c.

;;; Process a t/otherwise clause into an unambiguous normal clause.
(defun remove-otherwise-from-clauses (clauses)
  (mapcar #'(lambda (clause)
	      (let ((options (first clause)))
		(if (member options '(t otherwise))
		    (cons (list options) (rest clause))
		    clause)))
	  clauses))

(defun accumulate-cases (clauses)
  (loop for (mems) in clauses
        when (listp mems) append mems
          else collect mems))

(defmacro %ccase (keyplace &rest clauses)
  (let* ((key (gensym))
	 (repeat (gensym))
	 (block (gensym))
         (clauses (remove-otherwise-from-clauses clauses)))
    `(block ,block
       (tagbody ,repeat
	  (let ((,key ,keyplace))
	    (return-from ,block
	      (case ,key ,@clauses
	            (t (setf ,keyplace
			     (ccase-error ',keyplace ,key
					  ',(accumulate-cases clauses)))
		     (go ,repeat)))))))))
(defmacro %ecase (keyform &rest clauses)
  (let ((key (gensym))
        (clauses (remove-otherwise-from-clauses clauses)))
    `(let ((,key ,keyform))
       (case ,key ,@clauses
	     (t (ecase-error ,key ',(accumulate-cases clauses)))))))

(defmacro %etypecase (keyform &rest clauses)
  (let ((key (gensym)))
    `(let ((,key ,keyform))
       (cond ,@(loop for (type . body) in clauses
                     collect `((typep ,key ',type) ,@body))
             (t (etypecase-error ,key ',(mapcar #'car clauses)))))))

(defmacro %ctypecase (keyplace &rest clauses)
  (let ((key (gensym)))
    `(loop with ,key = ,keyplace
           do (cond ,@(loop for (type . body) in clauses
                            collect `((typep ,key ',type)
                                      (return (progn ,@body)))))
              (setf ,keyplace
                    (ctypecase-error ',keyplace ,key
                                     ',(mapcar #'car clauses))))))

;; proper-list-p code from Robert Strandh's Cleavir code
(defun proper-list-p (object)
  (cond  ((null object) t)
         ((atom object) nil)
         (t (let ((slow object)
                  (fast (cdr object)))
              (declare (type cons slow))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (when (eq fast slow)
                   (return-from proper-list-p nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (go again))))))

  ;;; Some operators "should signal a type error", meaning that in safe code
  ;;; they _must_ signal a type error, and otherwise the behavior is undefined.
  ;;; The bytecode compiler is not smart enough to do this in a nuanced way,
  ;;; in that it just ignores THE rather than type checking (which is allowed),
  ;;; but Cleavir's is. So to implement this behavior we use
  ;;; this THE-SINGLE macro.
  ;;; The bytecode will have an actual call to a %the-single function,
  ;;; defined below, which just does a type test. So in safe and unsafe code
  ;;; there is a test.
  ;;; When compiling with Cleavir, this call will be transformed into a THE,
  ;;; so there's no actual call and the compiler can do its usual processing
  ;;; on THE forms based to the safety level.
  ;;; This setup also means we extract only the primary value, so e.g.
  ;;; (+ (values 1 2)) => 1 and not 1 2 as we want. Additionally it properly
  ;;; removes toplevelness.
(defmacro the-single (type form &optional (return nil returnp))
  (if returnp
      `(%the-single-return ',type (values ,form) ,return)
      `(%the-single ',type (values ,form))))

(defun %the-single (type value)
  (unless (typep value type)
    (error 'type-error :datum value :expected-type type))
  value)

(defun %the-single-return (type value return)
  (unless (typep value type)
    (error 'type-error :datum value :expected-type type))
  return)

(defun simple-associate-args (fun first-arg more-args)
  (or more-args (error "more-args cannot be nil"))
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,fun ,first-arg ,arg)
        (simple-associate-args fun `(,fun ,first-arg ,arg) next))))

(defun expand-associative (fun two-arg-fun args identity
                           &optional (one-arg-result-type 'number))
  (declare (ignore fun))
  (case (length args)
    (0 identity)
    ;; Note that we only use the type information in the one argument
    ;; case because we need that check. With more arguments, the two-arg-fun
    ;; will do checks. This also applies to EXPAND-COMPARE below.
    (1 `(the-single ,one-arg-result-type ,(first args)))
    (2 (values `(,two-arg-fun ,@args) t))
    (t (simple-associate-args two-arg-fun (first args) (rest args)))))

(defun simple-compare-args (fun first-arg more-args)
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,fun ,first-arg ,arg)
        `(if (,fun ,first-arg ,arg)
             ,(simple-compare-args fun arg next)
             nil))))

(defun expand-compare (form fun args &optional (arg-type 't))
  (if (proper-list-p args)
      (case (length args)
        ((0)
         ;; need at least one argument. FIXME: warn?
         form)
        ((1)
         ;; preserve nontoplevelness and side effects
         `(the-single ,arg-type ,(first args) t))
        ((2)
         `(,fun ,(first args) ,(second args)))
        (otherwise
         ;; Evaluate arguments only once
         (let ((syms (mapcar (lambda (a) (declare (ignore a)) (gensym)) args)))
           `(let (,@(mapcar #'list syms args))
              ,(simple-compare-args fun (first syms) (rest syms))))))
      ;; bad syntax. warn?
      form))

;; /=, char/=, and so on have to compare every pair.
;; In general this results in order n^2 comparisons, requiring a loop etc.
;; For now we don't do that, and only inline the 1 and 2 arg cases.
(defun expand-uncompare (form fun args &optional (arg-type 't))
  (if (proper-list-p args)
      (case (length args)
        ((1)
         ;; preserve nontoplevelness and side effects.
         `(the-single ,arg-type ,(first args) t))
        ((2) `(not (,fun ,(first args) ,(second args))))
        (otherwise form))
      form))
