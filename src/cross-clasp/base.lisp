(in-package #:cross-clasp)

;;; Install stuff in an environment to match what we need for build.
;;; To paper over a lot of details: many macros, and then what functions
;;; are defined in C++ and thus available before any Lisp is loaded.

(defmethod maclina.compile:debug-lambda-name ((client client) decls)
  (loop for (declare . decs) in decls
        for p = (assoc 'core:lambda-name decs)
        when p return (second p)))

(defmethod maclina.compile:debug-lambda-list ((client client) decls)
  (loop for (declare . decs) in decls
        for p = (assoc 'core:lambda-list decs)
        when p
          return (values (rest p) t)
        finally (return (values nil nil))))

(defun cmp::register-global-function-def (type name)
  (declare (ignore type))
  (clostrum:note-function m:*client* *build-rte* name)
  (signal 'maclina.compile:resolve-function :name name)
  (values))

(defmethod common-macro-definitions::output-stream-from-string-function-name
    ((client client))
  'core::make-string-output-stream-from-string)

(defun proclaim (proclamation)
  ;; FIXME: record types, etc
  (when (consp proclamation)
    (case (car proclamation)
      ((special)
       (loop for s in (rest proclamation)
             do (clostrum:make-variable m:*client* *build-rte* s)))
      ((ftype)
       ;; We don't use types (yet?), but treat an ftype declaration as
       ;; noting a function, as some code (like alexandria) does.
       (loop for f in (cddr proclamation)
             do (cmp::register-global-function-def 'defun f)))))
  (values))

(defun core::*make-special (var)
  (clostrum:make-variable m:*client* *build-rte* var))

(defmethod common-macro-definitions:proclaim
    ((client client) declspec env)
  (declare (ignore env))
  (if (and (consp declspec) (eq (car declspec) 'special))
      ;; special case this so clasp can load it early.
      `(progn
         ,@(loop for s in (rest declspec)
                 collect `(core::*make-special ',s)))
      ;; proclaim is defined a bit late.
      nil #+(or)`(cl:proclaim ',declspec)))

(defmethod common-macro-definitions:add-local-nickname ((client client))
  'ext:add-package-local-nickname)

;; use trivial-package-local-nicknames, but resolve names in the
;; cross-clasp environment.
(defun ext:add-package-local-nickname
    (local-nickname actual-package
     &optional (package (m:symbol-value m:*client* *build-rte* '*package*)))
  (let ((actual-package
          (if (packagep actual-package)
              actual-package
              (or (clostrum:find-package m:*client* *build-rte*
                                         (string actual-package))
                (error "Missing package: ~s" actual-package))))
        (package
          (if (packagep package)
              package
              (or (clostrum:find-package m:*client* *build-rte*
                                         (string package))
                (error "Missing package: ~s" package)))))
    (trivial-package-local-nicknames:add-package-local-nickname
     local-nickname actual-package package)))

(defun clos::note-generic (name compiler-generic)
  (clostrum:note-function m:*client* *build-rte* name)
  (signal 'maclina.compile:resolve-function :name name)
  (setf (clostrum:operator-inline-data m:*client* *build-rte* name)
        compiler-generic)
  (values))

(defun core:put-f (plist value indicator)
  (setf (getf plist indicator) value)
  plist)

(defparameter *shared-package-names*
  '("ALEXANDRIA"
    "ECCLESIA"
    "ECLECTOR.BASE"
    "ECLECTOR.PARSE-RESULT"
    "ECLECTOR.READER"
    "ECLECTOR.READTABLE"
    "ECLECTOR.READTABLE.SIMPLE"
    "INCLESS"
    "INRAVINA"
    "INVISTRA"
    "KHAZERN"
    "QUAVIVER"
    "QUAVIVER.CONDITION"
    "QUAVIVER.MATH"
    "QUAVIVER/SCHUBFACH"
    "TRIVIAL-WITH-CURRENT-SOURCE-FORM"))

;;; make a package in the build environment.
;;; For most packages we prepend CROSS-CLASP.CLASP. to avoid
;;; stomping on any packages in the host (which may be Clasp).
;;; This should make it easier to build a new version of Clasp with an
;;; old version with a different definition of CORE, etc.
;;; Some packages we share with the host to make things easier for
;;; macroexpansions (i.e. they can expand into the symbols they usually
;;; do, rather than cross-clasp.clasp.whatever).
(defun %make-package (package-name &key nicknames use)
  (let* ((name (string package-name))
         (sharedp (member package-name *shared-package-names*
                          :test #'string=))
         (hname (if sharedp
                    name
                    (concatenate 'string "CROSS-CLASP.CLASP." name)))
         (use
           (loop for u in use
                 for s = (string u)
                 collect (or (clostrum:find-package
                              m:*client* *build-rte* s)
                           (error "Tried to use undefined package ~s" s))))
         (package (or (find-package hname)
                      (if sharedp
                          (error "BUG: Package ~a should exist in the host already but doesn't"
                                 hname)
                          (make-package hname :use use)))))
    (setf (clostrum:package-name m:*client* *build-rte* package) name
          (clostrum:find-package m:*client* *build-rte* name) package)
    (loop for nick in nicknames
          for snick = (string nick)
          do (setf (clostrum:find-package m:*client* *build-rte* snick)
                   package))
    package))


;;; We ignore package locks for now
(defun ext:add-implementation-package (implementors &optional package)
  (declare (ignore implementors package)))

(defun ext:setf-expander (name)
  (clostrum:setf-expander m:*client* *build-ce* name))
(defun (setf ext:setf-expander) (expander name)
  (setf (clostrum:setf-expander m:*client* *build-rte* name) expander))

(defmethod common-macro-definitions::wrap-in-setf-setf-expander
    ((client client) name function environment)
  (declare (ignore environment))
  `(setf (ext:setf-expander ',name) ,function))

(defmethod common-macro-definitions:get-setf-expansion
    ((client client) place &optional environment)
  (let ((env (or environment *build-rte*)))
    (extrinsicl:get-setf-expansion
     client env (macroexpand-hook) place)))

;;; We define our own SETF over Common Macros'. This is because we want to
;;; minimize how much code actually needs to be compiled by using simple
;;; expansions when available.
;;; E.g. if we have (defun foo ...), that will end up as
;;; (setf (fdefinition 'foo) (lambda ...)), which in turn ends up as
;;; (funcall #'(setf fdefinition) (lambda ...) 'foo).
;;; Without our own setf there would be a let* and such so the file compiler
;;; would just compile the whole thing rather than only the function definition
;;; which is repetitive and wasteful given how many thousands of functions
;;; we want to define.
(defmacro %setf (&whole form &rest pairs &environment env)
  `(progn ; progn is of course ok, just means top level form processing
     ,@(loop for sub on pairs by #'cddr
             when (null (cdr sub))
               do (error "Odd number of arguments to SETF: ~s~%" form)
             collect (expand-1-setf (first sub) (second sub) env))))

(defun expand-1-setf (place new-value-form env)
  (multiple-value-bind (vars vals stores write)
      (common-macro-definitions:get-setf-expansion m:*client* place env)
    (cond (; (setq whatever store) with no vars
           (and (= (length stores) 1) (null vars)
             (consp write) (consp (cdr write)) (consp (cddr write))
             (null (cdddr write))
             (eq (first write) 'cl:setq)
             (eq (third write) (first stores)))
           `(setq ,(second write) ,new-value-form))
          (; (funcall #'whatever store ...) with no vars
           (and (= (length stores) 1) (null vars)
             (consp write) (eq (car write) 'cl:funcall) (consp (cdr write))
             (consp (cadr write)) (eq (caadr write) 'cl:function)
             (consp (cddr write)) (eq (caddr write) (first stores))
             ;; See bug #1720 - weak hash table problem here
             (not (equal (cadadr write) '(setf core::variable-source-info))))
           `(funcall ,(cadr write) ,new-value-form ,@(cdddr write)))
          (; (set var store), as generated by extrinsicl
           (and (= (length stores) 1) (= (length vars) 1)
             (consp write) (eq (car write) 'cl:set)
             (consp (cdr write)) (consp (cddr write))
             (null (cdddr write))
             (eq (second write) (first vars))
             (eq (third write) (first stores)))
           `(set ,(first vals) ,new-value-form))
          ((null vars) `(multiple-value-bind ,stores ,new-value-form ,write))
          (t
           `(let* ,(mapcar #'list vars vals)
              (multiple-value-bind ,stores ,new-value-form ,write))))))

(defmacro %remf (&environment env place indicator)
  (multiple-value-bind (vars vals stores store-form access-form)
      (common-macro-definitions:get-setf-expansion m:*client* place env)
    (let ((s (gensym "s")))
      `(let* (,@(mapcar #'list vars vals) (,s ,indicator))
         (multiple-value-bind (,(car stores) flag)
             (core:rem-f ,access-form ,s)
           ,store-form
           flag)))))

(defun install-packages (&optional (client m:*client*)
                           (environment *build-rte*))
  (macrolet ((defpack (name hostname &rest nicknames)
               `(let ((package (find-package ',hostname)))
                  (setf (clostrum:package-name client environment package) ,name
                        (clostrum:find-package client environment ,name) package
                        (clostrum:find-package client environment ,(string hostname)) package
                        ,@(loop for nick in nicknames
                                collect `(clostrum:find-package client environment
                                                                ,nick)
                                collect 'package)))))
    (defpack "COMMON-LISP" #:common-lisp "CL")
    (defpack "COMMON-LISP-USER" #:common-lisp-user "CL-USER")
    (defpack "CORE" #:cross-clasp.clasp.core "SYS" "SYSTEM" "SI")
    (defpack "GCTOOLS" #:cross-clasp.clasp.gctools)
    (defpack "MP" #:cross-clasp.clasp.mp)
    (defpack "LLVM" #:cross-clasp.clasp.llvm)
    (defpack "LLVM-SYS" #:cross-clasp.clasp.llvm-sys)
    (defpack "CLOS" #:cross-clasp.clasp.clos)
    (defpack "COMPILER" #:cross-clasp.clasp.cmp "CMP")
    (defpack "SEQUENCE" #:cross-clasp.clasp.sequence)
    (defpack "GRAY" #:cross-clasp.clasp.gray)
    (defpack "MPI" #:cross-clasp.clasp.mpi)
    (defpack "CLASP-FFI" #:cross-clasp.clasp.clasp-ffi)
    (defpack "CLBIND" #:cross-clasp.clasp.clbind)
    (defpack "CLASP-DEBUG" #:cross-clasp.clasp.debug)
    (defpack "CLANG-COMMENTS" #:cross-clasp.clasp.clang-comments)
    (defpack "CLANG-AST" #:cross-clasp.clasp.clang-ast)
    (defpack "AST-TOOLING" #:cross-clasp.clasp.ast-tooling)
    (defpack "EXT" #:cross-clasp.clasp.ext)
    (defpack "KEYWORD" #:keyword)
    (defpack "ECCLESIA" #:ecclesia)
    (defpack "QUAVIVER" #:quaviver)
    (defpack "QUAVIVER.CONDITION" #:quaviver.condition)
    (defpack "QUAVIVER.MATH" #:quaviver.math)
    (defpack "QUAVIVER/SCHUBFACH" #:quaviver/schubfach)
    (defpack "INCLESS" #:incless)
    (defpack "INRAVINA" #:inravina)
    (defpack "INVISTRA" #:invistra)
    (defpack "INCLESS-INTRINSIC" #:cross-clasp.clasp.incless-intrinsic)
    (defpack "INRAVINA-INTRINSIC" #:cross-clasp.clasp.inravina-intrinsic)
    (defpack "INVISTRA-INTRINSIC" #:cross-clasp.clasp.invistra-intrinsic)
    (defpack "ECLECTOR.BASE" #:cross-clasp.clasp.eclector.base)
    (defpack "ECLECTOR.READER" #:cross-clasp.clasp.eclector.reader))

  ;; on clasp we have a few symbols from its actual core, like lambda-name.
  ;; So we need to be able to dump those correctly.
  #+clasp
  (setf (clostrum:package-name client environment (find-package "SYS"))
        "CORE")
  ;; Also on clasp, in interpreted-discriminator.lisp we dump an ext:byte8
  ;; during native build, presumably as part of a type declaration or test
  ;; or something?
  #+clasp
  (setf (clostrum:package-name client environment
                               (let ((*package* (find-package "CL")))
                                 ;; global ext, i.e. clasp's, not ours
                                 (find-package "EXT")))
        "EXT")
  (loop for (host-name . target-name) in '(("INCLESS-EXTRINSIC" . "INCLESS-INTRINSIC")
                                           ("INRAVINA-EXTRINSIC" . "INRAVINA-INTRINSIC")
                                           ("INVISTRA-EXTRINSIC" . "INVISTRA-INTRINSIC"))
        do (setf (clostrum:package-name client environment (find-package host-name))
                 target-name)))

;;; FIXME: defconstant should really be in common macros.
(defun core::symbol-constantp (name)
  (clostrum:constantp m:*client* *build-rte* name))
(defun (setf core::symbol-constantp) (value name)
  (when value
    (clostrum:make-constant m:*client* *build-rte* name
                            (m:symbol-value m:*client* *build-rte* name)))
  value)

(defun (setf ext:symbol-macro) (expander name &optional env)
  (let ((env (if env
                 (trucler:global-environment m:*client* env)
                 *build-rte*)))
    (setf (clostrum-sys:variable-status m:*client* env name) :symbol-macro
          (clostrum-sys:variable-macro-expander m:*client* env name) expander)))

(defun ext:specialp (name)
  (eq (clostrum:variable-status m:*client* *build-ce* name) :special))

(defmacro %defconstant (name value &optional doc)
  (declare (ignore doc))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (set ',name ,value)
       (funcall #'(setf core::symbol-constantp) t ',name)
       #+(or)
       (core::make-constant ',name ,value))
     ,@(when (ext:current-source-location)
         `((setf (core::variable-source-info ',name)
                 ',(ext:current-source-location))))
     ',name))

(defun core::make-simple-vector-t (dimension initial-element iep)
  (if iep
      (make-array dimension :initial-element initial-element)
      (make-array dimension)))

(defun initial-features (features)
  ;; FEATURES are those gathered from the executable. We just tack on
  ;; an indication that we're building from the host Lisp, as well as :CLOS.
  ;; We could add :CLOS from the sources instead?
  (let ((features (list* :gray-streams-line-length :gray-streams-interactive
                         :gray-streams-external-format/setf :gray-streams-external-format
                         :gray-streams-file-string-length :gray-streams-file-length/get
                         :gray-streams-file-length :gray-streams-file-position/optional
                         :gray-streams-file-position :gray-streams-sequence/optional
                         :gray-streams-sequence :gray-streams-truename :gray-streams-pathname
                         :gray-streams-directionp :gray-streams-streamp
                         :gray-streams-element-type/setf :building-clasp :clos features)))
    (when (member :long-float features)
      (pushnew :quaviver/long-float features))
    (when (member :short-float features)
      (pushnew :quaviver/short-float features))
    features))

;; This gets the currently present *features*. Used in build.
(defun features ()
  (m:symbol-value m:*client* *build-rte* '*features*))

(defparameter *copied-variables*
  '(;;Eclector expects these to be globally bound
    eclector.reader::*quasiquotation-state*
    eclector.reader::*quasiquotation-depth*
    eclector.reader::*consing-dot-allowed-p*
    incless-extrinsic::*client*
    invistra::*format-output*
    ;; We need these to be available for dumping
    cmp::*additional-clasp-character-names*
    cmp::*mapping-char-code-to-char-names*))

(defun install-setfs ()
  (macrolet ((def (name lambda-list &body body)
               `(setf (ext:setf-expander ',name)
                      #',(ext:parse-define-setf-expander
                          name lambda-list body))))
    (def getf (&environment env place indicator
                            &optional (default nil default-p))
      (multiple-value-bind (vars vals stores store-form access-form)
          (common-macro-definitions:get-setf-expansion m:*client* place env)
        (let* ((itemp (gensym "ITEMP")) (store (gensym "STORE")) (def (gensym "DEF")))
          (values `(,@vars ,itemp ,@(if default-p (list def) nil))
                  `(,@vals ,indicator ,@(if default-p (list default) nil))
                  `(,store)
                  `(let ((,(car stores) (core:put-f ,access-form ,store ,itemp)))
                     ,@(if default-p (list def) nil) ; prevent unused variable warning
                     ,store-form
                     ,store)
                  `(getf ,access-form ,itemp ,@(if default-p (list def) nil))))))
    (def sbit (&environment env array &rest subscripts)
      (common-macro-definitions:get-setf-expansion
       m:*client* `(aref ,array ,@subscripts) env))))

(defun install-delayed-macros (client rte)
  (loop for name being each hash-key of core::*delayed-macros*
          using (hash-value expander)
        when (member name '(declaim defclass defgeneric defmethod defstruct
                            defpackage))
          do (setf (clostrum:macro-function client rte name) expander)))

(defun %install-delayed-macros ()
  (install-delayed-macros m:*client* *build-rte*))

(defun install-mop (client rte)
  ;; In order to avoid any shenanigans, we do not import closer-mop symbols
  ;; into our CLOS package. We probably could arrange something to ensure that
  ;; the compiler dumps MOP symbols as being in the CLOS package, but egh.
  ;; I'm only importing stuff we really need for now.
  (loop for s in '(#:ensure-class #:ensure-generic-function)
        for mopsym = (or (find-symbol (symbol-name s) "CLOSER-MOP")
                       (error "Somehow missing MOP symbol ~a" s))
        for clossym = (intern (symbol-name s) "CROSS-CLASP.CLASP.CLOS")
        do (setf (clostrum:fdefinition client rte clossym)
                 (fdefinition mopsym))))

(defun %format-symbol (package control &rest arguments)
  (apply #'alexandria:format-symbol
         (etypecase package
           ((eql t) (m:symbol-value m:*client* *build-rte* '*package*))
           (null package)
           (package package)
           ((or symbol string character)
            (clostrum:find-package m:*client* *build-rte* (string package))))
         control arguments))

(defun %symbolicate (&rest things)
  (let ((*package* (m:symbol-value m:*client* *build-rte* '*package*)))
    (apply #'alexandria:symbolicate things)))

(defun install-environment (&optional (client m:*client*)
                              (rte *build-rte*)
                              (ce *build-ce*))
  (declare (ignore ce))
  (extrinsicl:install-cl client rte)
  (extrinsicl.maclina:install-eval client rte)
  (clostrum:make-variable client rte 'CROSS-CLASP.CLASP.INCLESS-INTRINSIC:*CLIENT* incless-extrinsic:*client*)
  (loop for vname in *copied-variables*
        do (clostrum:make-variable client rte vname (symbol-value vname)))
  (loop for vname in '(core::*condition-restarts* core::*restart-clusters*
                       core::*interrupts-enabled* core::*allow-with-interrupts*
                       core:*quasiquote* core::*sharp-equal-final-table*
                       core:*variable-source-infos*
                       cross-clasp.clasp.inravina-intrinsic::*initial-pprint-dispatch*
                       core::+standard-readtable+
                       ext:*invoke-debugger-hook* ext:*toplevel-hook*
                       ext:*inspector-hook* core::*documentation-pool*
                       core:*initialize-hooks* core:*terminate-hooks*
                       core:*extension-systems*
                       core::*circle-counter* core::*circle-stack*
                       core:*functions-to-inline* core:*functions-to-notinline*
                       mp:*current-process*
                       core:+type-header-value-map+
                       ext:+process-standard-input+ ext:+process-standard-output+
                       ext:+process-error-output+ ext:+process-terminal-io+
                       cmp::*default-output-type* cmp:*source-locations*
                       cmp::*optimize* cmp::*optimization-level*
                       cmp:*btb-compile-hook* cmp::*code-walker*
                       invistra::*format-output* invistra::*extra-space*
                       invistra::*line-length* invistra::*newline-kind*
                       invistra::*more-arguments-p* invistra::*argument-index*
                       invistra::*remaining-argument-count* invistra::*pop-argument*
                       invistra::*go-to-argument* invistra::*pop-remaining-arguments*
                       invistra::*inner-exit-if-exhausted* invistra::*outer-exit-if-exhausted*
                       invistra::*inner-exit* invistra::*outer-exit*
                       invistra::*format-output* invistra::*extra-space*
                       invistra::*line-length* invistra::*newline-kind*
                       invistra::*more-arguments-p* invistra::*argument-index*
                       invistra::*remaining-argument-count* invistra::*pop-argument*
                       invistra::*go-to-argument* invistra::*pop-remaining-arguments*
                       invistra::*inner-exit-if-exhausted* invistra::*outer-exit-if-exhausted*
                       invistra::*inner-exit* invistra::*outer-exit*)
        do (clostrum:make-variable client rte vname))
  (loop for fname in '(core::symbol-constantp (setf core::symbol-constantp)
                       (setf ext:symbol-macro)
                       core::*make-special ext:specialp
                       core::find-declarations core:process-declarations
                       core::dm-too-many-arguments core::dm-too-few-arguments
                       cmp::register-global-function-def
                       ext:setf-expander (setf ext:setf-expander)
                       mp::atomic-expander (setf mp::atomic-expander)
                       ext:parse-macro
                       core::function-block-name
                       ext:constant-form-value
                       core:put-f core::packages-iterator
                       core::process-lambda-list
                       ext:type-expander (setf ext:type-expander)
                       core::normalize-type
                       core::class-info (setf core::class-info)
                       ext:current-source-location
                       core::variable-source-info
                       (setf core::variable-source-info)
                       (setf documentation) ; used by e.g. defpackage
                       ext:add-package-local-nickname
                       ;; Used by compiler, not expected to exist in target
                       core::delay-macro
                       ;; used in CLOS, not expected to actually exist
                       ;; in the target
                       clos::note-generic clos::note-method
                       ;; used in CLOS, replaced in target
                       clos::parse-specialized-lambda-list
                       clos::fixup-method-lambda-list clos::method-lambda
                       ;; used specifically to reconstruct compiler metaobjects
                       ;; in CFASL loading
                       (setf slot-value)
                       ;; FIXME: Used in common-macros defmacro expansions
                       ecclesia:list-structure
                       ext:parse-compiler-macro ext:parse-deftype
                       ext:parse-define-setf-expander
                       ext:add-implementation-package
                       core::make-simple-vector-t
                       ;; Used in clasp local macroexpanders (native build)
                       #+clasp si::search-keyword #+clasp si::check-keywords
                       ;; used in compiler macro expansions
                       core::make-vector
                       core::make-simple-vector-character
                       core::concatenate-into-sequence
                       core::coerce-to-list
                       core::apply0 core::apply1
                       core::apply2 core::apply3 core::apply4
                       clos::classp core::subclassp
                       core::fixnump
                       core::two-arg-+ core::two-arg-*
                       core::two-arg-- core::negate
                       core::two-arg-/ core::reciprocal
                       core::two-arg-< core::two-arg-<=
                       core::two-arg-> core::two-arg->=
                       core::two-arg-=
                       core::logand-2op core::logior-2op
                       core::find-class-holder
                       ext::class-unboundp ext::class-get
                       cmp::warn-undefined-type
                       cmp::warn-cannot-coerce
                       #+clasp si:backquote-append
                       alexandria:make-gensym-list
                       alexandria:ensure-car
                       alexandria:ensure-list
                       alexandria::generate-switch-body
                       quaviver::unique-name
                       quaviver.math::compute-expt
                       quaviver.math::ceiling-log-expt
                       quaviver::primitive-triple-bits-form
                       quaviver::bits-primitive-triple-form
                       quaviver::primitive-triple-float-form
                       quaviver::float-primitive-triple-form
                       quaviver::traits-from-sizes
                       khazern:unique-name
                       inravina:expand-logical-block
                       invistra::unique-name
                       invistra:expand-function
                       invistra:make-downcase-stream
                       incless:write-object
                       invistra:format-with-client)
        for f = (fdefinition fname)
        do (setf (clostrum:fdefinition client rte fname) f))
  (loop for (fname . src) in '((cl:proclaim . proclaim)
                               (cl:make-package . %make-package)
                               (clos::class-slots . closer-mop:class-slots)
                               (clos::slot-definition-name
                                . closer-mop:slot-definition-name)
                               (clos::gf-info . gf-info)
                               (clos::find-compiler-class
                                . find-compiler-class)
                               (core::install-delayed-macros
                                . %install-delayed-macros)
                               (alexandria:format-symbol
                                . %format-symbol)
                               (alexandria:symbolicate
                                . %symbolicate))
        for f = (fdefinition src)
        do (setf (clostrum:fdefinition client rte fname) f))
  (flet ((traits (type &aux (plist (case type
                                     (:bfloat16
                                      '(:exponent-size 8 :significand-size 8))
                                     (:binary16
                                      '(:exponent-size 5 :significand-size 11))
                                     (:binary32
                                      '(:exponent-size 8 :significand-size 24))
                                     (:binary64
                                      '(:exponent-size 11 :significand-size 53))
                                     (:binary80
                                      '(:exponent-size 15 :significand-size 64))
                                     (:binary128
                                      '(:exponent-size 15 :significand-size 113))
                                     (:binary256
                                      '(:exponent-size 19 :significand-size 237))
                                     (otherwise
                                      (clostrum:symbol-plist client rte type)))))
           (quaviver::traits-from-sizes type
                                        (getf plist :exponent-size)
                                        (getf plist :significand-size)))
         (bytespec (form)
           (cons (second form) (third form))))
    (setf (clostrum:fdefinition client rte 'incless::write-object)
          (fdefinition 'incless:write-object)
          (clostrum:fdefinition client rte 'cl:format)
          (fdefinition 'invistra-extrinsic:format)
          (clostrum:compiler-macro-function client rte 'cl:format)
          (compiler-macro-function 'invistra-extrinsic:format)
          (clostrum:compiler-macro-function client rte 'cl:break)
          (compiler-macro-function 'invistra-extrinsic:break)
          (clostrum:compiler-macro-function client rte 'cl:error)
          (compiler-macro-function 'invistra-extrinsic:error)
          (clostrum:compiler-macro-function client rte 'cl:cerror)
          (compiler-macro-function 'invistra-extrinsic:cerror)
          (clostrum:compiler-macro-function client rte 'cl:invalid-method-error)
          (compiler-macro-function 'invistra-extrinsic:invalid-method-error)
          (clostrum:compiler-macro-function client rte 'cl:method-combination-error)
          (compiler-macro-function 'invistra-extrinsic:method-combination-error)
          (clostrum:compiler-macro-function client rte 'cl:signal)
          (compiler-macro-function 'invistra-extrinsic:signal)
          (clostrum:compiler-macro-function client rte 'cl:warn)
          (compiler-macro-function 'invistra-extrinsic:warn)
          (clostrum:compiler-macro-function client rte 'cl:y-or-n-p)
          (compiler-macro-function 'invistra-extrinsic:y-or-n-p)
          (clostrum:compiler-macro-function client rte 'cl:yes-or-no-p)
          (compiler-macro-function 'invistra-extrinsic:yes-or-no-p)
          (clostrum:compiler-macro-function client rte 'core::assert-failure)
          (lambda (form env)
            (declare (ignore env))
            (print (invistra:expand-function incless-extrinsic:*client* form 4)))
          (clostrum:compiler-macro-function client rte 'cross-clasp.clasp.core::assert-failure)
          (lambda (form env)
            (declare (ignore env))
            (print (invistra:expand-function incless-extrinsic:*client* form 1)))
          (clostrum:compiler-macro-function client rte 'core::simple-reader-error)
          (lambda (form env)
            (declare (ignore env))
            (invistra:expand-function incless-extrinsic:*client* form 2))
          (clostrum:compiler-macro-function client rte 'core::signal-simple-error)
          (lambda (form env)
            (declare (ignore env))
            (invistra:expand-function incless-extrinsic:*client* form 2 3))
          (clostrum:compiler-macro-function client rte 'mp::interrupt)
          (lambda (form env)
            (declare (ignore env))
            (invistra:expand-function incless-extrinsic:*client* form 3))
          (clostrum:compiler-macro-function client rte 'mp::raise)
          (lambda (form env)
            (declare (ignore env))
            (invistra:expand-function incless-extrinsic:*client* form 2))
          (clostrum:macro-function client rte 'cl:formatter)
          (macro-function 'invistra-extrinsic:formatter)
          (clostrum:macro-function client rte 'cl:print-unreadable-object)
          (macro-function 'incless-extrinsic:print-unreadable-object)
          (clostrum:fdefinition client rte 'cross-clasp.clasp.gray::redefine-cl-functions)
          (lambda ())
          (clostrum:fdefinition client rte 'quaviver::bits-float-form)
          (lambda (type value)
            (ecase (getf (clostrum:symbol-plist client rte type) :implementation-type)
              (short-float
               `(ext::bits-to-short-float ,value))
              (single-float
               `(ext::bits-to-single-float ,value))
              (double-float
               `(ext::bits-to-double-float ,value))
              (long-float
               `(ext::bits-to-long-float ,value))))
          (clostrum:fdefinition client rte 'quaviver::float-bits-form)
          (lambda (type value)
            (ecase (getf (clostrum:symbol-plist client rte type) :implementation-type)
              (short-float
               `(ext::short-float-to-bits ,value))
              (single-float
               `(ext::single-float-to-bits ,value))
              (double-float
               `(ext::double-float-to-bits ,value))
              (long-float
               `(ext::long-float-to-bits ,value))))
          (clostrum:fdefinition client rte 'quaviver::implementation-type)
          (lambda (type)
            (or (getf (clostrum:symbol-plist client rte type) :implementation-type)
                (getf (traits type) :implementation-type)))
          (clostrum:fdefinition client rte
                                'quaviver::exact-implementation-type-p)
          (lambda (type)
            (getf (traits type) :exact-implementation-type-p))
          (clostrum:fdefinition client rte 'quaviver::exponent-size)
          (lambda (type)
            (getf (traits type) :exponent-size))
          (clostrum:fdefinition client rte 'quaviver::significand-size)
          (lambda (type)
            (getf (traits type) :significand-size))
          (clostrum:fdefinition client rte 'quaviver::arithmetic-size)
          (lambda (type)
            (getf (traits type) :arithmetic-size))
          (clostrum:fdefinition client rte 'quaviver::max-exponent)
          (lambda (type)
            (getf (traits type) :max-exponent))
          (clostrum:fdefinition client rte 'quaviver::min-exponent)
          (lambda (type)
            (getf (traits type) :min-exponent))
          (clostrum:fdefinition client rte 'quaviver::exponent-bias)
          (lambda (type)
            (getf (traits type) :exponent-bias))
          (clostrum:fdefinition client rte 'quaviver::storage-size)
          (lambda (type)
            (getf (traits type) :storage-size))
          (clostrum:fdefinition client rte 'quaviver::sign-byte-form)
          (lambda (type)
            (getf (traits type) :sign-byte-form))
          (clostrum:fdefinition client rte 'quaviver::exponent-byte-form)
          (lambda (type)
            (getf (traits type) :exponent-byte-form))
          (clostrum:fdefinition client rte 'quaviver::exponent-bytespec)
          (lambda (type)
            (bytespec (getf (traits type) :exponent-byte-form)))
          (clostrum:fdefinition client rte 'quaviver::significand-byte-form)
          (lambda (type)
            (getf (traits type) :significand-byte-form))
          (clostrum:fdefinition client rte 'quaviver::significand-bytespec)
          (lambda (type)
            (bytespec (getf (traits type) :significand-byte-form)))
          (clostrum:fdefinition client rte 'quaviver::nan-type-byte-form)
          (lambda (type)
            (getf (traits type) :nan-type-byte-form))
          (clostrum:fdefinition client rte 'quaviver::nan-payload-byte-form)
          (lambda (type)
            (getf (traits type) :nan-payload-byte-form))
          (clostrum:fdefinition client rte 'quaviver::hidden-bit-p)
          (lambda (type)
            (getf (traits type) :hidden-bit-p))))

  (loop for mname in '(eclector.reader:quasiquote
                       invistra::with-arguments
                       invistra::with-remaining-arguments
                       #+clasp si:quasiquote
                       ext:with-current-source-form
                       core::with-clean-symbols core::with-unique-names
                       core::once-only
                       core::defconstant-eqx core::defconstant-equal
                       core::while core::until
                       clos::with-early-accessors
                       clos::define-method-combination
                       clos::define-simple-method-combination
                       clos::define-complex-method-combination
                       mp:with-lock
                       mp:without-interrupts mp:with-interrupts
                       mp::atomic mp::define-atomic-expander
                       mp::define-simple-atomic-expander mp::cas
                       mp::atomic-update-explicit mp::atomic-update
                       mp::atomic-incf-explicit mp::atomic-incf
                       mp::atomic-decf-explicit mp::atomic-decf
                       mp::atomic-push-explicit mp::atomic-push
                       mp::atomic-pop-explicit mp::atomic-pop
                       mp::atomic-pushnew-explicit mp::atomic-pushnew
                       clos::early-allocate-instance
                       clos::earlier-allocate-instance
                       clos::early-initialize-instance
                       clos::early-make-instance
                       clos::with-mutual-defclass
                       clos::with-effective-method-parameters
                       clos::base-satiate)
        for m = (macro-function mname)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (mname . src) in '((defun . core::%defun)
                               (defmacro . core::%defmacro)
                               (define-compiler-macro . core::%define-compiler-macro)
                               (deftype . core::%deftype)
                               (define-setf-expander . core::%define-setf-expander)
                               (defvar . core::%defvar)
                               (defparameter . core::%defparameter)
                               (defconstant . %defconstant)
                               (defclass . clos::early-defclass)
                               (defgeneric . clos::early-defgeneric)
                               (defmethod . clos::early-defmethod)
                               (defstruct . clos::early-defstruct)
                               (call-method . clos::%call-method)
                               (handler-bind . %handler-bind)
                               (assert . %assert)
                               (check-type . %check-type)
                               (restart-case . %restart-case)
                               (restart-bind . %restart-bind)
                               (with-condition-restarts . %with-condition-restarts)
                               (with-package-iterator . %with-package-iterator)
                               (ccase . core::%ccase)
                               (ecase . core::%ecase)
                               (ctypecase . core::%ctypecase)
                               (etypecase . core::%etypecase)
                               (setf . %setf)
                               (remf . %remf)
                               (pprint-logical-block . core::%pprint-logical-block)
                               (trivial-with-current-source-form:with-current-source-form
                                   . ext:with-current-source-form))
        for m = (macro-function src)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (fname . set) in '((mp::atomic . mp::expand-atomic))
        for f = (fdefinition set)
        do (setf (clostrum:setf-expander client rte fname) f))
  ;; We UNdefine defsetf, because the one in common macros uses the host backquote,
  ;; which we cannot in general deal with.
  ;; This lets Clasp's DEFSETF be defined during build.
  (loop for mname in '(defsetf)
        do (clostrum:fmakunbound client rte mname))
  (install-setfs)
  (install-mop client rte)
  ;; Extrinsicl copies over a bunch of classes, but we actually need
  ;; to use our own instead.
  (loop for s being the external-symbols of "CL"
        do (setf (clostrum:find-class client rte s) nil))
  ;; These exist in the compiler (but not at load time!) so that we can load the
  ;; CFASLs of clos/hierarchy.lisp and so on.
  (loop for cs in '(clos::compiler-class clos::compiler-eql-specializer
                    clos::compiler-slotd clos::direct-slotd clos::effective-slotd
                    clos::compiler-method-combination clos::compiler-generic
                    clos::compiler-method clos::compiler-reader
                    clos::compiler-writer clos::effective-reader
                    clos::effective-writer)
        for c = (find-class cs)
        do (setf (clostrum:find-class client rte cs) c))
  ;; also copies over many constants we don't want.
  ;; They will be defined in runtime-variables.lisp or the library.
  ;; The only ones we want to keep are
  ;; those that are implementation-independent.
  (loop for s being the external-symbols of "CL"
        unless (or (not (clostrum:constantp client rte s))
                   (member s '(nil pi t)))
          do (clostrum:makunbound client rte s))
  (values))

(defun initialize (character-names-path features-path)
  (load-unicode-file character-names-path)
  (setf m:*client* (make-instance 'client)
        *build-rte* (make-instance 'run-time-environment)
        *build-ce* (make-instance 'clostrum-basic:compilation-environment
                     :parent *build-rte*))
  (core::reset-delayed-macros)
  (reset-class-infos)
  (install-environment)
  (install-packages)
  (maclina.vm-cross:initialize-vm 20000)
  (let ((features (with-open-file (s features-path)
                    (read s))))
    (clostrum:make-parameter m:*client* *build-rte* '*features*
                             (initial-features features)))
  (values))
