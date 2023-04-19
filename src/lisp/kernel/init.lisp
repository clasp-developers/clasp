;; Set features :clasp-min for minimal system without CLOS
;; :clos to compile with CLOS
;;


;;;#-darwin
#+(or)(cmp:trampoline "bytecode")

#+(or)
(eval-when (:compile-toplevel :execute)
  (setq core:*debug-eval* t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

#+(or)(setq *features* (cons :dbg-print *features*))
(SYS:*MAKE-SPECIAL '*echo-repl-tpl-read*)
(export '(*echo-repl-tpl-read*
          ihs-top ; for asdf compatibility only; remove soon
          cons-car
          cons-cdr
          debug-break))
(export '*trace-startup*)


;;; ------------------------------------------------------------
;;;
;;; Set *echo-repl-read* to t to print each repl form
;;;
(setq *echo-repl-read* nil)

;;; ------------------------------------------------------------
;;; Turn on printing messages as object files are converted to runnable code
;;;
(if (member :dump-repl-object-files *features*)
    (llvm-sys:debug-object-files 'llvm-sys:debug-object-files-print)
    (if (member :debug-object-files *features*)
        (llvm-sys:debug-object-files 'llvm-sys:debug-object-files-print-save)))

(setq *echo-repl-tpl-read* (member :emacs-inferior-lisp *features*))
;;;(setq *load-print* nil)

(setq cl:*print-circle* nil)

(sys:*make-special 'core::*clang-bin*)
(export 'core::*clang-bin*)


;;; --------------------------------------------------
;;;
;;; Use force-compile-file-serial feature to
;;; shutdown compile-file-parallel.
;;;
(if (member :force-compile-file-serial *features*)
    (setq cmp:*use-compile-file-parallel* nil))

#+clasp-min
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (CORE:SETF-LAMBDA-LIST
   (FUNCALL #'(SETF MACRO-FUNCTION)
            #'(LAMBDA
                  (whole env
                   &AUX (CLAUSES (CDR (THE CONS whole))))
                (DECLARE (IGNORABLE rest whole)
                         (CORE:LAMBDA-NAME (MACRO-FUNCTION COND)))
                (DECLARE (IGNORE env))
                (BLOCK COND
                  (IF (CONSP CLAUSES)
                      (LET* ((clauses1 CLAUSES)
                             (clauses2 clauses1)
                             (XXX
                               (PROGN
                                 (IF (NULL clauses2)
                                     (error "too few arguments ~a ~a" clauses1
                                            '((PRED . FORMS) . REST-CLAUSES)))
                                 (let ((prog1-val (CAR (THE CONS clauses2))))
                                   (SETQ clauses2 (CDR (THE CONS clauses2)))
                                   prog1-val)))
                             (YYY XXX)
                             (PRED
                               (PROGN
                                 (IF (NULL YYY)
                                     (error "too few arguments ~a ~a" clauses1 '(PRED . FORMS)))
                                 (let ((PROG1-val (CAR (THE CONS YYY))))
                                   (SETQ YYY (CDR (THE CONS YYY)))
                                   prog1-val)))
                             (FORMS YYY)
                             (REST-CLAUSES clauses2))
                        (DECLARE (IGNORABLE YYY XXX clauses2 clauses1))
                        (IF (EQ PRED T)
                            (core:quasiquote
                             (PROGN (core::UNQUOTE-SPLICE FORMS)))
                            (core:QUASIQUOTE
                             (IF (core::UNQUOTE PRED)
                                 (PROGN (core::UNQUOTE-SPLICE FORMS))
                                 (COND (core::UNQUOTE-SPLICE REST-CLAUSES))))))
                      NIL)))
            'COND)
   '(&REST CLAUSES))
  'COND)

(cond ((member :generate-bytecode *features*)
       (setq core:*clasp-build-mode* :bytecode))
      ((member :generate-faso *features*)
       (setq core:*clasp-build-mode* :faso))
      ((member :generate-fasoll *features*)
       (setq core:*clasp-build-mode* :fasoll))
      ((member :generate-fasobc *features*)
       (setq core:*clasp-build-mode* :fasobc)))
  
(setq cmp:*default-object-type* core:*clasp-build-mode*)

;;; ------------------------------------------------------------
;;;
;;;   Sanity check that stamps are working properly
;;;

(eval-when (:compile-toplevel :execute)
  (let* ((obj "dummy-object-string")
         (obj-class (core:instance-class obj))
         (obj-class-name (core:name-of-class obj-class))
         (stamp (core:instance-stamp obj))
         (class-stamp (core:class-stamp-for-instances obj-class))
         (map-stamp (gethash obj-class-name core:+type-header-value-map+)))
    (if (not (numberp stamp))
        (progn
          (core:fmt t "Sanity check failure stamp {} must be a number%N" stamp)
          (core:cabort))
        (if (not (numberp class-stamp))
            (progn
              (core:fmt t "Sanity check failure class-stamp {} must be a number%N" class-stamp)
              (core:cabort))
            (if (not (numberp map-stamp))
                (progn
                  (core:fmt t "Sanity check failure map-stamp {} must be a number%N" map-stamp)
                  (finish-output)
                  (core:cabort)))))
    (if (not (= stamp class-stamp))
        (progn
          (core:fmt t "For object {} class {} class-name {} there is a mismatch between the stamp {} and the class-stamp {}%N"
                    obj obj-class obj-class-name stamp class-stamp)
          (finish-output)
          (core:cabort))
        (if (not (= stamp map-stamp))
            (progn
              (core:fmt t "For object {} class {} class-name {} there is a mismatch between the stamp {} and the map-stamp {}%N"
                        obj obj-class obj-class-name stamp map-stamp)
              (finish-output)
              (core:cabort))))))

;;; fixme2022 - We shouldn't need the varest feature
(setq *features* (cons :varest *features*))

;; Set up a few things for the CLOS package
(export '(clos::standard-class) "CLOS")

;; Setup a few things for the GRAY streams package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :gray))
(shadow '(STREAM-ELEMENT-TYPE OPEN-STREAM-P OUTPUT-STREAM-P INPUT-STREAM-P STREAMP CLOSE))

;; Setup a few things for the CORE package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))

(sys:*make-special '*use-cleavir-compiler*)
(sys:*make-special '*eval-with-env-hook*)

;; Setup a few things for the CMP package
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :cmp))
(sys:*make-special '*dbg-generate-dwarf*)
(setq *dbg-generate-dwarf* (null (member :disable-dbg-generate-dwarf *features*)))
(export '(llvm-link link-bitcode-modules link-fasoll-modules link-fasobc-modules))
;;; Turn on aclasp/bclasp activation-frame optimization
(sys:*make-special '*activation-frame-optimize*)
(setq *activation-frame-optimize* t)
(sys:*make-special '*use-human-readable-bitcode*)
(setq *use-human-readable-bitcode* (member :use-human-readable-bitcode *features*))
(sys:*make-special '*compile-file-debug-dump-module*)
(sys:*make-special '*debug-compile-file*)
(if (boundp '*compile-file-debug-dump-module*)
    nil
    (setq *compile-file-debug-dump-module* t))
(sys:*make-special '*compile-debug-dump-module*)
(if (boundp '*compile-debug-dump-module*)
    nil
    (setq *compile-debug-dump-module* nil))
(setq *debug-compile-file* (member :debug-compile-file *features*))
(sys:*make-special '*debug-cclasp-cmp*)
(setq *debug-cclasp-cmp* nil)
(export '*debug-cclasp-cmp*)

(export '(*compile-file-debug-dump-module* *compile-debug-dump-module* *use-human-readable-bitcode*))
(use-package :core)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :core))

(if (find-package "C")
    nil
    (make-package "C" :use '(:cl :core)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (if (find-package "SEQUENCE")
      nil
      (make-package "SEQUENCE" :use '())))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (if (find-package "LITERAL")
      nil
      (make-package "LITERAL" :use '(:cl :core))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (if (find-package "CLASP-CLEAVIR")
      nil
      (make-package "CLASP-CLEAVIR" :use '(:CL))))

;;; Setup a few things for the EXT package

;;; EXT exports are now in packages.lisp
(eval-when (:execute :compile-toplevel :load-toplevel)
  (select-package :ext))

(core:*make-special '*module-provider-functions*)
(core:*make-special '*source-location*)
(setq *source-location* nil)
(export 'current-source-location)
;;; Function: (EXT:CURRENT-SOURCE-LOCATION)
;;; - Returns the source location of the current top-level form
;;;   or nil if it's not known.
(core:fset
 'current-source-location
 #'(lambda () core:*current-source-pos-info*))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (core:select-package :core))

;;; Have to do this early so all defvars are ok.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys:*make-special '*variable-source-infos*))
(if (boundp '*variable-source-infos*)
    nil
    (set '*variable-source-infos*
         (make-hash-table :test #'eq :thread-safe t)))

(si:fset 'core::defvar #'(lambda (whole env)
                           (declare (ignore env))
                           (let ((var (cadr whole))
                                 (formp (cddr whole))
                                 (form (caddr whole)))
                             "Syntax: (defvar name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
                             `(LOCALLY (DECLARE (SPECIAL ,var))
                                (SYS:*MAKE-SPECIAL ',var)
                                ,@(if formp
                                      `((if (boundp ',var)
                                            ',var
                                            (progn
                                              (setq ,var ,form)
                                              ',var)))))))
         t)
(export 'defvar)

(si:fset 'core::defparameter #'(lambda (whole env)
                                 (declare (ignore env))
                                 (let ((var (cadr whole))
                                       (form (caddr whole)))
                                   "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
                                   `(LOCALLY (DECLARE (SPECIAL ,var))
                                      (SYS:*MAKE-SPECIAL ',var)
                                      (SETQ ,var ,form)
                                      ',var)))
         t)
(export 'defparameter)



(si:fset 'core::defconstant #'(lambda (whole env)
                                (declare (ignore env))
                                (let ((var (cadr whole))
                                      (form (caddr whole)))
                                  "Syntax: (defconstant name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
                                  `(if (core:symbol-constantp ',var)
                                       nil
                                       (progn
                                         (set ',var ,form)
                                         (funcall #'(setf core:symbol-constantp) t ',var)))))
         t)
(export 'defconstant)

(if (boundp '+ecl-safe-declarations+)
    nil ; don't redefine constant
    (defconstant +ecl-safe-declarations+
      '(optimize (safety 2) (speed 1) (debug 1) (space 1))))

(defvar +io-syntax-progv-list+
  (list
   '(
     *print-pprint-dispatch* #|  See end of pprint.lisp  |#
     *print-array*
     *print-base*
     *print-case*
     *print-circle*
     *print-escape*
     *print-gensym*
     *print-length*
     *print-level*
     *print-lines*
     *print-miser-width*
     *print-pretty*
     *print-radix*
     *print-readably*
     *print-right-margin*
     *read-base*
     *read-default-float-format*
     *read-eval*
     *read-suppress*
     *readtable*
     *package*
     si::*sharp-eq-context*
     si::*circle-counter*)               ;
   nil                                   ;;  *pprint-dispatch-table*
   t                                     ;;  *print-array*
   10                                    ;;  *print-base*
   :upcase                               ;;  *print-case*
   nil                                   ;;  *print-circle*
   t                                     ;;  *print-escape*
   t                                     ;;  *print-gensym*
   nil                                   ;;  *print-length*
   nil                                   ;;  *print-level*
   nil                                   ;;  *print-lines*
   nil                                   ;;  *print-miser-width*
   nil                                   ;;  *print-pretty*
   nil                                   ;;  *print-radix*
   t                                     ;;  *print-readably*
   nil                                   ;;  *print-right-margin*
   10                                    ;;  *read-base*
   'single-float ;;  *read-default-float-format*
   t             ;;  *read-eval*
   nil           ;;  *read-suppress*
   *readtable*   ;;  *readtable*
   (find-package :CL-USER)               ;;  *package*
   nil                                   ;;  si::*sharp-eq-context*
   nil                                   ;;  si::*circle-counter*
   ))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :cl))
(defvar *print-pretty* nil)  ;; Turn this on by default
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *read-default-float-format* 'double-float)



(core::export 'defun)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core::select-package :core))

(defparameter *debug-bclasp* (if (member :debug-bclasp-lisp *features*) t nil))

(defvar *special-init-defun-symbol* (gensym "special-init-defun-symbol"))

;;; A temporary definition of defun - the real one is in evalmacros
#+clasp-min
(eval-when (:execute)
  (si:fset 'defun
           #'(lambda (def env)
               (let ((name (second def))      ;cadr
                     (lambda-list (third def)) ; caddr
                     (lambda-body (cdddr def))) ; cdddr
                 (multiple-value-call
                     (function (lambda (&optional (decl) (body) (doc) &rest rest)
                       (declare (ignore rest))
                       (if decl (setq decl (list (cons 'declare decl))))
                       (let ((func `#'(lambda ,lambda-list ,@decl ,@doc (block ,(si::function-block-name name) ,@body))))
                         ;;(core:fmt t "PRIMITIVE DEFUN defun --> {}%N" func )
                          `(progn (eval-when (:compile-toplevel)
                                    (cmp::register-global-function-def 'defun ',name))
                                  (si:fset ',name ,func nil ',lambda-list)))))
                   (si::process-declarations lambda-body nil #| No documentation until the real DEFUN is defined |#))))
           t))

(export '(defun))

;;; Define these here so that Cleavir can do inlining
(defvar *defun-inline-hook* nil)
(defvar *do-inline-hook* nil)
(defvar *proclaim-hook* nil)
(export '(*defun-inline-hook*
          *do-inline-hook*
          *proclaim-hook*))

;; Discard documentation until helpfile.lisp is loaded
(defun set-documentation (o d s) (declare (ignore o d s)) nil)

(defun proclaim (decl)
  "Args: (decl-spec)
Gives a global declaration.  See DECLARE for possible DECL-SPECs."
  ;;decl must be a proper list
  (if (not (core:proper-list-p decl))
      (error 'type-error
             :datum decl
             :expected-type '(and list (satisfies core:proper-list-p)))) 
  (cond
    ((eq (car decl) 'SPECIAL)
     (mapc #'sys::*make-special (cdr decl)))
    ((eq (car decl) 'cl:inline)
     (dolist (name (cdr decl))
       (core:hash-table-setf-gethash *functions-to-inline* name t)
       (remhash name *functions-to-notinline*)))
    ((eq (car decl) 'cl:notinline)
     (dolist (name (cdr decl))
       (core:hash-table-setf-gethash *functions-to-notinline* name t)
       (remhash name *functions-to-inline*)))
    (*proclaim-hook*
     (funcall *proclaim-hook* decl))))

(defun global-inline-status (name)
  "Return 'cl:inline 'cl:notinline or nil"
  (cond
    ((declared-global-inline-p name) 'cl:inline)
    ((declared-global-notinline-p name) 'cl:notinline)
    (t nil)))

(export '(global-inline-status
          declared-global-notinline-p
          declared-global-inline-p))

;; This is used extensively in the ecl compiler and once in predlib.lisp
(defvar *alien-declarations* ())
(export '*alien-declarations*)

(defun split-at-white-space (s) (split s " "))

(defun default-link-flags ()
  "Return the link flags and the library dir where libLTO.<library-extension> can be found and the library extension"
  (let ((stream (nth-value 2 (ext:vfork-execvp (list "llvm-config" "--ldflags" "--libdir" "--libs") t))))
    (let* ((ldflags (split-at-white-space (read-line stream)))
           (libdir (read-line stream))
           (libdir-flag (list (core:fmt nil "-L{}" libdir)))
           (libs (split-at-white-space (read-line stream)))
           (build-lib (split-at-white-space *build-lib*))
           (build-stlib (split-at-white-space *build-stlib*))
           (build-linkflags (split-at-white-space *build-linkflags*))
           (link-flags (append ldflags #+(or)(list clasp-lib-dir) build-linkflags libdir-flag libs build-stlib build-lib)))
      (close stream)
      (if (member :use-boehm *features*)
          (setq link-flags (cons "-lgc" link-flags)))
      (let ((library-extension (if (member :darwin *features*)
                                   "dylib"
                                   "so")))
        (values link-flags libdir library-extension)))))

(defun link-flags ()
  (default-link-flags))
(export 'link-flags)


(si:fset 'and
         #'(lambda (whole env)
             (declare (ignore env))
             (let ((forms (cdr whole)))
               (if (null forms)
                   t
                   (if (null (cdr forms))
                       (car forms)
                         `(if ,(car forms)
                              (and ,@(cdr forms)))))))
           t)

(si:fset 'or
         #'(lambda (whole env)
             (declare (ignore env))
             (let ((forms (cdr whole)))
               (if (null forms)
                   nil
                   (if ( null (cdr forms))
                       (car forms)
                       (let ((tmp (gensym)))
                         `(let ((,tmp ,(car forms)))
                            (if ,tmp
                                ,tmp
                                (or ,@(cdr forms)))))))))
           t )
(export '(and or))

(defun 1- (num) (- num 1))
(defun 1+ (num) (+ num 1))

;;; These definitions do not use setf, and are replaced in setf.lisp.
#+clasp-min
(si::fset 'incf
	   #'(lambda (args env)
               (declare (core:lambda-name incf))
               (let* ((where (second args))
                      (what (caddr args)))
                 (if what
                     `(setq ,where (+ ,where ,what))
                     `(setq ,where (1+ ,where)))))
	  t)

#+clasp-min
(si::fset 'decf
	   #'(lambda (args env)
               (declare (core:lambda-name decf))
               (let* ((where (second args))
                      (what (caddr args)))
                 (if what
                     `(setq ,where (- ,where ,what))
                     `(setq ,where (1- ,where)))))
	  t)

(defun build-target-dir (type &optional stage)
  (declare (ignore type))
  (let* ((stage (if stage
                    stage
                    (default-target-stage)))
         (type-modified-host-suffix (build-configuration))
         (bitcode-host (core:fmt nil "{}{}-bitcode" stage type-modified-host-suffix)))
    bitcode-host))

(defun default-target-stage ()
  (if (member :eclasp *features*)
      "e"
      (if (member :mclasp *features*)
          "m"
          (if (member :vclasp *features*)
              "v"
              (if (member :cclasp *features*)
                  "c"
                  (if (member :bclasp *features*)
                      (if (member :compiling-cleavir *features*)
                          "pre"
                          "b")
                      "a"))))))

(defun build-configuration ()
  (let ((gc (cond
              ((member :use-mps *features*) "mps")
              ((member :use-mmtk *features*)
               (if (member :use-precise-gc *features*)
                   "mmtkprecise"
                   "mmtk"))
              ((member :use-boehm *features*)
               (if (member :use-precise-gc *features*)
                   "boehmprecise"
                   "boehm"))
              (t (error "Unknown clasp configuration"))))
        (mpi (if (member :use-mpi *features*) "-mpi" "")))
    (core:fmt nil "{}-{}{}" (lisp-implementation-type) gc mpi)))

(defun build-inline-bitcode-pathname (link-type &optional (filetype :intrinsics))
  (let ((name (cond
                ((eq filetype :intrinsics) "intrinsics")
                ((eq filetype :builtins) "builtins-no-debug-info")
                (t (error "illegal filetype - only :intrinsics or :builtins allowed")))))
    (cond ((eq link-type :fasl)
           (make-pathname :host "SYS"
                          :directory '(:absolute "LIB")
                          :name (core:fmt nil "{}-cxx.a" name)
                          :type "a"))
          ((eq link-type :compile)
           (make-pathname :host "SYS"
                          :directory '(:absolute "LIB")
                          :name (core:fmt nil "{}-cxx.bc" name)
                          :type "a"))
          ((eq link-type :executable)
           (make-pathname :host "SYS"
                          :directory '(:absolute "LIB")
                          :name (core:fmt nil "{}-all-cxx.a" name)
                          :type "a"))
          (t
           (error "Provide a bitcode file for the link-type ~a" link-type)))))

(defun build-common-lisp-bitcode-pathname ()
  (make-pathname :host "SYS"
                 :directory '(:absolute "LIB")
                 :name "common-lisp-cxx.a"
                 :type "a"))

(export '(build-inline-bitcode-pathname build-common-lisp-bitcode-pathname))

#+(or)
(progn
  (defconstant +image-pathname+ (make-pathname :directory '(:relative) :name "image" :type "fasl"))
  (export '(+image-pathname+ )))

(defun bitcode-extension ()
  (if cmp::*use-human-readable-bitcode*
      "ll"
      "bc"))

(export 'bitcode-extension)

(defun build-extension (type)
  (cond ((eq type :fasl)
         "fasl")
        ((eq type :ll)
         "ll")
        ((eq type :object)
         "o")
        ((eq type :bytecode)
         "faslbc")
        ((eq type :fasp)
         "fasp")
        ((eq type :faso)
         "faso")
        ((eq type :fasoll)
         "fasoll")
        ((eq type :faspll)
         "faspll")
        ((eq type :fasobc)
         "fasobc")
        ((eq type :faspbc)
         "faspbc")
        ((and (eq type :bitcode) cmp::*use-human-readable-bitcode*)
         "ll")
        ((eq type :bitcode)
         "bc")
        (t
         (error "Unsupported build-extension type ~a" type))))

(defun build-library-type (type)
  "Given the object-type TYPE return what type of library it generates"
  (cond ((eq type :fasl)
         :fasl)
        ((eq type :object)
         :fasl)
        ((eq type :fasp)
         :fasp)
        ((eq type :faso)
         :fasp)
        ((eq type :fasoll)
         :faspll)
        ((eq type :faspll)
         :faspll)
        ((eq type :fasobc)
         :faspbc)
        ((eq type :faspbc)
         :faspbc)
        (t
         (error "Unsupported build-extension type ~a" type))))

(defun bitcode-pathname (pathname &optional (type cmp:*default-object-type*) stage)
  (declare (ignore stage))
  (make-pathname :host "sys"
                 :directory (list* :absolute
                                   "LIB"
                                   (cdr (pathname-directory pathname)))
                 :name (pathname-name pathname)
                 :type (build-extension type)))

(export '(build-extension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Early macros ....
;;;
;;;


#-staging
(eval-when (:execute)
  (eval-when (eval compile load)
    (si::select-package "SI"))

  ;; This is needed only when bootstrapping CLASP using CLASP-MIN
  (eval-when (eval)
    (si::fset 'in-package
              #'(lambda (def env)
                  (declare (core:lambda-name in-package))
		  `(eval-when (eval compile load)
		     (si::select-package ,(string (second def)))))
	      t)
    )

  ;;
  ;; This is also needed for booting Clasp. In particular it is required in
  ;; defmacro.lisp.
  ;;

  ;; Required by REGISTER-GLOBAL in cmp/cmpvar.lisp
  (si::fset 'pushnew #'(lambda (w e)
                         (declare (ignore e))
                         (let ((item (cadr w))
                               (place (caddr w)))
                           `(setq ,place (adjoin ,item ,place))))
            t)

  (si::fset 'push #'(lambda (w e)
                      (declare (ignore e))
                      (let ((item (cadr w))
                            (place (caddr w)))
                        `(setq ,place (cons ,item ,place))))
            t)



  (fset 'when #'(lambda (def env)
                  (declare (ignore env))
                  `(if ,(cadr def) (progn ,@(cddr def))))
        t)


  (fset 'unless #'(lambda (def env)
                    (declare (ignore env))
                    `(if ,(cadr def) nil (progn ,@(cddr def))))
        t)


  (defun si::while-until (test body jmp-op)
    (let ((label (gensym))
          (exit (gensym)))
      `(TAGBODY
          (GO ,exit)
          ,label
          ,@body
          ,exit
          (,jmp-op ,test (GO ,label)))))

  (fset 'si::while #'(lambda (def env)
                       (declare (ignore env))
                       (si::while-until (cadr def) (cddr def) 'when))
        t)


  (fset 'si::until #'(lambda (def env)
                       (declare (ignore env))
                       (si::while-until (cadr def) (cddr def) 'unless))
        t)

  (core:fset 'multiple-value-bind
             #'(lambda (whole env)
                 (declare (core:lambda-name multiple-value-bind-macro))
                 (declare (ignore env))
                 (let ((vars (cadr whole))
                       (form (caddr whole))
                       (body (cdddr whole))
                       (restvar (gensym)))
                   `(multiple-value-call
                        #'(lambda (&optional ,@(mapcar #'list vars) &rest ,restvar)
                            (declare (ignore ,restvar))
                            ,@body)
                      ,form)))
             t)


  (defun filter-dolist-declarations (declarations)
    (let ((a nil))
      (mapc #'(lambda (clause)
                (when (not (and (consp clause)
                                (or (eq (car clause) 'type)
                                    (eq (car clause) 'ignore))))
                  (setq a (cons clause a))))
            declarations)
      (nreverse a)))

  (let ((f #'(lambda (whole env)
               (declare (ignore env) (core:lambda-name dolist))
               (let (body control var expr exit)
                 (setq body (rest whole))
                 (when (endp body)
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
                 (setq control (first body) body (rest body))
                 (when (endp control)
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
                 (setq var (first control) control (rest control))
                 (if (<= 1 (length control) 2)
                     (setq expr (first control) exit (rest control))
                     (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
                 (multiple-value-bind (declarations body)
                     (process-declarations body nil)
                   `(block nil
                      (let* ((%dolist-var ,expr))
                        (si::while %dolist-var
                                   (let ((,var (first %dolist-var)))
                                     (declare ,@declarations)
                                     (tagbody
                                        ,@body
                                        (setq %dolist-var (cdr %dolist-var))))))
                      ,(when exit
                         `(let ((,var nil))
                            (declare (ignorable ,var)
                                     ,@(filter-dolist-declarations declarations))
                            ,@exit))))))))
    (si::fset 'dolist f t '((var list-form &optional result-form) &body body)))

  (let ((f #'(lambda (whole env)
               (declare (ignore env) (core:lambda-name dotimes))
               (let (body control var expr exit)
                 (setq body (rest whole))
                 (when (endp body)
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
                 (setq control (first body) body (rest body))
                 (when (endp control)
                   (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
                 (setq var (first control) control (rest control))
                 (if (<= 1 (length control) 2)
                     (setq expr (first control) exit (rest control))
                     (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
                 (multiple-value-bind (declarations body)
                     (process-declarations body nil)
                   (when (and (integerp expr) (>= expr 0))
                     (setq declarations
                           (cons `(type (integer 0 ,expr) ,var) declarations)))
                   `(block nil
                      (let* ((%dotimes-var ,expr)
                             (,var 0))
                        (declare ,@declarations)
                        (si::while (< ,var %dotimes-var)
                                   ,@body
                                   (setq ,var (1+ ,var)))
                        ,@exit)))))))
    (si::fset 'dotimes f t '((var count-form &optional result-form) &body body)))

  (let ((f #'(lambda (whole env)
               (declare (ignore env) (core:lambda-name do/do*-expand))
               (let (do/do* control test result vlexport step let psetq body)
                 (setq do/do* (first whole) body (rest whole))
                 (if (eq do/do* 'do)
                     (setq let 'LET psetq 'PSETQ)
                     (setq let 'LET* psetq 'SETQ))
                 (when (endp body)
                   (simple-program-error "Syntax error first (endp body) in ~A:~%~A" do/do* whole))
                 (setq control (first body) body (rest body))
                 (when (endp body)
                   (simple-program-error "Syntax error second (endp body) in ~A:~%~A" do/do* whole))
                 (setq test (first body) body (rest body))
                 (when (endp test)
                   (simple-program-error "Syntax error (endp test) in ~A:~%~A" do/do* whole))
                 (setq result (rest test) test (first test))
                 (dolist (c control)
                   (when (symbolp c) (setq c (list c)))
                   (let ((lenc (length c)))
                     (cond
                       ((or (eql lenc 1) (eql lenc 2))
                        (setq vlexport (cons c vlexport)))
                       ((eql lenc 3)
                        (setq vlexport (cons (butlast c) vlexport)
                              step (list* (third c) (first c) step)))
                       (t
                        (simple-program-error "Syntax error (length not 1,2,3 - its ~a and c is ~s) in ~A:~%~A" (length c) c do/do* whole)))))
                 (multiple-value-bind (declarations real-body)
                     (process-declarations body nil)
                   `(BLOCK NIL
                      (,let ,(nreverse vlexport)
                        (declare ,@declarations)
                        (sys::until ,test
                                    ,@real-body
                                    ,@(when step (list (cons psetq (nreverse step)))))
                        ,@(or result '(nil)))))))))
    (si::fset 'do f t '(vars test &body body))
    (si::fset 'do* f t '(vars test &body body)))

  (si::fset 'prog1 #'(lambda (whole env)
                       (declare (ignore env))
                       (let ((sym (gensym))
                             (first (cadr whole))
                             (body (cddr whole)))
                         (if body
                             `(let ((,sym ,first))
                                ,@body
                                ,sym)
                             first)))
            t)
  )


#-staging
(eval-when (:execute)
  (load #P"sys:src;lisp;kernel;cmp;jit-setup.lisp")
  (load #P"sys:src;lisp;kernel;clsymbols.lisp"))

(defun command-line-paths (&optional (start 0)
                           &aux (index (length core:*command-line-arguments*))
                                paths)
  (tagbody
   next
    (if (> index start)
        (progn
          (setq index (- index 1)
                paths (cons (pathname (elt core:*command-line-arguments* index)) paths))
          (go next))))
  paths)

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'process-command-line-load-eval-sequence)
(defun process-command-line-load-eval-sequence ()
  (mapcar #'(lambda (entry)
              (if (eq (car entry) :load)
                  (load (cdr entry))
                  (if (eq (car entry) :script)
                    (core:load-source (cdr entry) nil nil nil t)
                    (eval (read-from-string (cdr entry))))))
          (core:command-line-load-eval-sequence)))

(export 'maybe-load-clasprc)
(defun maybe-load-clasprc ()
  "Maybe load the users startup code"
  (if (not (core:no-rc-p))
      (let ((clasprc (core:rc-file-name)))
        (if (probe-file clasprc)
            (progn
              (if (not (core:noinform-p))
                  (core:fmt t "Loading resource file {}%N" clasprc))
              (core:load-source clasprc))
            (if (not (core:noinform-p))
                (core:fmt t "Resource file {} not found, skipping loading of it.%N" clasprc))))))

(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-dir-command (raw-dir)
  (let* ((corrected-dir (format nil "~a/" (string-right-trim "/" (string raw-dir))))
         (dir (pathname-directory (parse-namestring corrected-dir)))
         (pn-dir (mapcar #'(lambda (x) (if (eq x :up) :back x)) dir))
         (new-pathname (merge-pathnames (make-pathname :directory pn-dir) *default-pathname-defaults*))
         )
    (setq *default-pathname-defaults* new-pathname)))


;;; I moved the build system code out of init.lisp and
;;; put it in clasp-builder.lisp

(when (member :clasp-builder *features*)
  (load "sys:src;lisp;kernel;clasp-builder.lisp"))


(defun tpl-hook (cmd)
  (cond
    ((eq (car cmd) :pwd) (tpl-default-pathname-defaults-command))
    ((eq (car cmd) :cd) (tpl-change-default-pathname-defaults-dir-command (cadr cmd)))
    (t (core:fmt t "Unknown command {}%N" cmd))))

(setq *top-level-command-hook* #'tpl-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start everything up
;;

(export 'core:top-level)

(defun init-toplevel ()
  (process-command-line-load-eval-sequence)
  (core:low-level-repl))

;(eval-when (:load-toplevel)
  (setq ext:*toplevel-hook* 'init-toplevel);)
