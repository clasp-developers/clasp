;;;
;;;    File: compilefile.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-


(in-package :cmp)

(defvar *compile-verbose* nil )
(defvar *compile-print* nil )




#+(or)
(defun compile-main-function (name ltv-manager-fn)
  (cmp-log "In compile-main-function\n")
  (let ((main-fn (with-new-function (main-fn fn-env fn-result
					     :function-name name
					     :parent-env nil
					     :linkage 'llvm-sys:internal-linkage ;; 'llvm-sys:external-linkage
					     :function-type +fn-prototype+
					     :argument-names +fn-prototype-argument-names+)
		   (irc-low-level-trace :up)
		   (let* ((given-name (llvm-sys:get-name main-fn)))
		     (irc-low-level-trace)
		     (cmp-log "About to add invokeMainFunction for ltv-manager-fn\n")
		     (irc-intrinsic "invokeMainFunction" (irc-constant-string-ptr *gv-source-namestring*) ltv-manager-fn)
                     (irc-intrinsic "cc_setTmvToNil" fn-result)))))
    ;;    (cmp-log-dump main-fn)
    (cmp-log "Done compile-main-function")
    main-fn))



(defun do-compilation-unit (closure &key override)
  (cond (override
	 (let* ((*active-protection* nil))
	   (do-compilation-unit closure)))
	((null *active-protection*)
	 (let* ((*active-protection* t)
		(*pending-actions* nil)
                (*compilation-unit-module-index* 0)
                (*all-functions-for-one-compile* nil))
	   (unwind-protect (do-compilation-unit closure)
             (progn
               (dolist (action *pending-actions*)
               (funcall action))))))
	(t
	 (funcall closure))))

(export 'do-compilation-unit)
(defmacro with-compilation-unit ((&rest options) &body body)
  `(do-compilation-unit #'(lambda () ,@body) ,@options))



#||
(defvar *compilation-messages* nil)
(defvar *compilation-warnings-p* nil)
(defvar *compilation-failures-p* nil)
         #+clasp-min (progn ,@body)
         #-clasp-min (handler-bind
                       ((error #'(lambda (c)
                                   (invoke-restart 'record-failure c)))
                        (warning #'(lambda (c)
                                     (invoke-restart 'record-warning c))))
                     ,@body))
||#



(defun describe-form (form)
  (cond
    ((and (consp form) (eq 'core:fset (car form)))
     (let* ((name (cadr (cadr form)))
	    (is-macro (cadddr form))
	    (header (if is-macro
			"DEFMACRO"
			"DEFUN")))
       (bformat t ";    %s %s\n" header name)))
    ((and (consp form)
          (eq (first form) 'cl:let)
          (and (let ((x (third form)))
                 (and (consp x) (eq 'cl:quote (first x))
                      (eq core::*special-defun-symbol* (second x))))))
     (bformat t ";    DEFUN %s\n" (second (fourth form))))
    ((and (consp form)
          (eq (first form) 'core::do-defsetf))
     (bformat t ";    DEFSETF %s\n" (third form)))
    ((and (consp form)
          (eq (first form) 'core::do-deftype))
     (bformat t ";    DEFTYPE %s\n" (third form)))
    ((and *compile-verbose* (consp form))
     (let* ((second-part (bformat nil "%s %s %s" (second form) (third form) (fourth form)))
            (trimmed-second-part (if (> (length second-part) 60)
                                     (bformat nil "%s..." (subseq second-part 0 60))
                                     second-part)))
       (bformat t";    %s %s\n" (car form) trimmed-second-part)))))

(defun compile-top-level (form)
  (when *compile-print*
    (describe-form form))
  (literal:with-top-level-form (compile-thunk 'repl form nil)))

(defun t1progn (rest env)
  "All forms in progn at top level are top level forms"
  (dolist (form rest)
    (t1expr form env)))

(defun t1eval-when (rest env)
  (let ((situations (car rest))
	(body (cdr rest)))
    (when (or (member 'cl:compile situations) (member :compile-toplevel situations))
      (cmp-log "Performing eval-when :compile-toplevel side-effects\n")
      (cmp-log "Evaluating: %s\n" body)
      (funcall core:*eval-with-env-hook* `(progn ,@body) env)
      (cmp-log "Done eval-when compile-toplevel side-effects\n"))
    (when (or (member 'cl:load situations) (member :load-toplevel situations))
      (cmp-log "Compiling body due to :load-toplevel --> %s\n" body)
      ;; Each subform is a top-level form
      (dolist (subform body)
	(t1expr subform env))
      (cmp-log "Done compiling body due to :load-toplevel\n")))) 

#+(or)(defun t1locally (rest env)
  (multiple-value-bind (declares code docstring specials)
      (process-declarations rest nil)
    ;; TODO: Do something with the declares!!!!!  They should be put into the environment
    (let ((new-env (core:make-value-environment-for-locally-special-entries specials env)))
      (t1progn code new-env))))

(defun t1locally (rest env)
  (multiple-value-bind (declares code docstring specials)
      (process-declarations rest nil)
    (declare (ignore specials docstring))
    ;; TODO: Do something with the declares!!!!!  They should be put into the environment
    (t1progn code (augment-environment-with-declares env declares))))



(defun parse-macrolet (body)
  (let ((macros (mapcar (lambda (macro-def)
                        (let ((macro-fn (eval (core:parse-macro (car macro-def)
                                                                (cadr macro-def)
                                                                (cddr macro-def)))))
                          (set-kind macro-fn :macro)
                          (cons (car macro-def) macro-fn)))
                      (car body))))
    (multiple-value-bind (decls macrolet-body)
        (core:process-declarations (cdr body) nil)
      (values macros decls macrolet-body))))

(defun parse-symbol-macrolet (body)
  (let ((macros (mapcar (lambda (macro-def)
                          (cons (car macro-def)
                                (constantly (cadr macro-def))))
                             (car body))))
    (multiple-value-bind (decls symbol-macrolet-body)
        (core:process-declarations (cdr body) nil)
      (values macros decls symbol-macrolet-body))))

(export '(parse-macrolet parse-symbol-macrolet))


(defun t1macrolet (rest env)
  (multiple-value-bind (macros declares body)
      (parse-macrolet rest)
    (let ((macro-env (irc-new-macrolet-environment env)))
      (mapc (lambda (macro)
              (core:add-macro macro-env (car macro) (cdr macro)))
            macros)
      (t1progn body (augment-environment-with-declares macro-env declares)))))

(defun t1symbol-macrolet (rest env)
  (multiple-value-bind (macros declares body)
      (parse-symbol-macrolet rest)
    (let ((macro-env (irc-new-symbol-macrolet-environment env)))
      (mapc (lambda (macro)
              (core:add-symbol-macro macro-env (car macro) (cdr macro)))
            macros)
      (t1progn body (augment-environment-with-declares macro-env declares)))))


(defun t1expr (form &optional env)
  (cmp-log "t1expr-> %s\n" form)
  (push form core:*top-level-form-stack*)
  (unwind-protect
       (let ((head (if (atom form) form (car form))))
         (cond
           ((eq head 'cl:eval-when) (t1eval-when (cdr form) env))
           ((eq head 'cl:progn) (t1progn (cdr form) env))
           ((eq head 'cl:locally) (t1locally (cdr form) env))
           ((eq head 'cl:macrolet) (t1macrolet (cdr form) env))
           ((eq head 'cl:symbol-macrolet) (t1symbol-macrolet (cdr form) env))
           ((and (listp form)
                 ;;(symbolp (car form))
                 (not (core:lexical-function (car form) env))
                 (not (core:lexical-macro-function (car form) env))
                 (not (core:declared-global-notinline-p (car form)))
                 (let ((expansion (core:compiler-macroexpand form env)))
                   (if (eq expansion form)
                       nil
                       (progn
                         (t1expr expansion env)
                         t)))))
           ((macro-function head env)
            (let ((expanded (macroexpand form env)))
              (t1expr expanded env)))
           (t (compile-top-level form))))
    (pop core:*top-level-form-stack*)))


(defun compile-file-form (form compile-file-hook)
  ;; If the Cleavir compiler hook is set up then use that
  ;; to generate code 
  (if compile-file-hook
      (funcall compile-file-hook form)
      (t1expr form)))

(defun cfp-output-extension (output-type)
  (cond
    ((eq output-type :bitcode) "bc")
    ((eq output-type :object) "o")
    ((eq output-type :fasl) "fasl")
    ((eq output-type :executable) #-windows "" #+windows "exe")
    (t (error "unsupported output-type ~a" output-type))))

(defun cfp-output-file-default (input-file output-type &key target-backend)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*)))
    (when target-backend
      (setq defaults (make-pathname :host target-backend :defaults defaults)))
    (make-pathname :type (cfp-output-extension output-type)
                   :defaults defaults)))


;;; Copied from sbcl sb!xc:compile-file-pathname
;;;   If INPUT-FILE is a logical pathname and OUTPUT-FILE is unsupplied,
;;;   the result is a logical pathname. If INPUT-FILE is a logical
;;;   pathname, it is translated into a physical pathname as if by
;;;   calling TRANSLATE-LOGICAL-PATHNAME.
;;; So I haven't really tried to make this precisely ANSI-compatible
;;; at the level of e.g. whether it returns logical pathname or a
;;; physical pathname. Patches to make it more correct are welcome.
(defun compile-file-pathname (input-file &key (output-file nil output-file-p)
                                           (output-type :fasl)
					   type
					   target-backend
                                           &allow-other-keys)
  (when type (error "Clasp compile-file-pathname uses :output-type rather than :type"))
  (let* ((pn (if output-file-p
		 (merge-pathnames output-file (translate-logical-pathname (cfp-output-file-default input-file output-type :target-backend target-backend)))
		 (cfp-output-file-default input-file output-type :target-backend target-backend)))
         (ext (cfp-output-extension output-type)))
    (make-pathname :type ext :defaults pn)))



(defun cf-module-name (type pathname)
  "Create a module name from the TYPE (either :user or :kernel)
and the pathname of the source file - this will also be used as the module initialization function name"
  (string-downcase (bformat nil "___%s_%s" (string type) (pathname-name pathname))))



(defun compile-file-results (output-file conditions)
  (let (warnings-p failures-p)
    (dolist (cond conditions)
      (cond
        ((typep cond 'compiler-error)
         (setq failures-p t))
        ((typep cond 'compiler-warning)
         (setq warnings-p t))
        (t (error "Illegal condition ~a" cond))))
    (values output-file warnings-p failures-p)))

(defvar *debug-compile-file* nil)

(defun compile-file-to-module (given-input-pathname output-path &key compile-file-hook type source-debug-namestring (source-debug-offset 0))
  "* Arguments
- given-input-pathname :: A pathname.
- output-path :: A pathname.
- compile-file-hook :: A function that will do the compile-file
- type :: :kernel or :user (I'm not sure this is useful anymore
- source-debug-namestring :: A namestring.
- source-debug-offset :: An integer.
Compile a lisp source file into an LLVM module.  type can be :kernel or :user"
  ;; TODO: Save read-table and package with unwind-protect
  (let* ((clasp-source-root (translate-logical-pathname "source-dir:"))
         (clasp-source (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type :wild) clasp-source-root))
         (source-location
          (if (pathname-match-p given-input-pathname clasp-source)
              (enough-namestring given-input-pathname clasp-source-root)
              given-input-pathname))
         (input-pathname (probe-file given-input-pathname))
	 (source-sin (open input-pathname :direction :input))
	 (module (create-llvm-module-for-compile-file (namestring input-pathname)))
	 (module-name (cf-module-name type given-input-pathname))
	 warnings-p failure-p)
    (or module (error "module is NIL"))
    (with-open-stream (sin source-sin)
      ;; If a truename is provided then spoof the file-system to treat input-pathname
      ;; as source-truename with the given offset
      (when source-debug-namestring
	(core:source-file-info (namestring input-pathname) source-debug-namestring source-debug-offset nil))
      (when *compile-verbose*
	(bformat t "; Compiling file: %s\n" (namestring input-pathname)))
      (with-one-source-database
	  (cmp-log "About to start with-compilation-unit\n")
	(let* ((*compile-file-pathname* (pathname (merge-pathnames given-input-pathname)))
	       (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*)))
	  (with-module (:module module
                                :source-namestring (namestring source-location)
                                :source-debug-namestring source-debug-namestring
                                :source-debug-offset source-debug-offset)
            (with-debug-info-generator (:module *the-module*
                                                :pathname *compile-file-truename*)
              (or *the-module* (error "*the-module* is NIL"))
              (let ((eof-value (gensym)))
                (with-make-new-run-all (run-all-function)
                  (with-run-all-body-codegen ;;(result)
                      (irc-intrinsic "ltvc_assign_source_file_info_handle"
                                     (irc-constant-string-ptr *gv-source-namestring*)
                                     (irc-constant-string-ptr *gv-source-debug-namestring*)
                                     (jit-constant-i64 *source-debug-offset*)
                                     (jit-constant-i32 (if *source-debug-use-lineno* 1 0))
                                     *gv-source-file-info-handle*))
                  (with-constants-table
                      (loop
                         (let* ((top-source-pos-info (core:input-stream-source-pos-info source-sin))
                                (form (read source-sin nil eof-value)))
                           (when *debug-compile-file* (bformat t "compile-file: %s\n" form))
                           (if (eq form eof-value)
                               (return nil)
                               (compile-file-form form compile-file-hook))))
                    (make-boot-function-global-variable *the-module* run-all-function)))))
            (cmp-log "About to verify the module\n")
            (cmp-log-dump *the-module*)
            (irc-verify-module-safe *the-module*)
            (quick-module-dump *the-module* "preoptimize"))
          (quick-module-dump module "postoptimize")
          module)))))

(defvar *compile-file-output-pathname* nil)
(defun compile-file* (compile-file-hook
                      given-input-pathname
                      &key
                        (output-file nil output-file-p)
                        (verbose *compile-verbose*)
                        (print *compile-print*)
                        (system-p nil system-p-p)
                        (external-format :default)
                        ;; If we are spoofing the source-file system to treat given-input-name
                        ;; as a part of another file then use source-truename to provide the
                        ;; truename of the file we want to mimic
                        source-debug-namestring
                        ;; This is the offset we want to spoof
                        (source-debug-offset 0)
                        ;; output-type can be (or :fasl :bitcode :object)
                        (output-type :fasl)
;;; type can be either :kernel or :user
                        (type :user)
                      &aux conditions
                        )
  "See CLHS compile-file."
  (if system-p-p (error "I don't support system-p keyword argument - use output-type"))
  (if (not output-file-p) (setq output-file (cfp-output-file-default given-input-pathname output-type)))
  (with-compiler-env (conditions)
    (let ((*compile-print* print)
	  (*compile-verbose* verbose))
      ;; Do the different kind of compile-file here
      (let* ((output-path (compile-file-pathname given-input-pathname :output-file output-file :output-type output-type ))
             (*compile-file-output-pathname* output-path)
	     (module (compile-file-to-module given-input-pathname output-path 
					     :type type 
					     :source-debug-namestring source-debug-namestring 
					     :source-debug-offset source-debug-offset
                                             :compile-file-hook compile-file-hook)))
	(cond
	  ((eq output-type :object)
	   (when verbose (bformat t "Writing object to %s\n" (core:coerce-to-filename output-path)))
	   (ensure-directories-exist output-path)
	   (with-open-file (fout output-path :direction :output)
	     (let ((reloc-model (cond
				  ((member :target-os-linux *features*) 'llvm-sys:reloc-model-pic-)
				  (t 'llvm-sys:reloc-model-undefined))))
	       (generate-obj-asm module fout :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model))))
	  ((eq output-type :bitcode)
	   (when verbose (bformat t "Writing bitcode to %s\n" (core:coerce-to-filename output-path)))
	   (ensure-directories-exist output-path)
	   (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename output-path)))
	  ((eq output-type :fasl)
	   (ensure-directories-exist output-path)
	   (let ((temp-bitcode-file (compile-file-pathname given-input-pathname :output-file output-file :output-type :bitcode)))
	     (ensure-directories-exist temp-bitcode-file)
	     (bformat t "Writing temporary bitcode file to: %s\n" temp-bitcode-file)
	     (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename temp-bitcode-file))
	     (bformat t "Writing fasl file to: %s\n" output-file)
	     (llvm-link output-file
                        :lisp-bitcode-files (list temp-bitcode-file))))
	  (t ;; fasl
	   (error "Add support to file of type: ~a" output-type)))
	(dolist (c conditions)
	  (bformat t "conditions: %s\n" c))
        (llvm-sys:module-delete module)
        (compile-file-results output-path conditions))))))

(defun compile-file (&rest args)
  ;; Use the *cleavir-compile-file-hook* to determine which compiler to use
  ;; if nil == bclasp
  ;; if #'clasp-cleavir:cleavir-compile-file-form  == cclasp
  (gctools:garbage-collect)
  (apply #'compile-file* *cleavir-compile-file-hook* args))


(defun bclasp-compile-file (input-file &rest args &key &allow-other-keys)
  (let ((*cleavir-compile-file-hook* nil)
        (core:*use-cleavir-compiler* nil))
    (apply #'compile-file input-file args)))

(export 'compile-file)
