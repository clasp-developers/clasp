;;;
;;;    File: debuginfo.lsp
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

(defconstant +file.dbg+ :file.dbg)

(defvar *dbg-generate-dwarf* nil)
(defvar *dbg-compile-unit* )
(defvar *dbg-current-file* )
(defvar *dbg-current-function*)
(defvar *dbg-current-scope* nil
  "Stores the current enclosing lexical scope for debugging info")



(defun walk-form-for-source-info (form)
  (let* ((lineno (source-pos-info-lineno *current-source-pos-info*))
         (filepos (source-pos-info-filepos *current-source-pos-info*))
         (source-file-info (source-file-info *current-source-pos-info*))
         (pathname (source-file-info-pathname source-file-info))
         (source-directory (directory-namestring pathname))
         (source-filename (file-namestring pathname)))
    (values source-directory source-filename filepos lineno 0)))
#|  
  (bformat t "walk-form-for-source-info *current-source-pos-info* -> %s\n" core:*current-source-pos-info*)
  (multiple-value-bind (source-file-info file-pos line-number column)
      (core:walk-to-find-source-info form)
    (when source-file-info
      (let* ((source-pathname (source-file-info-pathname source-file-info))
             (source-directory (directory-namestring source-pathname))
             (source-filename (file-namestring source-pathname)))
        (bformat t "    Returning source-filename: %s line-number: %d\n" source-filename line-number)
        (return-from walk-form-for-source-info
          (values source-directory source-filename file-pos line-number column)))))
  (bformat t "    Returning no-file 0\n")
  (values "no-dir" "no-file" 0 0 0))
|#



(defun dbg-create-function-type (difile function-type)
  "Currently create a bogus function type"
  (let ((arg-array (llvm-sys:get-or-create-type-array
		    *the-module-dibuilder*
		    (list
		     (llvm-sys:create-basic-type *the-module-dibuilder* "int" 32 32 llvm-sys:+dw-ate-signed-fixed+)
		     ))))
    (llvm-sys:create-subroutine-type *the-module-dibuilder* difile arg-array 0 )))


(defvar *dibuilder-type-hash-table* nil
  "Map types to DITypes")

(defmacro with-dibuilder ((module) &rest body)
  `(if *dbg-generate-dwarf*
       (let ((*the-module-dibuilder* (llvm-sys:make-dibuilder ,module))
	     (*dibuilder-type-hash-table* (make-hash-table :test #'eq)))
	 (unwind-protect
	      (progn ,@body)
	   (progn
	     (llvm-sys:finalize *the-module-dibuilder*)
	     ;; add the flag that defines the Dwarf Version
	     (llvm-sys:add-module-flag *the-module*
				       (llvm-sys:mdnode-get cmp:*llvm-context*
							    (list
							     (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
							     (llvm-sys:mdstring-get *llvm-context* "Dwarf Version")
							     (llvm-sys:value-as-metadata-get (jit-constant-i32 +debug-dwarf-version+)))))
	     (llvm-sys:add-module-flag *the-module*
				       (llvm-sys:mdnode-get cmp:*llvm-context*
							    (list
							     (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
							     (llvm-sys:mdstring-get *llvm-context* "Debug Info Version")
							     (llvm-sys:value-as-metadata-get (jit-constant-i32 llvm-sys:+debug-metadata-version+))))) ;; Debug Info Version
	     "This should not be the return value - it should be what is returned in the unwind-protect body"
	     )))
       (let ((*the-module-dibuilder* nil))
	 ,@body))
  )



(defun dbg-filename* (pn)
  (bformat nil "%s.%s" (pathname-name pn) (pathname-type pn)))

(defmacro with-dbg-compile-unit ((source-pathname) &rest body)
  (let ((path (gensym))
	(file-name (gensym))
	(dir-name (gensym)))
    `(if (and *dbg-generate-dwarf* *the-module-dibuilder*)
	 (progn
	   (let* ((,path (pathname ,source-pathname))
		  (,file-name (dbg-filename* ,path))
		  (,dir-name (directory-namestring ,path))
		  (*dbg-compile-unit* (llvm-sys:create-compile-unit
				       *the-module-dibuilder*
				       llvm-sys:dw-lang-c  ;; llvm-sys:dw-lang-common-lisp
				       ,file-name
				       ,dir-name
				       "clasp Common Lisp compiler" ; producer
				       nil ; isOptimized
				       "-v" ; compiler flags
				       1    ; run-time version
				       "split-name.log" ; splitname
                                       :full-debug
                                       t
				       )))
	     (cmp-log "with-dbg-compile-unit *dbg-compile-unit*: %s\n" *dbg-compile-unit*)
	     (cmp-log "with-dbg-compile-unit source-pathname: %s\n" ,source-pathname)
	     (cmp-log "with-dbg-compile-unit file-name: [%s]\n" ,file-name)
	     (cmp-log "with-dbg-compile-unit dir-name: [%s]\n" ,dir-name)
	     ,@body
	     ))
	 (progn
	   (cmp-log "with-dbg-compile-unit not generating *dbg-compile-unit*")
	   ,@body))))

(defmacro with-dbg-file-descriptor ((source-pathname) &rest body)
  (let ((path (gensym))
	(file-name (gensym))
	(dir-name (gensym)))
    `(if (and *dbg-generate-dwarf* *the-module-dibuilder*)
	 (let* ((,path (pathname ,source-pathname))
		(,file-name (dbg-filename* ,path))
		(,dir-name (directory-namestring ,path))
		(*dbg-current-file* (llvm-sys:create-file
				     *the-module-dibuilder*
				     ,file-name
				     ,dir-name)))
;;		(*dbg-current-scope* *dbg-current-file*))
	   ,@body)
	 (progn
	   ,@body))))

(defvar *with-debug-info-generator* nil)
(defmacro with-debug-info-generator ((&key module pathname) &rest body)
  "One macro that uses three other macros"
  `(let ((*with-debug-info-generator* t))
     (with-dibuilder (,module)
       (with-dbg-compile-unit (,pathname)
	 (with-dbg-file-descriptor (,pathname)
	   ,@body)))))

(defvar *with-dbg-function* nil)
;; Set to NIL in with-dbg-function and T in dbg-set-current-source-pos
(defvar *dbg-set-current-source-pos* nil)
(defmacro with-dbg-function ((name &key linkage-name form function function-type) &rest body)
  (let ((source-dir (gensym))
        (source-name (gensym))
	(filepos (gensym))
        (lineno (gensym))
        (column (gensym)))
    `(let ((*with-dbg-function* t)
           (*dbg-set-current-source-pos* nil))
       (if (and *dbg-generate-dwarf* *the-module-dibuilder*)
           (multiple-value-bind (,source-dir ,source-name ,filepos ,lineno ,column)
               (walk-form-for-source-info ,form)
             (let* ((*dbg-current-function*
                     (llvm-sys:create-function
                      *the-module-dibuilder*
                      *dbg-current-file* ; *dbg-compile-unit*	; function scope
                      ,linkage-name      ; function name
                      ,linkage-name      ; mangled function name
                      *dbg-current-file* ; file where function is defined
                      ,lineno            ; lineno
                      (dbg-create-function-type *dbg-current-file* ,function-type) ; function-type
                      nil ; isLocalToUnit - true if this function is not externally visible
                      t ; isDefinition - true if this is a function definition
                      ,lineno ; scopeLine - set to the beginning of the scope this starts
                      0       ; flags
                      nil   ; isOptimized - true if optimization is on
                      ,function		; llvm:Function pointer
                      nil               ; TParam = 0
                      nil               ; Decl = 0
                      ))
                    (*dbg-current-scope* *dbg-current-function*))
               (cmp-log "with-dbg-function *dbg-compile-unit*: %s\n" *dbg-compile-unit*)
               (cmp-log "with-dbg-function *dbg-current-function*: %s\n" *dbg-current-function*)
               (cmp-log "with-dbg-function name: [%s]\n" ,name)
               (cmp-log "with-dbg-function linkage-name: [%s]\n" ,linkage-name)
               ,@body))
           (progn
             ,@body)))))

(defvar *with-dbg-lexical-block* nil)
(defmacro with-dbg-lexical-block ((block-form) &body body)
  (let ((source-dir (gensym))
        (source-name (gensym))
	(filepos (gensym))
        (lineno (gensym))
        (column (gensym)))
    `(let ((*with-dbg-lexical-block* t))
       (if (and *dbg-generate-dwarf* *the-module-dibuilder*)
           (multiple-value-bind (,source-dir ,source-name ,filepos ,lineno ,column)
               (walk-form-for-source-info ,block-form)
             (let* ((*dbg-current-scope*
                     (llvm-sys:create-lexical-block *the-module-dibuilder*
                                                    *dbg-current-scope*
                                                    *dbg-current-file*
                                                    ,lineno
                                                    ,column
                                                    #| 0  -- not used anymore TODO: Dwarf path discriminator   |# )))
               (cmp-log "with-dbg-lexical-block\n")
               ,@body))
           (progn
             ,@body)))))

(defun dbg-attach-debugging-info-to-value-frame (af symbol-list-designator env)
  (when *dbg-generate-dwarf*
    (irc-attach-debugging-info-to-value-frame af symbol-list-designator env)))

#+(or)(defun dbg-set-activation-frame-for-ihs-top (renv)
        (error "Depreciated dbg-set-activation-frame-for-ihs-top")
        (when *dbg-generate-dwarf*
          (irc-intrinsic "trace_setActivationFrameForIHSTop" (irc-renv new-env))))

(defun dbg-set-current-source-pos (form)
  (when *dbg-generate-dwarf*
    (setq *dbg-set-current-source-pos* t)
    (cmp-log "dbg-set-current-source-pos on form: %s\n" form)
    (multiple-value-bind (source-dir source-file filepos line-number column)
        (walk-form-for-source-info form)
      (llvm-sys:set-current-debug-location-to-line-column-scope *irbuilder* line-number column *dbg-current-scope*)
      (values source-dir source-file line-number column))))

(defun dbg-set-current-source-pos-for-irbuilder (form irbuilder)
  (with-irbuilder (irbuilder)
    (dbg-set-current-source-pos form)))

(defun check-debug-info-setup (irbuilder)
  "Signal an error if debug-info for the irbuilder is not setup properly for inlining"
  (unless (llvm-sys:current-debug-location irbuilder)
    (format t "Check the module~%")
    (break "The debug-info is not set for the current irbuilder")))

(defparameter *llvm-metadata* (make-hash-table))
(defun dbg-set-current-debug-location (filename pathname lineno column)
  (let* ((scope-name (bformat nil "%s>>%s" pathname filename))
	 (scope (gethash scope-name *llvm-metadata*)))
    (unless scope
      (setq scope (mdnode-file-descriptor filename pathname))
      (core::hash-table-setf-gethash *llvm-metadata* scope-name scope))
    #+(or)(let ((debugloc (llvm-sys:debug-loc-get lineno column scope)))
	    (llvm-sys:set-current-debug-location *irbuilder* debugloc))
    (llvm-sys:set-current-debug-location-to-line-column-scope *irbuilder* lineno column scope)))

(defvar *current-file-metadata-node* nil
  "Store the metadata node for the current source file info")

(defun dbg-set-current-line ()
  (bformat t "DEBUG - line/column = %d/%d\n" 1 2))

(defmacro dbg-set-current-debug-location-here ()
#||
  (let ((filename-gs (gensym))
	(path-gs (gensym))
	(lineno-gs (gensym))
	(column-gs (gensym)))
    `(multiple-value-bind (,filename-gs ,path-gs) (source-file-name)
       (multiple-value-bind (,lineno-gs ,column-gs) (source-line-column)
	 (dbg-set-current-debug-location ,filename-gs ,path-gs ,lineno-gs ,column-gs))))
  ||#)



#||
(defun dbg-push-invocation-history-stack (invocation-history-frame)
  (print "dbg-push-invocation-history-stack"))

(defun dbg-pop-invocation-history-stack ()
  (print "dbg-pop-invocation-history-stack"))
||#

(defun dbg-push-invocation-history-stack-top-af (env form)
  (print "dbg-set-invocation-history-stack-top-environment"))

(defun dbg-pop-invocation-history-stack-top-af (env form)
  (print "dbg-set-invocation-history-stack-top-environment"))

(defun dbg-set-invocation-history-stack-top-source-pos (form)
  #+trace-source-manager(progn
                          (print (list "Dumping backtrace and core:*source-database*" core:*source-database*))
                          (core:ihs-backtrace)
                          (core:dump-source-manager))
  (when *dbg-generate-dwarf*
    (multiple-value-bind (source-dir source-file filepos lineno column)
        (walk-form-for-source-info form)
      (when source-file
        (let ((ln lineno)
              (col column))
          (depreciated)
          #+(or)(irc-intrinsic "trace_setLineNumberColumnForIHSTop"
                         *gv-source-namestring*
                         *gv-source-file-info-handle*
                         (jit-constant-i64 filepos)
                         (jit-constant-i32 ln) 
                         (jit-constant-i32 col)))
        nil))))






#|
(defmacro compile-debug-print-object (msg obj)
  "Insert a call to debugPrintObject"
  (let ((sym-line (gensym))
	(sym-col (gensym))
	(sym-filename (gensym))
	(sym-pathname (gensym))
	(sym-msg (gensym)))
    `(multiple-value-bind (,sym-line ,sym-col) (source-line-column)
       (multiple-value-bind (,sym-filename ,sym-pathname) (source-file-name)
	 (let ((,sym-msg (llvm-sys:make-string-global *the-module* (bformat nil "%s:%d --> %s : obj->%s" ,sym-filename ,sym-line ,msg ,obj))))
	   (irc-intrinsic "debugPrintObject" ,sym-msg ,obj))))))
|#


#||
(defmacro compile-debug-print-i32 (msg obj)
  "Insert a call to debugPrintI32"
  (let ((sym-line (gensym))
	(sym-col (gensym))
	(sym-filename (gensym))
	(sym-pathname (gensym))
	(sym-msg (gensym)))
    `(multiple-value-bind (,sym-line ,sym-col) (source-line-column)
       (multiple-value-bind (,sym-filename ,sym-pathname) (source-file-name)
	 (let ((,sym-msg (llvm-sys:make-string-global *the-module* (bformat nil "%s:%d --> %s" ,sym-filename ,sym-line ,msg))))
	   (irc-intrinsic "debugPrintI32" ,sym-msg ,obj))))))
||#

(defun debug-generate-source-code (form)
  (let ((all-code (bformat nil "%s" form)))
    ;; TODO:  Return only the first XXX characters of all-code
    (jit-make-global-string-ptr (subseq all-code 0 80))))




;; -------------------------------------------------------
;; -------------------------------------------------------
;; -------------------------------------------------------
;; -------------------------------------------------------
;;
;; Tracing code
;;
;; Inserts calls that keep track of the current lexical
;; scope of code.
;;
;;
;; -------------------------------------------------------
;; -------------------------------------------------------
;; -------------------------------------------------------



(defmacro trace-enter-lexical-scope ( scope-name env form )
  nil ;; do nothing
  #+(or)`(dbg-set-activation-frame-for-ihs-top (irc-renv ,env)))


(defun trace-exit-lexical-scope (scope-name env traceid)
  nil ;; do nothing
  `(irc-intrinsic "trace_setActivationFrameForIHSTop" (irc-parent-renv ,env)))




(defun trace-enter-let-scope (env form)
  (trace-enter-lexical-scope "Let" env form))

(defun trace-exit-let-scope (env traceid)
  (trace-exit-lexical-scope "Let" env traceid))


(defun trace-enter-let*-scope (env form)
  (trace-enter-lexical-scope "LetSTAR" env form))

(defun trace-exit-let*-scope (env traceid)
  (trace-exit-lexical-scope "LetSTAR" env traceid))


(defun trace-enter-flet-scope (env form)
  (trace-enter-lexical-scope "Flet" env form))

(defun trace-exit-flet-scope (env traceid)
  (trace-exit-lexical-scope "Flet" env traceid))

(defun trace-enter-labels-scope (env form)
  (trace-enter-lexical-scope "Labels" env form))

(defun trace-exit-labels-scope (env traceid)
  (trace-exit-lexical-scope "Labels" env traceid))



(defun trace-enter-catch-scope (env form)
  (trace-enter-lexical-scope "Catch" env form ))

(defun trace-exit-catch-scope (env traceid)
  (trace-exit-lexical-scope "Catch" env traceid))



(defun trace-enter-block-scope (env form)
  (trace-enter-lexical-scope "Block" env  form))

(defun trace-exit-block-scope (env traceid)
  (trace-exit-lexical-scope "Block" env traceid))







(defun debug-gdb (env)
    (irc-intrinsic "gdb"))
