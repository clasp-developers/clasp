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

(defvar *dbg-generate-dwarf* t)
(defvar *dbg-compile-unit* )
(defvar *dbg-current-file* )
(defvar *dbg-current-function*)
(defvar *dbg-current-scope* nil
  "Stores the current enclosing lexical scope for debugging info")

(defvar *llvm-metadata*
  nil
  "Stores map of integers to metadata scopes")


(defun walk-form-for-source-info (form)
  (let* ((cspi (ext:current-source-location))
         (lineno (source-pos-info-lineno cspi))
         (filepos (source-pos-info-filepos cspi))
         (source-file-info (source-file-info cspi))
         (pathname (source-file-info-pathname source-file-info))
         (source-directory (directory-namestring pathname))
         (source-filename (file-namestring pathname)))
    (values source-directory source-filename filepos lineno 0)))
#|  
  (bformat t "walk-form-for-source-info *current-source-pos-info* -> %s%N" core:*current-source-pos-info*)
  (multiple-value-bind (source-file-info file-pos line-number column)
      (core:walk-to-find-source-info form)
    (when source-file-info
      (let* ((source-pathname (source-file-info-pathname source-file-info))
             (source-directory (directory-namestring source-pathname))
             (source-filename (file-namestring source-pathname)))
        (bformat t "    Returning source-filename: %s line-number: %d%N" source-filename line-number)
        (return-from walk-form-for-source-info
          (values source-directory source-filename file-pos line-number column)))))
  (bformat t "    Returning no-file 0%N")
  (values "no-dir" "no-file" 0 0 0))
|#



(defun dbg-create-function-type (difile function-type)
  "Currently create a bogus function type"
  (let ((arg-array (llvm-sys:get-or-create-type-array
		    *the-module-dibuilder*
		    (list
		     (llvm-sys:create-basic-type *the-module-dibuilder* "int" 64 llvm-sys:+dw-ate-signed-fixed+)
		     ))))
    (llvm-sys:create-subroutine-type *the-module-dibuilder* arg-array
                                     (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero))
                                     0)))


(defvar *dibuilder-type-hash-table* nil
  "Map types to DITypes")

(defmacro with-dibuilder ((module) &rest body)
  `(if *dbg-generate-dwarf*
       (let ((*the-module-dibuilder* (llvm-sys:make-dibuilder ,module))
             (*llvm-metadata* (make-hash-table :test #'eql))
             (*dibuilder-type-hash-table* (make-hash-table :test #'eq)))
         (unwind-protect
              (progn ,@body)
           (progn
             (llvm-sys:finalize *the-module-dibuilder*)
             ;; add the flag that defines the Dwarf Version
             (llvm-sys:add-module-flag *the-module*
                                       (llvm-sys:mdnode-get *llvm-context*
                                                            (list
                                                             (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
                                                             (llvm-sys:mdstring-get *llvm-context* "Dwarf Version")
                                                             (llvm-sys:value-as-metadata-get (jit-constant-i32 +debug-dwarf-version+)))))
             (llvm-sys:add-module-flag *the-module*
                                       (llvm-sys:mdnode-get *llvm-context*
                                                            (list
                                                             (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
                                                             (llvm-sys:mdstring-get *llvm-context* "Debug Info Version")
                                                             (llvm-sys:value-as-metadata-get (jit-constant-i32 llvm-sys:+debug-metadata-version+))))) ;; Debug Info Version
             "This should not be the return value - it should be what is returned in the unwind-protect body"
             )))
       (let ((*the-module-dibuilder* nil))
         ,@body)))

(defmacro with-dbg-compile-unit ((source-pathname) &rest body)
  (let ((path (gensym))
	(file (gensym))
	(dir-name (gensym)))
    `(if (and *dbg-generate-dwarf* *the-module-dibuilder*)
	 (progn
	   (let* ((,path (pathname ,source-pathname))
		  (,file *dbg-current-file*)
		  (,dir-name (directory-namestring ,path))
		  (*dbg-compile-unit* (llvm-sys:create-compile-unit
				       *the-module-dibuilder* ; dibuilder
				       llvm-sys:dw-lang-c-plus-plus ; 1 llvm-sys:dw-lang-common-lisp
				       ,file   ; 2 file
				       "clasp Common Lisp compiler" ; 4 producer
				       nil  ; 5 isOptimized
				       "-v" ; 6 compiler flags
				       1    ; 7 RV run-time version
				       "the-split-name.log" ; 8 splitname
                                       :line-tables-only ; 9 DebugEmissionKind (:full-debug :line-tables-only)
                                       0 ; 10 DWOld
                                       t ; 11 SplitDebugInlining
                                       nil ; 12 DebugInfoForProfiling
                                       nil ; 13 GnuPubnames
				       )))
	     (cmp-log "with-dbg-compile-unit *dbg-compile-unit*: %s%N" *dbg-compile-unit*)
	     (cmp-log "with-dbg-compile-unit source-pathname: %s%N" ,source-pathname)
	     (cmp-log "with-dbg-compile-unit file-name: [%s]%N" ,file)
	     (cmp-log "with-dbg-compile-unit dir-name: [%s]%N" ,dir-name)
	     ,@body
	     ))
	 (progn
	   (cmp-log "with-dbg-compile-unit not generating *dbg-compile-unit*%N")
	   ,@body))))


(namestring (make-pathname :directory (pathname-directory #P"a/b/c/d.txt")))

(defun make-file-metadata (pathname)
  (let* ((dir-name-slash (directory-namestring pathname))
         (dir-name (if (> (length dir-name-slash) 0)
                       (if (char= (char dir-name-slash (1- (length dir-name-slash))) #\/)
                           (subseq dir-name-slash 0 (1- (length dir-name-slash)))
                           dir-name-slash)
                       dir-name-slash))
         (file-name (file-namestring pathname)))
    (llvm-sys:create-file *the-module-dibuilder*
                          (namestring file-name)
                          (namestring dir-name)
                          :csk-none
                          "")))

(defmacro with-dbg-file-descriptor ((source-pathname) &rest body)
  (let ((path (gensym))
        (file-name (gensym))
        (dir-name (gensym)))
    `(if (and *dbg-generate-dwarf* *the-module-dibuilder*)
         (let* ((,path (pathname ,source-pathname))
                (*dbg-current-file* (make-file-metadata ,path)))
           ,@body)
         (progn
           ,@body))))

(defvar *with-debug-info-generator* nil)
(defmacro with-debug-info-generator ((&key module pathname) &rest body)
  "One macro that uses three other macros"
  `(let ((*with-debug-info-generator* t))
     (with-dibuilder (,module)
       (with-dbg-file-descriptor (,pathname)
         (with-dbg-compile-unit (,pathname)
	   ,@body)))))



(defun make-function-metadata (&key file-metadata linkage-name function-type lineno)
  (llvm-sys:create-function
                       *the-module-dibuilder*   ; 0 DIBuilder
                       file-metadata ; 1 function scope
                       linkage-name            ; 2 function name
                       linkage-name    ; 3 mangled function name
                       file-metadata ; 4 file where function is defined
                       lineno            ; 5 lineno
                       (dbg-create-function-type file-metadata function-type) ; 6 function-type
                       nil ; 7 isLocalToUnit - true if this function is not externally visible
                       t ; 8 isDefinition - true if this is a function definition
                       lineno ; 9 scopeLine - set to the beginning of the scope this starts
                       (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero)) ; 10 flags
                       nil ; 11 isOptimized - true if optimization is on
                       nil ; 12 TParam = nullptr
                       nil ; 13 Decl = nullptr
                       nil ; 14 ThrownTypes = nullptr
                       ))

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
                      (make-function-metadata :file-metadata *dbg-current-file*
                                              :linkage-name ,linkage-name
                                              :function-type ,function-type
                                              :lineno ,lineno)
                      #+(or)(llvm-sys:create-function
                             *the-module-dibuilder* ; 0 DIBuilder
                             *dbg-current-file*     ; 1 function scope
                             ,linkage-name          ; 2 function name
                             ,linkage-name ; 3 mangled function name
                             *dbg-current-file* ; 4 file where function is defined
                             ,lineno            ; 5 lineno
                             (dbg-create-function-type *dbg-current-file* ,function-type) ; 6 function-type
                             nil ; 7 isLocalToUnit - true if this function is not externally visible
                             t ; 8 isDefinition - true if this is a function definition
                             ,lineno ; 9 scopeLine - set to the beginning of the scope this starts
                             (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero)) ; 10 flags
                             nil ; 11 isOptimized - true if optimization is on
                             nil ; 12 TParam = nullptr
                             nil ; 13 Decl = nullptr
                             nil ; 14 ThrownTypes = nullptr
                             ))
                    (*dbg-current-scope* *dbg-current-function*))
               (llvm-sys:set-subprogram ,function *dbg-current-function*)
               (cmp-log "with-dbg-function *dbg-compile-unit*: %s%N" *dbg-compile-unit*)
               (cmp-log "with-dbg-function *dbg-current-function*: %s%N" *dbg-current-function*)
               (cmp-log "with-dbg-function name: [%s]%N" ,name)
               (cmp-log "with-dbg-function linkage-name: [%s]%N" ,linkage-name)
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
               (cmp-log "with-dbg-lexical-block%N")
               ,@body))
           (progn
             ,@body)))))

(defun dbg-set-current-source-pos (form)
  (when *dbg-generate-dwarf*
    (setq *dbg-set-current-source-pos* t)
    (when *current-source-pos-info*
    (cmp-log "dbg-set-current-source-pos on form: %s%N" form)
    (llvm-sys:set-current-debug-location-to-line-column-scope
     *irbuilder* (core:source-pos-info-lineno *current-source-pos-info*) 0 *dbg-current-scope*))))

(defun dbg-set-current-source-pos-for-irbuilder (irbuilder lineno)
  (llvm-sys:set-current-debug-location-to-line-column-scope irbuilder lineno 0 *dbg-current-scope*))

(defun check-debug-info-setup (irbuilder)
  "Signal an error if debug-info for the irbuilder is not setup properly for inlining"
  (unless (llvm-sys:current-debug-location irbuilder)
    (format t "Check the module~%")
    (break "The debug-info is not set for the current irbuilder")))

(defun dbg-set-source-pos (source-pos)
  (multiple-value-bind (file-handle file-pos lineno column)
      (core:source-pos-info-unpack source-pos)
    (when file-handle
      (let ((scope (gethash file-handle *llvm-metadata*)))
        #+(or)(unless scope
          (setq scope (mdnode-file-descriptor filename pathname))
          (setf (gethash file-handle *llvm-metadata*) scope))
        (llvm-sys:set-current-debug-location-to-line-column-scope *irbuilder* lineno column scope)))))

(defun dbg-set-current-debug-location (filename pathname lineno column)
  (let* ((scope-name (bformat nil "%s>>%s" pathname filename))
	 (scope (gethash scope-name *llvm-metadata*)))
    (unless scope
      (setq scope (mdnode-file-descriptor filename pathname))
      (core::hash-table-setf-gethash *llvm-metadata* scope-name scope))
    (let ((debugloc (llvm-sys:debug-loc-get lineno column scope)))
	    (llvm-sys:set-current-debug-location *irbuilder* debugloc))
    #+(or)(warn "Dwarf metadata is not currently being generated - the llvm-sys:set-current-debug-location-to-line-column-scope call is disabled")
    (llvm-sys:set-current-debug-location-to-line-column-scope *irbuilder* lineno column scope)
    ))

(defvar *current-file-metadata-node* nil
  "Store the metadata node for the current source file info")
