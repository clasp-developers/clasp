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

;;; (defvar *dbg-generate-dwarf* t) <<--- defined in init.lsp
(defvar *dbg-compile-unit* )
(defvar *dbg-compile-unit-types*)
(defvar *dbg-current-file* )
(defvar *dbg-current-function-metadata*)
(defvar *dbg-current-function-lineno*)
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
  `(let ((*the-module-dibuilder* (llvm-sys:make-dibuilder ,module)))
     (if *dbg-generate-dwarf*
         (let ((*llvm-metadata* (make-hash-table :test #'eql))
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
         (progn
           ,@body))))

(defun setup-basic-dbg-types (types)
  (setf (gethash :long-unsigned-int types)
        (llvm-sys:create-basic-type "long unsigned int" 64 llvm-sys:+DW-ATE-unsigned+)))

    
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
                                       ,file ; 2 file
                                       "clasp Common Lisp compiler" ; 4 producer
                                       nil  ; 5 isOptimized
                                       "-v" ; 6 compiler flags
                                       1    ; 7 RV run-time version
                                       "the-split-name.log" ; 8 splitname
                                       :full-debug ; 9 DebugEmissionKind (:full-debug :line-tables-only)
                                       0           ; 10 DWOld
                                       t   ; 11 SplitDebugInlining
                                       nil ; 12 DebugInfoForProfiling
                                       nil ; 13 GnuPubnames
                                       ))
                  (*dbg-compile-unit-types* (make-hash-table)))
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
  (let ((body-func (gensym)))
    `(flet ((,body-func () ,@body))
       (let ((*with-debug-info-generator* t))
         (with-dibuilder (,module)
           (with-dbg-file-descriptor (,pathname)
             (with-dbg-compile-unit (,pathname)
               (funcall #',body-func))))))))



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
(defun do-dbg-function (closure name lineno linkage-name function-type function)
  (let ((*with-dbg-function* t)
        (*dbg-set-current-source-pos* nil))
    (if (and *dbg-generate-dwarf* *the-module-dibuilder*)
        (let* ((*dbg-current-function-metadata* (make-function-metadata :file-metadata *dbg-current-file*
                                        :linkage-name linkage-name
                                        :function-type function-type
                                                                        :lineno lineno))
               (*dbg-current-scope* *dbg-current-function-metadata*)
               (*dbg-current-function-lineno* lineno))
          (llvm-sys:set-subprogram function *dbg-current-function-metadata*)
          (funcall closure))
      (funcall closure))))

(defmacro with-dbg-function ((name &key lineno linkage-name function-type function) &rest body)
  `(do-dbg-function
       (lambda () (progn ,@body))
     ,name ,lineno ,linkage-name ,function-type ,function))

(defvar *with-dbg-lexical-block* nil)
(defun do-dbg-lexical-block (closure lineno)
  (let ((*with-dbg-lexical-block* t))
    (if (and *dbg-generate-dwarf* *the-module-dibuilder*)
        (progn
          (unless *dbg-current-scope*
            (error "The *dbg-current-scope* is nil - it cannot be when create-lexical-block is called"))
          (let* ((*dbg-current-scope*
                  (llvm-sys:create-lexical-block *the-module-dibuilder*
                                                 *dbg-current-scope*
                                                 *dbg-current-file*
                                                 lineno
                                                 0 ; column
                                                 #| 0  -- not used anymore TODO: Dwarf path discriminator   |# )))
            (cmp-log "with-dbg-lexical-block%N")
            (funcall closure)))
      (funcall closure))))
  
(defmacro with-dbg-lexical-block ((&key (lineno (core:source-pos-info-lineno core:*current-source-pos-info*))) &body body)
  `(do-dbg-lexical-block (lambda () ,@body) ,lineno))

(defun dbg-clear-irbuilder-source-location-impl (irbuilder)
  (llvm-sys:clear-current-debug-location irbuilder))

(defparameter *trap-zero-lineno* nil)
(defun dbg-set-irbuilder-source-location-impl (irbuilder lineno column &optional (scope *dbg-current-scope*))
  (unless scope (error "scope must not be NIL"))
  (when *trap-zero-lineno*
    (when (= lineno 0)
      (format *error-output* "In dbg-set-irbuilder-source-location-impl lineno was zero! Setting to 666666~%")
      (setf lineno 666666)))
  (when *dbg-generate-dwarf*
    (llvm-sys:set-current-debug-location-to-line-column-scope irbuilder lineno column scope)))

(defun dbg-set-current-source-pos (form)
  (when *dbg-generate-dwarf*
    (setq *dbg-set-current-source-pos* t)
    (when *current-source-pos-info*
    (cmp-log "dbg-set-current-source-pos on form: %s%N" form)
    (unless *dbg-current-scope*
      (error "*dbg-current-scope* must not be NIL"))
    (dbg-set-irbuilder-source-location-impl
     *irbuilder* (core:source-pos-info-lineno *current-source-pos-info*) 0 *dbg-current-scope*))))

(defun dbg-set-source-pos (source-pos)
  (when *dbg-generate-dwarf*
    (multiple-value-bind (file-handle file-pos lineno column)
        (core:source-pos-info-unpack source-pos)
      (when file-handle
        (let ((scope (gethash file-handle *llvm-metadata*)))
          (dbg-set-irbuilder-source-location-impl *irbuilder* lineno column scope))))))

(defun dbg-set-current-debug-location (filename pathname lineno column)
  (when *dbg-generate-dwarf*
    (let* ((scope-name (bformat nil "%s>>%s" pathname filename))
	   (scope (gethash scope-name *llvm-metadata*)))
      #+(or)
      (unless scope
        (setq scope (mdnode-file-descriptor filename pathname))
        (core::hash-table-setf-gethash *llvm-metadata* scope-name scope))
      (dbg-set-irbuilder-source-location-impl *irbuilder* lineno column scope))))


(defun dbg-create-parameter-variable (&key (scope *dbg-current-scope*)
                                        name
                                        argno
                                        (file *dbg-current-file*)
                                        lineno
                                        type
                                        always-preserve)
  (llvm-sys:create-parameter-variable *the-module-dibuilder*
                                      scope
                                      name
                                      argno
                                      file
                                      lineno
                                      type
                                      always-preserve
                                      (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero))))

                                           
(defvar *current-file-metadata-node* nil
  "Store the metadata node for the current source file info")


(defvar *current-source-position* nil)
(defvar *current-function-metadata* nil)

(defun set-instruction-source-position (origin function-metadata)
  (when *dbg-generate-dwarf*
    (if origin
        (let ((source-pos-info (if (consp origin) (car origin) origin)))
          (dbg-set-irbuilder-source-location-impl
           *irbuilder*
           (core:source-pos-info-lineno source-pos-info)
           (1+ (core:source-pos-info-column source-pos-info))
           function-metadata))
        (dbg-clear-irbuilder-source-location-impl *irbuilder*))))


(defun do-debug-info-source-position (enabledp origin body-lambda)
  (if (and enabledp (null origin))
      (funcall body-lambda)
      (unwind-protect
           (let ((*current-source-position* origin))
             (set-instruction-source-position origin *dbg-current-function-metadata*)
             (funcall body-lambda))
        (set-instruction-source-position *current-source-position* *dbg-current-function-metadata*))))

(defmacro with-debug-info-source-position ((origin) &body body)
  `(do-debug-info-source-position t ,origin (lambda () ,@body)))

