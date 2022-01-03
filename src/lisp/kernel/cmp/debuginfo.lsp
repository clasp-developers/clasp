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

;;; These variables are pretty important to what debug information actually goes
;;; into the file. For example, and as a note to later developers, I just got bit
;;; by the fact that a source position info's filename is often irrelevant - the
;;; dbg-current-scope and dbg-current-file are used.
;;; (defvar *dbg-generate-dwarf* t) <<--- defined in init.lsp
(defvar *dbg-compile-unit*)
(defvar *dbg-current-file*)
(defvar *dbg-current-function-metadata*)
(defvar *dbg-current-function-lineno*)
(defvar *dbg-current-scope* nil
  "Stores the current enclosing lexical scope for debugging info")
;;; Unlike other DI structures, each call to create-function seems to result
;;; in a distinct DISubprogram - not our intention. So we hash on the scope.
;;; ATM function scopes are lists so an EQUAL hash table is fine here.
(defvar *dbg-function-metadata-cache*)

(defun dbg-create-function-type (difile function-type)
  (declare (ignore difile function-type))
  "Currently create a bogus function type"
  (let ((arg-array (llvm-sys:get-or-create-type-array
		    *the-module-dibuilder*
		    (list
		     (llvm-sys:create-basic-type *the-module-dibuilder* "int" 64 llvm-sys:+dw-ate-signed-fixed+ (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero)))
		     ))))
    (llvm-sys:create-subroutine-type *the-module-dibuilder* arg-array
                                     (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero))
                                     0)))

(defmacro with-dibuilder ((module) &rest body)
  `(let ((*the-module-dibuilder* (llvm-sys:make-dibuilder ,module)))
     (unwind-protect
          (progn ,@body)
       (progn
         (llvm-sys:finalize *the-module-dibuilder*)
         ;; add the flag that defines the Dwarf Version
         (llvm-sys:add-module-flag *the-module*
                                   (llvm-sys:mdnode-get (thread-local-llvm-context)
                                                        (list
                                                         (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
                                                         (llvm-sys:mdstring-get (thread-local-llvm-context) "Dwarf Version")
                                                         (llvm-sys:value-as-metadata-get (jit-constant-i32 +debug-dwarf-version+)))))
         (llvm-sys:add-module-flag *the-module*
                                   (llvm-sys:mdnode-get (thread-local-llvm-context)
                                                        (list
                                                         (llvm-sys:value-as-metadata-get (jit-constant-i32 2))
                                                         (llvm-sys:mdstring-get (thread-local-llvm-context) "Debug Info Version")
                                                         (llvm-sys:value-as-metadata-get (jit-constant-i32 llvm-sys:+debug-metadata-version+)))))))))
    
(defmacro with-dbg-compile-unit ((source-pathname) &rest body)
  (let ((path (gensym))
        (file (gensym))
        (dir-name (gensym)))
    `(let* ((,path (pathname ,source-pathname))
            (,file *dbg-current-file*)
            (,dir-name (directory-namestring ,path))
            (*dbg-function-metadata-cache* (make-hash-table :test #'equal))
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
                                 :dntk-default ; 13 DebugNameTableKind
                                 nil ; 14 RangesBaseAddress
                                 "" ; 15 SysRoot (-isysroot value)
                                 "" ; 16 SDK
                                 )))
       (declare (ignorable ,dir-name)) ; cmp-log may expand empty
       (cmp-log "with-dbg-compile-unit *dbg-compile-unit*: %s%N" *dbg-compile-unit*)
       (cmp-log "with-dbg-compile-unit source-pathname: %s%N" ,source-pathname)
       (cmp-log "with-dbg-compile-unit file-name: [%s]%N" ,file)
       (cmp-log "with-dbg-compile-unit dir-name: [%s]%N" ,dir-name)
       ,@body)))

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
                          nil
                          "difile-source" ; source
                          )))

(defmacro with-dbg-file-descriptor ((source-pathname) &rest body)
  `(let ((*dbg-current-file* (make-file-metadata (pathname ,source-pathname))))
     ,@body))

(defmacro with-debug-info-generator ((&key module pathname) &rest body)
  "One macro that uses three other macros"
  (let ((body-func (gensym)))
    `(flet ((,body-func () ,@body))
       (if *dbg-generate-dwarf*
           (with-dibuilder (,module)
             (with-dbg-file-descriptor (,pathname)
               (with-dbg-compile-unit (,pathname)
                 (funcall #',body-func))))
           (funcall #',body-func)))))

(defun make-function-metadata (&key file-metadata linkage-name function-type lineno)
  (llvm-sys:create-function
   *the-module-dibuilder*   ; 0 DIBuilder
   file-metadata           ; 1 function scope
   linkage-name            ; 2 function name
   linkage-name            ; 3 function name
   file-metadata           ; 4 file where function is defined
   lineno                  ; 5 lineno
   (dbg-create-function-type file-metadata function-type) ; 6 function-type
   lineno                  ; 7 scopeLine - set to the beginning of the scope this starts
   (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero)) ; 8 flags
   (core:enum-logical-or llvm-sys:dispflag-enum '(llvm-sys:dispflag-definition)) ; 9 spflags
   nil ; 10 TParam = nullptr
   nil ; 11 Decl = nullptr
   nil ; 12 ThrownTypes = nullptr

   #|
(DIScope *Scope, ; 1
 StringRef Name, ; 2
 StringRef LinkageName, ;3
 DIFile *File, ;4
 unsigned LineNo, ; 5
 DISubroutineType *Ty, ; 6
 unsigned ScopeLine, ; 7
 DINode::DIFlags Flags=DINode::FlagZero, ; 8
 DISubprogram::DISPFlags SPFlags=DISubprogram::SPFlagZero, ; 9
 DITemplateParameterArray TParams=nullptr,
 DISubprogram *Decl=nullptr,
 DITypeArray ThrownTypes=nullptr)
|#
   ))

(defun do-dbg-function (closure lineno function-type function)
  (declare (ignore function-type))
  (unless *current-source-pos-info*
    (warn "*current-source-pos-info* is undefined - this may cause problems - wrap with-dbg-function in with-guaranteed-*current-source-pos-info* to fix this"))
  (let ((linkage-name (llvm-sys:get-name function)))
    (multiple-value-bind (file-scope file-handle)
        (core:file-scope (llvm-sys:get-path *dbg-current-file*))
      (declare (ignore file-scope))
      (if (and *dbg-generate-dwarf* *the-module-dibuilder*)
          (let* ((current-subprogram (cached-function-scope (list linkage-name lineno file-handle)))
                 (*dbg-current-function-metadata* current-subprogram)
                 (*dbg-current-scope* current-subprogram)
                 (*dbg-current-function-lineno* lineno))
            (llvm-sys:set-subprogram function current-subprogram)
            (funcall closure))
          (funcall closure)))))

(defmacro with-guaranteed-*current-source-pos-info* (() &rest body)
  `(let ((core:*current-source-pos-info* (if core:*current-source-pos-info*
                                             core:*current-source-pos-info*
                                             (core:make-source-pos-info :filename "dummy-filename"))))
     (progn
       ,@body)))
                                             
(defmacro with-dbg-function ((&key lineno function-type function) &rest body)
  (cmp-log "Entered with-dbg-function%N")
  `(do-dbg-function
       (lambda () (progn ,@body))
     ,lineno ,function-type ,function))

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

(defun dbg-clear-irbuilder-source-location (irbuilder)
  (llvm-sys:clear-current-debug-location irbuilder))

(defun cached-file-metadata (file-handle)
  ;; n.b. despite the name we don't cache, as llvm seems to handle it
  (make-file-metadata (file-scope-pathname (file-scope file-handle))))

(defun cached-function-scope (function-scope-info &optional function-type)
  ;; See production in cleavir/inline-prep.lisp
  #+cclasp(when core:*debug-source-pos-info*
            (let ((name (car function-scope-info)))
              (when (char= #\^ (elt name (1- (length name))))
                (break "The name ~s ends in ^" name))))
  (multiple-value-bind (value found)
      (gethash function-scope-info *dbg-function-metadata-cache*)
    (if found
        (values value :found-in-cache)
        (values (setf (gethash function-scope-info *dbg-function-metadata-cache*)
                      (destructuring-bind (function-name lineno file-handle)
                          function-scope-info
                        (make-function-metadata :linkage-name function-name
                                                :lineno lineno
                                                :function-type (if function-type function-type (fn-prototype :general-entry))
                                                :file-metadata (cached-file-metadata file-handle))))
                :created))))

(defparameter *trap-zero-lineno* nil)
(defun get-dilocation (spi dbg-current-scope)
  (let ((file-handle (core:source-pos-info-file-handle spi))
        (lineno (core:source-pos-info-lineno spi))
        (col (core:source-pos-info-column spi))
        (inlined-at (core:source-pos-info-inlined-at spi)))
    (declare (ignore file-handle))
    (when (and *trap-zero-lineno* (zerop lineno))
      (format *error-output* "In get-dilocation lineno was zero! Setting to ~d~%"
              (setf lineno 666666)))
    (if inlined-at
        (llvm-sys:get-dilocation (thread-local-llvm-context)
                                 lineno
                                 col
                                 dbg-current-scope #+(or)(cached-function-scope (core:source-pos-info-function-scope spi))
                                 (get-dilocation inlined-at dbg-current-scope))
        (llvm-sys:get-dilocation (thread-local-llvm-context)
                                 lineno
                                 col
                                 dbg-current-scope))))

(defun dbg-set-irbuilder-source-location (irbuilder spi)
  (when *dbg-generate-dwarf*
    (let ((diloc (get-dilocation spi *dbg-current-scope*)))
      (llvm-sys:set-current-debug-location irbuilder diloc))))

(defun dbg-create-auto-variable (&key (scope *dbg-current-scope*)
                                   name (file *dbg-current-file*)
                                   lineno type always-preserve)
  (llvm-sys:create-auto-variable *the-module-dibuilder*
                                 scope name file lineno type always-preserve
                                 (core:enum-logical-or
                                  llvm-sys:diflags-enum
                                  '(llvm-sys:diflags-zero))
                                 ;; FIXME: I'm guessing
                                 64))

(defun dbg-create-parameter-variable (&key (scope *dbg-current-scope*)
                                        name
                                        argno
                                        (file *dbg-current-file*)
                                        lineno
                                        type
                                        always-preserve)
  (unless (> argno 0)
    (error "The argno for ~a must start at 1 - got ~a" name argno))
  (llvm-sys:create-parameter-variable *the-module-dibuilder*
                                      scope
                                      name
                                      argno
                                      file
                                      lineno
                                      type
                                      always-preserve
                                      (core:enum-logical-or
                                       llvm-sys:diflags-enum
                                       '(llvm-sys:diflags-zero))))

(defun set-instruction-source-position (origin function-metadata)
  (when *dbg-generate-dwarf*
    (if origin
        (let ((source-pos-info (if (consp origin) (car origin) origin))
              (*dbg-current-scope* function-metadata))
          (dbg-set-irbuilder-source-location *irbuilder* source-pos-info))
        (dbg-clear-irbuilder-source-location *irbuilder*))))

(defun do-debug-info-source-position (origin body-lambda)
  (unwind-protect
       (progn
         (set-instruction-source-position origin *dbg-current-function-metadata*)
         (funcall body-lambda))
    (set-instruction-source-position nil *dbg-current-function-metadata*)))

(defmacro with-debug-info-source-position ((origin) &body body)
  `(do-debug-info-source-position ,origin (lambda () ,@body)))
