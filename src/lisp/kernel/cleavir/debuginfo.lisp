(in-package #:clasp-cleavir)

(defvar *generate-dwarf* t)

(defvar *dibuilder*)
(defvar *dbg-current-scope*)

(defun install-compile-unit (dibuilder file)
  ;; NOTE: Despite the "create" name, this function also installs the
  ;; new DICompileUnit into the DIBuilder.
  (llvm-sys:dibuilder/create-compile-unit
   dibuilder
   llvm-sys:dw-lang-c-plus-plus ; why not dw-lang-common-lisp?
   file "Clasp Common Lisp" nil "-v" 1 "the-split-name.log" :full-debug
   0 t nil :dntk-default nil "" ""))

(defun make-dibuilder (module)
  (llvm-sys:make-dibuilder module))

(defun make-difile (pathname &key (source ";;"))
  (let ((filename (file-namestring pathname))
        (directory
          (namestring
           (make-pathname :name nil :type nil :defaults pathname))))
    (llvm-sys:dibuilder/create-file *dibuilder* filename directory
                                    nil source)))

(defun add-module-di-flags (module)
  ;; add the flag that defines the Dwarf Version
  ;; FIXME: Why do we use a pretty old DWARF version here?
  (llvm-sys:add-module-flag
   module
   (llvm-sys:mdnode-get (cmp:thread-local-llvm-context)
                        (list
                         (llvm-sys:value-as-metadata-get (%i32 2))
                         (llvm-sys:mdstring-get
                          (cmp:thread-local-llvm-context) "Dwarf Version")
                         (llvm-sys:value-as-metadata-get
                          (%i32 cmp::+debug-dwarf-version+)))))
  (llvm-sys:add-module-flag
   module
   (llvm-sys:mdnode-get (cmp:thread-local-llvm-context)
                        (list
                         (llvm-sys:value-as-metadata-get (%i32 2))
                         (llvm-sys:mdstring-get
                          (cmp:thread-local-llvm-context) "Debug Info Version")
                         (llvm-sys:value-as-metadata-get
                          (%i32 llvm-sys:+debug-metadata-version+))))))

;;; Bind *dibuilder*, and if a pathname is provided,
;;; also bind *dbg-current-scope* to a new DIFile.
;;; Afterwords finalize the DIBuilder, and add debug flags to the module.
(defmacro with-debuginfo ((llvm-ir-module
                           &key (file nil filep) (source ";;"))
                          &body body)
  (let ((gbody (gensym "BODY"))
        (gmodule (gensym "MODULE")))
    ;; progn to error on declare expressions
    `(flet ((,gbody () (progn ,@body)))
       (if *generate-dwarf*
           (let* ((,gmodule ,llvm-ir-module)
                  (*dibuilder* (make-dibuilder ,gmodule)))
             (unwind-protect
                  ,(if filep
                       `(let ((*dbg-current-scope*
                                (make-difile ,file :source ,source)))
                          (install-compile-unit *dibuilder*
                                                *dbg-current-scope*)
                          (,gbody))
                       `(,gbody))
               (llvm-sys:dibuilder/finalize *dibuilder*)
               (add-module-di-flags ,gmodule)))
           (,gbody)))))

;; Given a vrtype, create and return a metadata node describing the type
;; for debug information. (LLVM should take care of caching.)
;; FIXME: unhardcode the 64s
(defgeneric vrtype->di (vrtype))
(defun di-zeroflags ()
  (core:enum-logical-or llvm-sys:diflags-enum '(llvm-sys:diflags-zero)))
(defun dispflags (&rest flags)
  (core:enum-logical-or llvm-sys:dispflag-enum
                        (or flags '(llvm-sys:dispflag-zero))))
(defun di-object-type ()
  (llvm-sys:create-basic-type *dibuilder* "T_O*" 64
                              llvm-sys:+dw-ate-address+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :object))) (di-object-type))
(defmethod vrtype->di ((vrtype (eql :boolean)))
  ;; 8 is cargo-culted from what clang does for bool.
  (llvm-sys:create-basic-type *dibuilder* "bool" 8
                              llvm-sys:+dw-ate-boolean+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :single-float)))
  (llvm-sys:create-basic-type *dibuilder* "float" 32
                              llvm-sys:+dw-ate-float+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :double-float)))
  (llvm-sys:create-basic-type *dibuilder* "double" 64
                              llvm-sys:+dw-ate-float+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :base-char)))
  (llvm-sys:create-basic-type *dibuilder* "char" 8
                              llvm-sys:+dw-ate-unsigned-char+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :character)))
  (llvm-sys:create-basic-type *dibuilder* "claspCharacter" 32
                              llvm-sys:+dw-ate-unsigned-char+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :fixnum)))
  (llvm-sys:create-basic-type *dibuilder* "fixnum" 64
                              llvm-sys:+dw-ate-signed+ (di-zeroflags)))
(defmethod vrtype->di ((vrtype (eql :utfixnum)))
  (llvm-sys:create-basic-type *dibuilder* "utfixnum" 64
                              llvm-sys:+dw-ate-unsigned+ (di-zeroflags)))

(defun create-di-struct-type (dibuilder name elements
                              &key scope (alignment 64))
  ;; These types are used by the runtime rather than being defined
  ;; anywhere, so the file spec is a little dumb.
  (let ((file (make-difile "-implicit-")) (lineno 0))
    (llvm-sys:create-struct-type
     dibuilder scope name file lineno
     ;; WARNING: This may not work in general,
     ;; since e.g. a three slot structure
     ;; with 32-64-32 may end up as three words.
     ;; I don't know how much this matters.
     (loop for ty in elements
           sum (llvm-sys:get-size-in-bits ty))
     alignment (di-zeroflags) nil
     (llvm-sys:get-or-create-array
      dibuilder elements)
     0 nil "")))

(defmethod vrtype->di ((vrtype (eql :vaslist)))
  (create-di-struct-type
   *dibuilder* "vaslist"
   (list
    (llvm-sys:create-pointer-type
     *dibuilder* (di-object-type) 64 64 0 "" nil)
    (llvm-sys:create-basic-type
     *dibuilder* "size_t" 64 llvm-sys:+dw-ate-unsigned+ (di-zeroflags)))))

(defgeneric rtype->di (rtype))
(defmethod rtype->di ((rtype (eql :multiple-values)))
  (create-di-struct-type
   *dibuilder* "T_mv"
   (list
    (di-object-type)
    (llvm-sys:create-basic-type
     *dibuilder* "size_t" 64 llvm-sys:+dw-ate-unsigned+ (di-zeroflags)))))
(defmethod rtype->di ((rtype (eql :vaslist)))
  (vrtype->di rtype))
(defmethod rtype->di ((rtype list))
  (create-di-struct-type *dibuilder* "" (mapcar #'vrtype->di rtype)))

(defun create-di-main-function-type (ir)
  (let* ((returni (bir:returni ir))
         (retrtype (if returni
                       (cc-bmir:rtype (bir:input returni))
                       nil))
         ;; An rtype of () is void, as is a function that never returns.
         ;; void is indicated as a null DIType*, which we indicate as NIL.
         (ret (if retrtype (rtype->di retrtype) nil))
         (envtypes (loop repeat (cleavir-set:size (bir:environment ir))
                         collect (di-object-type)))
         ;; FIXME: Recomputes crap from translate
         (arguments (compute-arglist (bir:lambda-list ir)))
         (arg-rtypes (mapcar #'cc-bmir:rtype arguments))
         (arg-vrtypes (mapcar #'first arg-rtypes))
         (arg-ditypes (mapcar #'vrtype->di arg-vrtypes))
         (paramspec (list* ret (nconc envtypes arg-ditypes)))
         (param-array
           (llvm-sys:get-or-create-type-array *dibuilder* paramspec)))
    (llvm-sys:create-subroutine-type *dibuilder* param-array
                                     (di-zeroflags) 0)))

;;; Given a source-pos-info, return a pathname, lineno, and column
;;; as values. FIXME: This integer file handle thing is really silly.
(defun spi-info (spi)
  (multiple-value-bind (handle lineno column)
      (core:source-pos-info-unpack spi)
    (values (core:file-scope-pathname (core:file-scope handle))
            lineno column)))

(defun create-di-main-function (ir name
                                &key (difile *dbg-current-scope*)
                                (linkage-name name))
  (let* ((spi (origin-spi (origin-source (bir:origin ir))))
         (lineno (core:source-pos-info-lineno spi)))
    (llvm-sys:dibuilder/create-function
     *dibuilder* *dbg-current-scope* name linkage-name difile lineno
     (create-di-main-function-type ir) lineno
     (di-zeroflags) (dispflags 'llvm-sys:dispflag-definition)
     nil nil nil nil "")))

(defun create-di-nxep-type (n)
  (let* ((args (make-list n :initial-element (di-object-type)))
         (params (list* (rtype->di :multiple-values) args)))
    (llvm-sys:create-subroutine-type
     *dibuilder*
     (llvm-sys:get-or-create-type-array *dibuilder* params)
     (di-zeroflags) 0)))
(defun create-di-gxep-type ()
  (let* ((t** (llvm-sys:create-pointer-type
               *dibuilder* (di-object-type) 64 64 0 "" nil))
         (size_t
           (llvm-sys:create-basic-type
            *dibuilder* "size_t" 64 llvm-sys:+dw-ate-unsigned+
            (di-zeroflags)))
         (params (list (rtype->di :multiple-values)
                       (di-object-type) size_t t**)))
    (llvm-sys:create-subroutine-type
     *dibuilder*
     (llvm-sys:get-or-create-type-array *dibuilder* params)
     (di-zeroflags) 0)))

(defun create-di-xep (ir name arity &key (difile *dbg-current-scope*)
                                      (linkage-name name))
  (let* ((spi (origin-spi (origin-source (bir:origin ir))))
         (lineno (core:source-pos-info-lineno spi)))
    (llvm-sys:dibuilder/create-function
     *dibuilder* *dbg-current-scope* name linkage-name difile lineno
     (if (eq arity :general-entry)
         (create-di-gxep-type)
         (create-di-nxep-type arity))
     lineno (di-zeroflags) (dispflags 'llvm-sys:dispflag-definition)
     nil nil nil nil "")))

(defmacro with-di-subprogram ((function subprogram) &body body)
  (let ((gfunction (gensym "FUNCTION")) (gsub (gensym "SUBPROGRAM")))
    `(let* ((,gfunction ,function) (,gsub (when *generate-dwarf* ,subprogram))
            (*dbg-current-scope* ,gsub))
       (when *generate-dwarf*
         (llvm-sys:set-subprogram ,gfunction ,gsub))
       ,@body)))

(defun get-dilocation (spi)
  (multiple-value-bind (file lineno column) (spi-info spi)
    (declare (ignore file))
    ;; We ignore the file because we use the local scope.
    ;; The scoping doesn't fit super well with BIR, but oh well.
    (llvm-sys:get-dilocation (cmp:thread-local-llvm-context)
                             lineno column *dbg-current-scope*)))

;;; if SPI is nil we unset the debug location.
(defun set-instruction-source-position (spi)
  (when *generate-dwarf*
    (if spi
        (llvm-sys:set-current-debug-location
         cmp:*irbuilder*
         (get-dilocation spi))
        (llvm-sys:clear-current-debug-location cmp:*irbuilder*))))

(defmacro with-instruction-source-position ((spi) &body body)
  `(unwind-protect
        (progn (set-instruction-source-position ,spi)
               ,@body)
     (set-instruction-source-position nil)))
