;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAIN  Compiler main program.

(in-package "COMPILER")

#-threads
(defmacro with-lock ((lock) &body body)
  `(progn ,@body))

(defun safe-system (string)
  (cmpnote "Invoking external command:~%  ~A" string)
  (let ((result (si:system string)))
    (unless (zerop result)
      (cerror "Continues anyway."
	      "(SYSTEM ~S) returned non-zero value ~D"
	      string result))
    result))

(defun compile-file-pathname (name &key (output-file T) (type nil type-supplied-p)
                              verbose print c-file h-file data-file shared-data-file
                              system-p load external-format source-truename
                              source-offset)
  (let* ((format '())
         (extension '()))
    (unless type-supplied-p
      (setf type (if system-p :object :fasl)))
    (case type
      ((:shared-library :dll) (setf format +shared-library-format+))
      ((:static-library :library :lib) (setf format +static-library-format+))
      (:data (setf extension "data"))
      (:sdata (setf extension "sdat"))
      (:c (setf extension "c"))
      (:h (setf extension "h"))
      (:object (setf extension +object-file-extension+))
      (:program (setf format +executable-file-format+))
      #+msvc
      (:import-library (setf extension "implib"))
      ((:fasl :fas) (setf extension "fas")))
    (cond ((not (member output-file '(T NIL)))
	   output-file)
	  (format
	   (merge-pathnames (format nil format (pathname-name name)) name))
	  (t
	   (make-pathname :type extension :defaults name)))))

#+msvc
(defun delete-msvc-generated-files (output-pathname)
  (loop for i in '("lib" "exp" "ilk" "pdb")
        do (let ((the-pathname (merge-pathnames (make-pathname :type i) output-pathname)))
	     (when (probe-file the-pathname)
	       (cmp-delete-file the-pathname)))))

(defun cmp-delete-file (file)
  (cond ((null *delete-files*))
	(*debug-compiler*
	 (cmpprogress "~%Postponing deletion of ~A" file)
	 (push file *files-to-be-deleted*))
	(t
	 (and (probe-file file)
	      (delete-file file)))))

(push #'(lambda () (mapc #'delete-file *files-to-be-deleted*))
      si::*exit-hooks*)

#-mingw32
(defmacro fix-for-mingw (directory-namestring)
  directory-namestring)

#+mingw32
(defun fix-for-mingw (directory-namestring)
  (let ((x (string-right-trim '(#\\ #\/) directory-namestring)))
    (if (zerop (length x)) "/" x)))

(defun linker-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *ld*
	   (si::coerce-to-filename o-pathname)
	   (fix-for-mingw (ecl-library-directory))
	   options
	   *ld-flags*)))

#+dlopen
(defun shared-cc (o-pathname &rest options)
  #-(or mingw32)
  (safe-system
   (format nil
	   *ld-format*
	   *ld*
	   (si::coerce-to-filename o-pathname)
	   (fix-for-mingw (ecl-library-directory))
	   options
	   *ld-shared-flags*))
  #+(or mingw32)
  (let ((lib-file (compile-file-pathname o-pathname :type :lib)))
    (safe-system
     (format nil
	     "gcc -shared -o ~S -L~S ~{~S ~} ~@?"
	     (si::coerce-to-filename o-pathname)
	     (fix-for-mingw (ecl-library-directory))
	     options
	     *ld-shared-flags*))))

#+dlopen
(defun bundle-cc (o-pathname init-name &rest options)
  #-(or mingw32)
  (safe-system
   (format nil
	   *ld-format*
	   *ld*
	   (si::coerce-to-filename o-pathname)
	   (fix-for-mingw (ecl-library-directory))
	   options
	   #-msvc *ld-bundle-flags*
	   #+msvc (concatenate 'string *ld-bundle-flags*
			       " /EXPORT:" init-name
			       " /LIBPATH:" (ecl-library-directory)
			       " /IMPLIB:"
			       (si::coerce-to-filename
				(compile-file-pathname
				 o-pathname :type :import-library)))))
  #+(or mingw32)
  (safe-system
   (format nil
	   "gcc -shared -o ~A -Wl,--export-all-symbols -L~S ~{~S ~} ~@?"
	   (si::coerce-to-filename o-pathname)
	   (fix-for-mingw (ecl-library-directory))
	   options
	   *ld-bundle-flags*)))

(defconstant +lisp-program-header+ "
#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~{	extern ECL_CPP_TAG void ~A(cl_object);~%~}

")

;;
;; This format string contains the structure of the code that initializes
;; a program, a library, a module, etc. Basically, it processes a codeblock
;; just like in a normal compiled file, but then adds all the codeblocks of
;; its corresponding modules.
;;
;; IMPORTANT: Notice how the modules are linked to the parent forming a
;; circular chain. This disables the garbage collection of the library until
;; _ALL_ functions in all modules are unlinked.
;;
(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif
void ~A(cl_object cblock)
{
	static cl_object Cblock;
        if (!FIXNUMP(cblock)) {
		Cblock = cblock;
		cblock->cblock.data_text = compiler_data_text;
		cblock->cblock.data_text_size = compiler_data_text_size;
#ifndef ECL_DYNAMIC_VV
		cblock->cblock.data = VV;
#endif
		cblock->cblock.data_size = VM;
		return;
	}
#if defined(ECL_DYNAMIC_VV) && defined(ECL_SHARED_DATA)
	VV = Cblock->cblock.data;
#endif
	~A
{
	cl_object current, next = Cblock;
~:[~{	current = read_VV(OBJNULL, ~A); current->cblock.next = next; next = current; ~%~}
	Cblock->cblock.next = current;
~;~{	~A(Cblock);~%~}~]
}
	~A
}")

(defconstant +lisp-program-main+ "
int
main(int argc, char **argv)
{
	~A
	cl_boot(argc, argv);
	read_VV(OBJNULL, ~A);
	~A
}")

#+:win32
(defconstant +lisp-program-winmain+ "
#include <windows.h>
int
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	char **argv;
	int argc;
	~A
	ecl_get_commandline_args(&argc, &argv);
	cl_boot(argc, argv);
	read_VV(OBJNULL, ~A);
	~A
}")

(defun guess-kind (pathname)
  "Given a file name, guess whether it is an object file, a library, a program
or a loadable module."
  (let ((record (assoc (pathname-type pathname)
		       '(("o" :object) ("obj" :object) ("c" :c)
			 ("lib" :static-library)
			 ("a" :static-library)
			 ("dll" :shared-library)
			 ("so" :shared-library)
			 ("fas" :fasl))
		       :test #'string-equal)))
    (if record
	(second record)
	(progn
	  (warn "File ~s is of no known file type. Assuming it is an object file."
		pathname)
	  :object))))

(defun guess-ld-flags (pathname &key (kind (guess-kind pathname)))
  "Given a file name, return the compiler command line argument to link this file in."
  (case kind
    ((:object :c)
     (si::coerce-to-filename pathname))
    ((:fasl :fas)
     nil)
    ((:static-library :lib)
     (si::coerce-to-filename pathname))
    ((:shared-library :dll)
     (si::coerce-to-filename pathname))
    ((:program)
     nil)
    (otherwise
     (error "C::BUILDER cannot accept files of kind ~s" kind))))

(defun system-ld-flag (library)
  "Given a symbol, try to find a library that matches it, either by looking in the
filesystem or in the database of ASDF modules."
  (let ((asdf (find-package "ASDF"))
        system)
    (labels ((asdfsym (x) (find-symbol (string x) asdf))
             (asdfcall (fun &rest rest) (apply (asdfsym fun) rest))
             (system-output (system type)
               (let ((build (make-instance (asdfsym :build-op) :type type)))
                 (first (asdfcall :output-files build system))))
             (existing-system-output (system type)
               (let ((o (system-output system type)))
                 (and o (setf o (probe-file o)) (namestring o))))
             (find-archive (system)
                 (or (existing-system-output system :library)
                     (existing-system-output system :shared-library)))
             (fallback () (format nil #-msvc "-l~A" #+msvc "~A.lib" (string-downcase library))))
      (or (and asdf
               (setf system (asdfcall :find-system library nil))
               (find-archive system))
        (fallback)))))

(defun builder (target output-name &key lisp-files ld-flags shared-data-file
		(init-name nil)
		(prologue-code "")
		(epilogue-code (when (eq target :program) '(SI::TOP-LEVEL)))
		#+:win32 (system :console)
		&aux
		(*suppress-compiler-messages* (or *suppress-compiler-messages*
						  (not *compile-verbose*))))
  ;; Deprecated, to be removed in next release
  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  ;;
  ;; The epilogue-code can be either a string made of C code, or a
  ;; lisp form.  In the latter case we add some additional C code to
  ;; clean up, and the lisp form is stored in a text representation,
  ;; to avoid using the compiler.
  ;;
  (cond ((null epilogue-code)
	 (setf epilogue-code ""))
	((stringp epilogue-code)
	 )
	(t
	 (with-standard-io-syntax
	   (setq epilogue-code
		 (with-output-to-string (stream)
		   (princ "{ const char *lisp_code = " stream)
		   (wt-filtered-data (write-to-string epilogue-code) stream)
		   (princ ";
cl_object output;
si_select_package(ecl_make_simple_base_string(\"CL-USER\",7));
output = cl_safe_eval(c_string_to_object(lisp_code), Cnil, OBJNULL);
" stream)
		   (when (eq target :program)
		     (princ "cl_shutdown(); return (output != OBJNULL);" stream))
		   (princ #\} stream)
		   )))))
  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (let* ((tmp-name (si::mkstemp #P"TMP:ECLINIT"))
	 (c-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :c)))
	 (o-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :object)))
	 submodules
	 c-file)
    (dolist (item (reverse lisp-files))
      (etypecase item
        (symbol
         (push (system-ld-flag item) ld-flags)
         (push (init-function-name item :kind :lib) submodules))
        ((or string pathname)
	 (let* ((pathname (parse-namestring item))
		(kind (guess-kind pathname)))
	   (unless (member kind '(:shared-library :dll :static-library :lib
				  :object :c))
	     (error "C::BUILDER does not accept a file ~s of kind ~s" item kind))
	   (let* ((path (parse-namestring item))
		  (init-fn (guess-init-name path (guess-kind path)))
		  (flags (guess-ld-flags path)))
	     ;; We should give a warning that we cannot link this module in
	     (when flags (push flags ld-flags))
	     (push init-fn submodules))))))
    (setq c-file (open c-name :direction :output :external-format :default))
    (format c-file +lisp-program-header+ submodules)
    (cond (shared-data-file
	   (data-init shared-data-file)
	   (format c-file "
#define VM ~A
#ifdef ECL_DYNAMIC_VV
static cl_object *VV;
#else
static cl_object VV[VM];
#endif
#define ECL_SHARED_DATA_FILE 1
" (data-permanent-storage-size))
	   (c-backend::data-dump c-file))
	  (t
	   (format c-file "
#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0" c-file)))
    (when (or (symbolp output-name) (stringp output-name))
      (setf output-name (compile-file-pathname output-name :type target)))
    (unless init-name
      (setf init-name (compute-init-name output-name :kind target)))
    (ecase target
      (:program
       (format c-file +lisp-program-init+ init-name "" shared-data-file
	       submodules "")
       (format c-file #+:win32 (ecase system (:console +lisp-program-main+)
				             (:windows +lisp-program-winmain+))
	              #-:win32 +lisp-program-main+
		      prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'linker-cc output-name (namestring o-name) ld-flags))
      ((:library :static-library :lib)
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (when (probe-file output-name) (delete-file output-name))
       #-msvc
       (progn
       (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			    output-name o-name ld-flags))
       (safe-system (format nil "ranlib ~A" output-name)))
       #+msvc
       (unwind-protect
         (progn
           (with-open-file (f "static_lib.tmp" :direction :output :if-does-not-exist :create :if-exists :supersede)
             (format f "/DEBUGTYPE:CV /OUT:~A ~A ~{~&\"~A\"~}"
                     output-name o-name ld-flags))
           (safe-system "link -lib @static_lib.tmp"))
         (when (probe-file "static_lib.tmp")
           (cmp-delete-file "static_lib.tmp")))
       )
      #+dlopen
      ((:shared-library :dll)
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'shared-cc output-name o-name ld-flags))
      #+dlopen
      (:fasl
       (format c-file +lisp-program-init+ init-name prologue-code shared-data-file
	       submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'bundle-cc output-name init-name o-name ld-flags)))
    (cmp-delete-file tmp-name)
    (cmp-delete-file c-name)
    (cmp-delete-file o-name)
    output-name))

(defun build-fasl (&rest args)
  (apply #'builder :fasl args))

(defun build-program (&rest args)
  (apply #'builder :program args))

(defun build-static-library (&rest args)
  (apply #'builder :static-library args))

(defun build-shared-library (&rest args)
  #-dlopen
  (error "Dynamically loadable libraries not supported in this system.")
  #+dlopen
  (apply #'builder :shared-library args))

(defun compile-file (input-pathname &rest args
                      &key
		      ((:verbose *compile-verbose*) *compile-verbose*)
		      ((:print *compile-print*) *compile-print*)
                      (source-truename nil)
                      (source-offset 0)
		      (c-file nil)
		      (h-file nil)
		      (data-file nil)
		      (shared-data-file nil)
		      (system-p nil)
		      (load nil)
                      (external-format :default)
		      output-file
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
                           (*package* *package*)
			   (*print-pretty* nil)
			   (*compile-file-pathname* nil)
			   (*compile-file-truename* nil)
                           (ext:*source-location* (cons source-truename 0))
			   (*suppress-compiler-messages*
			    (or *suppress-compiler-messages* (not *compile-verbose*)))
			   init-name)
  (declare (notinline compiler-cc))
  "Compiles the file specified by INPUT-PATHNAME and generates a fasl file
specified by OUTPUT-FILE.  If the filetype is not specified in INPUT-PATHNAME,
then \".lsp\" is used as the default file type for the source file.  LOAD
specifies whether to load the generated fasl file after compilation.  The
:O-FILE, :C-FILE, :H-FILE, and :DATA-FILE keyword parameters allow you to
control the intermediate files generated by the ECL compiler.If the file was
compiled successfully, returns the pathname of the compiled file"
  ;; Deprecated, to be removed in next release
  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  #-dlopen
  (unless system-p
    (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE-FILE without :SYSTEM-P T is unsupported.~
~%;;;"))

  (setq *compile-file-pathname* (pathname (merge-pathnames input-pathname)))
  (unless (probe-file *compile-file-pathname*)
    (if (pathname-type input-pathname)
	(error 'file-error :pathname input-pathname)
	(dolist (ext '("lsp" "LSP" "lisp" "LISP")
		 (error 'file-error :pathname input-pathname))
	  (setq *compile-file-pathname* (make-pathname :type ext :defaults input-pathname))
	  (when (probe-file *compile-file-pathname*)
	    (return)))))
  (setq input-file (truename *compile-file-pathname*)
        *compile-file-truename* input-file)

  (when (and system-p load)
    (error "Cannot load system files."))

  (cmpprogress "~&;;;~%;;; Compiling ~a." (namestring input-pathname))

  (let* ((eof '(NIL))
	 (*compiler-in-use* *compiler-in-use*)
	 (*load-time-values* nil) ;; Load time values are compiled
         (output-file (apply #'compile-file-pathname input-file args))
         (true-output-file nil) ;; Will be set at the end
         (c-pathname (apply #'compile-file-pathname output-file :output-file c-file
                            :type :c args))
         (h-pathname (apply #'compile-file-pathname output-file :output-file h-file
                            :type :h args))
         (data-pathname (apply #'compile-file-pathname output-file
                               :output-file data-file :type :data args))
	 (shared-data-pathname (apply #'compile-file-pathname output-file
                                      :output-file shared-data-file :type :sdata args))
	 (compiler-conditions nil)
         (to-delete (nconc (unless c-file (list c-pathname))
                           (unless h-file (list h-pathname))
                           (unless (or data-file shared-data-file)
                             (list data-pathname)))))

    (with-compiler-env (compiler-conditions)

      (print-compiler-info)

      (when (probe-file "./cmpinit.lsp")
	(load "./cmpinit.lsp" :verbose *compile-verbose*))

      (if shared-data-file
	  (if system-p
	      (data-init shared-data-pathname)
	      (error "Shared data files are only allowed when compiling ~&
		    with the flag :SYSTEM-P set to T."))
	  (data-init))

      (setf init-name (compute-init-name output-file :kind
					 (if system-p :object :fasl)))

      (with-t1expr (init-name)
        (with-open-file (*compiler-input* *compile-file-pathname*
                                          :external-format external-format)
          (unless source-truename
            (setf (car ext:*source-location*) *compile-file-pathname*))
          (do* ((*compile-file-position* 0 (file-position *compiler-input*))
                (form (si::read-object-or-ignore *compiler-input* eof)
                      (si::read-object-or-ignore *compiler-input* eof)))
               ((eq form eof))
            (when form
              (setf (cdr ext:*source-location*)
                    (+ source-offset *compile-file-position*))
              (t1expr form)))))
      (cmpprogress "~&;;; End of Pass 1.")

      (compiler-pass2 c-pathname h-pathname data-pathname system-p
		      init-name
		      shared-data-file
                      :input-designator (namestring input-pathname))

      (if shared-data-file
	  (c-backend::data-dump shared-data-pathname t)
	  (c-backend::data-dump data-pathname))

      (let ((o-pathname (if system-p
                            output-file
                            (compile-file-pathname output-file :type :object))))
        (compiler-cc c-pathname o-pathname)
        #+dlopen
        (unless system-p
          (push o-pathname to-delete)
          (bundle-cc (si::coerce-to-filename output-file)
                     init-name
                     (si::coerce-to-filename o-pathname))))

      (if (setf true-output-file (probe-file output-file))
          (cmpprogress "~&;;; Finished compiling ~a.~%;;;~%"
                       (namestring input-pathname))
          (cmperr "The C compiler failed to compile the intermediate file."))

      (mapc #'cmp-delete-file to-delete)

      (when (and load true-output-file (not system-p))
	(load true-output-file :verbose *compile-verbose*))

      ) ; with-compiler-env

    (compiler-output-values true-output-file compiler-conditions)))

(defun compiler-output-values (main-value conditions)
  (loop for i in conditions
     with warning-p = nil
     with failure-p = nil
     do (cond ((typep i 'style-warning)
	       (setf warning-p t))
	      ((typep i '(or compiler-error warning))
	       (setf warning-p t failure-p t)))
     finally (return (values main-value warning-p failure-p))))

#-dlopen
(defun compile (name &optional (def nil supplied-p))
  (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE is unsupported.~
~%;;;"))

#+dlopen
(defvar *gazonk-counter* 0)

#+dlopen
(defun compile (name &optional (def nil supplied-p)
                      &aux form data-pathname
                      (*suppress-compiler-messages* (or *suppress-compiler-messages*
							(not *compile-verbose*)))
                      (*compiler-in-use* *compiler-in-use*)
                      (*standard-output* *standard-output*)
                      (*error-output* *error-output*)
                      (*package* *package*)
                      (*compile-print* nil)
		      (*print-pretty* nil)
		      (*compiler-constants* t))
  "Args: (name &optional definition)

If DEFINITION is NIL, NAME must be the name of a not-yet-compiled function.
In this case, COMPILE compiles the function, installs the compiled function as
the global function definition of NAME, and returns NAME.  If DEFINITION is
non-NIL, it must be a lambda expression and NAME must be a symbol.  COMPILE
compiles the lambda expression, installs the compiled function as the function
definition of NAME, and returns NAME.  There is only one exception for this:
If NAME is NIL, then the compiled function is not installed but is simply
returned as the value of COMPILE.  In any case, COMPILE creates temporary
files, whose filenames begin with \"gazonk\", which are automatically deleted
after compilation."
  (unless (symbolp name) (error "~s is not a symbol." name))

  ;; Deprecated, to be removed in next release
  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  (cond ((and supplied-p def)
	 (when (functionp def)
	   (unless (function-lambda-expression def)
	     (return-from compile def))
	   (setf def (function-lambda-expression def)))
         (setq form (if name
                        `(setf (symbol-function ',name) #',def)
                        `(set 'GAZONK #',def))))
	((not (fboundp name))
	 (error "Symbol ~s is unbound." name))
	((typep (setf def (symbol-function name)) 'standard-generic-function)
	 (warn "COMPILE can not compile generic functions yet")
	 (return-from compile (values def t nil)))
	((null (setq form (function-lambda-expression def)))
	 (warn "We have lost the original function definition for ~s. Compilation to C failed"
               name)
	 (return-from compile (values def t nil)))
	(t
	 (setq form `(setf (symbol-function ',name) #',form))))

  (let ((template (format nil "TMP:ECL~3,'0x" (incf *gazonk-counter*))))
    (unless (setq data-pathname (si::mkstemp template))
      (error "Unable to create temporay file~%~
	~AXXXXXX
Make sure you have enough free space in disk, check permissions or set~%~
the environment variable TMPDIR to a different value." template)
      (return-from compile (values nil t t))))

  (let*((*load-time-values* 'values) ;; Only the value is kept
	(c-pathname (compile-file-pathname data-pathname :type :c))
	(h-pathname (compile-file-pathname data-pathname :type :h))
	(o-pathname (compile-file-pathname data-pathname :type :object))
	(so-pathname (compile-file-pathname data-pathname))
	(init-name (compute-init-name so-pathname :kind :fasl))
	(compiler-conditions nil))

    (with-compiler-env (compiler-conditions)
      (print-compiler-info)
      (data-init)
      (with-t1expr (init-name)
        (t1expr form))
      (cmpprogress "~&;;; End of Pass 1.")
      (let (#+(or mingw32 msvc cygwin)(*self-destructing-fasl* t))
	(compiler-pass2 c-pathname h-pathname data-pathname nil
			init-name nil
                        :input-designator (format nil "~A" def)))
      (setf *compiler-constants* (c-backend::data-dump data-pathname))

      (compiler-cc c-pathname o-pathname)
      (bundle-cc (si::coerce-to-filename so-pathname)
		 init-name
		 (si::coerce-to-filename o-pathname))
      (cmp-delete-file c-pathname)
      (cmp-delete-file h-pathname)
      (cmp-delete-file o-pathname)
      (cmp-delete-file data-pathname)
      (cond ((probe-file so-pathname)
	     (load so-pathname :verbose nil)
	     #-(or mingw32 msvc cygwin)
	     (cmp-delete-file so-pathname)
	     #+msvc
	     (delete-msvc-generated-files so-pathname)
	     (setf name (or name (symbol-value 'GAZONK)))
	     ;; By unsetting GAZONK we avoid spurious references to the
	     ;; loaded code.
	     (set 'GAZONK nil)
	     (si::gc t)
	     (values name nil nil))
	    (t
	     (cmperr "The C compiler failed to compile the intermediate code for ~s." name)))
      ) ; with-compiler-env

    (when (probe-file c-pathname) (cmp-delete-file c-pathname))
    (when (probe-file h-pathname) (cmp-delete-file h-pathname))
    (when (probe-file so-pathname) (cmp-delete-file so-pathname))
    (when (probe-file data-pathname) (cmp-delete-file data-pathname))
    #+msvc
    (delete-msvc-generated-files so-pathname)
    (compiler-output-values name compiler-conditions)))

(defun disassemble (thing &key (h-file nil) (data-file nil)
		    &aux def disassembled-form
		    (*compiler-in-use* *compiler-in-use*)
		    (*print-pretty* nil))
"Compiles the form specified by THING and prints the intermediate C language
code for that form.  But does not install the result of compilation.  If THING
is NIL, then the previously DISASSEMBLEd form is re-DISASSEMBLEd.  If THING is
a symbol that names a function not yet compiled, the function definition is
disassembled.  If THING is a lambda expression, it is disassembled as a
function definition.  Otherwise, THING itself is disassembled as a top-level
form.  H-FILE and DATA-FILE specify intermediate files to build a fasl file
from the C language code.  NIL means \"do not create the file\"."
  (when (si::valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (cond ((null thing))
	((functionp thing)
	 (unless (si::bc-disassemble thing)
	   (warn "Cannot disassemble the binary function ~S because I do not have its source code." thing)
	   (return-from disassemble nil)))
	((atom thing)
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing)))
	((eq (car thing) 'LAMBDA)
	 (setq disassembled-form `(defun gazonk ,@(cdr thing))))
	((eq (car thing) 'EXT:LAMBDA-BLOCK)
	 (setq disassembled-form `(defun ,@(rest thing))))
	(t
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing))))

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
				 (open h-file :direction :output :external-format :default)
				 null-stream))
         (t3local-fun (symbol-function 'c-backend::t3local-fun))
	 (compiler-conditions nil)
         (init-name (compute-init-name "foo" :kind :fasl)))
    (with-compiler-env (compiler-conditions)
      (unwind-protect
	   (progn
	     (setf (symbol-function 'c-backend::t3local-fun)
		   #'(lambda (&rest args)
		       (let ((*compiler-output1* *standard-output*))
			 (apply t3local-fun args))))
	     (data-init)
             (with-t1expr (init-name)
               (t1expr disassembled-form))
	     (c-backend::ctop-write init-name
                                    (if h-file h-file "")
                                    (if data-file data-file ""))
	     (c-backend::data-dump data-file))
	(setf (symbol-function 'c-backend::t3local-fun) t3local-fun)
	(when h-file (close *compiler-output2*)))))
  nil)

(defun compiler-pass2 (c-pathname h-pathname data-pathname system-p init-name
		       shared-data &key input-designator)
  (with-open-file (*compiler-output1* c-pathname :direction :output)
    (with-open-file (*compiler-output2* h-pathname :direction :output)
      (catch *cmperr-tag* (c-backend::ctop-write init-name
                                                 h-pathname
                                                 data-pathname
                                                 :input-designator input-designator
                                                 :shared-data shared-data))
      (terpri *compiler-output1*)
      (terpri *compiler-output2*))))

(defun ecl-include-directory ()
  "Finds the directory in which the header files were installed."
  (cond ((and *ecl-include-directory*
	      (probe-file (merge-pathnames "ecl/config.h" *ecl-include-directory*)))
	 *ecl-include-directory*)
	((probe-file "SYS:ecl;config.h")
	 (setf *ecl-include-directory* (namestring (translate-logical-pathname "SYS:"))))
	((error "Unable to find include directory"))))

(defun ecl-library-directory ()
  "Finds the directory in which the ECL core library was installed."
  (cond ((and *ecl-library-directory*
	      (probe-file (merge-pathnames (compile-file-pathname "ecl" :type
					    #+dlopen :shared-library
					    #-dlopen :static-library)
					   *ecl-library-directory*)))
	 *ecl-library-directory*)
	((probe-file "SYS:BUILD-STAMP")
	 (setf *ecl-library-directory* (namestring (translate-logical-pathname "SYS:"))))
	((error "Unable to find library directory"))))

(defun compiler-cc (c-pathname o-pathname)
  (safe-system
   (format nil
	   *cc-format*
	   *cc* *cc-flags* (>= (cmp-env-optimization 'speed) 2) *cc-optimize*
	   (fix-for-mingw (ecl-include-directory))
	   (si::coerce-to-filename c-pathname)
	   (si::coerce-to-filename o-pathname))
; Since the SUN4 assembler loops with big files, you might want to use this:
;   (format nil
;	   "~A ~@[~*-O1~] -S -I. -I~A -w ~A ; as -o ~A ~A"
;	   *cc* (>= *speed* 2)
;          *include-directory*
;	   (namestring c-pathname)
;	   (namestring o-pathname)
;	   (namestring s-pathname))
   ))

(defun print-compiler-info ()
  (cmpprogress "~&;;; OPTIMIZE levels: Safety=~d, Space=~d, Speed=~d, Debug=~d~%;;;~%"
	       *safety* *space* *speed* *debug*))

(defmacro with-compilation-unit (options &rest body)
  `(progn ,@body))

(si::package-lock "CL" nil)

#-ecl-min
(with-standard-io-syntax
  (load "sys:sysfun"))

(provide 'cmp)
