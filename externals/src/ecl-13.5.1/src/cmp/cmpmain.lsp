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

(ext:package-lock "CL" nil)

(in-package "COMPILER")

#-threads
(defmacro with-lock ((lock) &body body)
  `(progn ,@body))

(defun safe-mkstemp (template)
  ;; We do several things here. One is to check for success in MKSTEMP,
  ;; the other one is to ensure that the output of this function _always_
  ;; carries a file type -- this solves a problem with filesystems where
  ;; mkstemp may introduce one or more dots in the name causing several
  ;; functions below to ignore parts of the name. Note that this forces
  ;; us to have two files per temp: one with and one without extension.
  (let* ((base (si::mkstemp template)))
    (when base
      (let ((output (make-pathname :name
                                   (concatenate 'string (pathname-name base)
                                                (or (pathname-type base) ""))
                                   :type "tmp"
                                   :defaults base)))
        (if (and (not (probe-file output)) (si:copy-file base output))
            (setf base (list (truename output) (truename base)))
            (progn (delete-file base) (setf base nil)))))
    (unless base
      (error "Unable to create temporay file~%~
	~AXXXXXX
Make sure you have enough free space in disk, check permissions or set~%~
the environment variable TMPDIR to a different value." template))
    base))

(defun compile-file-pathname (name &key (output-file T) (type nil type-supplied-p)
                              verbose print c-file h-file data-file
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
      (:h (setf extension "eclh"))
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
  (loop for i in '("implib" "exp" "ilk" "pdb")
	for full = (make-pathname :type i :defaults output-pathname)
	for truename = (probe-file full)
	when truename
	do (cmp-delete-file truename)))

#+msvc
(defun embed-manifest-file (o-file &optional (type :dll))
  (let* ((real-file (probe-file o-file)))
    (when real-file
      (let* ((manifest-namestring (concatenate 'string (namestring o-file)
					       ".manifest"))
             (resource-code (ecase type
                              ((:dll :shared-library :fasl :fas) 2)
                              ((:program) 1)))
             (resource-option (format nil "-outputresource:~A;~D"
                                      (namestring real-file)
                                      resource-code))
	     (manifest (probe-file manifest-namestring)))
	(when manifest
	  (safe-run-program "mt"
                            (list "-nologo"
                                  "-manifest"
                                  manifest-namestring
                                  resource-option))
	  (delete-file manifest))))))

(defun cmp-delete-file (file)
  (cond ((null *delete-files*))
	((ext:getenv "ECL_PRESERVE_FILES"))
        ((null (probe-file file)))
	(*debug-compiler*
	 (cmpprogress "~%Postponing deletion of ~A" file)
         (push file *files-to-be-deleted*))
	(t
         (delete-file file))))

(push #'(lambda () (mapc #'delete-file *files-to-be-deleted*))
      si::*exit-hooks*)

#-mingw32
(defmacro fix-for-mingw (directory-namestring)
  directory-namestring)

#+mingw32
(defun fix-for-mingw (directory-namestring)
  (let ((x (string-right-trim '(#\\ #\/) directory-namestring)))
    (if (zerop (length x)) "/" x)))

#+msvc
(defun linker-cc (o-pathname object-files &key
                  (type :program)
                  (ld-flags (split-program-options *ld-flags*)))
  (safe-run-program
   *ld*
   `(,(concatenate 'string "-Fe" (brief-namestring o-pathname))
     ,@object-files
     ,@(split-program-options *ld-rpath*)
     ,@(split-program-options *user-ld-flags*)
     ,@ld-flags))
  (embed-manifest-file o-pathname type)
  (delete-msvc-generated-files o-pathname))

#-msvc
(defun linker-cc (o-pathname object-files &key
                  (type :program)
                  (ld-flags (split-program-options *ld-flags*)))
  (safe-run-program
   *ld*
   `("-o" ,(brief-namestring o-pathname)
     ,(concatenate 'string "-L" (fix-for-mingw (ecl-library-directory)))
     ,@object-files
     ,@(and *ld-rpath* (list *ld-rpath*))
     ,@(split-program-options *user-ld-flags*)
     ,@ld-flags)))

(defun linker-ar (output-name o-name ld-flags)
  #-msvc
  (static-lib-ar (namestring output-name)
                 (list* (brief-namestring o-name) ld-flags))
  #+msvc
  (unwind-protect
       (progn
         (with-open-file (f "static_lib.tmp" :direction :output
                            :if-does-not-exist :create :if-exists :supersede)
           (format f "/DEBUGTYPE:CV /OUT:~A ~A ~{~&\"~A\"~}"
                   output-name o-name ld-flags))
         (safe-run-program "link" '("-lib" "@static_lib.tmp")))
    (when (probe-file "static_lib.tmp")
      (cmp-delete-file "static_lib.tmp"))))

(defun static-lib-ar (lib object-files)
  (let ((lib (brief-namestring lib)))
    (when (probe-file lib)
      (delete-file lib))
    (safe-run-program *ar* (list* "cr" lib (mapcar #'brief-namestring object-files)))
    (safe-run-program *ranlib* (list lib))))

#+dlopen
(defun shared-cc (o-pathname object-files)
  (let ((ld-flags (split-program-options *ld-shared-flags*)))
    #+msvc
    (setf ld-flags
          (let ((implib (si::coerce-to-filename
                         (compile-file-pathname o-pathname :type :lib))))
            ;; MSVC linker options are added at the end, after the
            ;; /link flag, because they are not processed by the
            ;; compiler, but by the linker
	    (append ld-flags
		    (list (concatenate 'string "/LIBPATH:"
				       (ecl-library-directory))
			  (concatenate 'string "/IMPLIB:" implib)))))
    #+mingw32
    (setf ld-flags (list* "-shared" ld-flags))
    (linker-cc o-pathname object-files :type :dll :ld-flags ld-flags)))

#+dlopen
(defun bundle-cc (o-pathname init-name object-files)
  (let ((ld-flags (split-program-options *ld-bundle-flags*)))
    #+msvc
    (setf ld-flags
          (let ((implib (si::coerce-to-filename
                         (compile-file-pathname o-pathname :type :import-library))))
            ;; MSVC linker options are added at the end, after the
            ;; /link flag, because they are not processed by the
            ;; compiler, but by the linker
            (append ld-flags
		    (list
		     ;; Not needed because we use ECL_DLLEXPORT
		     ;; (concatenate 'string "/EXPORT:" init-name)
		     (concatenate 'string "/LIBPATH:"
				  (ecl-library-directory))
		     (concatenate 'string "/IMPLIB:" implib)))))
    #+mingw32
    (setf ld-flags (list* "-shared" "-Wl,--export-all-symbols" ld-flags))
    (linker-cc o-pathname object-files :type :fasl :ld-flags ld-flags)))

(defconstant +lisp-program-header+ "
#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~:{	extern ECL_CPP_TAG void ~A(cl_object);~%~}

")

;;
;; This format string contains the structure of the code that initializes
;; a program, a library, a module, etc. Basically, it processes a codeblock
;; just like in a normal compiled file, but then adds all the codeblocks of
;; its corresponding modules.
;;
(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif
ECL_DLLEXPORT
void ~A(cl_object cblock)
{
        /*
         * This function is first invoked with a pointer to a Cblock
         * structure, so that the function initializes it, and then
         * it is invoked with OBJNULL, to force initialization.
         */
	static cl_object Cblock = OBJNULL;
        if (cblock != OBJNULL) {
		Cblock = cblock;
#ifndef ECL_DYNAMIC_VV
		cblock->cblock.data = NULL;
#endif
		cblock->cblock.data_size = 0;
		return;
	}
	~A
{
	/*
         * At this point Cblock contains the cblock of the parent.
         * Notice how the modules are linked to the parent forming a
         * circular chain. This disables the garbage collection of
         * the library until _ALL_ functions in all modules are unlinked.
         */
	cl_object current, next = Cblock;
~:{
	current = ecl_make_codeblock();
	current->cblock.next = next;
	next = current;
	ecl_init_module(current, ~A);
~}
	Cblock->cblock.next = current;
}
	~A
}")

(defconstant +lisp-program-main+ "
extern int
main(int argc, char **argv)
{
	cl_boot(argc, argv);
	ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
	~A
	ecl_init_module(OBJNULL, ~A);
	~A
	} ECL_CATCH_ALL_END;
	si_exit(0);
}")

(defconstant +lisp-library-main+ "
extern int
~A(int argc, char **argv)
{
	cl_boot(argc, argv);
	ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
	~A
	ecl_init_module(OBJNULL, ~A);
	~A
	} ECL_CATCH_ALL_END;
}")

#+:win32
(defconstant +lisp-program-winmain+ "
#include <windows.h>
int
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	char **argv;
	int argc;
	ecl_get_commandline_args(&argc, &argv);
	cl_boot(argc, argv);
	ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
	~A
	ecl_init_module(OBJNULL, ~A);
	~A
	} ECL_CATCH_ALL_END;
	si_exit(0);
}")

(defun guess-kind (pathname)
  "Given a file name, guess whether it is an object file, a library, a program
or a loadable module."
  (let ((record (assoc (pathname-type pathname)
		       '((#.+object-file-extension+ :object)
                         ("o" :object)
                         ("obj" :object)
                         ("c" :c)
                         (#.+static-library-extension+ :static-library)
			 ("lib" :static-library)
			 ("a" :static-library)
                         (#.+shared-library-extension+ :shared-library)
                         ("dylib" :shared-library)
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
     (brief-namestring pathname))
    ((:fasl :fas)
     nil)
    ((:static-library :lib :standalone-static-library :standalone-lib)
     (brief-namestring pathname))
    ((:shared-library :dll :standalone-shared-library :standalone-dll)
     (brief-namestring pathname))
    ((:program)
     nil)
    (otherwise
     (error "C::BUILDER cannot accept files of kind ~s" kind))))

(defun system-ld-flag (library)
  "Given a symbol, try to find a library that matches it, either by looking in the
filesystem or in the database of ASDF modules."
  (let ((asdf #+asdf (find-package "ASDF"))
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
             (fallback ()
		 (translate-logical-pathname
		  (merge-pathnames
		   "SYS:"
		   (compile-file-pathname (string-downcase library)
					  :type :library)))))
      (or
       #-ecl-min
       (and asdf
            (setf system (asdfcall :find-system library nil))
            (find-archive system))
       (fallback)))))

(defun builder (target output-name &key lisp-files ld-flags
		(init-name nil)
                (main-name nil)
		(prologue-code "")
		(epilogue-code (when (eq target :program) '(SI::TOP-LEVEL T)))
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
si_select_package(ecl_make_simple_base_string(\"CL-USER\", 7));
output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), ECL_NIL);
}" stream)
		   )))))
  (cond ((null prologue-code)
	 (setf prologue-code ""))
	((stringp prologue-code)
	 )
	(t
	 (with-standard-io-syntax
	   (setq prologue-code
		 (with-output-to-string (stream)
		   (princ "{ const char *lisp_code = " stream)
		   (wt-filtered-data (write-to-string prologue-code) stream)
		   (princ ";
cl_object output;
si_select_package(ecl_make_simple_base_string(\"CL-USER\", 7));
output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), ECL_NIL);
}" stream)
		   )))))
  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (let* ((tmp-names (safe-mkstemp #P"TMP:ECLINIT"))
         (tmp-name (first tmp-names))
	 (c-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :c)))
	 (o-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :object)))
	 submodules
         (submodules-data ())
	 c-file)
    (dolist (item (reverse lisp-files))
      (let* ((path (etypecase item
		     (symbol (system-ld-flag item))
		     (pathname item)
		     (string (parse-namestring item))))
	     (kind (guess-kind path)))
	(unless (member kind '(:shared-library :dll :static-library :lib
					       :object :c))
	  (error "C::BUILDER does not accept a file ~s of kind ~s" item kind))
	(let* ((init-fn (guess-init-name path (guess-kind path)))
	       (flags (guess-ld-flags path)))
	  ;; We should give a warning that we cannot link this module in
	  (when flags (push flags ld-flags))
	  (push (list init-fn path) submodules))))
    (setf submodules-data (apply #'concatenate '(array base-char (*))
                                 submodules-data))
    (setq c-file (open c-name :direction :output :external-format :default))
    (format c-file +lisp-program-header+ submodules)
    (when (or (symbolp output-name) (stringp output-name))
      (setf output-name (compile-file-pathname output-name :type target)))
    (unless init-name
      (setf init-name (compute-init-name output-name :kind target)))
    (unless main-name
      (setf main-name (compute-main-name output-name :kind target)))
    (ecase target
      (:program
       (format c-file +lisp-program-init+ init-name "" submodules "")
       (format c-file #+:win32 (ecase system (:console +lisp-program-main+)
				             (:windows +lisp-program-winmain+))
	              #-:win32 +lisp-program-main+
                      prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (linker-cc output-name (list* (namestring o-name) ld-flags)))
      ((:library :static-library :lib)
       (format c-file +lisp-program-init+ init-name prologue-code
	       submodules epilogue-code)
       (cmpnote "Library initialization function is ~A" main-name)
       (format c-file +lisp-library-main+
               main-name prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (when (probe-file output-name) (delete-file output-name))
       (linker-ar output-name o-name ld-flags))
      #+dlopen
      ((:shared-library :dll)
       (format c-file +lisp-program-init+ init-name prologue-code
	       submodules epilogue-code)
       (cmpnote "Library initialization function is ~A" main-name)
       (format c-file +lisp-library-main+
               main-name prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (shared-cc output-name (list* o-name ld-flags)))
      #+dlopen
      (:fasl
       (format c-file +lisp-program-init+ init-name prologue-code
	       submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (bundle-cc output-name init-name (list* o-name ld-flags))))
    (mapc 'cmp-delete-file tmp-names)
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
		           input-file
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
	 (compiler-conditions nil)
         (to-delete (nconc (unless c-file (list c-pathname))
                           (unless h-file (list h-pathname))
                           (unless data-file (list data-pathname)))))

    (with-compiler-env (compiler-conditions)

      (print-compiler-info)

      (when (probe-file "./cmpinit.lsp")
	(load "./cmpinit.lsp" :verbose *compile-verbose*))

      (data-init)

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
            (t1expr form))))

      (cmpprogress "~&;;; End of Pass 1.")
      (setf init-name (compute-init-name output-file :kind
					 (if system-p :object :fasl)))
      (compiler-pass2 c-pathname h-pathname data-pathname init-name
                      :input-designator (namestring input-pathname))

      (data-c-dump data-pathname)

      (let ((o-pathname (if system-p
                            output-file
                            (compile-file-pathname output-file :type :object))))
        (compiler-cc c-pathname o-pathname)
        #+dlopen
        (unless system-p
          (push o-pathname to-delete)
          (bundle-cc (si::coerce-to-filename output-file)
                     init-name
                     (list (si::coerce-to-filename o-pathname)))))

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

  (let*((*load-time-values* 'values) ;; Only the value is kept
        (tmp-names (safe-mkstemp (format nil "TMP:ECL~3,'0x" (incf *gazonk-counter*))))
        (data-pathname (first tmp-names))
	(c-pathname (compile-file-pathname data-pathname :type :c))
	(h-pathname (compile-file-pathname data-pathname :type :h))
	(o-pathname (compile-file-pathname data-pathname :type :object))
	(so-pathname (compile-file-pathname data-pathname))
	(init-name (compute-init-name so-pathname :kind :fasl))
	(compiler-conditions nil))

    (with-compiler-env (compiler-conditions)
      (print-compiler-info)
      (data-init)
      (t1expr form)
      (cmpprogress "~&;;; End of Pass 1.")
      (let (#+(or mingw32 msvc cygwin)(*self-destructing-fasl* t))
	(compiler-pass2 c-pathname h-pathname data-pathname init-name
                        :input-designator (let* ((*print-circle* t)
						 (*print-length* 8)
						 (*print-depth* 4))
					    (format nil "~W" def))))
      (data-c-dump data-pathname)

      (compiler-cc c-pathname o-pathname)
      (bundle-cc (si::coerce-to-filename so-pathname)
		 init-name
		 (list (si::coerce-to-filename o-pathname)))
      (cmp-delete-file c-pathname)
      (cmp-delete-file h-pathname)
      (cmp-delete-file o-pathname)
      (mapc 'cmp-delete-file tmp-names)

      (cond ((probe-file so-pathname)
	     (load so-pathname :verbose nil)
	     (cmp-delete-file so-pathname))
	    (t
	     (setf name nil)
	     (set 'GAZONK nil)
	     (cmperr "The C compiler failed to compile the intermediate code for ~s." name)))
      ) ; with-compiler-env
    (cmp-delete-file c-pathname)
    (cmp-delete-file h-pathname)
    (cmp-delete-file so-pathname)
    (mapc 'cmp-delete-file tmp-names)
    (let ((output (or name (symbol-value 'GAZONK))))
      ;; By unsetting GAZONK we avoid spurious references to the
      ;; loaded code.
      (set 'GAZONK nil)
      (si::gc t)
      (compiler-output-values output compiler-conditions))))

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
         (t3local-fun (symbol-function 'T3LOCAL-FUN))
	 (compiler-conditions nil))
    (with-compiler-env (compiler-conditions)
      (unwind-protect
	   (progn
	     (setf (symbol-function 'T3LOCAL-FUN)
		   #'(lambda (&rest args)
		       (let ((*compiler-output1* *standard-output*))
			 (apply t3local-fun args))))
	     (data-init)
	     (t1expr disassembled-form)
	     (ctop-write (compute-init-name "foo" :kind :fasl)
			 (if h-file h-file "")
			 (if data-file data-file ""))
	     (when data-file
               (data-c-dump data-file)))
	(setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
	(when h-file (close *compiler-output2*)))))
  nil)

(defun compiler-pass2 (c-pathname h-pathname data-pathname init-name
		       &key input-designator)
  (with-open-file (*compiler-output1* c-pathname :direction :output
				      :if-does-not-exist :create :if-exists :supersede)
    (wt-comment-nl "Compiler: ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
    #-ecl-min
    (multiple-value-bind (second minute hour day month year)
        (get-decoded-time)
      (declare (ignore second))
      (wt-comment-nl "Date: ~D/~D/~D ~2,'0D:~2,'0D (yyyy/mm/dd)" year month day hour minute)
      (wt-comment-nl "Machine: ~A ~A ~A" (software-type) (software-version) (machine-type)))
    (wt-comment-nl "Source: ~A" input-designator)
    (with-open-file (*compiler-output2* h-pathname :direction :output
					:if-does-not-exist :create :if-exists :supersede)
      (wt-nl1 "#include " *cmpinclude*)
      (ctop-write init-name h-pathname data-pathname)
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
  (safe-run-program
   *cc*
   `("-I."
     ,(concatenate 'string "-I" (fix-for-mingw (ecl-include-directory)))
     ,@(split-program-options *cc-flags*)
     ,@(and (>= (cmp-env-optimization 'speed) 2)
            (split-program-options *cc-optimize*))
     "-c"
     ,(brief-namestring c-pathname)
     #-msvc
     ,@(list "-o" (brief-namestring o-pathname))
     #+msvc
     ,(concatenate 'string "-Fo" (brief-namestring o-pathname))
     ,@(split-program-options *user-cc-flags*))))
; Since the SUN4 assembler loops with big files, you might want to use this:
;   (format nil
;	   "~A ~@[~*-O1~] -S -I. -I~A -w ~A ; as -o ~A ~A"
;	   *cc* (>= *speed* 2)
;          *include-directory*
;	   (namestring c-pathname)
;	   (namestring o-pathname)
;	   (namestring s-pathname))

(defun print-compiler-info ()
  (cmpprogress "~&;;; OPTIMIZE levels: Safety=~d, Space=~d, Speed=~d, Debug=~d~%;;;~%"
	       *safety* *space* *speed* *debug*))

(defmacro with-compilation-unit (options &rest body)
  `(progn ,@body))

(ext:package-lock "CL" t)

(setf *features* (delete :ecl-bytecmp *features*))

(let* ((compile #'compile)
       (disassemble #'disassemble)
       (compile-file #'compile-file)
       (compile-file-pathname #'compile-file-pathname))
  (defun ext:install-c-compiler ()
    (ext::package-lock (find-package :cl) nil)
    (setf *features* (delete :ecl-bytecmp *features*))
    (setf (fdefinition 'disassemble) disassemble
	  (fdefinition 'compile) compile
	  (fdefinition 'compile-file) #'compile-file
	  (fdefinition 'compile-file-pathname) #'compile-file-pathname)
    (ext::package-lock (find-package :cl) t)))

(provide 'cmp)
