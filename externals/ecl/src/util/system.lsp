;;;
;;; **********************************************************************
;;; (c) Copyright G. Attardi, 1993.  All rights reserved.
;;; **********************************************************************
;;;
;;; A simple minded System Builder Tool.
;;;

;;; ----------------------------------------------------------------------
;;; Use:
;;;
;;; (defsystem name
;;;
;;;   :modules
;;;   -----------------------------------------------------------------
;;;    file     |   load           |   compile       | files which force
;;;             | environment      | environment     | recompilations of
;;;   -----------------------------------------------------------------
;;;   `(,@patches
;;;   (pkg          ,(car patches)  ,(car patches)         ())
;;;   (macros       (pkg macros)    (pkg macros)           ())
;;;   (low          (pkg walk)      (pkg macros)           (macros))
;;;   (,xxx-low     (low)           (macros low)           (low))
;;;   (boot         (,xxx)          (macros low ,xxx)      (low ,xxx))
;;;   (last         t               t                      (boot)))
;;;
;;;   :source-directory '("./")
;;;   :source-extension "./"
;;;   :fasl-directory "/usr/src/o/"
;;;   :fasl-extension "o"
;;;   :library-directory "./")
;;;
;;; ----------------------------------------------------------------------

(defpackage "SBT"
  (:use "CL")
  (:export defsystem
	   build-system
	   compile-system
	   load-system
	   build-ecl))

(in-package "SBT")

(defmacro defsystem (name &key modules
		     (source-directory '("./"))
		     (source-extension "lsp")
		     (fasl-directory "./")
		     (fasl-extension "o")
		     (library-directory "./"))
  `(defparameter ,name			; rather then defvar
     (make-system :NAME ',name
		  :MODULES ,modules
		  :SOURCE-DIRECTORY ,(if (consp source-directory) source-directory (list 'quote (list source-directory)))
		  :SOURCE-EXTENSION ,source-extension
		  :FASL-DIRECTORY ,fasl-directory
		  :FASL-EXTENSION ,fasl-extension
		  :LIBRARY-DIRECTORY ,library-directory)))

;;; ----------------------------------------------------------------------

(defstruct (system (:TYPE vector) :NAMED)
  name
  modules
  (source-directory '("./"))
  (source-extension "lsp")
  (fasl-directory "./")
  (fasl-extension "o")
  (library-directory "./"))

(defun make-source-pathname (name system)
  (let ((name (string-downcase name))
	(extension (system-source-extension system)))
    (dolist (i (system-source-directory system))
      (let ((pathname (make-pathname :name name :type extension
				     :defaults i)))
	(let ((ok (probe-file pathname)))
	  (when ok (return-from make-source-pathname ok)))))
    (error "sbt::make-source-pathname: source file not found")))

(defun make-binary-pathname (name system)
  (make-pathname :name (string-downcase name)
		 :type (system-fasl-extension system)
		 :defaults (system-fasl-directory system)))

(defun make-library-pathname (system target)
  (let* ((name (string-downcase (system-name system)))
	 (directory (system-library-directory system))
	 (output-name (merge-pathnames name directory)))
    (compile-file-pathname output-name :type target)))

;;; ----------------------------------------------------------------------
;;; Operations on modules
;;; 

(defstruct (module (:TYPE vector) :NAMED
		   (:CONSTRUCTOR make-module (name))
;                   (:PRINT-FUNCTION
;                     (lambda (m s d)
;                       (declare (ignore d))
;                       (format s "#<Module ~A>" (module-name m))))
		   )
  name
  load-env
  comp-env
  recomp-reasons)

(defun make-modules (system-description)
  (let ((modules ()))
    (labels ((get-module (name)
               (or (find name modules :KEY #'module-name)
                   (progn (setq modules (cons (make-module name) modules))
                          (first modules))))
             (parse-spec (spec)
               (if (eq spec 't)
                   (reverse (cdr modules))
                   (mapcar #'get-module spec))))
      (dolist (file system-description)
        (let* ((name (first file))
               (module (get-module name)))
          (setf (module-load-env module) (parse-spec (second file))
                (module-comp-env module) (parse-spec (third file))
                (module-recomp-reasons module) (parse-spec (fourth file))))))
    (reverse modules)))

(defun make-transformations (system filter make-transform)
  (let ((transforms (list nil)))
    (dolist (m (make-modules (system-modules system)))
      (when (funcall filter system m transforms)
        (funcall make-transform m transforms)))
    (nreverse (cdr transforms))))

(defun make-compile-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (first trans) :COMPILE)
                 (eq (second trans) module)
                 (return trans)))    
    (dolist (c (module-comp-env module))
      (make-load-transformation c transforms))
    (push `(:COMPILE ,module) (cdr transforms))))

(defun make-load-transformation (module transforms)
  (unless (dolist (trans transforms)
            (when (eq (second trans) module)
              (case (first trans)
		(:COMPILE (return nil))
		(:LOAD    (return trans)))))
    (dolist (l (module-load-env module))
      (make-load-transformation l transforms))
    (push `(:LOAD ,module) (cdr transforms))))


(defun make-load-without-dependencies-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (first trans) :LOAD)
                 (eq (second trans) module)
                 (return trans)))
    (push `(:LOAD ,module) (cdr transforms))))

(defun compile-filter (system module transforms)
  (or (dolist (r (module-recomp-reasons module))
        (when (dolist (transform transforms)
                (when (and (eq (first transform) :COMPILE)
                           (eq (second transform) r))
                  (return t)))
          (return t)))
      (null (probe-file (make-binary-pathname (module-name module) system)))
      (> (file-write-date (make-source-pathname (module-name module) system))
         (file-write-date (make-binary-pathname (module-name module) system)))))

(defun sbt-compile-file (&rest a)
  (apply #'compiler::compile-file a))

(defun operate-on-system (system mode &optional arg print-only
			  &aux (si::*init-function-prefix*
				(string-upcase (system-name system))))
  (let (transformations)
    (flet ((load-module (m s)
             (let ((name (module-name m)))
	       #-dlopen
	       (if print-only
		 (format t "~&Loading source of ~A..." name)
		 (load (make-source-pathname name s)))
	       #+dlopen
               (if (or (eq mode :source)
		       (dolist (trans transformations)
			       (and (eq (first trans) :compile)
				    (eq (second trans) m)
				    ; Is this ok?
				    (return nil))))
                   (if print-only
		     (format t "~&Loading source of ~A..." name)
		     (load (make-source-pathname name s)))
                   (if print-only
		     (format t "~&Loading binary of ~A..." name)
		     (load (make-binary-pathname name s))))))

           (compile-module (m s)
             (format t "~&Compiling ~A..." (module-name m))
             (unless print-only
	       (let  ((name (module-name m)))
                 (sbt-compile-file (make-source-pathname name s)
		   :OUTPUT-FILE (make-binary-pathname name s)))))

	   (true (&rest ignore) (declare (ignore ignore)) 't))

      (setq transformations
        (ecase mode
	  ((:STATIC-LIBRARY :LIBRARY :SHARED-LIBRARY :FASL)
	    (let* ((transforms (make-transformations system
						     #'true
						     #'make-load-transformation))
		   (objects (mapcar #'(lambda (x) (make-binary-pathname (module-name (cadr x)) system))
				    (remove-if-not #'(lambda (x) (eq (car x) :LOAD))
						   transforms)))
		   (library (make-library-pathname system mode)))
	      (operate-on-system system :COMPILE)
	      (c::builder mode library :lisp-files objects))
	    nil)
          (:COMPILE
            (make-transformations system
                                  #'compile-filter
                                  #'make-compile-transformation))
          (:RECOMPILE
            (make-transformations system
				  #'true
                                  #'make-compile-transformation))
          (:QUERY-COMPILE
            (make-transformations system
                                  #'(lambda (s m transforms)
                                      (or (compile-filter s m transforms)
                                          (y-or-n-p "Compile ~A?"
                                                    (module-name m))))
                                  #'make-compile-transformation))
          (:COMPILE-FROM
            (make-transformations system
                                  #'(lambda (s m transforms)
                                      (or (member (module-name m) arg)
                                          (compile-filter s m transforms)))
                                  #'make-compile-transformation))
          ((:LOAD :SOURCE)
            (make-transformations system
				  #'true
                                  #'make-load-transformation))
          (:QUERY-LOAD
            (make-transformations system
              #'(lambda (s m transforms)
		  (declare (ignore s transforms))
                  (y-or-n-p "Load ~A?" (module-name m)))
              #'make-load-without-dependencies-transformation))))
      
      (dolist (transform transformations)
	(ecase (first transform)
	  (:COMPILE (compile-module (second transform) system))
	  (:LOAD (load-module (second transform) system)))))))


(defun compile-system (system &optional m)
  (cond ((null m)      (operate-on-system system :COMPILE))
	((eq m 't)     (operate-on-system system :RECOMPILE))
	((eq m :PRINT) (operate-on-system system :COMPILE () t))
	((eq m :QUERY) (operate-on-system system :QUERY-COMPILE))
	((symbolp m)   (operate-on-system system :COMPILE-FROM (list m)))
	((listp m)     (operate-on-system system :COMPILE-FROM m))))

(defun load-system (system &optional mode)
  (case mode
    ((NIL) (operate-on-system system :LOAD))
    (:SOURCE (operate-on-system system :SOURCE))
    (:QUERY-LOAD (operate-on-system system :QUERY-LOAD))))


;;;----------------------------------------------------------------------
;;; User interface

(defmacro build-system (system &optional op mode)
  (case op
	(:LOAD
	 `(load-system ,system ,(case mode
				      (:QUERY :QUERY-LOAD)
				      (:SOURCE :SOURCE))))
	(:COMPILE
	 `(compile-system ,system ,(case mode
					 (:QUERY :QUERY-COMPILE)
					 (:FORCE :RECOMPILE))))
	(:PRINT
	 `(compile-system ,system :PRINT))
	(otherwise
	 `(load-system ,system))))
