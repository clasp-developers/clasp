;;; Copyright (c) 2005 - 2007, Michael Goffioul (michael dot goffioul at swing dot be)
;;; Copyright (c) 2008, Juan Jose Garcia Ripoll
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file 'ecl/Copyright' for full details.
;;;
;;; ECL SPECIFIC OPERATIONS FOR ASDF
;;;
#+xcvb (module (:depends-on ("asdf")))

(in-package :asdf)

;;;
;;; COMPILE-OP / LOAD-OP (in asdf.lisp)
;;;
;;; In ECL, these operations produce both FASL files and the
;;; object files that they are built from. Having both of them allows
;;; us to later on reuse the object files for bundles, libraries,
;;; standalone executables, etc.
;;;

;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be a FASL, a statically
;;; linked library, a shared library, etc The different targets are
;;; defined by specialization.
;;;

(defclass bundle-op (operation)
  ((type :reader bundle-op-type)
   (monolithic :initform nil :reader bundle-op-monolithic-p)
   (name-suffix :initarg :name-suffix :initform nil)
   (build-args :initarg :args :initform nil :accessor bundle-op-build-args)
   (lisp-files :initform nil :accessor bundle-op-lisp-files)))

(defclass fasl-op (bundle-op)
  ((type :initform :fasl)))

(defclass lib-op (bundle-op)
  ((type :initform :lib)))

(defclass dll-op (bundle-op)
  ((type :initform :dll)))

(defclass monolithic-bundle-op (bundle-op)
  ((monolithic :initform t)
   (prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)))

(defclass monolithic-fasl-op (monolithic-bundle-op fasl-op) ())

(defclass monolithic-lib-op (monolithic-bundle-op lib-op)
  ((type :initform :lib)))

(defclass monolithic-dll-op (monolithic-bundle-op dll-op)
  ((type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ((type :initform :program)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                       &key (name-suffix nil name-suffix-p)
                                       &allow-other-keys)
  (declare (ignorable initargs name-suffix))
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
          (if (bundle-op-monolithic-p instance) "-mono" "")))
  (when (typep instance 'monolithic-bundle-op)
    (destructuring-bind (&rest original-initargs
                         &key lisp-files prologue-code epilogue-code
                         &allow-other-keys)
        (slot-value instance 'original-initargs)
      (setf (slot-value instance 'original-initargs)
            (remove-keys '(lisp-files epilogue-code prologue-code) original-initargs)
            (bundle-op-lisp-files instance) lisp-files
            (monolithic-op-prologue-code instance) prologue-code
            (monolithic-op-epilogue-code instance) epilogue-code)))
  (setf (bundle-op-build-args instance)
        (remove-keys '(type monolithic name-suffix)
                     (slot-value instance 'original-initargs))))

(defmethod bundle-op-build-args :around ((op lib-op))
  (let ((args (call-next-method)))
    (remf args :ld-flags)
    args))

(defvar *force-load-p* nil)

(defmethod operation-done-p :around ((operation load-op) c)
  (declare (ignorable operation c))
  (if *force-load-p* nil (call-next-method)))

(defun gather-components (op-type system &key filter-system filter-type include-self)
  ;; This function creates a list of components, matched together with an
  ;; operation. This list may be restricted to sub-components of SYSTEM if
  ;; GATHER-ALL = NIL (default), and it may include the system itself.
  (let* ((operation (make-instance op-type))
         (*force-load-p* t)
         (tree (traverse (make-instance 'load-op) system)))
    (append
     (loop :for (op . component) :in tree
       :when (and (typep op 'load-op)
                  (typep component filter-type)
                  (or (not filter-system) (eq (component-system component) filter-system)))
       :collect (progn
                  (when (eq component system) (setf include-self nil))
                  (cons operation component)))
     (and include-self (list (cons operation system))))))

;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component) which contains all the
;;; dependencies of this bundle. This list is used by TRAVERSE and also
;;; by INPUT-FILES. The dependencies depend on the strategy, as explained
;;; below.
;;;
(defgeneric bundle-sub-operations (operation component))
;;;
;;; First we handle monolithic bundles. These are standalone systems
;;; which contain everything, including other ASDF systems required
;;; by the current one. A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
;;; Gather the static libraries of all components.
;;;
(defmethod bundle-sub-operations ((o monolithic-bundle-op) c)
  (declare (ignorable o))
  (gather-components 'lib-op c :filter-type 'system :include-self t))
;;;
;;; STATIC LIBRARIES
;;;
;;; Gather the object files of all components and, if monolithic, also
;;; of systems and subsystems.
;;;
(defmethod bundle-sub-operations ((o lib-op) c)
  (gather-components 'compile-op c
                     :filter-system (and (not (bundle-op-monolithic-p o)) c)
                     :filter-type '(not system)))
(defmethod bundle-sub-operations ((o monolithic-lib-op) c)
  (gather-components 'compile-op c
                     :filter-system nil
                     :filter-type '(not system)))
;;;
;;; SHARED LIBRARIES
;;;
;;; Gather the dynamically linked libraries of all components.
;;; They will be linked into this new shared library, together
;;; with the static library of this module.
;;;
(defmethod bundle-sub-operations ((o dll-op) c)
  (declare (ignorable o))
  (list (cons (make-instance 'lib-op) c)))
;;;
;;; FASL FILES
;;;
;;; Gather the statically linked library of this component.
;;;
(defmethod bundle-sub-operations ((o fasl-op) c)
  (declare (ignorable o))
  (list (cons (make-instance 'lib-op) c)))

(defmethod component-depends-on ((o bundle-op) (c system))
  (loop for (op . dep) in (bundle-sub-operations o c)
     when (typep dep 'system)
     collect (list (class-name (class-of op))
                   (component-name dep))))

(defmethod component-depends-on ((o lib-op) (c system))
  (declare (ignorable o))
  (list (list 'compile-op (component-name c))))

(defmethod component-depends-on ((o bundle-op) c)
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o bundle-op) (c system))
  (loop for (sub-op . sub-c) in (bundle-sub-operations o c)
     nconc (output-files sub-op sub-c)))

(defmethod output-files ((o bundle-op) (c system))
  (let ((name (concatenate 'base-string (component-name c)
                           (slot-value o 'name-suffix))))
    (list (merge-pathnames* (compile-file-pathname name :type (bundle-op-type o))
                            (component-relative-pathname c)))))

(defmethod output-files ((o fasl-op) (c system))
  (declare (ignorable o c))
  (loop for file in (call-next-method)
     collect (make-pathname :type "fasb" :defaults file)))

(defmethod perform ((o bundle-op) (c t))
  (declare (ignorable o c))
  t)

(defmethod operation-done-p ((o bundle-op) (c source-file))
  (declare (ignorable o c))
  t)

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (remove "fas" (input-files o c)
                               :key #'pathname-type :test #'string=))
         (output (output-files o c)))
    (ensure-directories-exist (first output))
    (apply #'c::builder (bundle-op-type o) (first output)
           :lisp-files (append object-files (bundle-op-lisp-files o))
           (append (bundle-op-build-args o)
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-prologue-code o))
                     `(:prologue-code ,(monolithic-op-prologue-code o)))
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-epilogue-code o))
                     `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))

(defun select-operation (monolithic type)
  (ecase type
    ((:binary)
     (if monolithic 'monolithic-binary-op 'binary-op))
    ((:dll :shared-library)
     (if monolithic 'monolithic-dll-op 'dll-op))
    ((:lib :static-library)
     (if monolithic 'monolithic-lib-op 'lib-op))
    ((:fasl)
     (if monolithic 'monolithic-fasl-op 'fasl-op))
    ((:program)
     'program-op)))

(defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                   (move-here nil move-here-p)
                   &allow-other-keys)
  (let* ((operation-name (select-operation monolithic type))
         (move-here-path (if (and move-here
                                  (typep move-here '(or pathname string)))
                             (pathname move-here)
                             (merge-pathnames "./asdf-output/")))
         (operation (apply #'operate operation-name
                           system
                           (remove-keys '(monolithic type move-here) args)))
         (system (find-system system))
         (files (and system (output-files operation system))))
    (if (or move-here (and (null move-here-p)
                           (member operation-name '(:program :binary))))
        (loop with dest-path = (truename (ensure-directories-exist move-here-path))
           for f in files
           for new-f = (make-pathname :name (pathname-name f)
                                      :type (pathname-type f)
                                      :defaults dest-path)
           do (progn
                (when (probe-file new-f)
                  (delete-file new-f))
                (rename-file f new-f))
           collect new-f)
        files)))

;;;
;;; LOAD-FASL-OP
;;;
;;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;;

(defclass load-fasl-op (operation) ())

(defun trivial-system-p (c)
  (every #'(lambda (c) (typep c 'compiled-file)) (module-components c)))

(defmethod component-depends-on ((o load-fasl-op) (c system))
  (declare (ignorable o))
  (unless (trivial-system-p c)
    (subst 'load-fasl-op 'load-op
           (subst 'fasl-op 'compile-op
                  (component-depends-on (make-instance 'load-op) c)))))

(defmethod input-files ((o load-fasl-op) (c system))
  (declare (ignore o))
  (unless (trivial-system-p c)
    (output-files (make-instance 'fasl-op) c)))

(defmethod perform ((o load-fasl-op) (c t))
  (declare (ignore o c))
  nil)

(defmethod perform ((o load-fasl-op) (c system))
  (let ((l (input-files o c)))
    (and l
         (load (first l))
         (loop for i in (module-components c)
            do (setf (gethash 'load-op (component-operation-times i))
                     (get-universal-time))))))

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF libraries in precompiled
;;; form. Only useful when the dependencies have also been precompiled.
;;;

(defclass compiled-file (component) ())
(defmethod component-relative-pathname ((component compiled-file))
  (let* ((pathname (or (slot-value component 'relative-pathname)
		       (component-name component))))
    (coerce-pathname
     pathname
     :type (or (pathname-type pathname)
	       (compile-file-pathname pathname))
     :defaults (component-parent-pathname component))))

(defmethod output-files (o (c compiled-file))
  (declare (ignore o c))
  nil)
(defmethod input-files (o (c compiled-file))
  (declare (ignore o c))
  nil)
(defmethod perform ((o load-op) (c compiled-file))
  (declare (ignore o))
  (load (component-pathname c)))
(defmethod perform ((o load-fasl-op) (c compiled-file))
  (declare (ignore o))
  (load (component-pathname c)))
(defmethod perform (o (c compiled-file))
  (declare (ignore o c))
  nil)

;;;
;;; Pre-built systems
;;;
(defclass prebuilt-system (system)
  ((static-library :accessor prebuilt-system-static-library :initarg :lib)))

(defmethod output-files ((o lib-op) (c prebuilt-system))
  (declare (ignore o))
  (values (list (prebuilt-system-static-library c))
          t ; Advertise that we do not want this path renamed
          ))

(defmethod perform ((o lib-op) (c prebuilt-system))
  (car (output-files o c)))

(defmethod component-depends-on ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o monolithic-lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (error "Prebuilt system ~S shipped with ECL can not be used in a monolithic library operation." c))

(defmethod bundle-sub-operations ((o monolithic-bundle-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

;;;
;;; PREBUILT SYSTEM CREATOR
;;;

(defclass binary-op (bundle-op)
  ())

(defclass monolithic-binary-op (binary-op monolithic-bundle-op)
  ())

(defun binary-op-dependencies (o s)
  (multiple-value-bind (lib-op fasl-op)
      (if (bundle-op-monolithic-p o)
          (values 'monolithic-lib-op 'monolithic-fasl-op)
          (values 'lib-op 'fasl-op))
    (list (list (make-instance lib-op :args (bundle-op-build-args o))
                s)
          (list (make-instance fasl-op :args (bundle-op-build-args o))
                s))))

(defmethod component-depends-on ((o binary-op) (s system))
  (loop for dep in (binary-op-dependencies o s)
     append (apply #'component-depends-on dep)))

(defmethod input-files ((o binary-op) (s system))
  (loop for dep in (binary-op-dependencies o s)
     append (apply #'input-files dep)))

(defmethod output-files ((o binary-op) (s system))
  (list* (merge-pathnames* (make-pathname :name (component-name s)
                                          :type "asd")
                           (component-relative-pathname s))
         (loop for dep in (binary-op-dependencies o s)
            append (apply #'output-files dep))))

(defmethod perform ((o binary-op) (s system))
  (let* ((dependencies (binary-op-dependencies o s))
         (library (first (apply #'output-files (first dependencies))))
         (fasl (first (apply #'output-files (second dependencies))))
         (filename (first (output-files o s)))
         (name (component-name s))
         (name-keyword (intern (string name) (find-package :keyword))))
    (loop for dep in dependencies
       do (apply #'perform dep))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (format s ";;; Prebuilt ASDF definition for system ~A" name)
      (format s ";;; Built for ~A ~A on a ~A/~A ~A"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (software-type)
              (machine-type)
              (software-version))
      (let ((*package* (find-package :keyword)))
        (pprint `(defsystem ,name-keyword
                     :class asdf::prebuilt-system
                     :components ((:compiled-file ,(pathname-name fasl)))
                     :lib ,(make-pathname :name (pathname-name library)
                                          :type (pathname-type library)))
                s)))))

;;;
;;; Final integration steps
;;;

(export '(make-build load-fasl-op prebuilt-system))
(push '("fasb" . si::load-binary) ext:*load-hooks*)

(defun register-pre-built-system (name)
  (register-system (make-instance 'system :name (string name) :source-file nil)))

(defvar *require-asdf-operator* 'load-op)
(export '*require-asdf-operator*)

(defun module-provide-asdf (name)
  (handler-bind ((style-warning #'muffle-warning))
    (let* ((*verbose-out* (make-broadcast-stream))
           (system (asdf:find-system name nil)))
      (when system
        (asdf:operate *require-asdf-operator* name)
        t))))

(setf ext:*module-provider-functions*
      (loop :for f :in ext:*module-provider-functions*
        :unless (eq f 'module-provide-asdf)
        :collect #'(lambda (name)
                     (let ((l (multiple-value-list (funcall f name))))
                       (and (first l) (register-pre-built-system name))
                       (values-list l)))))
#+win32 (setf ext:*load-hooks* (append ext:*load-hooks* '(("asd" . si::load-source))))
(pushnew 'module-provide-asdf ext:*module-provider-functions*)
(pushnew (translate-logical-pathname "SYS:") *central-registry*)

(provide :asdf)
