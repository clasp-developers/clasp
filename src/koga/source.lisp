(in-package #:koga)

(defparameter *build-path* nil)
(defparameter *code-path* nil)
(defparameter *script-path* nil)
(defparameter *variant-path* nil)
(defparameter *install-path* nil)
(defparameter *install-bin-path* nil)
(defparameter *install-clasp-path* nil)
(defparameter *install-variant-path* nil)

(defclass source ()
  ((path :accessor source-path
         :initarg :path)
   (root :accessor source-root
         :initarg :root
         :type (member :current :code :build :install :install-bin
                       :install-clasp :install-variant :variant))))

(defun resolve-source-root (source)
  (case (source-root source)
    (:build *build-path*)
    (:install *install-path*)
    (:install-bin *install-bin-path*)
    (:install-clasp *install-clasp-path*)
    (:install-variant *install-variant-path*)
    (:variant *variant-path*)
    (:code *code-path*)))

(defun resolve-source (source)
  (let ((root (resolve-source-root source)))
    (if root
        (merge-pathnames (source-path source) root)
        (source-path source))))

(defmethod print-object ((object source) stream)
  (princ (resolve-source object) stream))

(defclass directory-source (source)
  ())

(defclass c-source (source)
  ())

(defclass cc-source (c-source)
  ())
  
(defclass h-source (source)
  ())

(defclass lisp-source (source)
  ())
  
(defclass ninja-source (source)
  ())
  
(defclass sif-source (source)
  ())

(defparameter *source-classes*
  '((#P"*.c" . c-source)
    (#P"*.cc" . cc-source)
    (#P"*.h" . h-source)
    (#P"*.lsp" . lisp-source)
    (#P"*.lisp" . lisp-source)
    (#P"*.ninja" . ninja-source)
    (#P"*.sif" . sif-source)
    (#P"*.*" . source)))
  
(defun name-match-p (path pattern)
  (and (or (eq :wild (pathname-name pattern))
           (equalp (pathname-name path) (pathname-name pattern)))
       (or (eq :wild (pathname-type pattern))
           (equalp (pathname-type path) (pathname-type pattern)))))
  
(defun make-source (path &optional (root :current))
  (make-instance (if (uiop:directory-pathname-p path)
                     'directory-source
                     (cdr (assoc path *source-classes* :test #'name-match-p)))
                 :path path :root root))

(defun make-source-output (source &key type root &allow-other-keys)
  (let ((path (source-path source)))
    (when root
      (setf path (merge-pathnames path (uiop:parse-unix-namestring root))))
    (when type
      (setf path (merge-pathnames (make-pathname :type type) path)))
    (make-source path :variant)))

(defun make-source-outputs (sources &rest rest &key type root &allow-other-keys)
  (declare (ignore type root))
  (mapcar (lambda (source) (apply #'make-source-output source rest)) sources))

(defun read-code-path (stream char n)
  (declare (ignore char n))
  (let ((path (uiop:parse-unix-namestring (read stream))))
    (make-source (if *script-path*
                     (merge-pathnames path *script-path*)
                     path)
                 :code)))

(set-dispatch-macro-character #\# #\~ #'read-code-path)

(defun read-variant-path (stream char n)
  (declare (ignore char n))
  (let ((path (uiop:parse-unix-namestring (read stream))))
    (make-source (if *script-path*
                     (merge-pathnames path *script-path*)
                     path)
                 :variant)))

(set-dispatch-macro-character #\# #\@ #'read-variant-path)

