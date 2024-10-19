(in-package #:koga)

(defparameter *root-paths* nil)
(defparameter *script-path* nil)
(defparameter *variant-path* nil)

(defclass source ()
  ((path :accessor source-path
         :initarg :path)
   (root :accessor source-root
         :initarg :root
         :type keyword)))

(defun root (root)
  (getf *root-paths* root))

(defun resolve-source-root (source)
  (getf *root-paths* (source-root source)))

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
    (#P"*.cxx" . cc-source)
    (#P"*.h" . h-source)
    (#P"*.hpp" . h-source)
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
  (make-source (if type
                   (merge-pathnames (make-pathname :type type)
                                    (source-path source))
                   (source-path source))
               (or root :variant)))

(defun make-source-outputs (sources &rest rest &key type root &allow-other-keys)
  (declare (ignore type root))
  (mapcar (lambda (source) (apply #'make-source-output source rest)) sources))

(defun read-code-path (stream char n)
  (declare (ignore char n))
  (let ((path (uiop:parse-unix-namestring (read stream :recursive-p t))))
    (make-source (if *script-path*
                     (merge-pathnames path *script-path*)
                     path)
                 :code)))

(set-dispatch-macro-character #\# #\~ #'read-code-path)

(defun read-variant-generated-path (stream char n)
  (declare (ignore char n))
  (make-source (uiop:parse-unix-namestring (read stream :recursive-p t))
               :variant-generated))

(set-dispatch-macro-character #\# #\@ #'read-variant-generated-path)

(defun root-to-logical-host (root)
  (ecase root
    (:code "sys")
    (:variant-lib "lib")
    (:variant-generated "generated")))

(defun root-to-prefix (root)
  (ecase root
    (:code nil)
    (:variant-lib "lib")
    (:variant-generated "generated")))

(defun source-logical-namestring (source &key version)
  (ninja:make-logical-pathname-representation "SYS"
                                              (source-path source)
                                              :version version
                                              :prefix (root-to-prefix (source-root source))))
