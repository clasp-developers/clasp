(defpackage #:clasp-docs
  (:use #:cl)
  (:export))

(in-package #:clasp-docs)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-truename* *load-truename*)))
(defvar *default-output-directory* (merge-pathnames "../../../docs/" *here*))
(defvar *default-packages* '(:ext))

(defclass main-page (staple:templated-page)
  ((staple:document-package :initarg :document-package :initform NIL :accessor staple:document-package)
   (staple:document :initarg :document :initform (error "DOCUMENT required.") :accessor staple:document)))

(defmethod staple:template-data append ((page main-page))
  (list :documentation (when (staple:document page)
                         (staple:compile-source (staple:document page) page))))

(defmethod staple:compile-source ((document pathname) (page main-page))
  (let ((*package* (or (staple:document-package page)
                       (find-package "CL-USER"))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source document T))))

(defun make-main-page (project)
  (make-instance 'main-page
                 :project project
                 :title "Clasp"
                 :language :en
                 :input staple:*default-template*
                 :output (make-pathname :name "index" :type "html"
                                        :defaults (staple:output project))
                 :document (make-pathname :name "documentation" :type "md"
                                          :defaults *here*)))

(defclass package-page (staple:definitions-index-page)
  ())

(defmethod definition-wanted-p ((definition definitions:definition) (page package-page))
  (eql :external (definitions:visibility definition)))

(defmethod definition-wanted-p ((definition definitions:method) (page package-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:package) (page package-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:compiler-macro) (page package-page))
  NIL)

(defun make-package-page (package project)
  (let ((package (staple:ensure-package package)))
    (make-instance 'package-page
                   :project project
                   :title (package-name package)
                   :language :en
                   :input staple:*default-template*
                   :output (make-pathname :name (format NIL "package-~(~a~)" (package-name package))
                                          :type "html" :defaults (staple:output project))
                   :packages (list package))))

(defmethod staple:find-project ((project (eql (asdf:find-system :clasp-docs))) &rest args)
  (apply #'staple:find-project :clasp args))

(defmethod staple:find-project ((project (eql :clasp)) &key (output-directory *default-output-directory*)
                                                            (packages *default-packages*))
  (let ((project (make-instance 'staple:simple-project :output output-directory)))
    (push (make-main-page project) (staple:pages project))
    (loop for package in packages
          do (push (make-package-page package project) (staple:pages project)))
    project))

;; (asdf:load-system :clasp-docs)
;; (staple:generate :clasp :if-exists :supersede)
