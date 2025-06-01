(in-package :asdf-groveler)

;;;; This ASDF groveler does not use ASDF's own operation framework because
;;;; it does not need the systems to be loaded or compiled. It is possible
;;;; to do some introspection of ASDF systems via the ASDF perform methods,
;;;; but ASDF assumes that the systems will be loaded into the host
;;;; implementation. In other words, the ASDF3 feature expressions will use
;;;; CL:*FEATURES*, but overriding CL:*FEATURES* will result in ASDF
;;;; attempting to load and compile the system being introspected with those
;;;; features leading to disastrous results.
;;;;
;;;; Instead, we walk the dependency/child component graph ourselves and
;;;; evaluate the feature expressions using what we anticipate CL:*FEATURES*
;;;; to be in the client. One weakness of this approach is that read-time
;;;; feature expressions in an ASD file will not be visible to us.

(defvar *groveler-modules* nil)
(defvar *groveler-systems* nil)
(defvar *groveler-files* nil)
(defvar *groveler-additional-files* nil)
(defvar *groveler-features* nil)
(defvar *groveler-file-type* t)

(defgeneric grovel-component (component))

(defmethod grovel-component ((component (eql nil))))

(defmethod grovel-component ((component list))
  (ccase (first component)
    (:feature
     (when (uiop:featurep (second component) *groveler-features*)
       (grovel-component (third component))))
    (:version
     (let* ((min-version (third component))
            (name (second component))
            (system (asdf:find-system name))
            (version (asdf:component-version system)))
       (cond ((null version)
              (warn "Requested version ~a but ~a has no version" min-version name))
             ((not (uiop:version<= min-version version))
              (error "Requested version ~a but ~a has version ~a" min-version name version)))
       (grovel-component system)))
    (:require
     (pushnew (intern (string-upcase (second component)) :keyword) *groveler-modules*))))

(defmethod grovel-component ((component symbol))
  (grovel-component (asdf:find-system component)))

(defmethod grovel-component ((component string))
  (grovel-component (asdf:find-system component)))

(defmethod grovel-component :around ((component asdf:component))
  (when (or (not (asdf::component-if-feature component))
            (uiop:featurep (asdf::component-if-feature component) *groveler-features*))
    (loop for dependency in (asdf::component-sideway-dependencies component)
          if (typep component 'asdf:system)
            do (grovel-component dependency)
          else if (listp dependency)
            do (loop for sub-dep in dependency
                     do (grovel-component (if (typep sub-dep 'asdf:component)
                                              component
                                              (asdf:find-component (asdf:component-parent component) sub-dep))))
          else
            do (grovel-component (if (typep dependency 'asdf:component)
                                     component
                                     (asdf:find-component (asdf:component-parent component) dependency))))
    (loop for (nil path) in (asdf/component::%additional-input-files component)
          do (pushnew path *groveler-additional-files*))
    (call-next-method)))

(defmethod grovel-component ((component asdf:parent-component))
  (loop for child in (asdf:component-children component)
        do (grovel-component child)))

(defmethod grovel-component (component)
  (declare (ignore component)))

(defmethod grovel-component ((component asdf:system))
  (loop for dependency in (asdf::system-defsystem-depends-on component)
        do (grovel-component dependency))               
  (pushnew (intern (string-upcase (asdf:component-name component)) :keyword) *groveler-systems*)
  (call-next-method))

(defmethod grovel-component ((component asdf:file-component))
  (if (typep component *groveler-file-type*)
    (pushnew (asdf:component-pathname component) *groveler-files*)
    (pushnew (asdf:component-pathname component) *groveler-additional-files*)))

(defun grovel (systems &key (features *features*) (file-type t))
  (loop with *groveler-features* = features
        with *groveler-modules* = nil
        with *groveler-systems* = nil
        with *groveler-files* = nil
        with *groveler-file-type* = file-type
        for system in systems
        do (grovel-component system)
        finally (return (values (nreverse *groveler-modules*)
                                (nreverse *groveler-systems*)
                                (nreverse *groveler-files*)
                                (nreverse *groveler-additional-files*)))))
