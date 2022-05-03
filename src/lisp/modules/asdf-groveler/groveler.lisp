(in-package :asdf-groveler)

(defvar *all-systems* nil)
(defvar *all-source-files* nil)

(defclass sticky-beak-op (#-asdf3 asdf:operation
				                  #+asdf3 asdf:downward-operation)
  ())

(defmethod asdf:component-depends-on ((op sticky-beak-op) component)
  (append (list (cons 'sticky-beak-op (asdf::component-sideway-dependencies component)))
	        (call-next-method)))

(defmethod asdf:perform ((op sticky-beak-op) (component t))
  (declare (ignore op component)))

(defmethod asdf:perform ((op sticky-beak-op) (component asdf:system))
  (declare (ignore op))
  (pushnew component *all-systems*))

(defmethod asdf:perform ((op sticky-beak-op) (c asdf/component:source-file))
  (pushnew c *all-source-files*))

(defun grovel-systems (systems)
  (loop with *all-systems* = nil
        with *all-source-files* = nil
        for system in systems
        do (asdf:oos 'sticky-beak-op system :force t)
        finally (return (nreverse *all-systems*))))

(defun grovel-source-files (systems &key all-sources root-path)
  (loop with *all-systems* = nil
        with *all-source-files* = nil
        for system in systems
        do (asdf:oos 'sticky-beak-op system :force t)
        finally (return (loop for source in (nreverse *all-source-files*)
                              for path = (asdf/component:component-pathname source)
                              when (or all-sources
                                       (typep source 'asdf/lisp-action:cl-source-file))
                                collect (if root-path
                                            (uiop:subpathp path root-path)
                                            path)))))