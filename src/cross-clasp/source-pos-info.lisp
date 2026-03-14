(in-package #:cross-clasp)

(defun ext:current-source-location ()
  #-clasp(maclina.compile:default-source-location)
  ;; variable instead of EXT:CURRENT-SOURCE-LOCATION due to
  ;; shadowing problems.
  #+clasp sys:*current-source-pos-info*)

(defmethod maclina.compile-file:make-load-form
    ((client client) (object maclina.compile-file:source-location)
     &optional environment)
  (declare (ignore environment))
  (values `(core:make-cxx-object
            (find-class 'core:source-pos-info)
            :sfi (core:file-scope
                  ,(namestring (maclina.compile-file:source-location-pathname object)))
            :fp ',(car (maclina.compile-file:source-location-position object))
            :l 0 :c 0))) ; FIXME

;;; KLUDGE
(defmethod maclina.compile-file:make-load-form
    ((client ct-client) (object maclina.compile-file:source-location)
     &optional environment)
  (declare (ignore environment))
  'nil)
