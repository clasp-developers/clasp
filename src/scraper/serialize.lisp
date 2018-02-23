(in-package :cscrape)

(defun get-slots (object)
  ;; thanks to cl-prevalence
  #+openmcl
  (mapcar #'ccl:slot-definition-name
          (#-openmcl-native-threads ccl:class-instance-slots
                                    #+openmcl-native-threads ccl:class-slots
                                    (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defmethod print-object ((object tags:tag) stream)
  (format stream "{ ~s ~s}~%" (type-of object)
          (loop for name in (get-slots object)
                collect (cons name (slot-value object name)))))
