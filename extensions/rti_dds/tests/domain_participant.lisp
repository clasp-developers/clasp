
(defpackage "RTI-DDS.TEST"
  (:use "COMMON-LISP"))

(in-package "RTI-DDS.TEST")

(defun dbgmsg (fmt &rest args)
  (format *debug-io* "~&*** ")
  (apply 'format *debug-io* fmt args)
  (format *debug-io* "~&")
  (finish-output *debug-io*))

(defconstant +default-domain-id+ 42)
(defparameter *default-domain-id* #.+default-domain-id+)

(defconstant +default-domain-participant-name+ "clasp")
(defparameter *default-domain-participant-name* #.+default-domain-participant-name+)

(defparameter *dpf* nil)

(defun ensure-*dpf* ()
  (unless *dpf*
    (setq *dpf* (dds::domain-participant-factory-get-instance)))
  *dpf*)

(defun create-domain-participant (&key
                                    (domain-id *default-domain-id*)
                                    (dpf (ensure-*dpf*))
                                    (name *default-domain-participant-name*))
  (dbgmsg "Input parameters: Domain Participant Factory: ~S, Domain ID: ~S, Domain Participant Name: ~S." dpf domain-id name)
  (let* ((dp (dds::create-participant dpf domain-id nil nil dds:+dds-status-mask-all+))
         (qos (dds:get-qos dpf)))
    (dbgmsg "Domain Participant: ~S" p)
    (dbgmsg "DP QoS: ~S" qos)
    dp ))

(defun dpf-get-default-library (&optional (dpf *dpf*))
  (dds::get-default-library dpf))

(defun test ()
  (let ((dp (create-domain-participant)))
    dp))
