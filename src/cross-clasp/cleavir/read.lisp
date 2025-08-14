(in-package #:cross-clasp.cleavir)

(defclass reader-client (cmp:clasp-cst-client cross::reader-client)
  ())

(defvar *reader-client* (make-instance 'reader-client))

;;; Need a method here since we're not using maclina.compile-file's *environment*.
(defmethod eclector.reader:evaluate-expression ((client reader-client) expr)
  (maclina.compile:eval expr cross::*build-ce*))

;;; Both clasp and maclina define methods for these that are slightly incorrect
;;; for us: We want to use cl:*readtable*, but within the cross environment.
(defmethod eclector.reader:state-value ((client reader-client) aspect)
  (m:symbol-value m:*client* cross::*build-rte* aspect))
(defmethod eclector.reader:call-with-state-value
    ((client reader-client) thunk aspect value)
  (m:progv m:*client* cross::*build-rte* (list aspect) (list value)
    (funcall thunk)))

;;; Everything else should be okay, knock on wood, keeping in mind that we keep
;;; Clasp's client in the CPL before cross-clasp's.
