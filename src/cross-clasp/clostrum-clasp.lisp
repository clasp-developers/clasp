(in-package #:clasp-cleavir)

(defmethod clostrum-sys:parent (client (env clasp-global-environment))
  (declare (ignore client))
  nil)

(defmethod clostrum-sys:operator-status
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (cond ((and (symbolp op) (special-operator-p op))
         :special-operator)
        ((and (symbolp op) (macro-function op)) :macro)
        ((or (fboundp op) (cmp::known-function-p op)) :function)
        (t nil)))
;;; setf is uh... that's trickier

(defmethod clostrum-sys:operator-cell
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (core:function-cell op))
(defmethod clostrum-sys:ensure-operator-cell
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (core:ensure-function-cell op))

(defmethod clostrum-sys:operator-inline
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (global-inline-status op))
(defmethod (setf clostrum-sys:operator-inline)
    (new client (env clasp-global-environment) op)
  (declare (ignore client))
  (ecase new
    ((cl:inline)
     (setf (gethash op core::*functions-to-inline*) t)
     (remhash op core::*functions-to-notinline*))
    ((cl:notinline)
     (setf (gethash op core::*functions-to-notinline*) t)
     (remhash op core::*functions-to-inline*))
    ((nil)
     (remhash op core::*functions-to-inline*)
     (remhash op core::*functions-to-notinline*))))
(defmethod clostrum-sys:operator-inline-known-p
    (client (env clasp-global-environment) op)
  ;; no inheritance, so this is trivial
  (declare (ignore client op))
  t)
(defmethod clostrum-sys:operator-inline-data
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (inline-ast op))
(defmethod (setf clostrum-sys:operator-inline-data)
    (new client (env clasp-global-environment) op)
  (declare (ignore client))
  (setf (inline-ast op) new))

(defmethod clostrum-sys:compiler-macro-function
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (compiler-macro-function op))
(defmethod (setf clostrum-sys:compiler-macro-function)
    (new client (env clasp-global-environment) op)
  (declare (ignore client))
  (setf (compiler-macro-function op) new))

(defmethod clostrum-sys:setf-expander
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (ext::setf-expander op))
(defmethod (setf clostrum-sys:setf-expander)
    (new client (env clasp-global-environment) op)
  (declare (ignore client))
  (setf (ext::setf-expander op) new))

(defmethod clostrum-sys:operator-ftype
    (client (env clasp-global-environment) op)
  (declare (ignore client))
  (global-ftype op))
(defmethod (setf clostrum-sys:operator-ftype)
    (new client (env clasp-global-environment) op)
  (declare (ignore client))
  (setf (global-ftype op) new))

;;;

(defmethod clostrum-sys:variable-status
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (cond ((constantp var) :constant)
        ((ext:specialp var) :special)
        ((ext:symbol-macro var) :symbol-macro)
        (t nil)))

(defmethod clostrum-sys:variable-cell
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (core:variable-cell var))
(defmethod clostrum-sys:ensure-variable-cell
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (core:ensure-variable-cell var))

(defmethod clostrum-sys:variable-macro-expander
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (ext:symbol-macro var))
(defmethod (setf clostrum-sys:variable-macro-expander)
    (new client (env clasp-global-environment) var)
  (declare (ignore client))
  (setf (ext:symbol-macro var) new))

(defmethod clostrum-sys:variable-type
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (global-type var))
(defmethod (setf clostrum-sys:variable-type)
    (new client (env clasp-global-environment) var)
  (declare (ignore client))
  (setf (global-type var) new))

(defmethod clostrum-sys:symbol-plist
    (client (env clasp-global-environment) var)
  (declare (ignore client))
  (symbol-plist var))
(defmethod (setf clostrum-sys:symbol-plist)
    (new client (env clasp-global-environment) var)
  (declare (ignore client))
  (setf (symbol-plist var) new))
(defmethod clostrum-sys:symbol-plist-known-p
    (client (env clasp-global-environment) var)
  (declare (ignore client var))
  t) ; no inheritance

;;;

(in-package #:clostrum-trucler)

(defmethod trucler:describe-variable
    (client (environment clasp-cleavir:clasp-global-environment) name)
  (ecase (env:variable-status client environment name)
    ((nil) nil)
    ((:special)
     (make-instance 'trucler:global-special-variable-description
       :type (env:variable-type client environment name)
       :name name))
    ((:constant)
     (make-instance 'trucler:constant-variable-description
       :name name
       :value (env:symbol-value client environment name)))
    ((:symbol-macro)
     (make-instance 'trucler:global-symbol-macro-description
       :name name
       :type (env:variable-type client environment name)
       :expansion (env:macroexpand-1 client environment name)))))

(defmethod trucler:describe-function
    (client (environment clasp-cleavir:clasp-global-environment) name)
  (ecase (env:operator-status client environment name)
    ((nil) nil)
    ((:function)
     (make-instance 'trucler:global-function-description
       :name name
       :type (env:operator-ftype client environment name)
       :inline (env:operator-inline client environment name)
       :inline-data (env:operator-inline-data client environment name)
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:macro)
     (make-instance 'trucler:global-macro-description
       :name name
       :expander (env:macro-function client environment name)
       :inline (env:operator-inline client environment name)
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:special-operator)
     (make-instance 'trucler:special-operator-description
       :name name))))

(defmethod trucler:describe-optimize (client
                                      (environment clasp-cleavir:clasp-global-environment))
  ;; Assume it's a possibly not normalized list.
  (let ((optimize (env:optimize client environment)))
    (flet ((quality (quality)
             (cond ((member quality optimize) 3)
                   ((assoc quality optimize) (second (assoc quality optimize)))
                   ;; FIXME: No good default.
                   (t 3))))
      (make-instance 'trucler:optimize-description
        :speed (quality 'speed) :debug (quality 'debug) :space (quality 'debug)
        :safety (quality 'safety) :compilation-speed (quality 'compilation-speed)))))

(defmethod trucler:describe-block
    (client (environment clasp-cleavir:clasp-global-environment) name)
  (declare (ignore client name))
  nil)

(defmethod trucler:describe-tag
    (client (environment clasp-cleavir:clasp-global-environment) tag)
  (declare (ignore client tag))
  nil)

(defmethod trucler:global-environment
    (client (environment clasp-cleavir:clasp-global-environment))
  (declare (ignore client))
  environment)
