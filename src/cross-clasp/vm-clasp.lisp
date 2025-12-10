(in-package #:core)

(defgeneric fcge-ensure-fcell (environment name))
(defgeneric fcge-ensure-vcell (environment name))

(defmethod fcge-ensure-fcell (env name)
  (clostrum:ensure-operator-cell maclina.machine:*client* env name))
(defmethod fcge-ensure-vcell (env name)
  (clostrum:ensure-variable-cell maclina.machine:*client* env name))

(defgeneric fcge-lookup-fun (env name))
(defgeneric fcge-lookup-var (env name))

(defmethod fcge-lookup-fun (env name)
  (ecase (clostrum:operator-status maclina.machine:*client*
                                   env name)
    ((nil) nil)
    ((:function) (cmp:global-fun-info/make
                  (clostrum:compiler-macro-function
                   maclina.machine:*client* env name)))
    ((:macro) (cmp:global-macro-info/make
               (clostrum:macro-function
                maclina.machine:*client* env name)))
    ;; should have been picked off by bytecompile's normal processing
    ;; but we use this elsewere, e.g. cl:macro-function
    ;; We probably ought to have some kind of special-operator-info,
    ;; but failing that, here's a KLUDGE.
    ((:special-operator) nil)))

(defmethod fcge-lookup-var (env name)
  (ecase (clostrum:variable-status maclina.machine:*client*
                                   env name)
    ((nil) nil)
    ((:special) (cmp:special-var-info/make t))
    ((:constant)
     (cmp:constant-var-info/make
      (clostrum:symbol-value maclina.machine:*client* env name)))
    ((:symbol-macro)
     (cmp:symbol-macro-var-info/make
      (clostrum:variable-macro-expander maclina.machine:*client*
                                        env name)))))

;;; These methods are not actually necessary since the runtime treats NIL
;;; environments specially, but they're here for completeness.
(defmethod fcge-ensure-fcell ((env null) name)
  (ensure-function-cell name))
(defmethod fcge-ensure-vcell ((env null) name)
  (ensure-variable-cell name))

(defgeneric fcge-find-package (env name))

(defmethod fcge-find-package (env name)
  (clostrum:find-package maclina.machine:*client* env name))

(defmethod fcge-find-package ((env null) name) (find-package name))

(defpackage #:vm-clasp
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:mc #:maclina.compile))
  (:export #:client))

(in-package #:vm-clasp)

(defclass client () ())

(defmethod m:make-module ((client client) bytecode)
  (core:bytecode-module/make bytecode))

(defmethod m:literals ((module core:bytecode-module))
  (core:bytecode-module/literals module))
(defmethod (setf m:literals)
    (literals (module core:bytecode-module))
  (core:bytecode-module/setf-literals module literals)
  literals)

(defmethod m:pc-map ((module core:bytecode-module))
  (core:bytecode-module/debug-info module))
(defmethod (setf m:pc-map)
    (map (module core:bytecode-module))
  (core:bytecode-module/setf-debug-info module map)
  map)

(defmethod m:make-function
    ((client client) module nlocals nenv entry size)
  (core:bytecode-simple-fun/make
   (core:function-description/make)
   module nlocals nenv entry size
   (cmp:compile-trampoline nil)))

(defmethod m:locals-frame-size ((fun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/locals-frame-size fun))
(defmethod m:environment-size ((fun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/environment-size fun))
(defmethod m:entry-pc ((fun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/entry-pc-n fun))
(defmethod m:size ((fun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/bytecode-size fun))

(defmethod m:name ((fun core:bytecode-simple-fun))
  (core:function-name fun))
(defmethod (setf m:name) (name (fun core:bytecode-simple-fun))
  (core:function/setf-function-name fun name)
  name)

(defmethod m:link-function ((client client) env fname)
  (clostrum:ensure-operator-cell client env fname))
(defmethod m:link-variable ((client client) env vname)
  (clostrum:ensure-variable-cell client env vname))

(defmethod mc:load-map-info ((client client) (info m:source-info))
  ;; Nnnnnot sure the source locations are compatible. FIXME?
  (core:bytecode-debug-location/make (m:start info) (m:end info)
                                     (m:source info)))
(defmethod mc:load-map-info ((client client) (info m:declarations-info))
  (core:bytecode-ast-decls/make (m:start info) (m:end info)
                                (m:declarations info)))
(defmethod mc:load-map-info ((client client) (info m:the-info))
  (core:bytecode-ast-the/make (m:start info) (m:end info)
                              (m:the-type info) (m:receiving info)))
(defmethod mc:load-map-info ((client client) (info m:if-info))
  (core:bytecode-ast-if/make (m:start info) (m:end info)
                             (m:receiving info)))
(defmethod mc:load-map-info ((client client) (info m:tagbody-info))
  (core:bytecode-ast-tagbody/make (m:start info) (m:end info)
                                  (m:tags info)))
(defmethod mc:load-map-info ((client client) (info m:vars-info))
  (core:bytecode-debug-vars/make
   (m:start info) (m:end info)
   (loop for var in (m:bindings info)
         collect (core:bytecode-debug-var/make
                  (m:name var) (m:index var) (m:cellp var)
                  (m:declarations var)))))

(defmethod clostrum-basic:make-variable-cell ((client client) env name)
  (declare (ignore client env))
  (core:variable-cell/make name))
(defmethod clostrum-basic:make-operator-cell ((client client) env name)
  (declare (ignore client env))
  (core:function-cell/make name))

(defmethod clostrum-sys:variable-cell-value (client (cell core:variable-cell))
  (declare (ignore client))
  (core:variable-cell/value-unsafe cell))
(defmethod (setf clostrum-sys:variable-cell-value)
    (new client (cell core:variable-cell))
  (declare (ignore client))
  (setf (core:variable-cell/value cell) new))
(defmethod clostrum-sys:variable-cell-boundp (client (cell core:variable-cell))
  (declare (ignore client))
  (core:variable-cell/boundp cell))
(defmethod clostrum-sys:variable-cell-makunbound (client (cell core:variable-cell))
  (declare (ignore client))
  (core:variable-cell/makunbound cell))

(defmethod clostrum-sys:operator-cell-value (client (cell core:function-cell))
  (declare (ignore client))
  (core:function-cell/function cell))
(defmethod (setf clostrum-sys:operator-cell-value)
    (new client (cell core:function-cell))
  (declare (ignore client))
  (let ((new (if (eql new t) ; special operator
                 (lambda (&rest args)
                   (declare (ignore args))
                   (error 'core::do-not-funcall-special-operator
                          :name (core:function-name cell)
                          :operator (core:function-name cell)))
                 new)))
    (setf (core:function-cell/function cell) new))
  new)
(defmethod clostrum-sys:operator-cell-boundp (client (cell core:function-cell))
  (declare (ignore client))
  (core:function-cell/boundp cell))
(defmethod clostrum-sys:operator-cell-makunbound (client (cell core:function-cell))
  (declare (ignore client))
  (core:function-cell/makunbound cell (core:function-name cell)))

(defmethod m:symbol-value ((client client) env symbol)
  (core:variable-cell/value (clostrum:ensure-variable-cell client env symbol)))
(defmethod (setf m:symbol-value) (new (client client) env symbol)
  (setf (core:variable-cell/value
         (clostrum:ensure-variable-cell client env symbol))
        new))
(defmethod m:boundp ((client client) env symbol)
  ;; FIXME: Parents?
  (let ((cell (clostrum-sys:variable-cell client env symbol)))
    (and cell (core:variable-cell/boundp cell))))
(defmethod m:makunbound ((client client) env symbol)
  (let ((cell (clostrum-sys:variable-cell client env symbol)))
    (when cell
      (core:variable-cell/makunbound cell))))

(defmethod m:call-with-progv ((client client) env symbols values thunk)
  (core:progv-env-function symbols values env thunk))

(defmethod m:fboundp ((client client) env name)
  (clostrum:fboundp client env name))
(defmethod m:fdefinition ((client client) env name)
  (clostrum:fdefinition client env name))
(defmethod (setf m:fdefinition) (new (client client) env name)
  (setf (clostrum:fdefinition client env name) new))
(defmethod m:fmakunbound ((client client) env name)
  (clostrum:fmakunbound client env name))

(defmethod m:multiple-values-limit ((client client))
  multiple-values-limit)
