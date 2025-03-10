;;;
;;; Any symbols we want to export from EXT must be done in init.lisp
;;;

(in-package :core)
;;; Temporary alias for SLIME compatibility- remove as soon as possible
(setf (fdefinition 'function-lambda-list) #'ext:function-lambda-list)
(export 'function-lambda-list)

(in-package :ext)

(defun compiled-function-name (x)
  (core:function-name x))

(defun compiled-function-file (xfunction)
  (if (and xfunction (functionp xfunction))
      (multiple-value-bind (src-pathname pos lineno)
          (core:function-source-pos xfunction)
        (when (or (null src-pathname)
                  (and (stringp src-pathname)
                       (string-equal src-pathname "-unknown-file-")))
          ;; e.g., a repl function - no source location.
          (return-from compiled-function-file (values nil 0 0)))
        ;; FIXME: This indicates an internal bookkeeping problem, i.e., a bug.
        (unless (typep src-pathname 'pathname)
          (error "The source-debug-pathname for ~a was ~a - it needs to be a pathname"
                 xfunction src-pathname))
        (let ((src-directory (pathname-directory src-pathname))
              (filepos pos))
          (let ((pn (if (eq (car src-directory) :relative)
                        (merge-pathnames src-pathname (translate-logical-pathname "sys:"))
                        src-pathname)))
            (values pn filepos lineno))))
      (progn
        (warn "compiled-function-file expected a function as argument - but it got ~a - there may not be any backtrace available" xfunction)
        (values nil 0 0))))

;;; This happens to be the first non-:TYPE defstruct during build.
;;; But FIXME: It's redundant to core:source-pos-info and should be vaporized.
(defstruct source-location
  pathname offset
  ;; A symbol like DEFVAR, DEFUN, etc for display.
  (definer nil :type symbol)
  ;; Any other metadata, mostly for user display. E.g. specializers of a method.
  (description nil :type list))

(defun source-locations-set-info (source-locations definer &optional description)
  (loop for sl in source-locations
        do (setf (source-location-definer sl) definer
                 (source-location-description sl) description))
  source-locations)

(defun function-source-locations (function)
  (multiple-value-bind (file pos)
      (compiled-function-file function)
    (if file
        (list (make-source-location :pathname file :offset pos :definer 'defun))
        nil)))

;; FIXME: Move this source debug stuff to an interface
;; (in SPI, probably)
(defun source-position-info->source-location (source-position-info definer)
  (let ((csi (core:file-scope
              (core:source-pos-info-file-handle source-position-info))))
    (make-source-location
     :pathname (core:file-scope-pathname csi)
     :offset (core:source-pos-info-filepos source-position-info)
     :definer definer)))

;;; Class source positions are just stored in a slot, or ones from C++ are
;;; stored as sysprops. We prefer the slot.
(defun class-source-location (class)
  (let ((csp (clos:class-source-position class)))
    (if csp
        (source-position-info->source-location csp 'defclass)
        (let ((cppinfo (core:get-sysprop (class-name class)
                                         'core:class-source-location)))
          (when cppinfo
            ;; This is a list (filename offset)
            (make-source-location
             :pathname (pathname (first cppinfo))
             :offset (second cppinfo)
             :definer 'defclass))))))

;;; Method combinations don't have source positions. In fact,
;;; they don't even exist as objects globally. The only global
;;; is the "compiler", which is an ordinary function that does
;;; the computation. find-method-combination always makes a fresh
;;; object.
;;; As such, we use the compiler's source info.
;;; Note that due to this, when provided a name, we don't go through
;;; this function- check source-location-impl.
(defun method-combination-source-location (method-combination)
  (source-locations-set-info
   (source-location (clos::method-combination-compiler method-combination) t)
   'define-method-combination))

(defun method-source-location (method)
  ;; NOTE: Because we return this to MAPCON, make sure the lists are fresh.
  (let* ((method-spi (clos::method-source-position method))
         (method-sls (if method-spi
                         (list
                          (source-position-info->source-location method-spi
                                                                 'defmethod))
                         nil))
         (sls (or method-sls
                  (source-location (clos::fast-method-function method) t)
                  (source-location (clos::contf-method-function method) t)
                  (source-location (clos:method-function method) t)))
         (description
           (ignore-errors
            (append (method-qualifiers method)
                    ;; FIXME: Move this into CLOS probably
                    (loop for spec in (clos:method-specializers method)
                          collect (if (typep spec 'clos:eql-specializer)
                                      `(eql ,(clos:eql-specializer-object spec))
                                      (class-name spec)))))))
    (source-locations-set-info sls 'defmethod description)))

(defun generic-function-source-locations (gf)
  (let ((method-sls (mapcan #'method-source-location (clos:generic-function-methods gf))))
    (multiple-value-bind (file pos)
        (compiled-function-file gf)
      (if file
          (list* (make-source-location :pathname file :offset pos :definer 'defgeneric)
                 method-sls)
          method-sls))))

(defun special-operator-source-locations (name)
  ;; cclasp - use a function defined later
  (let ((p (find-package "CLASP-CLEAVIR")))
    (when p
      (let ((s (find-symbol "SPECIAL-OPERATOR-SOURCE-LOCATIONS" p)))
        (when (and s (fboundp s))
          (funcall s name))))))

(defun source-location-impl (name kind)
  "* Arguments
- name : A symbol or (setf symbol)
- kind : A symbol (:function :method :class)
Return the source-location for the name/kind pair"
  (labels ((fix-paths-and-make-source-locations (rels)
             (let ((sys-dir (translate-logical-pathname #P"sys:")))
               (mapcar (lambda (dir-pos)
                         (let ((dir (first dir-pos))
                               (pos (second dir-pos)))
                           (make-source-location :pathname (merge-pathnames dir sys-dir)
                                                 :offset pos
                                                 ;; FIXME
                                                 :definer 'defmethod)))
                       rels)))
           (get-source-info-for-function-object (func)
             (cond ((core:single-dispatch-generic-function-p func)
                      (source-location name :method))
                     ((typep func 'generic-function)
                      (generic-function-source-locations func))
                     (t ; normal function
                      (function-source-locations func)))))
    (case kind
      (:class
       (when (symbolp name)
         (let ((class (find-class name nil)))
           (when class
             (let ((source-loc (class-source-location class)))
               (when source-loc (list source-loc)))))))
      (:method
          (let ((source-loc (core:get-sysprop name 'core:cxx-method-source-location)))
            (fix-paths-and-make-source-locations source-loc)))
      (:function
       (flet ((fun (name)
                (let ((old (sys::traced-old-definition name)))
                  (get-source-info-for-function-object
                   (or old
                       (fdefinition name))))))
         (when (fboundp name)
           (if (symbolp name)
               (cond ((special-operator-p name)
                      (special-operator-source-locations name))
                     ((macro-function name)
                      (source-locations-set-info
                       (get-source-info-for-function-object
                        (macro-function name))
                       'defmacro))
                     ((fdefinition name)
                      (fun name)))
               ;; name is (setf sym)
               (fun name)))))
      (:compiler-macro
       (when (fboundp name)
         (let ((cmf (compiler-macro-function name)))
           (when cmf
             (source-locations-set-info (source-location cmf t)
                                        'define-compiler-macro)))))
      (:setf-expander
       (let ((expander (ext:setf-expander name)))
         (when expander
           (source-locations-set-info (source-location expander t)
                                      'define-setf-expander))))
      (:method-combination
       ;; See comment on method-combination-source-position
       (let ((method-combination-compiler (clos::search-method-combination name)))
         (when method-combination-compiler
           (source-locations-set-info (source-location method-combination-compiler t)
                                      'define-method-combination))))
      (:type
       ;; We use the source location of the expander function.
       (let ((expander (ext:type-expander name)))
         (when expander
           (source-locations-set-info (source-location expander t)
                                      'deftype))))
      (:variable
       (when (symbolp name)
         (let ((spi (core::variable-source-info name))
               (definer (cond ((ext:specialp name) 'defvar)
                              ((constantp name) 'defconstant)
                              (t 'define-symbol-macro))))
           (when spi
             (list
              (source-position-info->source-location spi definer)))))))))

(defparameter *source-location-kinds* '(:class :method :function :compiler-macro
                                        :method-combination :type :setf-expander
                                        :variable))

(defun source-location (obj kind)
  "* Arguments
- obj : A symbol or object.
- kind : A symbol - either T or one of those listed in *source-location-kinds*
Return the source-location for the name/kind pair"
  (cond
    ((eq kind t)
     (cond
       ((clos:classp obj)
        (let ((source-loc (class-source-location obj)))
          (when source-loc (list source-loc))))
       ((core:single-dispatch-generic-function-p obj)
        (source-location (core:function-name obj) :method))
       ((typep obj 'generic-function)
        (generic-function-source-locations obj))
       ((functionp obj) (function-source-locations obj))
       ((typep obj 'clos:method-combination)
        (let ((source-loc (method-combination-source-location obj)))
          (when source-loc (list source-loc))))))
    ((symbolp kind) (source-location-impl obj kind))
    (t (error "Cannot obtain source-location for ~a of kind ~a" obj kind))))
