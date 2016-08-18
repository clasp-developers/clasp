;;;
;;; Any symbols we want to export from EXT must be done in init.lsp
;;;

(in-package :ext)
(defun compiled-function-name (x)
  (core:function-name x))

(defun compiled-function-file (x)
  (cond
    ((and x (functionp x))
     (multiple-value-bind (sfi pos lineno)
         (core:function-source-pos x)
       (let* ((source-file (core:source-file-info-source-debug-namestring sfi)))
         (when source-file
           (let* ((src-pathname (pathname source-file))
                  (src-directory (pathname-directory src-pathname))
                  (src-name (pathname-name src-pathname))
                  (src-type (pathname-type src-pathname))
                  (filepos (+ (core:source-file-info-source-debug-offset sfi) pos)))
             (let* ((pn (if (eq (car src-directory) :relative)
                            (merge-pathnames src-pathname (translate-logical-pathname "source-dir:"))
                            src-pathname)))
               (return-from compiled-function-file (values pn filepos lineno))))))))
    ((and x (fboundp x) (core:single-dispatch-generic-function-p (fdefinition x)))
     (error "use source-location for methods"))
    (t (values nil 0 0))))


(defstruct source-location
  pathname offset)

(defun source-location-impl (name kind)
  "* Arguments
- name : A symbol.
- kind : A symbol (:function :method :class)
Return the source-location for the name/kind pair"
  (labels ((fix-paths-and-make-source-locations (rels)
             (declare (core:lambda-name 'fix-paths-and-make-source-locations))
             (let ((source-dir (translate-logical-pathname #P"source-dir:")))
               (mapcar (lambda (dir-pos)
                         (let ((dir (first dir-pos))
                               (pos (second dir-pos)))
                           (make-source-location :pathname (merge-pathnames dir source-dir)
                                                 :offset pos)))
                       rels))))
    (cond
      (:class
       (let ((source-loc (list (core:get-sysprop name 'core:class-source-location))))
         (fix-paths-and-make-source-locations source-loc)))
      (:method
       (let ((source-loc (core:get-sysprop name 'core:cxx-method-source-location)))
         (fix-paths-and-make-source-locations source-loc)))
      (:function
       (if (fboundp name)
           (multiple-value-bind (file pos)
               (compiled-function-file (fdefinition name))
             (list (make-source-location :pathname file :offset pos)))
           (values nil))))))

(defparameter *source-location-kinds* '(:class :method :function))

(defun source-location (obj kind)
  "* Arguments
- obj : A symbol or object.
- kind : A symbol (:function :method :class t)
Return the source-location for the name/kind pair"
  (cond
    ((eq kind t)
     (cond
       ((clos:classp obj) (source-location-impl (class-name obj) :class))
       ((multiple-value-bind (file pos)
            (compiled-function-file obj)
          (list (make-source-location :pathname file :offset pos))))
       (t (error "Cannot obtain source-location for ~a" obj))))
    ((symbolp kind) (source-location-impl obj kind))
    (t (error "Cannot obtain source-location for ~a of kind ~a" obj kind))))
