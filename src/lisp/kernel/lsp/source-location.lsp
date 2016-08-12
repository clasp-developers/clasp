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

(defun source-location (name kind)
  "* Arguments
- what : A symbol.
- kind : A symbol (:function :method :class :variable)
Return the source-location for the name/kind pair"
  (error "Return the source-location for the ~a named ~a" kind name))
#|   Use this in source-location
     ;; Return a list of source info with absolute paths
     (let ((rel-source-info (core:get-sysprop x 'core:cxx-method-source-info)))
       (mapcar (lambda (file-pos)
                 (let* ((file (car file-pos))
                        (character-offset (cadr file-pos))
                        (pn (pathname file)))
                   (if (eq (car (pathname-directory pn)) :relative)
                       (list (merge-pathnames pn (translate-logical-pathname "source-dir:")) character-offset)
                       (list pn character-offset))))
               rel-source-info)))
|#     

