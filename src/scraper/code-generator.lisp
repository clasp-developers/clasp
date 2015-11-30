(in-package :cscrape)

(defun group-expose-functions-by-namespace (tags)
  (let ((ns-hashes (make-hash-table :test #'equal))
        current-namespace)
    (dolist (tag tags)
      (cond
        ((typep tag 'tags:namespace-tag)
         (setq current-namespace (tags:namespace tag)))
        ((typep tag 'tags:expose-function-tag)
         (or current-namespace (error "There must be a current namespace defined"))
         (let ((ns-list (gethash current-namespace ns-hashes)))
           (push tag ns-list)
           (setf (gethash current-namespace ns-hashes) ns-list)))
        (t #|nothing|#)))
    ns-hashes))



(defun generate-expose-function-signatures (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_SIGNATURES~%")
  (maphash (lambda (ns funcs)
             (format sout "namespace ~a {~%" ns)
             (dolist (f funcs)
               (format sout "    ~a;~%" (tags:signature-text f)))
             (format sout "};~%"))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_SIGNATURES~%"))


(defun split-c++-name (name)
  (let* ((under (position #\_ name))
         (internal (eq (elt name (1+ under)) #\_))
         (name-pos (if internal
                       (1+ (1+ under))
                       (1+ under))))
    (values (string-upcase (subseq name 0 under))
            (subseq name name-pos)
            (not internal))))

(defun generate-expose-function-bindings (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_BINDINGS~%")
  (maphash (lambda (ns funcs)
             (dolist (f funcs)
               (let* ((lambda-list-tag (tags:lambda-tag f))
                      (lambda-list-str (if lambda-list-tag
                                           (tags:lambda-list lambda-list-tag)
                                           "")))
                 (multiple-value-bind (pkg name extern)
                     (split-c++-name (tags:function-name f))
                   (format sout "  expose_function(\"~a\",\"~a\",~a,&~a::~a,\"(~a)\");~%"
                           pkg name
                           (if extern "true" "false")
                           ns (tags:function-name f) lambda-list-str)))))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_BINDINGS~%"))


(defun extract-source-info-tags (tags)
  (let ((source-info nil))
    (dolist (tag tags)
      (cond
        ((typep tag 'tags:expose-function-tag)
         (push tag source-info))
        (t #|nothing|#)))
    source-info))

(defun generate-expose-source-info (sout source-info-tags cppdefine)
  (format sout "#ifdef ~a~%" cppdefine)
  (dolist (f source-info-tags)
    (let* ((fn (tags:function-name f))
           (file (tags:file f))
           (char-offset (tags:character-offset f))
           (docstring-tag (tags:docstring-tag f))
           (docstring (if docstring-tag
                          (tags:docstring docstring-tag)
                          "")))
      (multiple-value-bind (package-name symbol-name)
          (split-c++-name fn)
        (format sout "  { \"~a\", \"~a\", ~s, ~d, ~a }, ~%"
                package-name symbol-name file char-offset docstring ))))
  (format sout "#endif // ~a~%" cppdefine))

(defun generate-code-for-init-functions (tags)
  (with-output-to-string (sout)
    (let ((ns-grouped (group-expose-functions-by-namespace tags)))
      (generate-expose-function-signatures sout ns-grouped)
      (generate-expose-function-bindings sout ns-grouped))))

(defun generate-code-for-source-info (tags)
  (with-output-to-string (sout)
    (let ((source-info-tags (extract-source-info-tags tags)))
      (generate-expose-source-info sout source-info-tags "SOURCE_INFO"))))


(defun write-if-changed (code app-root app-relative)
  (let ((pn (merge-pathnames (pathname app-relative) (pathname app-root))))
    (let ((data-in-file (when (probe-file pn)
                          (with-open-file (stream pn :direction :input)
                            (let ((data (make-string (file-length stream))))
                              (read-sequence data stream)
                              data)))))
      (unless (string= data-in-file code)
        (with-open-file (stream pn :direction :output :if-exists :supersede)
          (write-sequence code stream))))))

(defun generate-code (tags app-root app-config)
  (let ((init-functions (generate-code-for-init-functions tags))
        (source-info (generate-code-for-source-info tags)))
    (write-if-changed init-functions app-root (gethash "INIT_FUNCTIONS_INC_H" app-config))
    (write-if-changed source-info app-root (gethash "SOURCE_INFO_INC_H" app-config))))
