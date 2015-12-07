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
         (let ((ns-ht (gethash current-namespace ns-hashes (make-hash-table :test #'equal))))
           (setf (gethash (tags:function-name tag) ns-ht) tag)
           (setf (gethash current-namespace ns-hashes) ns-ht)))
        (t #|nothing|#)))
    ns-hashes))



(defun generate-expose-function-signatures (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_SIGNATURES~%")
  (maphash (lambda (ns func-ht)
             (format sout "namespace ~a {~%" ns)
             (maphash (lambda (name f)
                        (declare (ignore name))
                        (format sout "    ~a;~%" (tags:signature-text f)))
                      func-ht)
             (format sout "};~%"))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_SIGNATURES~%"))


(defun split-c++-name (name)
  (declare (optimize (debug 3)))
  (let ((under (search "__" name :test #'string=)))
    (unless under
      (error "Could not translate ~a into a package:symbol-name pair" name))
    (let* ((name-pos (+ 2 under)))
      (values (subseq name 0 under)
              (subseq name name-pos)))))

(defun generate-expose-function-bindings (sout ns-grouped-expose-functions)
  (declare (optimize (debug 3)))
  (format sout "#ifdef EXPOSE_FUNCTION_BINDINGS~%")
  (maphash (lambda (ns funcs-ht)
             (maphash (lambda (name f)
                        (declare (ignore name))
                        (let* ((lambda-list-tag (tags:lambda-tag f))
                               (lambda-list-str (if lambda-list-tag
                                                    (tags:lambda-list lambda-list-tag)
                                                    nil)))
                          (multiple-value-bind (pkg name)
                              (split-c++-name (tags:function-name f))
                            (format sout "  expose_function(\"~a\",\"~a\",~a,&~a::~a,~s);~%"
                                    pkg name
                                    "true"
                                    ns
                                    (tags:function-name f)
                                    (if lambda-list-str
                                        (format nil "(~a)" lambda-list-str)
                                        "")))))
                      funcs-ht))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_BINDINGS~%"))


(defun extract-unique-source-info-tags (tags)
  (let ((unique-source-info (make-hash-table :test #'equal)))
    (dolist (tag tags)
      (cond
        ((typep tag 'tags:expose-function-tag)
         (setf (gethash (tags:function-name tag) unique-source-info) tag))
        (t #|nothing|#)))
    (let (source-info)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (push v source-info)) unique-source-info)
      source-info)))

(defun generate-expose-source-info (sout source-info-tags cppdefine)
  (format sout "#ifdef ~a~%" cppdefine)
  (dolist (f source-info-tags)
    (let* ((fn (tags:function-name f))
           (file (tags:file f))
           (line (tags:line f))
           (char-offset (tags:character-offset f))
           (docstring-tag (tags:docstring-tag f))
           (docstring (if docstring-tag
                          (tags:docstring docstring-tag)
                          "")))
      (multiple-value-bind (package-name symbol-name)
          (split-c++-name fn)
        (format sout "  { \"~a\", \"~a\", ~s, ~d, ~d, ~a }, ~%"
                package-name symbol-name file char-offset line docstring ))))
  (format sout "#endif // ~a~%" cppdefine))

(defun generate-tags-file (tags-file-name tags)
  (declare (optimize (debug 3)))
  (let* ((source-info-tags (extract-unique-source-info-tags tags))
         (file-ht (make-hash-table :test #'equal)))
    (dolist (tag source-info-tags)
      (push tag (gethash (tags:file tag) file-ht)))
    (let ((tags-data-ht (make-hash-table :test #'equal)))
      (maphash (lambda (file-name file-tags-list)
                 (let ((buffer (make-string-output-stream #+(or):element-type #+(or)'(unsigned-byte 8))))
                   (dolist (tag file-tags-list)
                     (format buffer "~a~a~a,~a~%"
                             (tags:function-name tag)
                             (code-char #x7f)
                             (tags:line tag)
                             (tags:character-offset tag)))
                   (setf (gethash file-name tags-data-ht) (get-output-stream-string buffer))))
               file-ht)
      (with-open-file (sout tags-file-name :direction :output #+(or):element-type #+(or)'(unsigned-byte 8)
                            :if-exists :supersede)
        (maphash (lambda (file buffer)
                   (format sout "~a,~a~%"
                           file
                           (length buffer))
                   (princ buffer sout))
                 tags-data-ht)))))


(defun generate-code-for-init-functions (tags)
  (declare (optimize (debug 3)))
  (with-output-to-string (sout)
    (let ((ns-grouped (group-expose-functions-by-namespace tags)))
      (generate-expose-function-signatures sout ns-grouped)
      (generate-expose-function-bindings sout ns-grouped))))

(defun generate-code-for-source-info (tags)
  (with-output-to-string (sout)
    (let ((source-info-tags (extract-unique-source-info-tags tags)))
      (generate-expose-source-info sout source-info-tags "SOURCE_INFO"))))

(defparameter *unique-symbols* nil)
(defparameter *symbols-by-package* nil)
(defun generate-code-for-symbols (tags)
  (declare (optimize (debug 3)))
  ;; Uniqify the symbols
  (let ((unique-symbols (make-hash-table :test #'equal)))
    (setq *unique-symbols* unique-symbols)
    (dolist (tag tags)
      (cond
        ((typep tag 'tags:symbol-tag)
         (setf (gethash (list (tags:package% tag) (tags:name% tag) (tags:name% tag)) unique-symbols) tag))
        ;; classes generate symbols
        ((typep tag 'tags:lisp-class-tag)
         (setf (gethash (list (tags:package% tag) (tags:class-symbol% tag) (tags:c++-class% tag)) unique-symbols)
               (make-instance 'tags:symbol-tag
                              :package% (tags:package% tag)
                              :exported% t
                              :c++-name% (tags:c++-class% tag)
                              :name% (tags:class-symbol% tag)
                              :namespace% (tags:namespace% tag))))))
    (with-output-to-string (sout)
      (let ((symbols-by-package (make-hash-table :test #'equal))
            (index 0))
        (setq *symbols-by-package* symbols-by-package)
        ;; Organize symbols by package
        (maphash (lambda (key tag)
                   (declare (ignore key))
                   (when (typep tag 'tags:symbol-tag)
                     (push tag (gethash (tags:package% tag) symbols-by-package))))
                 unique-symbols)
        (maphash (lambda (package package-tags)
                   (format sout "#ifdef ~a_SYMBOLS~%" package)
                   (dolist (tag package-tags)
                     (format sout "DO_SYMBOL(_sym_~a,~d,~a,\"~a\",~a);~%"
                             (tags:c++-name% tag)
                             index
                             (tags:package% tag)
                             (tags:name% tag)
                             (if (tags:exported% tag)
                                 "true"
                                 "false"))
                     (incf index))
                   (format sout "#endif // ~a_SYMBOLS~%" package))
                 symbols-by-package)))))

(defun write-if-changed (code main-path app-relative)
  (let ((pn (make-pathname :name (pathname-name app-relative)
                           :type (pathname-type app-relative)
                           :directory '(:relative "include" "generated")
                           :defaults (pathname main-path))))
    (let ((data-in-file (when (probe-file pn)
                          (with-open-file (stream pn :direction :input)
                            (let ((data (make-string (file-length stream))))
                              (read-sequence data stream)
                              data)))))
      (unless (string= data-in-file code)
        (with-open-file (stream pn :direction :output :if-exists :supersede)
          (write-sequence code stream))))))

(defun generate-code (tags main-path app-config)
  (let ((init-functions (generate-code-for-init-functions tags))
        (source-info (generate-code-for-source-info tags))
        (symbol-info (generate-code-for-symbols tags)))
    (write-if-changed init-functions main-path (gethash "INIT_FUNCTIONS_INC_H" app-config))
    (write-if-changed source-info main-path (gethash "SOURCE_INFO_INC_H" app-config))
    (write-if-changed symbol-info main-path (gethash "SYMBOLS_SCRAPED_INC_H" app-config))
    (generate-tags-file (merge-pathnames #P"TAGS" (translate-logical-pathname main-path)) tags)))
