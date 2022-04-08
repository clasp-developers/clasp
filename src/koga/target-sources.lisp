(in-package #:koga)

;; Sources that are added to iclasp also need to be installed and scanned for tags.
(defmethod add-target-source :after (configuration (target (eql :iclasp)) source)
  (when (eq :code (source-root source))
    (add-target-source configuration :install-code source)
    (add-target-source configuration :etags source))
  (add-target-source configuration :dclasp source))

;; Sources that are added to aclasp also need to be installed and scanned for tags.
(defmethod add-target-source :after (configuration (target (eql :aclasp)) source)
  (when (eq :code (source-root source))
    (add-target-source configuration :install-code source)
    (add-target-source configuration :etags source)))

;; Sources that are added to bclasp also need to be installed and scanned for tags.
(defmethod add-target-source :after (configuration (target (eql :bclasp)) source)
  (when (eq :code (source-root source))
    (add-target-source configuration :install-code source)
    (add-target-source configuration :etags source)))

;; Sources that are added to cclasp also need to be installed and scanned for tags.
(defmethod add-target-source :after (configuration (target (eql :cclasp)) source)
  (when (eq :code (source-root source))
    (add-target-source configuration :install-code source)
    (add-target-source configuration :etags source)))

;; For directories that are to be installed skip hidden files and waf files.
(defun add-target-directory (configuration target source)
  (loop with root = (merge-pathnames (resolve-source-root source)
                                     (uiop:getcwd))
        for path in (directory (merge-pathnames #P"**/*.*"
                                                (merge-pathnames (resolve-source source)
                                                                 (uiop:getcwd))))
        for rel-path = (uiop:subpathp (truename path) root)
        unless (or (uiop:absolute-pathname-p rel-path)
                   (uiop:directory-pathname-p rel-path)
                   (equal "wscript" (file-namestring rel-path))
                   (hidden-component-p (pathname-name rel-path))
                   (some #'hidden-component-p (pathname-directory rel-path)))
          do (add-target-source configuration target (make-source rel-path (source-root source)))))

(defmethod add-target-source (configuration (target (eql :install-code)) (source directory-source))
  (add-target-directory configuration target source))

(defmethod add-target-source (configuration (target (eql :etags)) (source directory-source))
  (add-target-directory configuration target source))

