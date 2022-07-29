(in-package :cscrape)

(defclass function-method-name ()
  ((source% :initform nil :initarg :source :accessor source%)
   (package% :initform nil :initarg :package :accessor package%)
   (class% :initform nil :initarg :class :accessor class%)
   (name% :initform nil :initarg :name :accessor name%)))

(defun make-function-method-name (&key source package class name)
  (let ((slash (position #\/ name)))
    (when (and slash
               (< 0 slash (1- (length name)))
               (string/= name "STRING/=")
               (string/= name "CHAR/="))
      (error "The name ~s contains a slash" name))
    (make-instance 'function-method-name
                   :source source
                   :package package
                   :class class
                   :name name)))

(defmethod print-object ((obj function-method-name) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ":source ~a :package ~s :class ~s :name ~s"
            (source% obj)
            (package% obj)
            (class% obj)
            (name% obj))))

(defun function-method-name-key (obj)
  (cond
    ((class% obj) (format nil "~a:~a/~a" (package% obj) (class% obj) (name% obj)))
    (t (format nil "~a:~a" (package% obj) (name% obj)))))

(defun function-method-name-copy (obj)
  (make-function-method-name
                 :source :function-method-name-copy
                 :package (package% obj)
                 :class (class% obj)
                 :name (name% obj)))

(defun maybe-split-class-method-name (class-name)
  (let ((slash (position #\/ class-name)))
    (if (and slash
             (not (= slash (1- (length class-name)))))
        (let ((class (subseq class-name 0 slash))
              (name (subseq class-name (1+ slash))))
          (values class name))
        (values nil class-name))))

(defgeneric packaged-name (namespace-tag tag packages)
  (:documentation "* Arguments
- tag :: A name-base-tag.
- packages :: A hash table that maps namespaces/package-identifiers to package names.
* Description
Return a string in the form <package>:<name>. If the name-base-tag has that form then return it, otherwise figure out the package name from the namespace and packages."))

(defmethod packaged-name (namespace-tag (tag tags:cl-name-tag) packages)
  (declare (optimize (debug 3)))
  (let ((colon (position #\: (tags:name% tag))))
    (if colon
        (let ((pkg-part (subseq (tags:name% tag) 0 colon))
              (name-part (subseq (tags:name% tag) (1+ colon))))
          (multiple-value-bind (class nm)
              (maybe-split-class-method-name name-part)
            (make-function-method-name :source :packaged-name1
                                       :package pkg-part
                                       :class class
                                       :name nm)))
        (let ((pkg (gethash (tags:namespace% namespace-tag) packages)))
          (unless pkg
            (let ((pkg-key-vals (let (pkvs)
                                  (maphash (lambda (k v)
                                             (push (cons k v) pkvs))
                                           packages)
                                  pkvs)))
              (error "CL_NAME ~s missing package tried to use namespace ~s but couldn't find package in ~s"
                     (tags:name% tag)
                     (tags:namespace% namespace-tag)
                     pkg-key-vals)))
          (make-function-method-name
                         :source :packaged-name2
                         :package pkg
                         :name (tags:name% tag))))))

(defmethod packaged-name (namespace-tag (tag tags:cl-pkg-name-tag) packages)
  (declare (optimize (debug 3)))
  (declare (ignore namespace-tag))
  (make-function-method-name
                 :source :packaged-name3
                 :package (gethash (tags:package% tag) packages)
                 :name (lispify-symbol-name (tags:name% tag))))

(defmethod packaged-name (namespace-tag (tag tags:cl-lispify-name-tag) packages)
  (declare (optimize (debug 3)))
  (let ((colon (position #\: (tags:name% tag))))
    (if colon
        (let* ((pkg-part (subseq (tags:name% tag) 0 colon))
               (name-part (subseq (tags:name% tag) (1+ colon)))
               (pkg (gethash pkg-part packages))
               (name (lispify-symbol-name name-part)))
          (unless pkg
            (let ((pkg-key-vals (let (pkvs)
                                  (maphash (lambda (k v)
                                             (push (cons k v) pkvs))
                                           packages)
                                  pkvs)))
              (error "Could not identify package from package part ~s of CL_LISPIFY_NAME with ~s : package dict: ~s" pkg-part (tags:name% tag) pkg-key-vals)))
          (make-function-method-name
                         :source :packaged-name4
                         :package pkg
                         :name name))
        (let* ((slash (position #\/ (tags:name% tag)))
               (class (and slash
                           (lispify-symbol-name (subseq (tags:name% tag) 0 slash))))
               (name (lispify-symbol-name (if slash
                                              (subseq (tags:name% tag) (1+ slash))
                                              (tags:name% tag))))
               (pkg-name (tags:namespace% namespace-tag))
               (pkg (gethash pkg-name packages)))
          (unless pkg
            (let ((pkg-key-vals (let (pkvs)
                                  (maphash (lambda (k v)
                                             (push (cons k v) pkvs))
                                           packages)
                                  pkvs)))
              (error "Could not identify package from namespace ~s CL_LISPIFY_NAME with ~s : package dict: ~s" pkg-name (tags:name% tag) pkg-key-vals)))
          (make-function-method-name
                         :source :packaged-name5
                         :package pkg
                         :class class
                         :name name)))))

(defmethod packaged-name (namespace-tag (name string) packages)
  (declare (optimize (debug 3)))
  (if (search "__" name)
      (make-function-method-name
                     :source :packaged-name6
                     :package (lispify-symbol-name (subseq name 0 (search "__" name)))
                     :name (lispify-symbol-name (subseq name (+ 2 (search "__" name)))))
      (make-function-method-name
                     :source :packaged-name7
                     :package (lispify-symbol-name (gethash (tags:namespace% namespace-tag) packages))
                     :name (lispify-symbol-name name))))

(defmethod packaged-name (namespace-tag (tag tags:internal-code-tag) packages)
  (declare (optimize (debug 3)))
  (let ((name (extract-function-name-from-signature (tags:signature-text% tag) tag)))
    (packaged-name namespace-tag name packages)))

(defmethod packaged-name (namespace-tag (tag tags:external-code-tag) packages)
  (declare (optimize (debug 3)))
  (let ((name (extract-function-name-from-pointer (tags:pointer% tag) tag)))
    (if (search "__" name)
        (make-function-method-name
                       :source :packaged-name8
                       :package (subseq name 0 (search "__" name))
                       :name (subseq name (+ 2 (search "__" name))))
        (make-function-method-name
                       :source :packaged-name9
                       :package (gethash (tags:namespace% namespace-tag) packages)
                       :name name))))

(defgeneric packaged-class-name (namespace-tag name-or-tag packages)
  (:documentation "* Arguments
- namespace-tag :: A namespace-tag.
- name-or-tag :: A class name or tag.
- packages :: A hash table that maps namespaces/package-identifiers to package names.
* Description
Return a string in the form <package>:<name>. Figure out the package name from the namespace and packages."))

(defmethod packaged-class-name (namespace-tag (class-name string) packages)
  (let ((cc-pos (search "::" class-name)))
    (if cc-pos
        (let* ((namespace-part (subseq class-name 0 cc-pos))
               (class-name-part (subseq class-name (+ 2 cc-pos)))
               (package-part (gethash namespace-part packages)))
          (format nil "~a:~a" package-part class-name-part))
        (let ((package (gethash (tags:namespace% namespace-tag) packages)))
          (format nil "~a:~a" package class-name)))))

(defgeneric make-class-key (cur-namespace-tag tag)
  (:method (namespace-tag (class-name string))
    (let ((cc-pos (search "::" class-name)))
      (if cc-pos
          class-name
          (let ((ns (tags:namespace% namespace-tag)))
            (format nil "~a::~a" ns class-name))))))

(defun maybe-namespace-symbol (namespace-tag symbol-name)
  (if (search "::" symbol-name)
      symbol-name
      (format nil "~a::~a" (tags:namespace% namespace-tag) symbol-name)))

(defun maybe-namespace-enum (namespace-tag enum-name)
  (if (search "::" enum-name)
      enum-name
      (format nil "~a::~a" (tags:namespace% namespace-tag) enum-name)))

(defun maybe-namespace-type (namespace-tag type-name)
  (if (search "::" type-name)
      type-name
      (format nil "~a::~a" (tags:namespace% namespace-tag) type-name)))
