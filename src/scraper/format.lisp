(in-package :cscrape)

(defgeneric packaged-name (namespace-tag tag packages)
  (:documentation "* Arguments
- tag :: A name-base-tag.
- packages :: A hash table that maps namespaces/package-identifiers to package names.
* Description
Return a string in the form <package>:<name>. If the name-base-tag has that form then return it, otherwise figure out the package name from the namespace and packages."))

(defmethod packaged-name (namespace-tag (tag tags:cl-name-tag) packages)
  (if (position #\: (tags:name% tag))
      (format nil "~s" (tags:name% tag))
      (format nil "\"~a:~a\""
              (gethash (tags:namespace% namespace-tag) packages)
              (tags:name% tag))))

(defmethod packaged-name (namespace-tag (tag tags:cl-pkg-name-tag) packages)
  (declare (ignore namespace-tag))
  (format nil "\"~a:~a\"" (gethash (tags:package% tag) packages) (tags:name% tag)))

(defmethod packaged-name (namespace-tag (tag tags:cl-lispify-name-tag) packages)
  (if (position #\: (tags:name% tag))
      (format nil "core::magic_name(~s)" (tags:name% tag))
      (format nil "core::magic_name(\"~a:~a\")"
              (gethash (tags:namespace% namespace-tag) packages)
              (tags:name% tag))))

(defmethod packaged-name (namespace-tag (name string) packages)
    (if (search "__" name)
        (format nil "core::magic_name(~s)" name)
        (format nil "core::magic_name(\"~a:~a\")"
                (gethash (tags:namespace% namespace-tag) packages)
                name)))

(defmethod packaged-name (namespace-tag (tag tags:internal-code-tag) packages)
  (let ((name (extract-function-name-from-signature (tags:signature-text% tag) tag)))
    (packaged-name namespace-tag name packages)))

(defmethod packaged-name (namespace-tag (tag tags:external-code-tag) packages)
  (let ((name (extract-function-name-from-pointer (tags:pointer% tag) tag)))
    (if (search "__" name)
        (format nil "core::magic_name(~s)" name)
        (format nil "core::magic_name(\"~a:~a\")"
                (gethash (tags:namespace% namespace-tag) packages)
                name))))

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
