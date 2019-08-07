(in-package :cscrape)

;;; ------------------------------------------------------------
;;;
;;; Interpret the tags and generate objects used to generate code
;;;
;;;

(define-condition bad-cl-defun/defmethod ()
  ((tag :initarg :tag :accessor tag)
   (other-tag :initarg :other-tag :accessor other-tag))
  (:report (lambda (condition stream)
             (format stream "Error at ~a interpreting tag ~a ~a the tag ~a at ~a is too far away"
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))
                     (tags:tag-code (other-tag condition))
                     (tags:source-pos (other-tag condition)))))
  (:documentation "Error when CL_DEFUN, CL_DEFMETHOD are too far away from their modifiers are too far away"))

(defclass expose-code ()
  ((file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (character-offset% :initarg :character-offset% :accessor character-offset%)
   (namespace% :initform nil :initarg :namespace% :accessor namespace%)
   (lisp-name% :initform nil :initarg :lisp-name% :accessor lisp-name%)
   (lambda-list% :initform nil :initarg :lambda-list% :accessor lambda-list%)
   (declare% :initform nil :initarg :declare% :accessor declare%)
   (docstring% :initform nil :initarg :docstring% :accessor docstring%)
   (priority% :initform tags::*default-priority* :initarg :priority% :accessor priority%)))

(defclass package-to-create ()
  ((name% :initarg :name% :accessor name%)
   (file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (character-offset% :initarg :character-offset% :accessor character-offset%)
   (packages-to-use% :initarg :packages-to-use% :accessor packages-to-use%)
   (shadow% :initarg :shadow% :accessor shadow%)
   (nicknames% :initarg :nicknames% :accessor nicknames%)))

(defclass function-mixin ()
  ((function-name% :initform nil :initarg :function-name% :accessor function-name%)))

(defclass expose-initializer (expose-code function-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)))

(defclass expose-pregcstartup (expose-code function-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)))

(defclass expose-defun (expose-code function-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)
   (provide-declaration% :initform t :initarg :provide-declaration% :accessor provide-declaration%)))

(defclass expose-defun-setf (expose-code function-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)
   (provide-declaration% :initform t :initarg :provide-declaration% :accessor provide-declaration%)))

(defclass expose-extern-defun (expose-code function-mixin)
  ((pointer% :initform nil :initarg :pointer% :accessor pointer%)
   (function-ptr% :initform nil :initarg :function-ptr% :accessor function-ptr%)))

(defclass method-mixin ()
  ((class% :initform nil :initarg :class% :accessor class%)
   (method-name% :initform nil :initarg :method-name% :accessor method-name%)))

(defclass expose-defmethod (expose-code method-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)))

(defclass expose-extern-defmethod (expose-code method-mixin)
  ((pointer% :initform nil :initarg :pointer% :accessor pointer%)))

(defclass class-method-mixin ()
  ((class% :initform nil :initarg :class% :accessor class%)
   (method-name% :initform nil :initarg :method-name% :accessor method-name%)))

(defclass expose-def-class-method (expose-code class-method-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)))

(defclass gc-managed-type ()
  ((file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (c++type% :initarg :c++type% :accessor c++type%)
   (stamp% :initarg :stamp% :accessor stamp%)))

(defclass exposed-class ()
  ((file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (character-offset% :initarg :character-offset% :accessor character-offset%)
   (meta-class% :initarg :meta-class% :accessor meta-class%)
   (docstring% :initarg :docstring% :accessor docstring%)
   (package% :initarg :package% :accessor package%)
   (class-tag% :initarg :class-tag% :accessor class-tag%)
   (class-key% :initarg :class-key% :accessor class-key%)
   (base% :initarg :base% :accessor base%)
   (lisp-name% :initarg :lisp-name% :accessor lisp-name%)
   (methods% :initform nil :initarg :methods% :accessor methods%)
   (class-methods% :initform nil :initarg :class-methods% :accessor class-methods%)
   (direct-subclasses% :initform nil :accessor direct-subclasses%)
   (stamp% :initform nil :accessor stamp%)
   (flags% :initform nil :accessor flags%)   ))

(defclass exposed-internal-class (exposed-class) ())
(defclass exposed-external-class (exposed-class) ())

(defclass expose-symbol ()
  ((lisp-name% :initarg :lisp-name% :accessor lisp-name%)
   (c++-name% :initarg :c++-name% :accessor c++-name%)
   (namespace% :initarg :namespace% :accessor namespace%)
   (package% :initarg :package% :accessor package%)
   (package-str% :initarg :package-str% :accessor package-str%)
   (exported% :initform t :initarg :exported% :accessor exported%)
   (shadow% :initform nil :initarg :shadow% :accessor shadow%)))

(defclass expose-external-symbol (expose-symbol) ())
(defclass expose-shadow-external-symbol (expose-external-symbol) ())
(defclass expose-internal-symbol (expose-symbol) ())
(defclass expose-intern-symbol (expose-symbol) ())

(defclass begin-enum ()
  ((file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (character-offset% :initarg :character-offset% :accessor character-offset%)
   (type% :initarg :type% :accessor type%)
   (symbol% :initarg :symbol% :accessor symbol%)
   (description% :initarg :description% :accessor description%)))

(defclass value-enum ()
  ((file% :initarg :file% :accessor file%)
   (line% :initarg :line% :accessor line%)
   (character-offset% :initarg :character-offset% :accessor character-offset%)
   (symbol% :initarg :symbol% :accessor symbol%)
   (value% :initarg :value% :accessor value%)))

(defclass completed-enum ()
  ((begin-enum% :initarg :begin-enum% :accessor begin-enum%)
   (values% :initarg :values% :accessor values%)))

(defun lispify-class-name (tag packages)
  (format nil "core::magic_name(\"~a:~a\")" (gethash (tags:package% tag) packages) (tags:class-symbol% tag)))

(defun maybe-override-name (namespace-tag override-name-tag name packages)
  "* Arguments
- override-name-tag :: A cl-name-tag.
- name :: A string
- packages :: A map of namespace and C++ package names to package name strings.
* Description
If override-name-tag is not nil then return its value, otherwise return name"
  (if (null override-name-tag)
      name
      (etypecase override-name-tag
        (tags:cl-lispify-name-tag
         (packaged-name namespace-tag override-name-tag packages))
        (tags:cl-name-tag
         (packaged-name namespace-tag override-name-tag packages))
        (tags:cl-pkg-name-tag
         (format nil "core::magic_name(~s,~s)"
                 (tags:name% override-name-tag)
                 (gethash (tags:package% override-name-tag) packages))))))

(defun order-packages-by-use (packages)
  "* Arguments
- packages :: A list.
* Description
Sort the packages in order of how they use each other."
  (declare (optimize debug))
  (let ((unsorted (make-hash-table :test #'eq))
        sorted)
    (dolist (p packages)
      (setf (gethash p unsorted) t))
    (loop
       (block restart-top
         (maphash (lambda (p dummy)
                    (declare (ignore dummy))
                    (let ((missing-used nil))
                      (dolist (u (packages-to-use% p))
                        (unless (member u sorted :key (lambda (x) (name% x)) :test #'string=)
                          (setq missing-used t)))
                      (unless missing-used
                        (push p sorted)
                        (remhash p unsorted)
                        (return-from restart-top))))
                  unsorted)
         (if (= (hash-table-count unsorted) 0)
             (return-from order-packages-by-use (nreverse sorted))
             (error "There is a circular use dependency for the packages ~s~%Sorted packages ~s"
                    (let (problem-packages)
                      (maphash (lambda (k v)
                                 (declare (ignore v))
                                 (push (list (name% k) (list :use (packages-to-use% k) )) problem-packages))
                               unsorted)
                      problem-packages)
                    (reverse sorted)))))))

(defun check-symbol-against-previous (symbol previous-symbols)
  "* Arguments
- symbol :: A symbol
- previous-symbols :: A hash-table
* Description
Compare the symbol against previous definitions of symbols - if there is a mismatch in definition signal an error"
  (let* ((key (format nil "~a/~a" (package% symbol) (lisp-name% symbol)))
         (previous (gethash key previous-symbols)))
    (if previous
        (progn
          (when (not (eq (exported% symbol) (exported% previous)))
            (warn "The symbol ~a in package ~a was declared twice with different export status - c++-names ~a ~a" (lisp-name% symbol) (package% symbol) (c++-name% symbol) (c++-name% previous)))
          (when (not (eq (shadow% symbol) (shadow% previous)))
            (warn "The symbol ~a in package ~a was declared twice with different shadow status - c++-names ~a ~a" (lisp-name% symbol) (package% symbol) (c++-name% symbol) (c++-name% previous))))
        (setf (gethash key previous-symbols) symbol))))

(defun shift-stamp (unshifted-stamp)
  "Stamps are now preshifted two bits and have the object tag #B01 attached to them"
  unshifted-stamp)

(defun calculate-class-stamps-and-flags (classes gc-managed-types)
  (declare (optimize (debug 3)))
  (let ((cur-unshifted-stamp 0) ; start at 1
        top-classes)
    (labels ((traverse-assign-stamps (class flags)
               (setf (stamp% class) (shift-stamp (incf cur-unshifted-stamp)))
               (cond
                 ((string= "core::WrappedPointer_O" (class-key% class))
                  (setf flags :FLAGS_STAMP_IN_WRAPPER))
                 ((or (string= "core::Instance_O" (class-key% class))
                      (string= "core::FuncallableInstance_O" (class-key% class)))
                  (setf flags :FLAGS_STAMP_IN_RACK))
                 ((string= "core::DerivableCxxObject_O" (class-key% class))
                  (setf flags :FLAGS_STAMP_IN_CALLBACK)))
               (setf (flags% class) flags)
               (dolist (subclass (direct-subclasses% class))
                 (traverse-assign-stamps subclass flags))))
      (maphash (lambda (key class)
                 (let ((super-class-key (base% class)))
                   (if super-class-key
                       (let ((super-class (gethash (base% class) classes)))
                         (if super-class
                             (progn
                               (push class (direct-subclasses% super-class)))
                             (push class top-classes)))
                       (push class top-classes))))
               classes)
      (dolist (top-class top-classes)
        (traverse-assign-stamps top-class :FLAGS_STAMP_IN_HEADER)))
    (maphash (lambda (key type)
               (setf (stamp% type) (shift-stamp (incf cur-unshifted-stamp))))
             gc-managed-types)))

(defun interpret-tags (tags)
  "* Arguments
- tags :: A list of tags.
* Description
This interprets the tags and generates objects that are used to generate code."
  (declare (optimize (debug 3)))
  (let ((source-info (gather-source-files tags)))
    (calculate-character-offsets source-info))
  (let (cur-lambda
        cur-priority
        cur-name
        cur-docstring
        cur-declare
        cur-package-nickname-tags
        cur-package-use-tags
        cur-package-shadow-tags
        cur-namespace-tag
        cur-class
        cur-meta-class
        cur-begin-enum
        cur-values
        enums
        initializers
        pregcstartups
        (namespace-to-assoc (make-hash-table :test #'equal))
        (package-to-assoc (make-hash-table :test #'equal))
        (packages (make-hash-table :test #'equal)) ; map ns/package to package string
        (packages-to-create nil)
        (classes (make-hash-table :test #'equal))
        (gc-managed-types (make-hash-table :test #'equal))
        functions symbols
        (previous-symbols (make-hash-table :test #'equal)))
    (declare (special namespace-to-assoc package-to-assoc packages))
    (dolist (tag tags)
      (handler-case 
          (etypecase tag
            (tags:namespace-package-association-tag
             (setf (gethash (tags:namespace% tag) namespace-to-assoc) tag)
             (setf (gethash (tags:package% tag) package-to-assoc) tag)
             (setf (gethash (tags:namespace% tag) packages) (tags:package-str% tag))
             (setf (gethash (tags:package% tag) packages) (tags:package-str% tag))
             (pushnew (make-instance 'package-to-create
                                     :file% (tags:file% tag)
                                     :line% (tags:line% tag)
                                     :character-offset% (tags:character-offset% tag)
                                     :name% (tags:package-str% tag)
                                     :packages-to-use% (mapcar (lambda (x) (tags:name% x)) cur-package-use-tags)
                                     :shadow% (mapcar (lambda (x) (tags:name% x)) cur-package-shadow-tags)
                                     :nicknames% (mapcar (lambda (x) (tags:name% x)) cur-package-nickname-tags))
                      packages-to-create
                      :test #'string=
                      :key (lambda (x) (name% x)))
             (setf cur-package-use-tags nil
                   cur-package-shadow-tags nil
                   cur-package-nickname-tags nil))
            (tags:namespace-tag
             (setf cur-namespace-tag tag))
            (tags:cl-name-tag
             (setf cur-name tag))
            (tags:meta-class-tag
             (setf cur-meta-class tag))
            (tags:cl-lispify-name-tag
             (setf cur-name tag))
            (tags:cl-pkg-name-tag
             (setf cur-name tag))
            (tags:cl-lambda-tag
             (setf cur-lambda tag))
            (tags:cl-declare-tag
             (setf cur-declare tag))
            (tags:cl-docstring-tag
             (setf cur-docstring tag))
            (tags:cl-priority-tag
             (setf cur-priority tag))
            (tags:package-nickname-tag
             (push tag cur-package-nickname-tags))
            (tags:package-use-tag
             (push tag cur-package-use-tags))
            (tags:package-shadow-tag
             (push tag cur-package-shadow-tags))
            (tags:cl-defun-tag
             (error-if-bad-expose-info-setup tag
                                             cur-name
                                             cur-lambda
                                             cur-declare
                                             cur-docstring
                                             cur-priority)
             (let* ((packaged-function-name
                      (maybe-override-name cur-namespace-tag
                                           cur-name
                                           (packaged-name cur-namespace-tag tag packages)
                                           packages)))
               (let* ((namespace (tags:namespace% cur-namespace-tag))
                      (signature (tags:signature-text% tag))
                      (signature-text (tags:signature-text% tag))
                      (lambda-list (or (tags:maybe-lambda-list cur-lambda)
                                       (parse-lambda-list-from-signature signature-text)))
                      (declare-form (tags:maybe-declare cur-declare))
                      (docstring (tags:maybe-docstring cur-docstring))
                      (priority (tags:maybe-priority cur-priority)))
                 (multiple-value-bind (function-name full-function-name simple-function)
                     (extract-function-name-from-signature signature-text tag)
                   (declare (ignore function-name))
                   (pushnew (make-instance 'expose-defun
                                           :namespace% namespace
                                           :lisp-name% packaged-function-name
                                           :function-name% full-function-name
                                           :file% (tags:file% tag)
                                           :line% (tags:line% tag)
                                           :character-offset% (tags:character-offset% tag)
                                           :lambda-list% lambda-list
                                           :declare% declare-form
                                           :docstring% docstring
                                           :priority% priority
                                           :provide-declaration% simple-function
                                           :signature% signature)
                            functions
                            :test #'string=
                            :key #'lisp-name%))
                 (setf cur-lambda nil
                       cur-declare nil
                       cur-docstring nil
                       cur-priority nil
                       cur-name nil))))
            (tags:cl-defun-setf-tag ; identical to previous case, except for...
             (error-if-bad-expose-info-setup tag
                                             cur-name
                                             cur-lambda
                                             cur-declare
                                             cur-docstring)
             (let* ((packaged-function-name
                      (maybe-override-name cur-namespace-tag
                                           cur-name
                                           (packaged-name cur-namespace-tag tag packages)
                                           packages)))
               (let* ((namespace (tags:namespace% cur-namespace-tag))
                      (signature (tags:signature-text% tag))
                      (signature-text (tags:signature-text% tag))
                      (lambda-list (or (tags:maybe-lambda-list cur-lambda)
                                       (parse-lambda-list-from-signature signature-text)))
                      (declare-form (tags:maybe-declare cur-declare))
                      (docstring (tags:maybe-docstring cur-docstring)))
                 (multiple-value-bind (function-name full-function-name simple-function)
                     (extract-function-name-from-signature signature-text tag)
                   (declare (ignore function-name))
                   (pushnew (make-instance 'expose-defun-setf ; here.
                                           :namespace% namespace
                                           :lisp-name% packaged-function-name
                                           :function-name% full-function-name
                                           :file% (tags:file% tag)
                                           :line% (tags:line% tag)
                                           :character-offset% (tags:character-offset% tag)
                                           :lambda-list% lambda-list
                                           :declare% declare-form
                                           :docstring% docstring
                                           :provide-declaration% simple-function
                                           :signature% signature)
                            functions
                            :test #'string=
                            :key #'lisp-name%))
                 (setf cur-lambda nil
                       cur-declare nil
                       cur-docstring nil
                       cur-priority nil
                       cur-name nil))))
            (tags:cl-extern-defun-tag
             (error-if-bad-expose-info-setup tag cur-name cur-lambda cur-declare cur-docstring)
             (let* ((packaged-function-name
                      (maybe-override-name
                       cur-namespace-tag
                       cur-name
                       (packaged-name cur-namespace-tag tag packages)
                       packages))
                    (namespace (tags:namespace% cur-namespace-tag))
                    (pointer (string-trim " " (tags:pointer% tag)))
                    (function-name (extract-function-name-from-pointer pointer tag))
                    (function-ptr (esrap:parse 'function-ptr pointer))
                    (namespace (function-ptr-namespace function-ptr))
                    (lambda-list (or (tags:maybe-lambda-list cur-lambda)
                                     (and (function-ptr-type function-ptr)
                                          (convert-function-ptr-to-lambda-list function-ptr))))
                    (declare-form (tags:maybe-declare cur-declare))
                    (docstring (tags:maybe-docstring cur-docstring)))
               (pushnew (make-instance 'expose-extern-defun
                                       :namespace% namespace
                                       :lisp-name% packaged-function-name
                                       :function-name% function-name
                                       :file% (tags:file% tag)
                                       :line% (tags:line% tag)
                                       :character-offset% (tags:character-offset% tag)
                                       :lambda-list% lambda-list
                                       :declare% declare-form
                                       :docstring% docstring
                                       :pointer% pointer
                                       :function-ptr% function-ptr)
                        functions
                        :test #'string=
                        :key #'lisp-name%)
               (setf cur-lambda nil
                     cur-declare nil
                     cur-docstring nil
                     cur-priority nil
                     cur-name nil)))
            (tags:cl-defmethod-tag
             (error-if-bad-expose-info-setup tag cur-name cur-lambda cur-declare cur-docstring)
             (multiple-value-bind (tag-class-name method-name)
                 (cscrape:extract-method-name-from-signature (tags:signature-text% tag))
               (let* ((class-name (or tag-class-name (tags:name% cur-class)))
                      (class-key (make-class-key cur-namespace-tag class-name))
                      (class (gethash class-key classes))
                      (packaged-method-name (maybe-override-name
                                             cur-namespace-tag
                                             cur-name
                                             (packaged-name cur-namespace-tag method-name packages)
                                             packages))
                      (signature-text (tags:signature-text% tag))
                      (lambda-list (or (tags:maybe-lambda-list cur-lambda)
                                       (parse-lambda-list-from-signature signature-text :class class)))
                      (declare-form (tags:maybe-declare cur-declare))
                      (docstring (tags:maybe-docstring cur-docstring)))
                 (unless class (error "For cl-defmethod-tag couldn't find class ~a" class-key))
                 (pushnew (make-instance 'expose-defmethod
                                         :class% class
                                         :lisp-name% packaged-method-name
                                         :method-name% method-name
                                         :file% (tags:file% tag)
                                         :line% (tags:line% tag)
                                         :character-offset% (tags:character-offset% tag)
                                         :lambda-list% lambda-list
                                         :declare% declare-form
                                         :docstring% docstring)
                          (methods% class)
                          :test #'string=
                          :key #'lisp-name%)))
             (setf cur-lambda nil
                   cur-declare nil
                   cur-docstring nil
                   cur-priority nil
                   cur-name nil))
            (tags:cl-def-class-method-tag
             (error-if-bad-expose-info-setup tag cur-name cur-lambda cur-declare cur-docstring)
             (multiple-value-bind (tag-class-name method-name)
                 (cscrape:extract-class-method-name-from-signature (tags:signature-text% tag))
               (let* ((class-name (or tag-class-name (tags:name% cur-class)))
                      (class-key (make-class-key cur-namespace-tag class-name))
                      (class (gethash class-key classes))
                      (packaged-method-name (maybe-override-name
                                             cur-namespace-tag
                                             cur-name
                                             (packaged-name cur-namespace-tag method-name packages)
                                             packages))
                      (signature-text (tags:signature-text% tag))
                      (lambda-list (or (tags:maybe-lambda-list cur-lambda)
                                       (parse-lambda-list-from-signature signature-text)))
                      (declare-form (tags:maybe-declare cur-declare))
                      (docstring (tags:maybe-docstring cur-docstring)))
                 (unless class (error "For cl-def-class-method-tag couldn't find class ~a" class-key))
                 (pushnew (make-instance 'expose-def-class-method
                                         :class% class
                                         :lisp-name% packaged-method-name
                                         :method-name% method-name
                                         :file% (tags:file% tag)
                                         :line% (tags:line% tag)
                                         :character-offset% (tags:character-offset% tag)
                                         :lambda-list% lambda-list
                                         :declare% declare-form
                                         :docstring% docstring)
                          (class-methods% class)
                          :test #'string=
                          :key #'lisp-name%)))
             (setf cur-lambda nil
                   cur-declare nil
                   cur-docstring nil
                   cur-priority nil
                   cur-name nil))
            (tags:cl-extern-defmethod-tag
             (error-if-bad-expose-info-setup tag cur-name cur-lambda cur-declare cur-docstring)
             (let* ((method-name (extract-method-name-from-pointer (tags:pointer% tag) tag))
                    (class-name (tags:class-name% tag))
                    (class-key (make-class-key cur-namespace-tag class-name))
                    (class (gethash class-key classes))
                    (packaged-method-name
                      (maybe-override-name
                       cur-namespace-tag
                       cur-name
                       (packaged-name cur-namespace-tag method-name packages)
                       packages))
                    (lambda-list (or (tags:maybe-lambda-list cur-lambda) ""))
                    (declare-form (tags:maybe-declare cur-declare))
                    (docstring (tags:maybe-docstring cur-docstring))
                    (pointer (tags:pointer% tag)))
               (unless class (error "Couldn't find class ~a" class-key))
               (pushnew (make-instance 'expose-extern-defmethod
                                       :class% class
                                       :lisp-name% packaged-method-name
                                       :method-name% method-name
                                       :file% (tags:file% tag)
                                       :line% (tags:line% tag)
                                       :character-offset% (tags:character-offset% tag)
                                       :lambda-list% lambda-list
                                       :declare% declare-form
                                       :docstring% docstring
                                       :pointer% pointer)
                        (methods% class)
                        :test #'string=
                        :key #'lisp-name%)
               (setf cur-lambda nil
                     cur-declare nil
                     cur-docstring nil
                     cur-priority nil
                     cur-name nil)))
            (tags:gc-managed-type-tag
             (let ((type-key (tags:c++type% tag)))
               (unless (gethash type-key gc-managed-types)
                 (setf (gethash type-key gc-managed-types)
                       (make-instance 'gc-managed-type
                                      :file% (tags:file% tag)
                                      :line% (tags:line% tag)
                                      :c++type% type-key)))))
            (tags:lisp-internal-class-tag
             (when cur-docstring (error-if-bad-expose-info-setup* tag cur-docstring))
             (unless cur-namespace-tag (error 'missing-namespace :tag tag))
             (unless (string= (tags:namespace% cur-namespace-tag) (tags:namespace% tag))
               (error 'namespace-mismatch :tag tag))
             (setf cur-class tag)
             (let ((class-key (make-class-key cur-namespace-tag (tags:name% tag))))
               (unless (gethash class-key classes)
                 (setf (gethash class-key classes)
                       (make-instance 'exposed-internal-class
                                      :file% (tags:file% tag)
                                      :line% (tags:line% tag)
                                      :package% (tags:package% tag)
                                      :character-offset% (tags:character-offset% tag)
                                      :meta-class% (tags:maybe-meta-class cur-namespace-tag cur-meta-class)
                                      :docstring% (tags:maybe-docstring cur-docstring)
                                      :base% (make-class-key cur-namespace-tag (tags:base% tag))
                                      :class-key% class-key
                                      :lisp-name% (lispify-class-name tag packages)
                                      :class-tag% tag))))
             (setf cur-docstring nil
                   cur-priority nil
                   cur-meta-class nil))
            (tags:lisp-external-class-tag
             (when cur-docstring (error-if-bad-expose-info-setup* tag cur-docstring))
             (unless cur-namespace-tag (error 'missing-namespace :tag tag))
             (unless (string= (tags:namespace% cur-namespace-tag) (tags:namespace% tag))
               (error 'namespace-mismatch :tag tag))
             (setf cur-class tag)
             (let ((class-key (make-class-key cur-namespace-tag (tags:name% tag))))
               (unless (gethash class-key classes)
                 (setf (gethash class-key classes)
                       (make-instance 'exposed-external-class
                                      :file% (tags:file% tag)
                                      :line% (tags:line% tag)
                                      :package% (tags:package% tag)
                                      :character-offset% (tags:character-offset% tag)
                                      :meta-class% (tags:maybe-meta-class cur-namespace-tag cur-meta-class)
                                      :docstring% (tags:maybe-docstring cur-docstring)
                                      :class-key% class-key
                                      :base% (make-class-key cur-namespace-tag (tags:base% tag))
                                      :lisp-name% (lispify-class-name tag packages)
                                      :class-tag% tag))))
             (setf cur-docstring nil
                   cur-priority nil
                   cur-meta-class nil))
            (tags:cl-begin-enum-tag
             (when cur-begin-enum
               (error 'tag-error
                      :message "Unexpected CL_BEGIN_ENUM - previous CL_BEGIN_ENUM at ~a:~d"
                      :message-args (list (file% cur-begin-enum) (line% cur-begin-enum))
                      :tag tag ))
             (setf cur-begin-enum (make-instance 'begin-enum
                                                 :file% (tags:file% tag)
                                                 :line% (tags:line% tag)
                                                 :character-offset% (tags:character-offset% tag)
                                                 :type% (maybe-namespace-type cur-namespace-tag (tags:type% tag))
                                                 :symbol% (maybe-namespace-symbol cur-namespace-tag (tags:symbol% tag))
                                                 :description% (tags:description% tag))
                   cur-values nil))
            (tags:cl-value-enum-tag
             (push (make-instance 'value-enum
                                  :file% (tags:file% tag)
                                  :line% (tags:line% tag)
                                  :character-offset% (tags:character-offset% tag)
                                  :symbol% (maybe-namespace-symbol cur-namespace-tag (tags:symbol% tag))
                                  :value% (maybe-namespace-enum cur-namespace-tag (tags:value% tag)))
                   cur-values))
            (tags:cl-end-enum-tag
             (let ((end-symbol (maybe-namespace-symbol cur-namespace-tag (tags:symbol% tag))))
               (unless cur-begin-enum
                 (error 'tag-error :message "Missing BEGIN_ENUM" :tag tag))
               (unless cur-values
                 (error 'tag-error :message "Missing VALUES" :tag tag))
               (unless (string= (symbol% cur-begin-enum) end-symbol)
                 (let ((begin-symbol (maybe-namespace-symbol cur-namespace-tag (symbol% cur-begin-enum)))
                       (end-symbol (maybe-namespace-symbol cur-namespace-tag end-symbol)))
                   (error 'tag-error
                          :message "Mismatch between symbols of CL_BEGIN_ENUM ~a and CL_END_ENUM ~a"
                          :message-args (list begin-symbol end-symbol)
                          :tag tag)))
               (pushnew (make-instance 'completed-enum
                                       :begin-enum% cur-begin-enum
                                       :values% cur-values)
                        enums
                        :test #'string=
                        :key (lambda (x) (symbol% (begin-enum% x))))
               (setf cur-begin-enum nil
                     cur-values nil)))
            (tags:symbol-tag
             (let* ((c++-name (or (tags:c++-name% tag) (tags:lisp-name% tag)))
                    (namespace (or (tags:namespace% tag) (tags:namespace% (gethash (tags:package% tag) package-to-assoc))))
                    (package (or (tags:package% tag) (tags:package% (gethash (tags:namespace% tag) namespace-to-assoc))))
                    (lisp-name (tags:lisp-name% tag))
                    (exported (typep tag 'tags:symbol-external-tag))
                    (shadow (typep tag 'tags:symbol-shadow-external-tag))
                    (symbol (make-instance 'expose-symbol
                                           :lisp-name% lisp-name
                                           :c++-name% c++-name
                                           :namespace% namespace
                                           :package% package
                                           :package-str% (gethash package packages)
                                           :exported% exported
                                           :shadow% shadow)))
               (check-symbol-against-previous symbol previous-symbols)
               (push symbol symbols)))
            (tags:cl-initializer-tag
             (let* ((namespace (tags:namespace% cur-namespace-tag))
                    (signature-text (tags:signature-text% tag)))
               (multiple-value-bind (function-name full-function-name simple-function)
                   (extract-function-name-from-signature signature-text tag)
                 (declare (ignore function-name))
                 (pushnew (make-instance 'expose-initializer
                                         :namespace% namespace
                                         :function-name% full-function-name
                                         :file% (tags:file% tag)
                                         :line% (tags:line% tag)
                                         :character-offset% (tags:character-offset% tag))
                          initializers
                          :test #'string=
                          :key #'function-name% ))))
            (tags:cl-pregcstartup-tag
             (let* ((namespace (tags:namespace% cur-namespace-tag))
                    (signature-text (tags:signature-text% tag)))
               (multiple-value-bind (function-name full-function-name simple-function)
                   (extract-function-name-from-signature signature-text tag)
                 (declare (ignore function-name))
                 (pushnew (make-instance 'expose-pregcstartup
                                         :namespace% namespace
                                         :function-name% full-function-name
                                         :file% (tags:file% tag)
                                         :line% (tags:line% tag)
                                         :character-offset% (tags:character-offset% tag))
                          pregcstartups
                          :test #'string=
                          :key #'function-name% )))))
        (error (e)
          (error "While parsing tag from ~a:~d~%ERROR ~a~%TAG: ~s~%"
                 (tags:file% tag)
                 (tags:line% tag)
                 e
                 tag))))
    (calculate-class-stamps-and-flags classes gc-managed-types)
    (values (order-packages-by-use packages-to-create) functions symbols classes gc-managed-types enums pregcstartups initializers)))
