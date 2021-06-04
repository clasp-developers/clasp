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

(defclass expose-expose (expose-code function-mixin)
  ((signature% :initform nil :initarg :signature% :accessor signature%)))

(defclass expose-terminator (expose-code function-mixin)
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
   (flags% :initform nil :accessor flags%)))

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

(defclass kind ()
  ((tag% :initarg :tag% :accessor tag%)
   (fixed-fields% :initform nil :accessor fixed-fields%)
   (variable-info% :initform nil :accessor variable-info%)
   (variable-capacity% :initform nil :accessor variable-capacity%)
   (variable-fields% :initform nil :accessor variable-fields%)))

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
                                 (push (list (name% k) (list :use (packages-to-use% k))) problem-packages))
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
                 (declare (ignore key))
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
               (declare (ignore key))
               (setf (stamp% type) (shift-stamp (incf cur-unshifted-stamp))))
             gc-managed-types)))

(defstruct (tag-interpreter-state (:conc-name "STATE-"))
  (cur-lambda nil) (cur-priority nil) (cur-name nil) (cur-docstring nil)
  (cur-declare nil) (cur-package-nickname-tags nil) (cur-package-use-tags nil)
  (cur-package-shadow-tags nil) (cur-namespace-tag nil) (cur-class nil)
  (cur-meta-class nil) (cur-begin-enum nil) (cur-values nil) (cur-kind nil)
  (enums nil) (initializers nil) (exposes nil)
  (terminators nil) (pregcstartups nil)
  (namespace-to-assoc (make-hash-table :test #'equal))
  (package-to-assoc (make-hash-table :test #'equal))
  (packages (make-hash-table :test #'equal)) ; map ns/package to package string
  (packages-to-create nil)
  (classes (make-hash-table :test #'equal))
  (gc-managed-types (make-hash-table :test #'equal))
  (functions nil) (symbols nil)
  (previous-symbols (make-hash-table :test #'equal)))

(defgeneric interpret-tag (tag state))

(defmethod interpret-tag ((tag tags:namespace-package-association-tag) state)
  (setf (gethash (tags:namespace% tag) (state-namespace-to-assoc state)) tag)
  (setf (gethash (tags:package% tag) (state-package-to-assoc state)) tag)
  (setf (gethash (tags:namespace% tag) (state-packages state))
        (tags:package-str% tag))
  (setf (gethash (tags:package% tag) (state-packages state))
        (tags:package-str% tag))
  (pushnew (make-instance 'package-to-create
             :file% (tags:file% tag)
             :line% (tags:line% tag)
             :character-offset% (tags:character-offset% tag)
             :name% (tags:package-str% tag)
             :packages-to-use% (mapcar (lambda (x) (tags:name% x))
                                       (state-cur-package-use-tags state))
             :shadow% (mapcar (lambda (x) (tags:name% x))
                              (state-cur-package-shadow-tags state))
             :nicknames% (mapcar (lambda (x) (tags:name% x))
                                 (state-cur-package-nickname-tags state)))
           (state-packages-to-create state)
           :test #'string=
           :key (lambda (x) (name% x)))
  (setf (state-cur-package-use-tags state) nil
        (state-cur-package-shadow-tags state) nil
        (state-cur-package-nickname-tags state) nil))

(defmethod interpret-tag ((tag tags:namespace-tag) state)
  (setf (state-cur-namespace-tag state) tag))
(defmethod interpret-tag ((tag tags:cl-name-tag) state)
  (setf (state-cur-name state) tag))
(defmethod interpret-tag ((tag tags:meta-class-tag) state)
  (setf (state-cur-meta-class state) tag))
(defmethod interpret-tag ((tag tags:cl-lispify-name-tag) state)
  (setf (state-cur-name state) tag))
(defmethod interpret-tag ((tag tags:cl-pkg-name-tag) state)
  (setf (state-cur-name state) tag))
(defmethod interpret-tag ((tag tags:cl-lambda-tag) state)
  (setf (state-cur-lambda state) tag))
(defmethod interpret-tag ((tag tags:cl-declare-tag) state)
  (setf (state-cur-declare state) tag))
(defmethod interpret-tag ((tag tags:cl-docstring-tag) state)
  (setf (state-cur-docstring state) tag))
(defmethod interpret-tag ((tag tags:cl-priority-tag) state)
  (setf (state-cur-priority state) tag))
(defmethod interpret-tag ((tag tags:package-nickname-tag) state)
  (push tag (state-cur-package-nickname-tags state)))
(defmethod interpret-tag ((tag tags:package-use-tag) state)
  (push tag (state-cur-package-use-tags state)))
(defmethod interpret-tag ((tag tags:package-shadow-tag) state)
  (push tag (state-cur-package-shadow-tags state)))

(defmethod interpret-tag ((tag tags:cl-defun-tag) state)
  (error-if-bad-expose-info-setup tag
                                  (state-cur-name state)
                                  (state-cur-lambda state)
                                  (state-cur-declare state)
                                  (state-cur-docstring state)
                                  (state-cur-priority state))
  (let* ((packaged-function-name
           (maybe-override-name (state-cur-namespace-tag state)
                                (state-cur-name state)
                                (packaged-name (state-cur-namespace-tag state)
                                               tag (state-packages state))
                                (state-packages state))))
    (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
           (signature (tags:signature-text% tag))
           (signature-text (tags:signature-text% tag))
           (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state))
                            (parse-lambda-list-from-signature signature-text)))
           (declare-form (tags:maybe-declare (state-cur-declare state)))
           (docstring (tags:maybe-docstring (state-cur-docstring state)))
           (priority (tags:maybe-priority (state-cur-priority state))))
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
                 (state-functions state)
                 :test #'string=
                 :key #'lisp-name%))
      (setf (state-cur-lambda state) nil
            (state-cur-declare state) nil
            (state-cur-docstring state) nil
            (state-cur-priority state) nil
            (state-cur-name state) nil))))

(defmethod interpret-tag ((tag tags:cl-defun-setf-tag) state)
  ;; identical to previous case, except for...
  (error-if-bad-expose-info-setup tag
                                  (state-cur-name state)
                                  (state-cur-lambda state)
                                  (state-cur-declare state)
                                  (state-cur-docstring state)
                                  (state-cur-priority state))
  (let* ((packaged-function-name
           (maybe-override-name (state-cur-namespace-tag state)
                                (state-cur-name state)
                                (packaged-name (state-cur-namespace-tag state)
                                               tag (state-packages state))
                                (state-packages state))))
    (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
           (signature (tags:signature-text% tag))
           (signature-text (tags:signature-text% tag))
           (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state))
                            (parse-lambda-list-from-signature signature-text)))
           (declare-form (tags:maybe-declare (state-cur-declare state)))
           (docstring (tags:maybe-docstring (state-cur-docstring state)))
           (priority (tags:maybe-priority (state-cur-priority state))))
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
                   :priority% priority
                   :provide-declaration% simple-function
                   :signature% signature)
                 (state-functions state)
                 :test #'string=
                 :key #'lisp-name%))
      (setf (state-cur-lambda state) nil
            (state-cur-declare state) nil
            (state-cur-docstring state) nil
            (state-cur-priority state) nil
            (state-cur-name state) nil))))

(defmethod interpret-tag ((tag tags:cl-extern-defun-tag) state)
  (error-if-bad-expose-info-setup
   tag (state-cur-name state) (state-cur-lambda state)
   (state-cur-declare state) (state-cur-docstring state))
  (let* ((packaged-function-name
           (maybe-override-name
            (state-cur-namespace-tag state)
            (state-cur-name state)
            (packaged-name
             (state-cur-namespace-tag state) tag (state-packages state))
            (state-packages state)))
         (pointer (string-trim " " (tags:pointer% tag)))
         (function-name (extract-function-name-from-pointer pointer tag))
         (function-ptr (esrap:parse 'function-ptr pointer))
         (namespace (function-ptr-namespace function-ptr))
         (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state))
                          (and (function-ptr-type function-ptr)
                               (convert-function-ptr-to-lambda-list
                                function-ptr))))
         (declare-form (tags:maybe-declare (state-cur-declare state)))
         (docstring (tags:maybe-docstring (state-cur-docstring state))))
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
             (state-functions state)
             :test #'string=
             :key #'lisp-name%)
    (setf (state-cur-lambda state) nil
          (state-cur-declare state) nil
          (state-cur-docstring state) nil
          (state-cur-priority state) nil
          (state-cur-name state) nil)))

(defmethod interpret-tag ((tag tags:cl-defmethod-tag) state)
  (error-if-bad-expose-info-setup
   tag (state-cur-name state) (state-cur-lambda state)
   (state-cur-declare state) (state-cur-docstring state))
  (multiple-value-bind (tag-class-name method-name)
      (cscrape:extract-method-name-from-signature (tags:signature-text% tag))
    (let* ((class-name (or tag-class-name (tags:name% (state-cur-class state))))
           (class-key
             (make-class-key (state-cur-namespace-tag state) class-name))
           (class (gethash class-key (state-classes state)))
           (packaged-method-name (maybe-override-name
                                  (state-cur-namespace-tag state)
                                  (state-cur-name state)
                                  (packaged-name (state-cur-namespace-tag state)
                                                 method-name
                                                 (state-packages state))
                                  (state-packages state)))
           (signature-text (tags:signature-text% tag))
           (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state))
                            (parse-lambda-list-from-signature signature-text
                                                              :class class)))
           (declare-form (tags:maybe-declare (state-cur-declare state)))
           (docstring (tags:maybe-docstring (state-cur-docstring state))))
      (unless class (error "For cl-defmethod-tag couldn't find class ~a"
                           class-key))
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
  (setf (state-cur-lambda state) nil
        (state-cur-declare state) nil
        (state-cur-docstring state) nil
        (state-cur-priority state) nil
        (state-cur-name state) nil))

(defmethod interpret-tag ((tag tags:cl-def-class-method-tag) state)
  (error-if-bad-expose-info-setup
   tag (state-cur-name state) (state-cur-lambda state)
   (state-cur-declare state) (state-cur-docstring state))
 (multiple-value-bind (tag-class-name method-name)
     (cscrape:extract-class-method-name-from-signature
      (tags:signature-text% tag))
   (let* ((class-name (or tag-class-name (tags:name% (state-cur-class state))))
          (class-key
            (make-class-key (state-cur-namespace-tag state) class-name))
          (class (gethash class-key (state-classes state)))
          (packaged-method-name (maybe-override-name
                                 (state-cur-namespace-tag state)
                                 (state-cur-name state)
                                 (packaged-name (state-cur-namespace-tag state)
                                                method-name
                                                (state-packages state))
                                 (state-packages state)))
          (signature-text (tags:signature-text% tag))
          (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state))
                           (parse-lambda-list-from-signature signature-text)))
          (declare-form (tags:maybe-declare (state-cur-declare state)))
          (docstring (tags:maybe-docstring (state-cur-docstring state))))
     (unless class (error "For cl-def-class-method-tag couldn't find class ~a"
                          class-key))
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
 (setf (state-cur-lambda state) nil
       (state-cur-declare state) nil
       (state-cur-docstring state) nil
       (state-cur-priority state) nil
       (state-cur-name state) nil))

(defmethod interpret-tag ((tag tags:cl-extern-defmethod-tag) state)
  (error-if-bad-expose-info-setup
   tag (state-cur-name state) (state-cur-lambda state)
   (state-cur-declare state) (state-cur-docstring state))
  (let* ((method-name
           (extract-method-name-from-pointer (tags:pointer% tag) tag))
         (class-name (tags:class-name% tag))
         (class-key (make-class-key (state-cur-namespace-tag state) class-name))
         (class (gethash class-key (state-classes state)))
         (packaged-method-name
           (maybe-override-name
            (state-cur-namespace-tag state)
            (state-cur-name state)
            (packaged-name (state-cur-namespace-tag state) method-name
                           (state-packages state))
            (state-packages state)))
         (lambda-list (or (tags:maybe-lambda-list (state-cur-lambda state)) ""))
        (declare-form (tags:maybe-declare (state-cur-declare state)))
         (docstring (tags:maybe-docstring (state-cur-docstring state)))
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
   (setf (state-cur-lambda state) nil
         (state-cur-declare state) nil
         (state-cur-docstring state) nil
         (state-cur-priority state) nil
         (state-cur-name state) nil)))

(defmethod interpret-tag ((tag tags:gc-managed-type-tag) state)
  (let ((type-key (tags:c++type% tag)))
    (unless (gethash type-key (state-gc-managed-types state))
      (setf (gethash type-key (state-gc-managed-types state))
            (make-instance 'gc-managed-type
              :file% (tags:file% tag)
              :line% (tags:line% tag)
              :c++type% type-key)))))

(defmethod interpret-tag ((tag tags:lisp-internal-class-tag) state)
  (when (state-cur-docstring state)
    (error-if-bad-expose-info-setup* tag (state-cur-docstring state)))
  (unless (state-cur-namespace-tag state) (error 'missing-namespace :tag tag))
  (unless (string= (tags:namespace% (state-cur-namespace-tag state))
                   (tags:namespace% tag))
    (error 'namespace-mismatch :tag tag))
  (setf (state-cur-class state) tag)
  (let ((class-key
          (make-class-key (state-cur-namespace-tag state) (tags:name% tag))))
    (unless (gethash class-key (state-classes state))
      (setf (gethash class-key (state-classes state))
            (make-instance 'exposed-internal-class
              :file% (tags:file% tag)
              :line% (tags:line% tag)
              :package% (tags:package% tag)
              :character-offset% (tags:character-offset% tag)
              :meta-class% (tags:maybe-meta-class
                            (state-cur-namespace-tag state)
                            (state-cur-meta-class state))
              :docstring% (tags:maybe-docstring (state-cur-docstring state))
              :base% (make-class-key (state-cur-namespace-tag state)
                                     (tags:base% tag))
              :class-key% class-key
              :lisp-name% (lispify-class-name tag (state-packages state))
              :class-tag% tag))))
  (setf (state-cur-docstring state) nil
        (state-cur-priority state) nil
        (state-cur-meta-class state) nil))

(defmethod interpret-tag ((tag tags:lisp-external-class-tag) state)
  (when (state-cur-docstring state)
    (error-if-bad-expose-info-setup* tag (state-cur-docstring state)))
  (unless (state-cur-namespace-tag state) (error 'missing-namespace :tag tag))
  (unless (string= (tags:namespace% (state-cur-namespace-tag state))
                   (tags:namespace% tag))
    (error 'namespace-mismatch :tag tag))
  (setf (state-cur-class state) tag)
  (let ((class-key
          (make-class-key (state-cur-namespace-tag state) (tags:name% tag))))
    (unless (gethash class-key (state-classes state))
      (setf (gethash class-key (state-classes state))
            (make-instance 'exposed-external-class
              :file% (tags:file% tag)
              :line% (tags:line% tag)
              :package% (tags:package% tag)
              :character-offset% (tags:character-offset% tag)
              :meta-class% (tags:maybe-meta-class
                            (state-cur-namespace-tag state)
                            (state-cur-meta-class state))
              :docstring% (tags:maybe-docstring (state-cur-docstring state))
              :class-key% class-key
              :base% (make-class-key (state-cur-namespace-tag state)
                                     (tags:base% tag))
              :lisp-name% (lispify-class-name tag (state-packages state))
              :class-tag% tag))))
  (setf (state-cur-docstring state) nil
        (state-cur-priority state) nil
        (state-cur-meta-class state) nil))

(defmethod interpret-tag ((tag tags:cl-begin-enum-tag) state)
  (when (state-cur-begin-enum state)
    (error 'tag-error
           :message "Unexpected CL_BEGIN_ENUM - previous CL_BEGIN_ENUM at ~a:~d"
           :message-args (list (file% (state-cur-begin-enum state))
                               (line% (state-cur-begin-enum state)))
           :tag tag))
  (setf (state-cur-begin-enum state)
        (make-instance 'begin-enum
          :file% (tags:file% tag)
          :line% (tags:line% tag)
          :character-offset% (tags:character-offset% tag)
          :type% (maybe-namespace-type (state-cur-namespace-tag state)
                                       (tags:type% tag))
          :symbol% (maybe-namespace-symbol (state-cur-namespace-tag state)
                                           (tags:symbol% tag))
          :description% (tags:description% tag))
        (state-cur-values state) nil))

(defmethod interpret-tag ((tag tags:cl-value-enum-tag) state)
  (push (make-instance 'value-enum
          :file% (tags:file% tag)
          :line% (tags:line% tag)
          :character-offset% (tags:character-offset% tag)
          :symbol% (maybe-namespace-symbol (state-cur-namespace-tag state)
                                           (tags:symbol% tag))
          :value% (maybe-namespace-enum (state-cur-namespace-tag state)
                                        (tags:value% tag)))
        (state-cur-values state)))

(defmethod interpret-tag ((tag tags:cl-end-enum-tag) state)
  (let ((end-symbol (maybe-namespace-symbol (state-cur-namespace-tag state)
                                            (tags:symbol% tag))))
    (unless (state-cur-begin-enum state)
      (error 'tag-error :message "Missing BEGIN_ENUM" :tag tag))
    (unless (state-cur-values state)
      (error 'tag-error :message "Missing VALUES" :tag tag))
    (unless (string= (symbol% (state-cur-begin-enum state)) end-symbol)
      (let ((begin-symbol
              (maybe-namespace-symbol (state-cur-namespace-tag state)
                                      (symbol% (state-cur-begin-enum state))))
            (end-symbol
              (maybe-namespace-symbol (state-cur-namespace-tag state)
                                      end-symbol)))
        (error 'tag-error
               :message "Mismatch between symbols of CL_BEGIN_ENUM ~a and CL_END_ENUM ~a"
               :message-args (list begin-symbol end-symbol)
               :tag tag)))
    (pushnew (make-instance 'completed-enum
               :begin-enum% (state-cur-begin-enum state)
               :values% (state-cur-values state))
             (state-enums state)
             :test #'string=
             :key (lambda (x) (symbol% (begin-enum% x))))
    (setf (state-cur-begin-enum state) nil
          (state-cur-values state) nil)))

(defmethod interpret-tag ((tag tags:symbol-tag) state)
  (let* ((c++-name (or (tags:c++-name% tag) (tags:lisp-name% tag)))
         (namespace (or (tags:namespace% tag)
                        (tags:namespace%
                         (gethash (tags:package% tag)
                                  (state-package-to-assoc state)))))
         (package (or (tags:package% tag)
                      (tags:package%
                       (gethash (tags:namespace% tag)
                                (state-namespace-to-assoc state)))))
         (lisp-name (tags:lisp-name% tag))
         (exported (typep tag 'tags:symbol-external-tag))
         (shadow (typep tag 'tags:symbol-shadow-external-tag))
         (symbol (make-instance 'expose-symbol
                   :lisp-name% lisp-name
                   :c++-name% c++-name
                   :namespace% namespace
                   :package% package
                   :package-str% (gethash package (state-packages state))
                   :exported% exported
                   :shadow% shadow)))
    (check-symbol-against-previous symbol (state-previous-symbols state))
    (push symbol (state-symbols state))))

(defmethod interpret-tag ((tag tags:cl-initializer-tag) state)
  (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
         (signature-text (tags:signature-text% tag)))
    (multiple-value-bind (function-name full-function-name)
        (extract-function-name-from-signature signature-text tag)
      (declare (ignore function-name))
      (pushnew (make-instance 'expose-initializer
                 :namespace% namespace
                 :function-name% full-function-name
                 :file% (tags:file% tag)
                 :line% (tags:line% tag)
                 :character-offset% (tags:character-offset% tag))
               (state-initializers state)
               :test #'string=
               :key #'function-name%))))

(defmethod interpret-tag ((tag tags:cl-expose-tag) state)
  (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
         (signature-text (tags:signature-text% tag)))
    (multiple-value-bind (function-name full-function-name)
        (extract-function-name-from-signature signature-text tag)
      (declare (ignore function-name))
      (pushnew (make-instance 'expose-expose
                 :namespace% namespace
                 :function-name% full-function-name
                 :file% (tags:file% tag)
                 :line% (tags:line% tag)
                 :character-offset% (tags:character-offset% tag))
               (state-exposes state)
               :test #'string=
               :key #'function-name%))))

(defmethod interpret-tag ((tag tags:cl-terminator-tag) state)
  (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
         (signature-text (tags:signature-text% tag)))
    (multiple-value-bind (function-name full-function-name)
        (extract-function-name-from-signature signature-text tag)
      (declare (ignore function-name))
      (pushnew (make-instance 'expose-terminator
                 :namespace% namespace
                 :function-name% full-function-name
                 :file% (tags:file% tag)
                 :line% (tags:line% tag)
                 :character-offset% (tags:character-offset% tag))
               (state-terminators state)
               :test #'string=
               :key #'function-name%))))
 
(defmethod interpret-tag ((tag tags:cl-pregcstartup-tag) state)
  (let* ((namespace (tags:namespace% (state-cur-namespace-tag state)))
         (signature-text (tags:signature-text% tag)))
    (multiple-value-bind (function-name full-function-name)
        (extract-function-name-from-signature signature-text tag)
      (declare (ignore function-name))
      (pushnew (make-instance 'expose-pregcstartup
                 :namespace% namespace
                 :function-name% full-function-name
                 :file% (tags:file% tag)
                 :line% (tags:line% tag)
                 :character-offset% (tags:character-offset% tag))
               (state-pregcstartups state)
               :test #'string=
               :key #'function-name%))))


(defmethod interpret-tag ((tag tags:class-kind) state)
  (setf (state-cur-kind state) (make-instance 'kind :tag tag)))
(defmethod interpret-tag ((tag tags:container-kind) state)
  (setf (state-cur-kind state) (make-instance 'kind :tag tag)))
(defmethod interpret-tag ((tag tags:bitunit-container-kind) state)
  (setf (state-cur-kind state) (make-instance 'kind :tag tag)))
(defmethod interpret-tag ((tag tags:templated-kind) state)
  (setf (state-cur-kind state) (make-instance 'kind :tag tag)))

(defmethod interpret-tag ((tag tags:fixed-field) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (null (variable-info% kind)))
    (setf (fixed-fields% kind) (nconc (fixed-fields% kind) (list tag)))))

(defmethod interpret-tag ((tag tags:variable-bit-array0) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (null (variable-info% kind)))
    (setf (variable-info% kind) tag)))
(defmethod interpret-tag ((tag tags:variable-array0) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (null (variable-info% kind)))
    (setf (variable-info% kind) tag)))
(defmethod interpret-tag ((tag tags:variable-capacity) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (not (null (variable-info% kind))))
    (assert (null (variable-capacity% kind)))
    (setf (variable-capacity% kind) tag)))
(defmethod interpret-tag ((tag tags:variable-field) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (not (null (variable-info% kind))))
    (assert (not (null (variable-capacity% kind))))
    (assert (listp (variable-fields% kind)))
    (setf (variable-fields% kind) (nconc (variable-fields% kind) (list tag)))))
(defmethod interpret-tag ((tag tags:variable-field-only) state)
  (let ((kind (state-cur-kind state)))
    (assert (not (null kind)))
    (assert (not (null (variable-info% kind))))
    (assert (not (null (variable-capacity% kind))))
    (assert (null (variable-fields% kind)))
    (setf (variable-fields% kind) tag)))


(defun interpret-tags (tags)
  "* Arguments
- tags :: A list of tags.
* Description
This interprets the tags and generates objects that are used to generate code."
  (declare (optimize (debug 3)))
  (let ((source-info (gather-source-files tags)))
    (calculate-character-offsets source-info))
  (let ((state (make-tag-interpreter-state)))
    (dolist (tag tags)
      (handler-case (interpret-tag tag state)
        (error (e)
          (error "While parsing tag from ~a:~d~%ERROR ~a~%TAG: ~s~%"
                 (tags:file% tag)
                 (tags:line% tag)
                 e
                 tag))))
    (calculate-class-stamps-and-flags
     (state-classes state) (state-gc-managed-types state))
    (values (order-packages-by-use (state-packages-to-create state))
            (state-functions state)
            (state-symbols state)
            (state-classes state)
            (state-gc-managed-types state)
            (state-enums state)
            (state-pregcstartups state)
            (state-initializers state)
            (state-exposes state)
            (state-terminators state))))
