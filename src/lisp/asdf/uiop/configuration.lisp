;;;; ---------------------------------------------------------------------------
;;;; Generic support for configuration files

(uiop/package:define-package :uiop/configuration
  (:nicknames :asdf/configuration)
  (:recycle :uiop/configuration :asdf/configuration :asdf)
  (:use :uiop/common-lisp :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image :uiop/lisp-build)
  (:export
   #:get-folder-path
   #:user-configuration-directories #:system-configuration-directories
   #:in-first-directory
   #:in-user-configuration-directory #:in-system-configuration-directory
   #:validate-configuration-form #:validate-configuration-file #:validate-configuration-directory
   #:configuration-inheritance-directive-p
   #:report-invalid-form #:invalid-configuration #:*ignored-configuration-form* #:*user-cache*
   #:*clear-configuration-hook* #:clear-configuration #:register-clear-configuration-hook
   #:resolve-location #:location-designator-p #:location-function-p #:*here-directory*
   #:resolve-relative-location #:resolve-absolute-location #:upgrade-configuration))
(in-package :uiop/configuration)

(with-upgradability ()
  (define-condition invalid-configuration ()
    ((form :reader condition-form :initarg :form)
     (location :reader condition-location :initarg :location)
     (format :reader condition-format :initarg :format)
     (arguments :reader condition-arguments :initarg :arguments :initform nil))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~? (will be skipped)~@:>")
                       (condition-format c)
                       (list* (condition-form c) (condition-location c)
                              (condition-arguments c))))))

  (defun get-folder-path (folder)
    "Semi-portable implementation of a subset of LispWorks' sys:get-folder-path,
this function tries to locate the Windows FOLDER for one of
:LOCAL-APPDATA, :APPDATA or :COMMON-APPDATA."
    (or #+(and lispworks mswindows) (sys:get-folder-path folder)
        ;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
        (ecase folder
          (:local-appdata (getenv-absolute-directory "LOCALAPPDATA"))
          (:appdata (getenv-absolute-directory "APPDATA"))
          (:common-appdata (or (getenv-absolute-directory "ALLUSERSAPPDATA")
                               (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/"))))))

  (defun user-configuration-directories ()
    "Determine user configuration directories"
    (let ((dirs
            `(,@(when (os-unix-p)
                  (cons
                   (subpathname* (getenv-absolute-directory "XDG_CONFIG_HOME") "common-lisp/")
                   (loop :for dir :in (getenv-absolute-directories "XDG_CONFIG_DIRS")
                         :collect (subpathname* dir "common-lisp/"))))
              ,@(when (os-windows-p)
                  `(,(subpathname* (get-folder-path :local-appdata) "common-lisp/config/")
                    ,(subpathname* (get-folder-path :appdata) "common-lisp/config/")))
              ,(subpathname (user-homedir-pathname) ".config/common-lisp/"))))
      (remove-duplicates (remove-if-not #'absolute-pathname-p dirs)
                         :from-end t :test 'equal)))

  (defun system-configuration-directories ()
    "Determine system user configuration directories"
    (cond
      ((os-unix-p) '(#p"/etc/common-lisp/"))
      ((os-windows-p)
       (if-let (it (subpathname* (get-folder-path :common-appdata) "common-lisp/config/"))
         (list it)))))

  (defun in-first-directory (dirs x &key (direction :input))
    "Determine system user configuration directories"
    (loop :with fun = (ecase direction
                        ((nil :input :probe) 'probe-file*)
                        ((:output :io) 'identity))
          :for dir :in dirs
          :thereis (and dir (funcall fun (subpathname (ensure-directory-pathname dir) x)))))

  (defun in-user-configuration-directory (x &key (direction :input))
    "return pathname under user configuration directory, subpathname X"
    (in-first-directory (user-configuration-directories) x :direction direction))
  (defun in-system-configuration-directory (x &key (direction :input))
    "return pathname under system configuration directory, subpathname X"
    (in-first-directory (system-configuration-directories) x :direction direction))

  (defun configuration-inheritance-directive-p (x)
    "Is X a configuration inheritance directive?"
    (let ((kw '(:inherit-configuration :ignore-inherited-configuration)))
      (or (member x kw)
          (and (length=n-p x 1) (member (car x) kw)))))

  (defun report-invalid-form (reporter &rest args)
    "Report an invalid form according to REPORTER and various ARGS"
    (etypecase reporter
      (null
       (apply 'error 'invalid-configuration args))
      (function
       (apply reporter args))
      ((or symbol string)
       (apply 'error reporter args))
      (cons
       (apply 'apply (append reporter args)))))

  (defvar *ignored-configuration-form* nil
    "Have configuration forms been ignored while parsing the configuration?")

  (defun validate-configuration-form (form tag directive-validator
                                            &key location invalid-form-reporter)
    "Validate a configuration FORM"
    (unless (and (consp form) (eq (car form) tag))
      (setf *ignored-configuration-form* t)
      (report-invalid-form invalid-form-reporter :form form :location location)
      (return-from validate-configuration-form nil))
    (loop :with inherit = 0 :with ignore-invalid-p = nil :with x = (list tag)
          :for directive :in (cdr form)
          :when (cond
                  ((configuration-inheritance-directive-p directive)
                   (incf inherit) t)
                  ((eq directive :ignore-invalid-entries)
                   (setf ignore-invalid-p t) t)
                  ((funcall directive-validator directive)
                   t)
                  (ignore-invalid-p
                   nil)
                  (t
                   (setf *ignored-configuration-form* t)
                   (report-invalid-form invalid-form-reporter :form directive :location location)
                   nil))
            :do (push directive x)
          :finally
             (unless (= inherit 1)
               (report-invalid-form invalid-form-reporter
                                    :form form :location location
                                    ;; we throw away the form and location arguments, hence the ~2*
                                    ;; this is necessary because of the report in INVALID-CONFIGURATION
                                    :format (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]. ~
                                                        One and only one of ~S or ~S is required.~@:>")
                                    :arguments '(:inherit-configuration :ignore-inherited-configuration)))
             (return (nreverse x))))

  (defun validate-configuration-file (file validator &key description)
    "Validate a configuration file for conformance of its form with the validator function"
    (let ((forms (read-file-forms file)))
      (unless (length=n-p forms 1)
        (error (compatfmt "~@<One and only one form allowed for ~A. Got: ~3i~_~S~@:>~%")
               description forms))
      (funcall validator (car forms) :location file)))

  (defun validate-configuration-directory (directory tag validator &key invalid-form-reporter)
    "Map the VALIDATOR across the .conf files in DIRECTORY, the TAG will
be applied to the results to yield a configuration form.  Current
values of TAG include :source-registry and :output-translations."
    (let ((files (sort (ignore-errors ;; SORT w/o COPY-LIST is OK: DIRECTORY returns a fresh list
                        (remove-if
                         'hidden-pathname-p
                         (directory* (make-pathname :name *wild* :type "conf" :defaults directory))))
                       #'string< :key #'namestring)))
      `(,tag
        ,@(loop :for file :in files :append
                                    (loop :with ignore-invalid-p = nil
                                          :for form :in (read-file-forms file)
                                          :when (eq form :ignore-invalid-entries)
                                            :do (setf ignore-invalid-p t)
                                          :else
                                            :when (funcall validator form)
                                              :collect form
                                          :else
                                            :when ignore-invalid-p
                                              :do (setf *ignored-configuration-form* t)
                                          :else
                                            :do (report-invalid-form invalid-form-reporter :form form :location file)))
        :inherit-configuration)))

  (defun resolve-relative-location (x &key ensure-directory wilden)
    "Given a designator X for an relative location, resolve it to a pathname"
    (ensure-pathname
     (etypecase x
       (pathname x)
       (string (parse-unix-namestring
                x :ensure-directory ensure-directory))
       (cons
        (if (null (cdr x))
            (resolve-relative-location
             (car x) :ensure-directory ensure-directory :wilden wilden)
            (let* ((car (resolve-relative-location
                         (car x) :ensure-directory t :wilden nil)))
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               car))))
       ((eql :*/) *wild-directory*)
       ((eql :**/) *wild-inferiors*)
       ((eql :*.*.*) *wild-file*)
       ((eql :implementation)
        (parse-unix-namestring
         (implementation-identifier) :ensure-directory t))
       ((eql :implementation-type)
        (parse-unix-namestring
         (string-downcase (implementation-type)) :ensure-directory t))
       ((eql :hostname)
        (parse-unix-namestring (hostname) :ensure-directory t)))
     :wilden (and wilden (not (pathnamep x)) (not (member x '(:*/ :**/ :*.*.*))))
     :want-relative t))

  (defvar *here-directory* nil
    "This special variable is bound to the currect directory during calls to
PROCESS-SOURCE-REGISTRY in order that we be able to interpret the :here
directive.")

  (defvar *user-cache* nil
    "A specification as per RESOLVE-LOCATION of where the user keeps his FASL cache")

  (defun compute-user-cache ()
    "Compute the location of the default user-cache for translate-output objects"
    (setf *user-cache*
          (flet ((try (x &rest sub) (and x `(,x ,@sub))))
            (or
             (try (getenv-absolute-directory "XDG_CACHE_HOME") "common-lisp" :implementation)
             (when (os-windows-p)
               (try (or (get-folder-path :local-appdata)
                        (get-folder-path :appdata))
                    "common-lisp" "cache" :implementation))
             '(:home ".cache" "common-lisp" :implementation)))))
  (register-image-restore-hook 'compute-user-cache)

  (defun resolve-absolute-location (x &key ensure-directory wilden)
    "Given a designator X for an absolute location, resolve it to a pathname"
    (ensure-pathname
     (etypecase x
       (pathname x)
       (string
        (let ((p #-mcl (parse-namestring x)
                 #+mcl (probe-posix x)))
          #+mcl (unless p (error "POSIX pathname ~S does not exist" x))
          (if ensure-directory (ensure-directory-pathname p) p)))
       (cons
        (return-from resolve-absolute-location
          (if (null (cdr x))
              (resolve-absolute-location
               (car x) :ensure-directory ensure-directory :wilden wilden)
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               (resolve-absolute-location
                (car x) :ensure-directory t :wilden nil)))))
       ((eql :root)
        ;; special magic! we return a relative pathname,
        ;; but what it means to the output-translations is
        ;; "relative to the root of the source pathname's host and device".
        (return-from resolve-absolute-location
          (let ((p (make-pathname* :directory '(:relative))))
            (if wilden (wilden p) p))))
       ((eql :home) (user-homedir-pathname))
       ((eql :here) (resolve-absolute-location
                     (or *here-directory* (pathname-directory-pathname (load-pathname)))
                     :ensure-directory t :wilden nil))
       ((eql :user-cache) (resolve-absolute-location
                           *user-cache* :ensure-directory t :wilden nil)))
     :wilden (and wilden (not (pathnamep x)))
     :resolve-symlinks *resolve-symlinks*
     :want-absolute t))

  ;; Try to override declaration in previous versions of ASDF.
  (declaim (ftype (function (t &key (:directory boolean) (:wilden boolean)
                               (:ensure-directory boolean)) t) resolve-location))

  (defun* (resolve-location) (x &key ensure-directory wilden directory)
    "Resolve location designator X into a PATHNAME"
    ;; :directory backward compatibility, until 2014-01-16: accept directory as well as ensure-directory
    (loop* :with dirp = (or directory ensure-directory)
           :with (first . rest) = (if (atom x) (list x) x)
           :with path = (resolve-absolute-location
                         first :ensure-directory (and (or dirp rest) t)
                               :wilden (and wilden (null rest)))
           :for (element . morep) :on rest
           :for dir = (and (or morep dirp) t)
           :for wild = (and wilden (not morep))
           :for sub = (merge-pathnames*
                       (resolve-relative-location
                        element :ensure-directory dir :wilden wild)
                       path)
           :do (setf path (if (absolute-pathname-p sub) (resolve-symlinks* sub) sub))
           :finally (return path)))

  (defun location-designator-p (x)
    "Is X a designator for a location?"
    (flet ((absolute-component-p (c)
             (typep c '(or string pathname
                        (member :root :home :here :user-cache))))
           (relative-component-p (c)
             (typep c '(or string pathname
                        (member :*/ :**/ :*.*.* :implementation :implementation-type)))))
      (or (typep x 'boolean)
          (absolute-component-p x)
          (and (consp x) (absolute-component-p (first x)) (every #'relative-component-p (rest x))))))

  (defun location-function-p (x)
    "Is X the specification of a location function?"
    (and
     (length=n-p x 2)
     (eq (car x) :function)))

  (defvar *clear-configuration-hook* '())

  (defun register-clear-configuration-hook (hook-function &optional call-now-p)
    "Register a function to be called when clearing configuration"
    (register-hook-function '*clear-configuration-hook* hook-function call-now-p))

  (defun clear-configuration ()
    "Call the functions in *CLEAR-CONFIGURATION-HOOK*"
    (call-functions *clear-configuration-hook*))

  (register-image-dump-hook 'clear-configuration)

  (defun upgrade-configuration ()
    "If a previous version of ASDF failed to read some configuration, try again now."
    (when *ignored-configuration-form*
      (clear-configuration)
      (setf *ignored-configuration-form* nil))))


