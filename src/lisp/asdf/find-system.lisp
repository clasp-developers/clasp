;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/cache :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:coerce-name #:primary-system-name #:coerce-filename
   #:find-system #:locate-system #:load-asd
   #:system-registered-p #:register-system #:registered-systems #:clear-system #:map-systems
   #:missing-component #:missing-requires #:missing-parent
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:find-system-if-being-defined
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:clear-defined-system #:clear-defined-systems #:*defined-systems*
   #:*immutable-systems*
   ;; defined in source-registry, but specially mentioned here:
   #:initialize-source-registry #:sysdef-source-registry-search))
(in-package :asdf/find-system)

(with-upgradability ()
  (declaim (ftype (function (&optional t) t) initialize-source-registry)) ; forward reference

  (define-condition missing-component (system-definition-error)
    ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
     (parent :initform nil :reader missing-parent :initarg :parent)))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments))

  (defun coerce-name (name)
    (typecase name
      (component (component-name name))
      (symbol (string-downcase (symbol-name name)))
      (string name)
      (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

  (defun primary-system-name (name)
    ;; When a system name has slashes, the file with defsystem is named by
    ;; the first of the slash-separated components.
    (first (split-string (coerce-name name) :separator "/")))

  (defun coerce-filename (name)
    (frob-substrings (coerce-name name) '("/" ":" "\\") "--"))

  (defvar *defined-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings, being the
names of the systems, and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.")

  (defun system-registered-p (name)
    (gethash (coerce-name name) *defined-systems*))

  (defun registered-systems ()
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :collect (coerce-name (cdr registered))))

  (defun register-system (system)
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering ~3i~_~A~@:>~%") system)
      (unless (eq system (cdr (gethash name *defined-systems*)))
        (setf (gethash name *defined-systems*)
              (cons (if-let (file (ignore-errors (system-source-file system)))
                      (get-file-stamp file))
                    system)))))

  (defun clear-defined-system (system)
    (let ((name (coerce-name system)))
      (remhash name *defined-systems*)
      (unset-asdf-cache-entry `(locate-system ,name))
      (unset-asdf-cache-entry `(find-system ,name))
      nil))

  (defun clear-defined-systems ()
    ;; Invalidate all systems but ASDF itself, if registered.
    (loop :for name :being :the :hash-keys :of *defined-systems*
          :unless (equal name "asdf")
            :do (clear-defined-system name)))

  (register-hook-function '*post-upgrade-cleanup-hook* 'clear-defined-systems nil)

  (defun clear-system (name)
    "Clear the entry for a system in the database of systems previously loaded.
Note that this does NOT in any way cause the code of the system to be unloaded."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (remhash (coerce-name name) *defined-systems*))

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :do (funcall fn (cdr registered)))))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-
(with-upgradability ()
  (defvar *system-definition-search-functions* '())

  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           (remove-if #'(lambda (x) (member x '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
                      *system-definition-search-functions*)
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever does that
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  (defun search-for-system-definition (system)
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*)
        (try 'sysdef-preloaded-system-search))))

  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.
")

  (defun probe-asd (name defaults &key truename)
    (block nil
      (when (directory-pathname-p defaults)
        (if-let (file (probe-file*
                       (ensure-absolute-pathname
                        (parse-unix-namestring name :type "asd")
                        #'(lambda () (ensure-absolute-pathname defaults 'get-pathname-defaults nil))
                        nil)
                       :truename truename))
          (return file))
        #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
        (when (and (os-windows-p) (physical-pathname-p defaults))
          (let ((shortcut
                  (make-pathname
                   :defaults defaults :case :local
                   :name (strcat name ".asd")
                   :type "lnk")))
            (when (probe-file* shortcut)
              (ensure-pathname (parse-windows-shortcut shortcut) :namestring :native)))))))

  (defun sysdef-central-registry-search (system)
    (let ((name (primary-system-name system))
          (to-remove nil)
          (to-replace nil))
      (block nil
        (unwind-protect
             (dolist (dir *central-registry*)
               (let ((defaults (eval dir))
                     directorized)
                 (when defaults
                   (cond ((directory-pathname-p defaults)
                          (let* ((file (probe-asd name defaults :truename *resolve-symlinks*)))
                            (when file
                              (return file))))
                         (t
                          (restart-case
                              (let* ((*print-circle* nil)
                                     (message
                                       (format nil
                                               (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not an absolute directory.~@:>")
                                               system dir defaults)))
                                (error message))
                            (remove-entry-from-registry ()
                              :report "Remove entry from *central-registry* and continue"
                              (push dir to-remove))
                            (coerce-entry-to-directory ()
                              :test (lambda (c) (declare (ignore c))
                                      (and (not (directory-pathname-p defaults))
                                           (directory-pathname-p
                                            (setf directorized
                                                  (ensure-directory-pathname defaults)))))
                              :report (lambda (s)
                                        (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                                directorized dir))
                              (push (cons dir directorized) to-replace))))))))
          ;; cleanup
          (dolist (dir to-remove)
            (setf *central-registry* (remove dir *central-registry*)))
          (dolist (pair to-replace)
            (let* ((current (car pair))
                   (new (cdr pair))
                   (position (position current *central-registry*)))
              (setf *central-registry*
                    (append (subseq *central-registry* 0 position)
                            (list new)
                            (subseq *central-registry* (1+ position))))))))))

  (defvar *preloaded-systems* (make-hash-table :test 'equal))

  (defun make-preloaded-system (name keys)
    (apply 'make-instance (getf keys :class 'system)
           :name name :source-file (getf keys :source-file)
           (remove-plist-keys '(:class :name :source-file) keys)))

  (defun sysdef-preloaded-system-search (requested)
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun register-preloaded-system (system-name &rest keys)
    (setf (gethash (coerce-name system-name) *preloaded-systems*) keys))

  (dolist (s '("asdf" "uiop" "asdf-driver" "asdf-defsystem" "asdf-package-system"))
    ;; don't bother with these, no one relies on them: "asdf-utils" "asdf-bundle"
    (register-preloaded-system s :version *asdf-version*))

  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; notable side effect: mark the system as being defined, to avoid infinite loops
    (first (gethash `(find-system ,(coerce-name name)) *asdf-cache*)))

  (defun load-asd (pathname &key name (external-format (encoding-external-format (detect-encoding pathname))) &aux (readtable *readtable*) (print-pprint-dispatch *print-pprint-dispatch*))
    ;; Tries to load system definition with canonical NAME from PATHNAME.
    (with-asdf-cache ()
      (with-standard-io-syntax
        (let ((*package* (find-package :asdf-user))
              ;; Note that our backward-compatible *readtable* is
              ;; a global readtable that gets globally side-effected. Ouch.
              ;; Same for the *print-pprint-dispatch* table.
              ;; We should do something about that for ASDF3 if possible, or else ASDF4.
              (*readtable* readtable)
              (*print-pprint-dispatch* print-pprint-dispatch)
              (*print-readably* nil)
              (*default-pathname-defaults*
                ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
                (pathname-directory-pathname (physicalize-pathname pathname))))
          (handler-bind
              ((error #'(lambda (condition)
                          (error 'load-system-definition-error
                                 :name name :pathname pathname
                                 :condition condition))))
            (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                          name pathname)
            (load* pathname :external-format external-format))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  (defun check-not-old-asdf-system (name pathname)
    (or (not (equal name "asdf"))
        (null pathname)
        (let* ((version-pathname (subpathname pathname "version.lisp-expr"))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname)))
               (old-version (asdf-version)))
          (cond
            ((version< old-version version) t) ;; newer version: good!
            ((equal old-version version) nil) ;; same version: don't load, but don't warn
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                 (let ((old-pathname
                         (if-let (pair (system-registered-p "asdf"))
                           (system-source-file (cdr pair)))))
                   (warn "~@<~
        You are using ASDF version ~A ~:[(probably from (require \"asdf\") ~
        or loaded by quicklisp)~;from ~:*~S~] and have an older version of ASDF ~
        ~:[(and older than 2.27 at that)~;~:*~A~] registered at ~S. ~
        Having an ASDF installed and registered is the normal way of configuring ASDF to upgrade itself, ~
        and having an old version registered is a configuration error. ~
        ASDF will ignore this configured system rather than downgrade itself. ~
        In the future, you may want to either: ~
        (a) upgrade this configured ASDF to a newer version, ~
        (b) install a newer ASDF and register it in front of the former in your configuration, or ~
        (c) uninstall or unregister this and any other old version of ASDF from your configuration. ~
        Note that the older ASDF might be registered implicitly through configuration inherited ~
        from your system installation, in which case you might have to specify ~
        :ignore-inherited-configuration in your in your ~~/.config/common-lisp/source-registry.conf ~
        or other source-registry configuration file, environment variable or lisp parameter. ~
        Indeed, a likely offender is an obsolete version of the cl-asdf debian or ubuntu package, ~
        that you might want to upgrade (if a recent enough version is available) ~
        or else remove altogether (since most implementations ship with a recent asdf); ~
        if you lack the system administration rights to upgrade or remove this package, ~
        then you might indeed want to either install and register a more recent version, ~
        or use :ignore-inherited-configuration to avoid registering the old one. ~
        Please consult ASDF documentation and/or experts.~@:>~%"
                         old-version old-pathname version pathname))))
             nil))))) ;; only issue the warning the first time, but always return nil

  (defvar *immutable-systems* nil
    "An hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

If you deliver an image with many systems precompiled, *and* do not want to check the filesystem
for them every time a user loads an extension, what more risk a problematic upgrade or catastrophic
downgrade, before you dump an image, use:
   (setf asdf::*immutable-systems* (uiop:list-to-hash-set (asdf:already-loaded-systems)))")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (cdr (system-registered-p requested))
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A is in the *immutable-systems* set, ~
but not loaded in memory"
                   :format-arguments (list name))))))

  (defun locate-system (name)
    "Given a system NAME designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed.
PATHNAME when not null is a path from which to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded."
    (let* ((name (coerce-name name))
           (in-memory (system-registered-p name)) ; load from disk if absent or newer on disk
           (previous (cdr in-memory))
           (previous (and (typep previous 'system) previous))
           (previous-time (car in-memory))
           (found (search-for-system-definition name))
           (found-system (and (typep found 'system) found))
           (pathname (ensure-pathname
                      (or (and (typep found '(or pathname string)) (pathname found))
                          (and found-system (system-source-file found-system))
                          (and previous (system-source-file previous)))
                      :want-absolute t :resolve-symlinks *resolve-symlinks*))
           (foundp (and (or found-system pathname previous) t)))
      (check-type found (or null pathname system))
      (unless (check-not-old-asdf-system name pathname)
        (cond
          (previous (setf found nil pathname nil))
          (t
           (setf found (sysdef-preloaded-system-search "asdf"))
           (assert (typep found 'system))
           (setf found-system found pathname nil))))
      (values foundp found-system pathname previous previous-time)))

  (defmethod find-system ((name string) &optional (error-p t))
    (with-asdf-cache (:key `(find-system ,name))
      (let ((primary-name (primary-system-name name)))
        (unless (equal name primary-name)
          (find-system primary-name nil)))
      (or (and *immutable-systems* (gethash name *immutable-systems*)
               (cdr (system-registered-p name)))
          (multiple-value-bind (foundp found-system pathname previous previous-time)
              (locate-system name)
            (assert (eq foundp (and (or found-system pathname previous) t)))
            (let ((previous-pathname (and previous (system-source-file previous)))
                  (system (or previous found-system)))
              (when (and found-system (not previous))
                (register-system found-system))
              (when (and system pathname)
                (setf (system-source-file system) pathname))
              (when (and pathname
                         (let ((stamp (get-file-stamp pathname)))
                           (and stamp
                                (not (and previous
                                          (or (pathname-equal pathname previous-pathname)
                                              (and pathname previous-pathname
                                                   (pathname-equal
                                                    (physicalize-pathname pathname)
                                                    (physicalize-pathname previous-pathname))))
                                          (stamp<= stamp previous-time))))))
                ;; only load when it's a pathname that is different or has newer content, and not an old asdf
                (load-asd pathname :name name)))
            (let ((in-memory (system-registered-p name))) ; try again after loading from disk if needed
              (cond
                (in-memory
                 (when pathname
                   (setf (car in-memory) (get-file-stamp pathname)))
                 (cdr in-memory))
                (error-p
                 (error 'missing-component :requires name))
                (t ;; not found: don't keep negative cache, see lp#1335323
                 (unset-asdf-cache-entry `(locate-system ,name))
                 (return-from find-system nil)))))))))
