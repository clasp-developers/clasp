(require :asdf)

(defparameter *git* nil)

(defun check-repo (&key directory repository &allow-other-keys)
  (format t "~:[Did not find~;Found~] ~A clone in ~A, assuming everything is okay.~%"
          (probe-file directory) repository directory))

(defun sync-repo (help &key directory repository branch commit &allow-other-keys
                  &aux (exists (probe-file directory)))
  (unless (and help exists)
    (cond (exists
           (format t "Fetching ~A~%" repository)
           (uiop:run-program (list *git* "fetch" "--quiet")
                             :output :interactive
                             :error-output :output
                             :directory directory))
         (t
          (format t "Cloning ~A~%" repository)
          (uiop:run-program (list *git* "clone" repository (namestring directory))
                            :output :interactive
                            :error-output :output)))
    (when (or commit branch)
      (format t "Checking out ~A from ~A~%" (or commit branch) repository)
      (uiop:run-program (list *git* "checkout" "--quiet" (or commit branch))
                        :output :interactive
                        :error-output :output
                        :directory directory))
    (when (and branch (not commit))
      (format t "Fast forwarding to origin/~A from ~A~%" branch repository)
      (uiop:run-program (list *git* "merge" "--ff-only" (format nil "origin/~A" branch))
                        :output :interactive
                        :error-output :output
                        :directory directory))))

(defun split-keywords (value)
  (if (stringp value)
      (loop with end = (length value)
            for left = 0 then (1+ right)
            for right = (or (position #\, value :start left) end)
            collect (intern (string-upcase (subseq value left right)) "KEYWORD")
            until (>= right end))
      value))

(defparameter +option-parsers+
  (list :extensions #'split-keywords
        :skip-sync #'split-keywords))

(defun parse-string-option (arg start eq-pos)
  (let ((name (intern (string-upcase (subseq arg start eq-pos))
                      "KEYWORD")))
    (list name (funcall (getf +option-parsers+
                               name
                               #'identity)
                        (subseq arg (1+ eq-pos))))))

(defun parse-boolean-option (arg start)
  (if (and (>= (length arg) (+ 3 start))
           (char= #\n (char arg start))
           (char= #\o (char arg (1+ start)))
           (char= #\- (char arg (+ 2 start))))
    (list (intern (string-upcase (subseq arg (+ 3 start))) "KEYWORD")
          nil)
    (list (intern (string-upcase (subseq arg start)) "KEYWORD")
          t)))

(defun parse-command-line-arguments ()
  (loop for arg in (uiop:command-line-arguments)
        for start = (position-if (lambda (x)
                                   (not (char= #\- x)))
                                 arg)
        for eq-pos = (position #\= arg)
        when eq-pos
          append (parse-string-option arg start eq-pos)
        else
          append (parse-boolean-option arg start)))

(let* ((initargs (nconc (parse-command-line-arguments)
                        (ignore-errors (uiop:read-file-form #P"config.sexp"))
                        (ignore-errors (uiop:read-file-form #P"version.sexp"))))
       (*git* (getf initargs :git "git"))
       (build (getf initargs :build-path "build/"))
       (extensions (getf initargs :extensions))
       (skip-sync (getf initargs :skip-sync))
       (help (getf initargs :help)))
  ;; Get all the external dependencies
  (unless help
    (format t "Synchronizing external repositories~%~%"))
  (loop for source in (uiop:read-file-form #P"repos.sexp")
        for name = (getf source :name)
        for extension = (getf source :extension)
        if (or (eq t skip-sync)
               (member name skip-sync))
          do (apply #'check-repo source)
        else if (or (not extension)
                    (member extension extensions))
          do (apply #'sync-repo help source)
        unless help
          do (terpri))
  (when (and (not help)
             (getf initargs :clean))
    (format t "Cleaning up previous build~%~%")
    (uiop:delete-directory-tree (truename build)
                                :validate t
                                :if-does-not-exist :ignore))
  ;; Do the absolute minimum to inform ASDF about the location of systems
  ;; in order to find the clasp root and the desired build directory.
  (asdf:initialize-source-registry
    `(:source-registry (:tree ,(uiop:getcwd))
                       :inherit-configuration))
  (asdf:initialize-output-translations
    `(:output-translations (t (,(merge-pathnames (merge-pathnames #P"host-fasl/" build)
                                                 (uiop:getcwd))
                               :implementation))
                           :inherit-configuration))
  (asdf:load-system :koga)
  (apply #'uiop:symbol-call "KOGA" (if help "HELP" "SETUP") initargs))