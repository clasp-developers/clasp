(require :asdf)

(defparameter *git* nil)

(defun sync-repo (&key directory repository branch commit &allow-other-keys)
  (cond ((probe-file directory)
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
                      :directory directory)))

(defun remove-repo (&key directory repository &allow-other-keys)
  (cond ((probe-file directory)
         (format t "Removing clone of ~A at ~A~%" repository directory)
         (uiop:delete-directory-tree (truename directory)
                                     :validate t))
        (t
         (format t "Nothing to be done for ~A~%" repository))))

(defun split-keywords (value)
  (if (stringp value)
      (loop with end = (length value)
            for left = 0 then (1+ right)
            for right = (or (position #\, value :start left) end)
            collect (intern (string-upcase (subseq value left right)) "KEYWORD")
            until (>= right end))
      value))

(defparameter +option-parsers+
  (list :extensions #'split-keywords))

(defun optionp (arg)
  (and (plusp (length arg))
       (char= #\- (char arg 0))))

(defun option-name (arg)
  (intern (string-upcase (subseq arg
                                 (position-if (lambda (x)
                                                (not (char= #\- x)))
                                              arg)))
          "KEYWORD"))

(defun parse-command-line-arguments ()
  (loop with previous-option = nil
        for arg in (uiop:command-line-arguments)
        for optionp = (optionp arg)
        when previous-option
          collect (or optionp (funcall (getf +option-parsers+
                                             previous-option
                                             #'identity)
                                       arg))
            into options
        if optionp
          do (setf previous-option (option-name arg)) and
          collect previous-option into options
        else if previous-option
          do (setf previous-option nil)
        else
          collect arg into parameters
        finally (return (values (if (evenp (length options))
                                    options
                                    (nconc options (list t)))
                                parameters))))

(let* ((initargs (nconc (parse-command-line-arguments)
                        (ignore-errors (uiop:read-file-form #P"config.sexp"))))
       (*git* (getf initargs :git "git"))
       (build (getf initargs :build-path "build/"))
       (extensions (getf initargs :extensions))
       (code (uiop:getcwd)))
  ;; Get all the external dependencies
  (format t "Synchronizing external repositories~%~%")
  (loop for source in (uiop:read-file-form #P"repos.sexp")
        for extension = (getf source :extension)
        if (or (not extension)
               (member extension extensions))
          do (apply #'sync-repo source)
        else
          do (apply #'remove-repo source)
        do (terpri))
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
  (apply #'uiop:symbol-call "KOGA" "SETUP" initargs))
  
