(in-package #:koga)

(defun read-file-form (path &optional tag)
  (if tag
      (read-from-string (run-program-capture (format nil "git show ~a:~a" tag path)))
      (uiop:read-file-form path)))

(defun archive (&rest initargs
                &key archive extensions tag
                &allow-other-keys)
  (declare (ignore initargs))
  (let* ((prefix (format nil "clasp-~a/"
                        (getf (read-file-form #P"version.sexp" tag) :version)))
         (tar-name (concatenate 'string (if (stringp archive) archive "archive") ".tar"))
         (gz-name (concatenate 'string tar-name ".gz")))
    (uiop:delete-file-if-exists tar-name)
    (uiop:delete-file-if-exists gz-name)
    (message nil "Creating main archive...")
    (run-program (format nil "git archive --output=~a --prefix=~a --format=tar ~a"
                         tar-name prefix (or tag "HEAD")))
    (uiop:call-with-temporary-file
     (lambda (temp-path)
       (loop for source in (nconc (read-file-form #P"repos.sexp" tag)
                                  (loop for path in (directory #P"repos-*.sexp")
                                        nconc (uiop:read-file-form path)))
             for name = (getf source :name)
             for directory = (getf source :directory)
             for extension = (getf source :extension)
             when (or (not extension)
                      (member extension extensions))
               do (message nil "Creating ~(~a~) archive..." name)
                  (run-program (format nil "git archive --output=~a --format=tar --prefix=~a~a ~a"
                                       temp-path prefix directory
                                       (if tag
                                           (or (getf source :branch)
                                               (getf source :commit))
                                           "HEAD"))
                               :directory directory)
                  (run-program (format nil "tar --concatenate --file ~a ~a"
                                       tar-name temp-path))))
     :want-stream-p nil)
    (message nil "Compressing archive...")
    (run-program (format nil "gzip ~a" tar-name))))
