(in-package #:koga)

(defun read-file-form (path &optional tag)
  (if tag
      (read-from-string (run-program-capture (format nil "git show ~a:~a" tag path)))
      (uiop:read-file-form path)))

(defun archive (&rest initargs
                &key archive extensions tag
                &allow-other-keys)
  (declare (ignore initargs))
  (let* ((version (getf (read-file-form #P"version.sexp" tag) :version))
         (prefix (format nil "clasp-~a/" version))
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
    (run-program (format nil "gzip ~a" tar-name))
    (generate-ebuild version gz-name)))

(defun generate-ebuild (&optional (version "9999") archive)
  (labels ((mk-os-path (&rest args)
             ;; args: (full-os-path) | (dir fname) | (dir base ext)
             (format nil "~<~^~a~^/~a~^.~a~:>" args))
           (os-path-fname (path)
             (nth-value 2 (uiop:split-unix-namestring-directory-components path)))
           (os->cl-path (os-path)
             (uiop:ensure-absolute-pathname os-path (uiop:getcwd)))
           (fbytes (os-path)
             (or (with-open-file (s (os->cl-path os-path) :element-type 'unsigned-byte
                                                          :if-does-not-exist nil)
                   (when s (file-length s)))
                 0))
           (calc-hash (os-path hash-type)
             (subseq (uiop:run-program (format nil "cksum -a ~a --untagged \"~a\"" os-path hash-type)
                                       :output :string)
                     0 128))
           (copy-f (src-os-path dest-os-path)
             (let ((s-path (os->cl-path src-os-path))
                   (d-path (os->cl-path dest-os-path)))
               (uiop:ensure-all-directories-exist (list d-path))
               (uiop:delete-file-if-exists d-path)
               (uiop:copy-file s-path d-path))))
    (let* ((src-dir "gentoo")
           (dest-dir "gentoo/clasp")
           (manifest-name "Manifest")
           (pkg-ver (concatenate 'string "clasp-" version))
           ;; Files to copy: each entry (manifest-label source dest)
           (flist `(,@(unless (equal version "9999")
                        `(("EBUILD" ,(mk-os-path src-dir "ebuild.template")
                                    ,(mk-os-path dest-dir pkg-ver "ebuild"))))
                    ("EBUILD" ,(mk-os-path src-dir "ebuild.template")
                              ,(mk-os-path dest-dir "clasp-9999.ebuild"))
                    ("MISC" ,(mk-os-path src-dir "metadata.xml")
                            ,(mk-os-path dest-dir "metadata.xml"))
                    ,@(when archive `(("DIST" nil ,(mk-os-path archive))))))
           ;; --
           (manifest-path (os->cl-path (mk-os-path dest-dir manifest-name)))
           (manifest-temp-path (uiop:tmpize-pathname manifest-path)))
      (message nil "Creating Gentoo/Portage distro files for ~a..." pkg-ver)
      (uiop:ensure-all-directories-exist (list manifest-temp-path))
      (with-open-file (mfst-dest manifest-temp-path :direction :output :if-exists :supersede)
        ;; Copy existing Manifest into temp file, dropping entries we're about
        ;; to create.
        (with-open-file (mfst-src manifest-path :direction :input :if-does-not-exist nil)
          (when mfst-src
            (loop
              for line = (read-line mfst-src nil nil)
              while line
              ;; Omit manifest entries we're about to create
              unless (some (lambda (x) (search (os-path-fname (third x)) line)) flist)
                do (write-line line mfst-dest))))
        ;; Copy files, add them to manifest
        (loop for (mfst-type s-os-path  d-os-path) in flist
              when s-os-path
                do (copy-f s-os-path d-os-path)
              do (with-standard-io-syntax
                   (format mfst-dest "~a ~a ~d BLAKE2B ~a SHA512 ~a~%"
                           mfst-type
                           (os-path-fname d-os-path)
                           (fbytes d-os-path)
                           (calc-hash "blake2b" d-os-path)
                           (calc-hash "sha512" d-os-path)))))
      (uiop:rename-file-overwriting-target manifest-temp-path manifest-path)
      manifest-path)))
