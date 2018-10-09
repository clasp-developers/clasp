;;; Configures ASDF to find systems under sys:
;;; Ideally we would use ASDF's configuration API, but sadly, nobody actually
;;; understands how to use it. I just spent an hour trying and I'm fed up.

(format t "Configuring ASDF for local directories~%")
(defun all-subdirs (dir)
  (let (dirs)
    (labels ((trav (d)
               (dolist (d (uiop:subdirectories d))
                 (unless (find ".git" (pathname-directory d) :test #'string=)
                   (push d dirs)
                   (trav d)))))
      (trav dir))
    dirs))

(defun add-all-subdirs-to-asdf-*central-registry* (topdir)
  (let ((dirs (all-subdirs (translate-logical-pathname topdir))))
    (dolist (dir dirs)
      (push dir asdf:*central-registry*))
    (format t "Added ~d subdirectories of ~a to ASDF:*CENTRAL-REGISTRY*~%"
            (length dirs) (translate-logical-pathname #p"sys:"))))

(add-all-subdirs-to-asdf-*central-registry* #p"sys:")
