
(defun save-archive (obj pathname)
  "Serialize obj into an archive and write the archive to the pathname"
  (let ((archive (core:make-sexp-save-archive)))
    (core:put archive :only obj)
    (with-open-file (fout pathname :direction :output :if-exists :superscede)
      (core:sexp-save-archive-write archive fout)))
  (print "Leaving save-archive")
  (gdb "leaving")
  nil)

