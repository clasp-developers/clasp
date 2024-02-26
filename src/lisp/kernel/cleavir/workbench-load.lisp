;;; Set up everything to start cleavir

(progn
  (load "sys:src;lisp;kernel;clasp-builder.lisp")
  (defun cleavir-system ()
    (with-open-file (fin "sys:tools-for-build;cleavir-file-list.lisp" :direction :input)
      (read fin)))
  (defun load-cleavir ()
    (let* ((system (cleavir-system))
           (last (position-if (lambda (x) (search "inline-prep" x)) system))
           (subsystem (subseq system 0 last)))
      (format t "subsystem: ~s~%" subsystem)
      (format t "last position: ~s name ~s~%" last (elt system last))
      (core::load-system subsystem)))

  (defun start-cleavir ()
    (let ((system (cleavir-system)))
      (core::load-system system)
      (format t "Cleavir is go~%")))

  (defun load-cleavir-no-inline ()
    (let ((system (cleavir-system)))
      (core::load-system (butlast system 3))))

  (defun compile-stuff ()
    (dotimes (i 50)
      (format t "Compilation #~a~%" i)
      (compile-file "sys:src;lisp;kernel;lsp;setf.lisp" :output-file "/tmp/setf.fasl")))
  )


;;#+(or)
(start-cleavir)

;;; Start cleavir with no inline
#+(or)(progn
  (load-cleavir-no-inline)
  (format t "!!!!!!!!!!! Cleavir loaded~%"))
