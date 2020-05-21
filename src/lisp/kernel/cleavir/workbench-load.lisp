;;; Set up everything to start cleavir

;;; Do you want CST?
(pushnew :cst *features*)
(progn
  (load "sys:kernel;clasp-builder.lsp")
  (defun cleavir-system ()
    (with-open-file (fin "source-dir:tools-for-build;cleavir-file-list.lisp" :direction :input)
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
      (compile-file "sys:kernel;lsp;setf.lsp" :output-file "/tmp/setf.fasl")))
  )

;;; Start cleavir with no inline
(load-cleavir-no-inline)


(let ((clasp-cleavir::*use-closurettes* t))
  (clasp-cleavir:cleavir-compile 'foo '(lambda () (lambda () (list 1 2 3 4)))))


(let ((clasp-cleavir::*use-closurettes* t))
  (compile 'foo '(lambda () (lambda () (list 1 2 3 4)))))



;;; Start cleavir
(start-cleavir)

