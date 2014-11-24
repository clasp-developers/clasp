(defun build-and-draw-ast (filename code)
  (let ((ast (cleavir-generate-ast:generate-ast code *clasp-env*)))
    (cleavir-ast-graphviz:draw-ast ast filename)
    ast))

(defun build-and-draw-hir (filename code)
  (let* ((ast (cleavir-generate-ast:generate-ast code *clasp-env*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ir-graphviz:draw-flowchart hir stream))))

(defun generate-asts-for-clasp-source (start end)
  (let* ((parts (core:select-source-files end :first-file start :system core:*init-files*))
	 (pathnames (mapcar (lambda (part) (core:lisp-source-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
	 do (with-open-file (stream file :direction :input)
	      (loop for form = (read stream nil eof)
		 until (eq form eof)
		 do (format t "FORM: ~a~%" form)
		 do (let ((ast (cleavir-generate-ast:generate-ast form *clasp-env*)))))))))


(defun generate-hir-for-clasp-source (start end &key skip-errors)
  (let* ((parts (core:select-source-files end :first-file start :system core:*init-files*))
	 (pathnames (mapcar (lambda (part) (core:lisp-source-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
	 do (with-open-file (stream file :direction :input)
	      (loop for form = (read stream nil eof)
		 until (eq form eof)
		 do (format t "FORM: ~a~%" form)
		 do (let* ((ast (cleavir-generate-ast:generate-ast form *clasp-env*))
			   (hir (cleavir-ast-to-hir:compile-toplevel ast)))))))))
