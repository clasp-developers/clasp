(defpackage "CCMP-TEST"
  (:use "COMMON-LISP" "CORE" "EXT")
  (:nicknames "CT"))

(in-package "CCMP-TEST")

(defun view-dot-file (filename)
  (let* ((dot-pathname (pathname filename))
	 (dot-namestring (namestring (translate-logical-pathname dot-pathname)))
	 (png-pathname (make-pathname :type "png" :defaults dot-pathname))
	 (png-namestring (namestring (translate-logical-pathname png-pathname)))
	 (dot-cmd (format nil "dot -Tpng -o~a ~a" png-namestring dot-namestring)))
    (core:system dot-cmd)
    (let ((open-cmd (format nil "open ~a" png-namestring)))
      (core:system open-cmd))))
  



(defun build-and-draw-ast (code &optional (filename "/tmp/_ast.dot") &key save-only)
  (let ((ast (cleavir-generate-ast:generate-ast code cleavir-cmp:*clasp-env*)))
    (cleavir-ast-graphviz:draw-ast ast filename)
    (unless save-only (view-dot-file filename))
    ast))

(defun build-and-draw-hir (code &optional (filename "/tmp/_hir.dot") &key save-only)
  (let* ((ast (cleavir-generate-ast:generate-ast code cleavir-cmp:*clasp-env*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ir-graphviz:draw-flowchart hir stream))
    (unless save-only (view-dot-file filename))
    hir))



(defun generate-asts-for-clasp-source (start end)
  (let* ((parts (core:select-source-files end :first-file start :system core:*init-files*))
	 (pathnames (mapcar (lambda (part) (core:lisp-source-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
	 do (with-open-file (stream file :direction :input)
	      (loop for form = (read stream nil eof)
		 until (eq form eof)
		 do (format t "FORM: ~a~%" form)
		 do (let ((ast (cleavir-generate-ast:generate-ast form cleavir-cmp:*clasp-env*)))))))))


(defun generate-hir-for-clasp-source (start end &key skip-errors)
  (let* ((parts (core:select-source-files end :first-file start :system core:*init-files*))
	 (pathnames (mapcar (lambda (part) (core:lisp-source-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
	 do (with-open-file (stream file :direction :input)
	      (loop for form = (read stream nil eof)
		 until (eq form eof)
		 do (format t "FORM: ~a~%" form)
		 do (let* ((ast (cleavir-generate-ast:generate-ast form cleavir-cmp:*clasp-env*))
			   (hir (cleavir-ast-to-hir:compile-toplevel ast)))))))))
