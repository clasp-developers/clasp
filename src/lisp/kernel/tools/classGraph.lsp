

(defun find-subtree (top )
  (cons top (mapcan #'(lambda (x) (find-subtree x)) (clos::class-direct-subclasses top))))


(defun top-classes ()
  (let ((tops (clos::class-direct-subclasses (find-class 't)))
	clusters)
    (dolist (top tops)
      (let ((one-subtree (remove-duplicates (find-subtree top))))
	(push one-subtree clusters)))
    clusters))

(defconstant +t-class+ (find-class 't))
(defparameter *fout* t)


(defun one-edge (child parent uniqify)
  (unless (gethash child uniqify)
    (setf (gethash child uniqify) child)
    (format *fout* "\"~A\" -> \"~A\";~%" (clos::class-name child) (clos::class-name parent))))

(defun node-label (class-symbol)
  (let ((package (symbol-package class-symbol))
	(name (symbol-name class-symbol)))
    (format nil "~A\\n~A" (package-name package) name)))

(defun node-color (class-symbol)
  (let* ((package-name (string-upcase (package-name (symbol-package class-symbol))))
	 (color (case (intern package-name)
		  (common-lisp "lightgreen")
		  (core "cyan")
		  (ext "yellow")
		  (gray "gray")
		  (clos "palevioletred")
		  (units "orange")
		  (kinematics "pink")
		  (chem "tan")
		  (otherwise "white"))))
    color))


(defun one-node (node)
  (let ((sym (clos::class-name node)))
    (format *fout* "\"~A\" [ label=\"~A\",style=filled,fillcolor=~A];~%" sym (node-label sym) (node-color sym))))

(defun one-cluster (cluster uniqify-edges)
  (format t "Generating subgraph ~A~%" (clos::class-name (car cluster)))
  (format *fout* "subgraph \"cluster-~A\" {~%" (clos::class-name (car cluster)))
  (dolist (node cluster)
    (one-node node)
    (mapc #'(lambda (p) (one-edge node p uniqify-edges)) (clos::class-direct-superclasses node)))
  (format *fout* "}~%"))



(defun dot-graph (outputName)
  (let ((filename (namestring (merge-pathnames (make-pathname :name outputName :type "dot")))))
    (with-open-file (*fout* filename :direction :output)
      (format *fout* "digraph G {~%")
      (format *fout* "rankdir=RL;~%")
      ;;    (format *fout* "  ratio=fill;~%")
      (format *fout* "  size=\"50,50\";~%")
      ;;    (format *fout* "  aspect=1;~%")
      (let ((uniqify-edges (make-hash-table :test #'eq)))
	(dolist (cluster (top-classes))
	  (one-cluster cluster uniqify-edges)))
      (format *fout* "}~%"))))


;;(dot-graph (namestring (merge-pathnames (make-pathname :name "allClasses" :type "dot"))))
