
(defstruct include-file
  source-dir
  include-path)

(defparameter *include-regex* (make-regex "^#include[[:space:]]*\"([^\"]*)\"[[:space:]]*$"))

(defun read-file-for-includes (name includes-table)
  (format t "About to scan file ~a for #include~%" name)
  (let ((source-dir (make-pathname :directory (pathname-directory name))))
    (with-open-file (stream name)
      (loop for line = (read-line stream nil 'foo)
	 until (eq line 'foo)
	 do (let ((match (regex-match *include-regex* line)))
	      (when (regex-match-matched match 0)
		(let* ((rel-path (regex-match-part match 1))
		       (inc-file (make-include-file :source-dir source-dir
						    :include-path (pathname rel-path))))
		  (setf (gethash inc-file includes-table) t))))))))

(defun rewrite-file-with-includes (old-path rewrites &key testing)
  "Return the name of the new file"
  (format t "rewrite-file-with-includes: ~a~%" old-path)
  (let* ((source-dir (make-pathname :directory (pathname-directory old-path)))
	 (old-type (pathname-type old-path))
	 (new-path (make-pathname :type (concatenate 'string old-type "-new") :defaults old-path)))
    (with-open-file (sout new-path :direction :output)
      (with-open-file (sin old-path)
	(loop for line = (read-line sin nil 'all-done)
	   until (eq line 'all-done)
	   do (let ((match (regex-match *include-regex* line)))
		(if (regex-match-matched match 0)
		    (let* ((rel-path (regex-match-part match 1))
			   (inc-file (make-include-file :source-dir source-dir
							:include-path (pathname rel-path)))
			   (new-include (gethash inc-file rewrites)))
		      (format sout "#include <~a>~%" new-include))
		    (format sout "~a~%" line))))))
    (unless testing
      (delete-file old-path)
      (rename-file new-path old-path))
    new-path
    ))



(defvar +source-directories+
  '( 
    #P"/Users/meister/Development/clasp/src/asttooling/"
    #P"/Users/meister/Development/clasp/src/cffi/"
    #P"/Users/meister/Development/clasp/src/clbind/"
    #P"/Users/meister/Development/clasp/src/core/"
    #P"/Users/meister/Development/clasp/src/gctools/"
    #P"/Users/meister/Development/clasp/src/llvmo/"
    #P"/Users/meister/Development/clasp/src/main/"
    #P"/Users/meister/Development/clasp/src/mpip/"
    #P"/Users/meister/Development/clasp/src/serveEvent/"
    #P"/Users/meister/Development/clasp/src/sockets/"))

(defun gather-source-files (dir)
  (let ((header-files (directory (make-pathname :name :wild :type "h" :defaults dir)))
	(cxx-files (directory (make-pathname :name :wild :type "cc" :defaults dir))))
    (append header-files cxx-files)))


(defun gather-all-source-files (&optional (source-directories +source-directories+))
  (let (all-file-lists)
    (dolist (dir source-directories)
      (push (gather-source-files dir) all-file-lists))
    (apply #'append all-file-lists)))




(defun gather-all-includes (includes-table source-files)
  (clrhash includes-table)
  (dolist (file source-files)
    (read-file-for-includes file includes-table))
  includes-table)




(defun fix-include (inc &optional base-path relative-to)
  (let* ((source-dir (include-file-source-dir inc))
	 (include-path (include-file-include-path inc))
	 (new-path (cond
		     ((null (pathname-directory include-path))
		      (merge-pathnames include-path source-dir))
		     ((pathname-directory include-path)
		      (merge-pathnames include-path base-path))
		     (t (warn "Do something")))))
    (if (probe-file new-path)
	(let ((inc (enough-namestring new-path relative-to)))
	  (make-pathname :directory (remove-if (lambda (x) (string-equal "src" x)) (pathname-directory inc)) :defaults inc))
	(namestring include-path))))

(defun generate-rewrites (includes-table)
  (let ((includes-rewrites (make-hash-table :test #'equalp)))
    (maphash (lambda (orig v)
	       (let ((fix (fix-include orig 
				       #P"/Users/meister/Development/clasp/src/"
				       #P"/Users/meister/Development/")))
		 (setf (gethash orig includes-rewrites) fix)))
	     includes-table)
    includes-rewrites))


(defun search-and-generate-rewrites ()
  (let* ((source-files (gather-all-source-files +source-directories+))
	 (includes (gather-all-includes (make-hash-table :test #'equalp) source-files))
	 (rewrites (generate-rewrites includes)))
    (values source-files rewrites)))



(defun rewrite-all (source-files rewrites &key testing)
  (dolist (file source-files)
    ;;(format t "Rewriting ~a~%" file)
    (rewrite-file-with-includes file rewrites :testing testing)))


;;
;; Testing
;;

(defparameter *source-files* nil)
(defparameter *rewrites* nil)

(multiple-value-setq (*source-files* *rewrites*)
  (search-and-generate-rewrites))

(rewrite-all *source-files* *rewrites* :testing t)

