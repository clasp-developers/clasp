
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
    #P"/Users/meister/Development/clasp/src/sockets/"
    #P"/Users/meister/Development/clasp/projects/cando/src/candoBase/"
    #P"/Users/meister/Development/clasp/projects/cando/src/units/"
    ))

(defparameter +base-paths+ (make-hash-table :test #'equal))
(dolist (path +source-directories+)
  (setf (gethash (car (last (pathname-directory path))) +base-paths+) (butlast (pathname-directory path))))
+base-paths+

  
(second (pathname-directory #P"Users/meister/Development/clasp/"))
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



(defun calculate-absolute-pathname (include-path base-paths)
  (let* ((rel-dir (second (pathname-directory include-path)))
         (abs-dir (gethash rel-dir base-paths))
         (abs-path (merge-pathnames include-path (make-pathname :directory abs-dir))))
    abs-path))

   
(defun absolute-include (inc base-paths)
  (let* ((source-dir (include-file-source-dir inc))
	 (include-path (include-file-include-path inc)))
    (if (null (pathname-directory include-path))
        (merge-pathnames include-path source-dir)
        (calculate-absolute-pathname include-path base-paths))))

(defun shortest-enough-namestring (absolute-include-path prefix-list)
  (let (shortest)
    (dolist (prefix prefix-list)
      (let ((one (enough-namestring absolute-include-path prefix)))
        (when (or (null shortest) (< (length one) (length shortest)))
          (setq shortest one))))
    (make-pathname :directory (remove-if (lambda (x) (string-equal "src" x)) (pathname-directory shortest))
                   :name (pathname-name shortest)
                   :type (pathname-type shortest))))


(defun fix-include (inc &optional base-paths relative-to-list)
  (let* ((source-dir (include-file-source-dir inc))
	 (include-path (include-file-include-path inc))
	 (absolute-include-path (if (null (pathname-directory include-path))
                                    (merge-pathnames include-path source-dir)
                                    (calculate-absolute-pathname include-path base-paths))))
    (if (probe-file absolute-include-path)
	(shortest-enough-namestring absolute-include-path relative-to-list)
	(namestring include-path))))
(trace shortest-enough-namestring)
(trace probe-file)

(fix-include (make-include-file :source-dir #P"/Users/meister/Development/clasp/projects/src/claspBase"
                                :include-path #P"core/foundation.h")
             +base-paths+
             '(#P"/Users/meister/Development/"
               #P"/Users/meister/Development/clasp/projects/"))

(defun generate-rewrites (includes-table)
  (let ((includes-rewrites (make-hash-table :test #'equalp)))
    (maphash (lambda (orig v)
	       (let ((fix (fix-include orig +base-paths+
                                       '(#P"/Users/meister/Development/"
                                         #P"/Users/meister/Development/clasp/projects/"))))
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

(defun do-all (&key testing)
  (multiple-value-setq (*source-files* *rewrites*)
    (search-and-generate-rewrites))
  (rewrite-all *source-files* *rewrites* :testing t))

(multiple-value-setq (*source-files* *rewrites*)
  (search-and-generate-rewrites))

(print "Hello")

(do-all)


*rewrites*
(trace fix-include)
