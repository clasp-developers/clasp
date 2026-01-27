#|
NOTE!!!!!   Running the static analyzer often runs into problems because clang can't find the system header files.

Look at the definitions of +isystem-dir+, +resource-dir+ and possibly +additional-arguments+
Find directories that look like them and replace the ones defined in the constants by the correct ones.

|#


(provide :clang-tool)

;;;(declaim (debug 3))
(defpackage #:clang-tool
  (:shadow #:function-info #:function-type)
  (:use #:common-lisp #:core #:ast-tooling #:clang-ast)
  (:shadow #:ensure-directory #:dump #:get-string #:size #:type #:source-manager)
  (:export
   #:with-compilation-tool-database
   #:match-in-compilation-tool-database-source-tree
   #:main-pathname
   #:clang-database
   #:copy-compilation-tool-database
   #:multitool-compilation-tool-database
   #:source-namestrings
   #:main-source-filename
   #:arguments-adjuster-list
   #:load-compilation-tool-database
   #:compile-matcher
   #:select-source-namestrings
   #:match-info
   #:id-to-node-map
   #:ast-context
   #:source-manager
   #:batch-run-multitool
   #:batch-match-run
   #:make-multitool
   #:multitool-add-matcher
   #:multitool-results
   #:code-match-callback
   #:code-match-timer
   #:*match-refactoring-tool*
   #:*run-and-save*
   #:lsel
   #:mtag-node
   #:mtag-loc-start
   #:mtag-loc-end
   #:mtag-source
   #:mtag-source-impl
   #:mtag-name
   #:mtag-replace
   #:mtag-result
   #:match-count-loaded-asts
   #:match-dump-loaded-asts
   #:match-comments-loaded-asts
   #:match-run-loaded-asts
   #:dump-match-callback
   #:source-loc-as-string
   #:sub-match-run
   #:cform-matcher
   #:*print-reports*
   #:load-asts
   #:type))

(in-package :clang-tool)

(use-package :ast-tooling)
(use-package :clang-ast)


(defparameter *print-reports* nil)
(defparameter *current-multitool* nil
  "Keep track of the current multitool")

(defparameter *externals-clasp-pathname* (make-pathname :directory (pathname-directory (pathname ext:*clasp-clang-path*))))
#+(or linux freebsd) (defparameter *externals-clasp-include-dir* (namestring (car (directory (pathname (format nil "~a../lib/clang/*/" *externals-clasp-pathname*))))))

(defvar +resource-dir+ 
  #+darwin (namestring (car (directory "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/*/")))
  #+(or linux freebsd) *externals-clasp-include-dir*
  "Define the -resource-dir command line option for Clang compiler runs")

(defvar +additional-arguments+
  #+darwin (vector "-I/usr/local/include")
  #+linux (vector)
)

(defmacro with-unmanaged-object ((var obj) &body body)
  `(let ((,var ,obj))
     (unwind-protect
          (progn
            ,@body)
       #+(or)(gctools:deallocate-unmanaged-instance ,var))))

(defvar *compilation-tool-database*)

(defmacro with-compilation-tool-database (compilation-tool-database &body body)
  "* Arguments
- compilation-tool-database :: Compilation tool database that will be set into *compilation-tool-database*.
* Description
Sets up a dynamic environment where clang-tooling:*compilation-tool-database* is set to the given compilation-tool-database."
  `(let ((*compilation-tool-database* ,compilation-tool-database))
     (declare (special *compilation-tool-database*))
     ,@body))

(defclass compilation-tool-database ()
  ((clang-database :initarg :clang-database :accessor clang-database)
   (source-path-identifier :initform nil :initarg :source-path-identifier :accessor source-path-identifier)
   (main-source-filename :initform "main.cc" :initarg :main-source-filename :accessor main-source-filename)
   (source-namestrings :initarg :source-namestrings :accessor source-namestrings)
   (arguments-adjuster-list :initform nil :initarg :arguments-adjuster-list :accessor arguments-adjuster-list)))


(defun copy-compilation-tool-database (defaults &key source-pattern)
  (let ((copy (make-instance 'compilation-tool-database
                             :clang-database (clang-database defaults)
                             :source-path-identifier (source-path-identifier defaults)
                             :main-source-filename (main-source-filename defaults)
                             :source-namestrings (source-namestrings defaults)
                             :arguments-adjuster-list (arguments-adjuster-list defaults)
                             )))
    (if source-pattern
      (setf (source-namestrings copy) (select-source-namestrings defaults source-pattern)))
    copy))


(defmethod initialize-instance :after ((obj compilation-tool-database) &rest args)
  (declare (ignore args))
  "* Description
Initialize the source-namestrings using all filenames in the database if they haven't already been set."
  (declare (ignore args))
  (unless (slot-boundp obj 'source-namestrings)
    (setf (source-namestrings obj) (select-source-namestrings obj))))

(defun apply-arguments-adjusters (compilation-tool-database tool)
  "* Arguments
- compilation-tool-database :: The database.
- tool :: The clang tool to apply the arguments-adjusters to.
* Description
Apply the compilation-tool-database's arguments-adjusters to the clang tool."
  (ast-tooling:clear-arguments-adjusters tool)
  (dolist (aa (arguments-adjuster-list compilation-tool-database))
    (ast-tooling:append-arguments-adjuster tool aa)))

(defun match-in-compilation-tool-database-source-tree (compilation-tool-database match-info)
  (let* ((match-loc (mtag-loc-start match-info :whole))
         (ml-pathname (pathname (subseq match-loc 0 (position #\: match-loc))))
         (ml-dir-pn (make-pathname :name nil :type nil :defaults ml-pathname))
         (ml-dir (namestring ml-dir-pn))
         (in-source-tree (if (source-path-identifier compilation-tool-database)
                             (let ((found (search (source-path-identifier compilation-tool-database) ml-dir)))
                               found)
                             t)))
    in-source-tree))

(defun fix-path (root rel)
  (let* ((pnroot (pathname root))
         (pnrel (pathname rel))
         (merged (merge-pathnames pnrel pnroot))
         (abs #+(or)(uiop/filesystem:native-namestring merged) (namestring merged)))
    (or abs (error "Could not find absolute path for ~a + ~a" root rel))))

(defun ensure-directory-name (relpath)
  (if (eq (elt relpath (1- (length relpath))) #\/)
      relpath
      (concatenate 'string relpath "/")))

(defun convert-relative-includes-to-absolute (args absolute-root)
  "Build a function that fixes up compile command arguments.
It converts relative -I../... arguments to absolute paths"
  (let ((new-args (copy-seq args))
        (root-directory (make-pathname :name nil :type nil :defaults (pathname absolute-root))))
    (dotimes (i (length new-args))
      (let ((arg (elt new-args i)))
        (cond
          ((string= arg "-I.." :start1 0 :end1 4)
           (let* ((fixed-path (fix-path root-directory (ensure-directory-name (subseq arg 2))))
                  (new-arg (concatenate 'string "-I" fixed-path)))
             (setf (elt new-args i) new-arg)))
          ((string= arg "../" :start1 0 :end1 3)
           (let ((fixed-path (fix-path root-directory arg)))
             (setf (elt new-args i) fixed-path)))
          (t #| do nothing |# ))))
    new-args))

(defun isysroot ()
  (let* ((args (core:split core:*build-cppflags* " "))
         (pos (position "-isysroot" args :test #'equal)))
    (if pos
        (elt args (1+ pos))
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/")))

(defun setup-default-arguments-adjusters (compilation-tool-database &key convert-relative-includes-to-absolute )
  "* Arguments
- compilation-tool-database :: The compilation-tool-database to add the arguments adjusters to.
- convert-relative-includes-to-absolute :: If T the main-pathname of the compilation-tool-database is used
as the root of the absolute includes, if nil relative includes are left alone.
Otherwise the value of convert-relative-includes-to-absolute is used.
* Description
Setup the default arguments adjusters."
  (push (lambda (args filename)
          (declare (ignore filename))
          (format t "Arguments:~%")
          (format t "~{~A \\~%~}%" (coerce args 'list))
          (format t "If nothing seems to be working - try running the arguments on the command line and look for errors~%")
          args)
        (arguments-adjuster-list compilation-tool-database))
  (push (ast-tooling:get-clang-syntax-only-adjuster) (arguments-adjuster-list compilation-tool-database))
  (push (ast-tooling:get-clang-strip-output-adjuster) (arguments-adjuster-list compilation-tool-database))
  (push (lambda (args filename)
          (declare (ignore filename))
          (concatenate 'vector
                       args
                       #+darwin (vector "-isysroot" (isysroot))
                       (vector "-resource-dir" +resource-dir+)
                       +additional-arguments+))
        (arguments-adjuster-list compilation-tool-database))
  (cond
    ((eq convert-relative-includes-to-absolute t)
     (push (lambda (args filename)
             (declare (ignore filename))
             (convert-relative-includes-to-absolute args (main-pathname compilation-tool-database)))
           (arguments-adjuster-list compilation-tool-database)))
    (convert-relative-includes-to-absolute
     (push (lambda (args filename)
             (declare (ignore filename))
             (convert-relative-includes-to-absolute args convert-relative-includes-to-absolute))
           (arguments-adjuster-list compilation-tool-database)))
    (t #| Do nothing |#)))

(defun load-compilation-tool-database (pathname &key (main-source-filename "main.cc")
                                                  convert-relative-includes-to-absolute
                                                  source-path-identifier)
  "* Arguments
- pathname :: The name of the file that contains the compilation database in JSON format.
See http://clang.llvm.org/docs/JSONCompilationDatabase.html for the format
- main-source-filename :: The name of the source file that is considered the main source file.  Default is main.cc.
- convert-relative-includes-to-absolute :: If T the main-pathname of the compilation-tool-database is used
as the root of the absolute includes, if nil relative includes are left alone.
Otherwise the value of convert-relative-includes-to-absolute is used.
- source-path-identifier :: nil or a string
* Description
Load the compilation database and return it. If source-path-identifier is defined then every match will have it's source location checked to see if the 
it contains the string source-path-identifier.  So /a/b/c/d.cc will match /b/"
  (let* ((db (ast-tooling:wrapped-jsoncompilation-database-load-from-file
              (namestring (or (probe-file pathname) (error "Could not find file: ~a" pathname)))
              :auto-detect))
         (all-files (map 'list #'identity (ast-tooling:get-all-files db)))
         (ctd (make-instance 'compilation-tool-database
                             :clang-database db
                             :main-source-filename main-source-filename)))
    (format t "Loaded database contains ~a source files~%" (length all-files))
    (setup-default-arguments-adjusters ctd :convert-relative-includes-to-absolute convert-relative-includes-to-absolute)
    (setf (source-path-identifier ctd) source-path-identifier)
    ctd))

(defun main-pathname (&optional (compilation-tool-database *compilation-tool-database*))
  (declare (ignore compilation-tool-database))
  "* Arguments
- compilation-tool-database :: The compilation database.
* Return Value
A pathname.
* Description
Return the pathname of the directory that contains the main source file. This is where the project.dat and clasp_gc.cc file will be written."
  (declare (ignore compilation-tool-database))
  (translate-logical-pathname #P"sys:src;main;"))


(defun select-source-namestrings (compilation-tool-database &optional (pattern nil))
  "* Arguments
- compilation-tool-database :: The compilation database.
- pattern :: A string for selecting file names from the database that contain that string.
* Description
Select a subset (or all) source file names from the compilation database and return them as a list."
  (let ((list-names (map 'list #'identity (ast-tooling:get-all-files (clang-database compilation-tool-database)))))
    (if pattern
        (remove-if-not #'(lambda (x) (search pattern x)) list-names)
        list-names)))



(defparameter *match-refactoring-tool* nil)
(defparameter *run-and-save* nil)
(defparameter *number-of-files* 0)
(defparameter *current-file-index* 0)

(defparameter *match-counter* 0)
(defparameter *match-counter-limit* nil)
(defparameter *search-start-real-time* 0.0)
(defparameter *search-start-run-time* 0.0)
(defparameter *search-start-bytes-allocated* 0)

(defclass code-match-timer ()
  ((name :initarg :name :accessor name)
   (callback-counter :initform 0 :accessor callback-counter)
   (start-real-time :initarg :start-real-time :accessor start-real-time)
   (start-run-time :initarg :start-run-time :accessor start-run-time)
   (start-bytes-allocated :initarg :start-bytes-allocated :accessor start-bytes-allocated)
   (accumulated-real-time :initform 0.0 :initarg :accumulated-real-time :accessor accumulated-real-time)
   (accumulated-run-time :initform 0.0 :initarg :accumulated-run-time :accessor accumulated-run-time)
   (accumulated-bytes-allocated :initform 0 :initarg :accumulated-bytes-allocated :accessor accumulated-bytes-allocated)))

(defun get-seconds-real-time ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun get-seconds-run-time ()
  (float (/ (get-internal-run-time) internal-time-units-per-second)))

(defun start-timer (timer)
  (setf (start-real-time timer) (get-seconds-real-time)
        (start-run-time timer) (get-seconds-run-time)
        (start-bytes-allocated timer) (gctools:bytes-allocated)))

(defun stop-timer (timer)
  (incf (accumulated-real-time timer) (- (get-seconds-real-time) (start-real-time timer)))
  (incf (accumulated-run-time timer) (- (get-seconds-run-time) (start-run-time timer)))
  (incf (accumulated-bytes-allocated timer) (- (gctools:bytes-allocated) (start-bytes-allocated timer)))
  (incf (callback-counter timer)))

(defun describe-timer (timer)
  (if (> (callback-counter timer) 0)
      (format t "Timer ~a(~a times) Real(~a[~a]) Run(~a[~a]) Bytes(~a[~a])~%"
              (name timer)
              (callback-counter timer)
              (accumulated-real-time timer) (/ (accumulated-real-time timer) (callback-counter timer))
              (accumulated-run-time timer) (/ (accumulated-run-time timer) (callback-counter timer))
              (accumulated-bytes-allocated timer) (floor (accumulated-bytes-allocated timer) (callback-counter timer)))
      (format t "Timer ~a - never invoked~%" (name timer))))

(defun print-report ()
  (format t "-------- *match-counter*: ~a~%" *match-counter*)
  (format t "Overall search Real(~a sec) Run(~a sec) Bytes(~a)~%"
          (- (get-seconds-real-time) *search-start-real-time*)
          (- (get-seconds-run-time) *search-start-run-time*)
          (- (gctools:bytes-allocated) *search-start-bytes-allocated*))
  (dolist (tool (multitool-active-tools *current-multitool*))
    (let ((callback (single-tool-callback tool)))
      (when (typep callback 'code-match-callback)
        (describe-timer (timer callback))))))

(defun start-search-timer ()
  (setf *search-start-real-time* (get-seconds-real-time))
  (setf *search-start-run-time* (get-seconds-run-time))
  (setf *search-start-bytes-allocated* (gctools::bytes-allocated)))

(defun advance-match-counter ()
  (setf *match-counter* (1+ *match-counter*))
  (when *match-counter-limit*
      (when (>= *match-counter* *match-counter-limit*)
        (throw 'match-counter-reached-limit *match-counter*)))
  (when (and *print-reports* (= (rem *match-counter* 10000) 0))
    (print-report)))


(defparameter *match-dump-tag* nil)
(defclass good-dump-match-callback (ast-tooling:match-callback) ()
  (:metaclass core:derivable-cxx-class))
(core:defvirtual ast-tooling:run ((self good-dump-match-callback) match)
  (declare (ignore self))
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash *match-dump-tag* id-to-node-map))
         (context (match-result-context match))
         (source-manager (ast-tooling:source-manager match))
         (lang-options (get-lang-opts context))
         (begin (get-begin-loc node))
         (_end (get-end-loc node))
         (end (lexer-get-loc-for-end-of-token _end 0 source-manager lang-options))
         (token-range (new-char-source-range-get-char-range begin end)))
    (multiple-value-bind (source invalid)
        (lexer-get-source-text token-range source-manager lang-options)
      (format t "begin location: ~a~%" (print-to-string begin source-manager))
      (format t "Did lexer-get-source-text return valid source --> ~:[no~;yes~]~%" (not invalid))
      (format t "------------source from Lexer::getSourceText(CharSourceRange(begin,end),...) starts below~%")
      (format t "~a~%" source)
      (format t "------------node dump~%")
      (cast:dump node)
      (advance-match-counter))))

(defclass dump-match-callback (ast-tooling:match-callback) ()
  (:metaclass core:derivable-cxx-class))
(core:defvirtual ast-tooling:run ((self dump-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash *match-dump-tag* id-to-node-map)))
    (format t "*match-dump-tag* = ~a~%" *match-dump-tag*)
    (cast:dump node)
      (advance-match-counter)))



(defclass count-match-callback (ast-tooling:match-callback) ()
  (:metaclass core:derivable-cxx-class))
(core:defvirtual ast-tooling:run ((self count-match-callback) match)
  (declare (ignore match))
  (advance-match-counter))




(defclass match-info ()
  ((id-to-node-map :initarg :id-to-node-map :accessor id-to-node-map)
   (ast-context :initarg :ast-context :accessor ast-context)
   (source-manager :initarg :source-manager :accessor source-manager)))

;;Requires a lambda CODE that takes a single match-info argument
(defclass code-match-callback (ast-tooling:match-callback)
  ((start-of-translation-unit-code :initarg :start-of-translation-unit-code :accessor start-of-translation-unit-code)
   (timer :initarg :timer
          :initform (make-instance 'code-match-timer :name (gensym))
          :accessor timer)
   (match-code :initarg :match-code :accessor match-code)
   (end-of-translation-unit-code :initarg :end-of-translation-unit-code :accessor end-of-translation-unit-code))
  (:metaclass core:derivable-cxx-class))

(defparameter *on-start-translation-unit-depth* 0)
(defparameter *on-end-translation-unit-depth* 0)

(core:defvirtual ast-tooling:on-start-of-translation-unit ((self code-match-callback))
  (setf *on-end-translation-unit-depth* 0)
  (incf *on-start-translation-unit-depth*)
  (when (= *on-start-translation-unit-depth* 1)
    (format t "In on-start-of-translation-unit for file ~a of ~a~%" *current-file-index* *number-of-files*)
    (incf *current-file-index*)
    (when (slot-boundp self 'start-of-translation-unit-code)
      (assert (start-of-translation-unit-code self))
      (funcall (start-of-translation-unit-code self)))))

(core:defvirtual ast-tooling:on-end-of-translation-unit ((self code-match-callback))
  (setf *on-start-translation-unit-depth* 0)
  (incf *on-end-translation-unit-depth*)
  (when (= *on-end-translation-unit-depth* 1)
    #+(or)(format t "on-end-of-translation-unit for code-match-callback of: ~a~%" self)
    (when (slot-boundp self 'end-of-translation-unit-code)
      (assert (end-of-translation-unit-code self))
      (funcall (end-of-translation-unit-code self)))))

(core:defvirtual ast-tooling:run ((self code-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (match-info (make-instance 'match-info
                                    :id-to-node-map (ast-tooling:idto-node-map nodes)
                                    :ast-context (match-result-context match)
                                    :source-manager (ast-tooling:source-manager match))))
    (when (match-code self)
      (start-timer (timer self))
      (funcall (match-code self) match-info)
      (stop-timer (timer self))
      (advance-match-counter))))


(defclass source-loc-match-callback (code-match-callback)
  ((comments-substring-list :accessor comments-substring-list :initarg :comments-substring-list))
  (:metaclass core:derivable-cxx-class))


(defun source-loc-equal (match-info source-loc-match-callback node)
  "* Arguments
- match-info :: The match info for the match.
- source-loc-match-callback :: A source-loc-match-callback.
- node :: The node that we are testing if its source location matches that described by source-loc-match-callback.
* Description
Return true if the node describes source code that matches source-loc-match-callback."
  (let ((comment (comment-for-decl match-info node))
        one-matches)
    (if comment
        (progn
          (dolist (comment-substring (comments-substring-list source-loc-match-callback))
            (when (search comment-substring comment)
              (format t "Comment match: ~a  ~%" comment)
              (setq one-matches t)))
          one-matches)
        nil)))
          

(defparameter *match-source-location* nil)
(core:defvirtual ast-tooling:run ((self source-loc-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash :whole id-to-node-map))
         (source-manager (ast-tooling:source-manager match))
         (match-info (make-instance 'match-info
                                    :id-to-node-map id-to-node-map
                                    :ast-context (match-result-context match)
                                    :source-manager source-manager)))
    (when (source-loc-equal match-info self node)
      (when (match-code self)
        (funcall (match-code self) match-info))
      (setq *match-source-location* node))
    (advance-match-counter)))

(define-condition no-node-for-tag-error (error)
    ((tag :accessor no-node-for-tag-error-tag :initarg :tag))
  (:report (lambda (condition stream)
             (format stream "Could not find node for tag ~s" (no-node-for-tag-error-tag condition)))))

(defun mtag-node (match-info tag)
  "* Arguments
- match-info :: The match-info.
- tag :: The tag (symbol) that has been associated with a node in the match-info.
* Description
Get the AST node that has been associated with the tag."
  (let* ((node (gethash tag (id-to-node-map match-info))))
    (unless node
      (error (make-condition 'no-node-for-tag-error :tag tag)))
    node))


(defun mtag-source-decl-stmt-impl (match-info node)
  "Get the source code for the current node
This can only be run in the context set up by the code-match-callback::run method"
  (let* ((lang-options (get-lang-opts (ast-context match-info)))
         (begin (get-begin-loc node))
         (_end (get-end-loc node))
         (end (lexer-get-loc-for-end-of-token _end 0 (source-manager match-info) lang-options))
         (token-range (new-char-source-range-get-char-range begin end)))
    #+(or)(format t "mtag-source-decl-stmt-impl:: begin: ~s  end: ~s  isTokenRange: ~a (class-of begin) ~s~%"
                  (ast-tooling:print-to-string begin (source-manager match-info))
                  (ast-tooling:print-to-string end (source-manager match-info))
                  (ast-tooling:is-token-range token-range)
                  (class-of begin))
    (multiple-value-bind (source invalid)
        (lexer-get-source-text token-range (source-manager match-info) lang-options)
      (when invalid
        (error "The source range ~a ~a was invalid"
            (ast-tooling:print-to-string begin (source-manager match-info))
            (ast-tooling:print-to-string end (source-manager match-info))))
      #+(or)(format t "mtag-source-decl-stmt-impl:: size of source: ~a~%" (length source))
      source)))


(defgeneric mtag-name (match-info tag-node)
  (:documentation
   "* Arguments
- match-info :: The match-info.
- tag :: The tag that has been associated with a node.
* Description
Return the name of the node that has been associated with a tag."))

(defmethod mtag-name (match-info (tag symbol))
  (when (null tag)
    (error "tag is nil - this should never happen"))
  (let ((node (mtag-node match-info tag)))
    (mtag-name match-info node)))


(defmethod mtag-name (match-info (node clang-ast:decl))
  (declare (ignore match-info))
  (if (cast:get-identifier node)
      (cast:get-name node)
      "NO-NAME"))

(defgeneric mtag-source-impl (match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:decl))
  (mtag-source-decl-stmt-impl match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:stmt))
  (mtag-source-decl-stmt-impl match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:qual-type))
  (declare (ignore match-info))
  (get-as-string node))

(defmethod mtag-source-impl (match-info (node clang-ast:type-loc))
  "Use the SourceRange of the type-loc to extract its source code"
  (let* ((source-range (get-source-range node))
         (lang-options (get-lang-opts (ast-context match-info)))
         (char-source-range (new-char-source-range-get-token-range-source-range source-range)))
    (lexer-get-source-text char-source-range (source-manager match-info) lang-options)))
         

(defun mtag-source (match-info tag)
  "* Arguments
- match-info :: The match-info.
- tag :: A tag (symbol) that has been associated with the node we are looking for.
* Description
Return the source code for the node that has been associated with the tag."
  (let* ((node (mtag-node match-info tag)))
    (mtag-source-impl match-info node)))

(defvar *probed-files* (make-hash-table :test #'equal))

(defun memoized-probe-file (pathname)
  (let ((lookup (gethash pathname *probed-files*)))
    (if lookup
        lookup
        (setf (gethash pathname *probed-files*) (probe-file pathname)))))

(defun ploc-parts (ploc)
  (declare (special *compilation-tool-database*))
  (when (ast-tooling:is-invalid ploc)
    (error "invalid source-location"))
  (let* ((relative (pathname (ast-tooling:presumed-loc-get-filename ploc)))
         (absolute (make-pathname :name nil :type nil :defaults (main-pathname *compilation-tool-database*)))
         (pathname (merge-pathnames (make-pathname :host (pathname-host absolute)
                                                   :device (pathname-device absolute)
                                                   :defaults relative) absolute))
         (probed-file (memoized-probe-file pathname)))
    (values probed-file (ast-tooling:get-line ploc) (ast-tooling:get-column ploc))))

(defun ploc-as-string (ploc)
  (multiple-value-bind (probed-file line column)
      (ploc-parts ploc)
    (if (null probed-file)
        (progn
          (format nil "[ploc-as-string could not locate ~a]" ploc))
        (format nil "~a:~a:~a" (namestring probed-file) line column))))


(defun source-loc-as-string (match-info sloc)
  "* Arguments
- match-info :: The match-info.
- sloc :: The source-location object.
* Description
Return a string that describes the source location corresponding to sloc."
  (if (ast-tooling:is-file-id sloc)
      (ploc-as-string (ast-tooling:get-presumed-loc (source-manager match-info) sloc))
      (format nil "~a <Spelling=~a>"
              (ploc-as-string (ast-tooling:get-presumed-loc (source-manager match-info)
                                                            (ast-tooling:get-expansion-loc (source-manager match-info) sloc)))
              (ploc-as-string (ast-tooling:get-presumed-loc (source-manager match-info)
                                                            (ast-tooling:get-spelling-loc (source-manager match-info) sloc))))))

(defun mtag-loc-start (match-info tag)
  "* Arguments
- match-info :: The match-info.
- tag :: The tag of the node.
* Description 
Return a string that describes the start of a source location for the node indicated by the tag."
  (let* ((node (mtag-node match-info tag))
         (begin-sloc (get-begin-loc node)))
    (source-loc-as-string match-info begin-sloc)))

(defun mtag-loc-end (match-info tag)
  "* Arguments
- match-info :: The match-info.
- tag :: The tag of the node.
* Description 
Return a string that describes the end of a source location for the node indicated by the tag."
  (let* ((node (mtag-node match-info tag))
         (end-sloc (get-end-loc node)))
    (source-loc-as-string match-info end-sloc)))



(defun comment-for-decl (match-info decl-node)
  (check-type decl-node cast:decl)
  (let* ((comment (ast-tooling:get-comment-for-decl (ast-context match-info) decl-node nil)))
    (when comment
      (let* ((source-range (clang-comments:get-source-range comment))
             (char-source-range (new-char-source-range-get-token-range-source-range source-range))
             (text (lexer-get-source-text char-source-range (source-manager match-info) (ast-tooling:get-lang-opts (ast-context match-info)))))
        text))))


(defun mtag-decl-comment (match-info tag)
  (let ((node (mtag-node match-info tag)))
    (if (subtypep (type-of node) 'clang-ast:decl)
        (comment-for-decl match-info node)
        (error "Comments can only be obtained for DECL nodes - you passed ~a" node))))



(defun mtag-type-of-node (match-info tag)
  "* Arguments
- match-info :: The match-info.
- tag :: A tag that has been associated with a node in the match-info.
* Description
Return the type of the node that corresponds to tag."
  (let* ((node (mtag-node match-info tag)))
    (type-of node)))

(defun mtag-replace (match-info tag replace-callback)
  "* Arguments
- match-info :: A match-info.
- tag :: A tag that is associated with a node in match-info.
- replace-callback :: A function that takes match-info and tag and returns a string.
* Description
Calls the replace-callback function (funcall replace-callback match-info tag) and generates a replacement
for the node corresponding to tag in match-info."
  (let* ((rep-src (funcall replace-callback match-info tag))
         (node (mtag-node match-info tag)) ;; (id-to-node-map match-info)))
         (begin (get-begin-loc node))
         (end (get-end-loc node))
         (rep-range (new-char-source-range-get-token-range begin end))
         (rep (new-replacement (source-manager match-info) rep-range rep-src)))
    (if *match-refactoring-tool*
        (ast-tooling:refactoring-tool-replacements-add *match-refactoring-tool* rep)
        (format t "NOT Replacing: ~a~%     with: ~a~%" (clang-tool:mtag-source match-info tag) rep-src))))

(defparameter *match-results* nil)
(defmacro mtag-result (match-info tag fmt &rest fmt-args)
  "* Arguments
- match-info :: A match-info.
- tag :: A tag that is associated with a node in match-info.
- replace-callback :: A function that takes match-info and tag and returns a string.
* Description
Generates a string using fmt/fmt-args and accumulates internally so that they can be accessed after the clang-tool runs."
  (let ((rep-src-gs (gensym))
        (node-gs (gensym))
        (begin-gs (gensym))
        (end-gs (gensym))
        (rep-range-gs (gensym))
        (rep-gs (gensym)))
  `(let* ((,rep-src-gs (format nil ,fmt ,@fmt-args))
          (,node-gs (gethash ,tag (id-to-node-map ,match-info)))
          (,begin-gs (get-begin-loc ,node-gs))
          (,end-gs (get-end-loc ,node-gs))
          (,rep-range-gs (new-char-source-range-get-token-range ,begin-gs ,end-gs))
          (,rep-gs (new-replacement (source-manager match-info) ,rep-range-gs ,rep-src-gs)))
     (push ,rep-gs *match-results*))))


(defun lsel (list-name search-str)
  "Select file names from the list and return them"
  (remove-if-not #'(lambda (x) (search search-str x)) list-name))


(defun load-asts (compilation-tool-database)
  "* Arguments
- compilation-tool-database :: The compilation-tool-database that describes the source files to run the clang-tool over.
* Description
Load all of the ASTs into memory at once.  DANGER: This should not be done with too many source files or you will
run out of memory. This function can be used to rapidly search ASTs for testing and ASTMatcher development."
  (with-compilation-tool-database compilation-tool-database
    (let* ((files (source-namestrings compilation-tool-database))
           (tool (ast-tooling:new-refactoring-tool
                  (clang-database compilation-tool-database)
                  files)))
      (apply-arguments-adjusters compilation-tool-database tool)
      ;;    (ast-tooling:run tool factory)
      (format t "Loading ASTs for the files: ~a~%" files)
      (format t "Tool -> ~a~%" tool)
      (let ((asts (ast-tooling:build-asts tool)))
        (if (> (length asts) 0)
            (progn
              (format t "build-asts result: ~s ~s~%" (length asts) asts))
            (progn
              (format t "NO ASTS WERE LOADED!!!!~%")))
        (format t "Built asts: ~a~%" asts)
        asts))))

(defun safe-add-dynamic-matcher (match-finder compiled-matcher callback &key matcher-sexp)
  (or (ast-tooling:add-dynamic-matcher match-finder compiled-matcher callback)
      (error "Could not add dynamic-matcher ~s" matcher-sexp)))

(defvar *save-match-finders* nil)
(defun make-permanent-match-finder (&optional save)
  (let ((mf (ast-tooling:new-match-finder)))
    (format t "In make-permanent-match-finder made new match-finder: ~s~%" mf)
    (push mf *save-match-finders*)
    (when (and (> (length *save-match-finders*) 10000) (= (mod (length *save-match-finders*) 100) 0))
      (warn "Exceeded 1000 match finders at ~a" (length *save-match-finders*)))
    mf))

(defun make-temporary-match-finder (&optional save)
  (let ((mf (ast-tooling:new-match-finder)))
    mf))

(defun batch-run-matcher (match-sexp &key compilation-tool-database callback run-and-save)
  (declare (type list match-sexp)
           (type ast-tooling:match-callback callback))
  (with-compilation-tool-database compilation-tool-database
    (format t "About to start batch-run-matcher~%")
    (let* ((*match-refactoring-tool* (ast-tooling:new-refactoring-tool
                                      (clang-database compilation-tool-database)
                                      (source-namestrings compilation-tool-database))))
      (apply-arguments-adjusters compilation-tool-database *match-refactoring-tool*)
      (let* ((*run-and-save* run-and-save)
             (matcher (compile-matcher match-sexp))
             (match-finder (let ((temp-match-finder (make-permanent-match-finder)))
                             (safe-add-dynamic-matcher temp-match-finder matcher callback :matcher-sexp match-sexp)
                             temp-match-finder))
             (factory (ast-tooling:new-frontend-action-factory match-finder)))
        (if (not run-and-save)
            (ast-tooling:clang-tool-run *match-refactoring-tool* factory)
            (ast-tooling:run-and-save *match-refactoring-tool* factory))
        (format t "Number of matches ~a~%" *match-counter*)))))

(defstruct multitool
  "Store multiple tools to run in one go across a bunch of source files."
  compilation-tool-database
  all-tools   ;; the list of all tools
  selected-tools  ;; the list of selected tools - if nil then use all-tools
  results
  arguments-adjuster )


(defstruct single-tool
  name
  initializer
  matcher
  matcher-sexp
  callback
)

(defun multitool-add-matcher (mtool &key name matcher-sexp callback initializer)
  "Keep track of matchers and callbacks so they don't go out of scope while the tool is alive"
  (let* ((matcher (compile-matcher matcher-sexp))
         (tool (make-single-tool :name name
                                 :initializer initializer
                                 :matcher matcher
                                 :matcher-sexp matcher-sexp
                                 :callback callback)))
    (push tool (multitool-all-tools mtool)))
)


(defun multitool-activate-all-tools (mtool)
  (setf (multitool-selected-tools mtool) nil))

(defun multitool-activate-tools (mtool list-of-tool-names)
  (let (selected-tools)
    (dolist (ot (multitool-all-tools mtool))
      (when (position (single-tool-name ot) list-of-tool-names)
        (push ot selected-tools)))
    (unless selected-tools
      (format t "No tools were selected - all tools being used~%"))
    (setf (multitool-selected-tools mtool) selected-tools)))

(defun multitool-active-tools (mtool)
    (if (multitool-selected-tools mtool)
        (multitool-selected-tools mtool)
        (multitool-all-tools mtool)))

(defun multitool-active-tool-names (mtool)
  (mapcar (lambda (ot) (single-tool-name ot)) (multitool-active-tools mtool)))


(defun batch-run-multitool (mtool compilation-tool-database
                            &key (source-namestrings (source-namestrings compilation-tool-database))
                              run-and-save (print-reports t))
  (let* ((*match-refactoring-tool* (ast-tooling:new-refactoring-tool
                                    (clang-database compilation-tool-database)
                                    source-namestrings))
         (*match-counter* 0)
         (*current-multitool* mtool)
         (*print-reports* print-reports)
         (*number-of-files* (length source-namestrings))
         (*current-file-index* 0))
    (format t "Starting search of ~a files~%" *number-of-files*)
    (format t "source-namestrings: ~a~%" source-namestrings)
    (finish-output)
    (incf *current-file-index*)
    (start-search-timer)
    (apply-arguments-adjusters compilation-tool-database *match-refactoring-tool*)
    (when (multitool-arguments-adjuster mtool)
      (ast-tooling:append-arguments-adjuster *match-refactoring-tool* (multitool-arguments-adjuster mtool)))
    (let* ((*run-and-save* run-and-save)
           (tools (multitool-active-tools mtool))
           (match-finder (let ((mf (make-permanent-match-finder)))
                           (dolist (tool tools)
                             (safe-add-dynamic-matcher mf (single-tool-matcher tool) (single-tool-callback tool) :matcher-sexp (single-tool-matcher-sexp tool)))
                           mf))
           (factory (ast-tooling:new-frontend-action-factory match-finder)))
      (dolist (tool tools)
        (let ((initializer (single-tool-initializer tool)))
          (when initializer (funcall (single-tool-initializer tool)))))
      (format t "About to run!  run-and-save -> ~a~%" run-and-save)
      (ecase (if run-and-save
                 (ast-tooling:run-and-save *match-refactoring-tool* factory)
                 (ast-tooling:clang-tool-run *match-refactoring-tool* factory))
        (0)
        (1 (error "Error while running clang tool."))
        (2 (error "Some files skipped due to incomplete database.")))
      (format t "Ran tools: ~a~%" (multitool-active-tool-names mtool))
      (format t "Total number of matches ~a~%" *match-counter*)
      (print-report))))

(defun sub-match-run (compiled-matcher matcher-sexp node ast-context code)
  "* Arguments
- compiled-matcher :: The matcher that you want to run on the node and everything under it.
- matcher-sexp :: The origin sexp of the compiled matcher.
- node :: The node that you want to run the matcher on.
- ast-context :: The AST context of the node.
- code :: The code that should be run on the matches.
* Description
Run a matcher on a node and everything underneath it.
The callback is a closure that closes over objects that are updated depending
on what the sub-matcher matches."
  (with-unmanaged-object (callback (make-instance 'code-match-callback :match-code code))
    (let ((sub-match-finder (make-temporary-match-finder)))
      (safe-add-dynamic-matcher sub-match-finder compiled-matcher callback :matcher-sexp matcher-sexp)
      (match sub-match-finder node ast-context))))

(defun run-matcher-on-loaded-asts (asts match-sexp &key callback counter-limit)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- callback :: A callback that is evaluated on matches.
- counter-limit :: The maximum number of times to call the callback.
* Description
Compile the match-sexp and run it on the loaded ASTs and call the callback for each match. 
Limit the number of times you call the callback with counter-limit."
  (unless (contains-hint-request match-sexp)
    (let* ((*match-counter* 0)
           (*match-counter-limit* counter-limit)
           (whole-matcher-sexp `(:bind :whole ,match-sexp)) ;(:bind :whole ,match-sexp))
           (matcher (compile-matcher whole-matcher-sexp))
           (match-finder (make-permanent-match-finder)))
      (safe-add-dynamic-matcher match-finder matcher callback :matcher-sexp match-sexp)
      (format t "About to start matching asts -> ~a~%" asts)
      (catch 'match-counter-reached-limit
        (map 'list #'(lambda (x) (match-ast match-finder (get-astcontext x))) asts))
      (format t "Number of matches ~a~%" *match-counter*))))

(defun match-count-loaded-asts (match-sexp &key limit &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: The maximum number of times to run the matcher.
* Description
Compile the match-sexp and run it on the loaded ASTs and count how many times it matches.
Limit the number of times you match."
  (with-unmanaged-object (callback (make-instance 'count-match-callback))
    (run-matcher-on-loaded-asts match-sexp
                            :callback callback
                            :counter-limit limit)))


(defun match-dump-loaded-asts (match-sexp &key limit (tag :whole) &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: The maximum number of times to run the matcher.
- tag :: A tag for the node to dump the AST of.
* Description
Dump the matches - if :tag is supplied then dump the given tag, otherwise :whole"
  (with-unmanaged-object (callback (make-instance 'dump-match-callback))
    (let ((*match-dump-tag* tag))
      (run-matcher-on-loaded-asts match-sexp
                         :callback callback
                         :counter-limit limit))))

(defun match-comments-loaded-asts (match-sexp &key match-comments code &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- match-comments :: A substring or list of substrings to match within comments.
- code :: A function to run.
* Description
I'm guessing at what this function does!!!!!
Run the match-sexp on the loaded ASTs and for each match, extract the associated comments
and match them to the match-comments regex.  If they match, run the code."
  (with-unmanaged-object (callback (make-instance
                                    'source-loc-match-callback
                                    :comments-substring-list (if (atom match-comments)
                                                             (list match-comments)
                                                             match-comments)
                                    :code code))
    (let ((*match-source-location* nil))
      (run-matcher-on-loaded-asts match-sexp
                         :callback callback)
      (format t "Matched the desired location: ~a~%" *match-source-location*))))


(defun match-run-loaded-asts (asts match-sexp &key limit callback)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: Limit the number of callback calls.
- callback :: The code evaluated for each match.
* Description
I'm guessing at what this function does!!!!!
Run the-code-match-callback (a functionRun the match-sexp on the loaded ASTs and for each match, extract the associated comments
and match them to the match-comments regex.  If they match, run the code."
  (run-matcher-on-loaded-asts asts match-sexp
                              :callback callback
                              :counter-limit limit))

(defun batch-match-run (match-sexp &key compilation-tool-database the-code-match-callback run-and-save)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- compilation-tool-database :: The compilation-tool-database.
- the-code-match-callback :: A regular expression to match to comments.
* Description
I'm guessing at what this function does!!!!!
Run the-code-match-callback (a functionRun the match-sexp on the loaded ASTs and for each match, extract the associated comments
and match them to the match-comments regex.  If they match, run the code."
  "Run code on every match in every filename in batch mode"
  (or the-code-match-callback (error "You must provide the-code-match-callback to batch-match-run"))
  (batch-run-matcher match-sexp
                           :compilation-tool-database compilation-tool-database
                           :callback the-code-match-callback
			   :run-and-save run-and-save))


#| ----------------------------------------------------------------------

Code for representing ASTMatchers as s-expressions

|#

(defvar *node-matcher-rules* nil)
(defparameter +narrowing-matcher-hints+ (make-hash-table :test #'eq))
(defparameter +node-matcher-hints+ (make-hash-table :test #'eq))
(defparameter +all-matchers+ (append *node-matcher-rules* *narrowing-matcher-rules* *traversal-matcher-rules*))

(dolist (i *node-matcher-rules*)
  (setf (gethash (car i) +node-matcher-hints+) (cdr i)))

(dolist (i *narrowing-matcher-rules*)
  (setf (gethash (car i) +narrowing-matcher-hints+) (cdr i)))

(define-condition wrong-matcher (condition)
  ((node-type :initarg :node-type :accessor wrong-matcher-node-type )))

(defun identify-node-type (node)
  (or (find-if #'(lambda (x) (eq node (second x))) *narrowing-matcher-rules*)
      (find-if #'(lambda (x) (eq node (second x))) *traversal-matcher-rules*)))

(defun applicable-matcher-p (prev-env matcher-env)
  (check-type prev-env list)
  (if (member :* prev-env)
      t
      (let ((super-classes (super-class-matchers prev-env)))
        (member matcher-env super-classes))))

(define-condition node-matcher-ambiguous-error (error)
  ((node-matchers :initarg :node-matchers :accessor node-matchers)))

(defun identify-node-matcher-environment (prev-environment node)
  (check-type prev-environment list)
  (block match
  (let ((node-matchers
         (remove-if-not (lambda (x)  ;; select-only-entries-that-satisfy
                          (and (eq node (second x))
                               (or (not prev-environment)
                                   (applicable-matcher-p prev-environment (first x)))))
                        *node-matcher-rules*)))
    (if node-matchers
        (progn
          (when (> (length node-matchers) 1)
            (return-from match (map 'list (lambda (x) (third x)) node-matchers))
;;;            (error (make-condition 'node-matcher-ambiguous-error :node-matchers matchers))
            )
          (let* ((matcher (first node-matchers)) ;; only one entry in node-matchers
                 (arg (third matcher)))
            (list arg)))
        nil))))

(defun identify-traversal-matcher-environment (prev-environment node)
  (check-type prev-environment list)
  (block id
    (let ((matchers (remove-if-not  ;; select-only-entries-that-satisfy
                     (lambda (x)
                       (and (eq node (second x))
                            (or (not prev-environment)
                                (eq (first x) :*)
                                (applicable-matcher-p prev-environment (first x)))))
                     *traversal-matcher-rules*)))
      (if matchers
          (progn
            (when (> (length matchers) 1)
              (return-from id (map 'list (lambda (x) (caddr x)) matchers))
;;;              (error (make-condition 'node-matcher-ambiguous-error :node-matchers matchers))
              )
            (let* ((matcher (car matchers)))
              (cond
                ((eq (caddr matcher) :*) (return-from id (list :*) #| prev-environment |# ))
                ((eq (caddr matcher) :unsigned-n) (return-from id (list (cadddr matcher))))
                )
              (list (caddr matcher))))
          nil))))

(defun identify-narrowing-matcher-environment (prev-environment node)
  (check-type prev-environment list)
  (block id
    (let ((matchers (remove-if-not (lambda (x) ;; select-only-entries-that-satisfy
                                     (and (eq node (second x))
                                          (or
;;;                                         (not prev-environment)
                                           (eq (first x) :*)
                                           (applicable-matcher-p prev-environment (first x)))))
                                   *narrowing-matcher-rules*)))
      (when matchers
        (when (> (length matchers) 1)
          (return-from id (map 'list (lambda (x) (third x)) matchers))
;;;        (error (make-condition 'node-matcher-ambiguous-error :node-matchers matchers))
          )
        (let* ((matcher (car matchers)))
          (when (eq (caddr matcher) :*)
            (return-from identify-narrowing-matcher-environment prev-environment))
          (if (third matcher)
              (list (third matcher))
              :no-argument) ; if we reach here we are in the argument of a narrowing matcher
          )))))


(defun error-unless-valid-predicate (p)
  (block good
    (when (position-if (lambda (x) (eq p (cadr x))) *node-matcher-rules*)
      (return-from good t))
    (when (position-if (lambda (x) (eq p (cadr x))) *traversal-matcher-rules*)
      (return-from good t))
    (when (position-if (lambda (x) (eq p (cadr x))) *narrowing-matcher-rules*)
      (return-from good t))
    (error "Invalid predicate ~a" p)))

(defun identify-hint-environment (prev-environment sexp)
  (check-type prev-environment list)
  (handler-case 
      (let ((node (car sexp)))
        (error-unless-valid-predicate node)
        (let ((env (identify-node-matcher-environment prev-environment node)))
          (check-type env list)
          (when env
            (return-from identify-hint-environment env)))
        (let ((env (identify-traversal-matcher-environment prev-environment node)))
          (check-type env list)
          (when env
            (return-from identify-hint-environment env)))
        (let ((env (identify-narrowing-matcher-environment prev-environment node)))
          (check-type env (or list symbol))
          (when env
            (return-from identify-hint-environment env)))
        (error "A matcher of one of type ~a (prev-environment -> ~a) was expected~% - you gave the matcher ~a~% - in the matcher-expression ~a~%"
               (if (eq prev-environment :*)
                   "ANYTHING"
                   (super-class-matchers prev-environment))
               prev-environment node sexp))
    (node-matcher-ambiguous-error ()
      (error "Hint node environment is ambiguous prev-environment is ~a; node is ~a; sexp is ~a~%"
             prev-environment (car sexp)  sexp))))

(defun provide-hint (environment)
  (format t "Environment: ~a~%" environment)
  (check-type environment list)
  (let ((*print-circle* nil))
    (let* ((super-classes (super-class-matchers environment))
           (node-matcher-records (remove-if-not (lambda (x) (member (car x) super-classes)) *node-matcher-rules*))
           (node-matchers (mapcar #'second node-matcher-records))
           (short-names (remove-if (lambda (x) (> (length (symbol-name x)) 20)) node-matchers))
           (long-names (remove-if-not (lambda (x) (> (length (symbol-name x)) 20)) node-matchers)))
      (when node-matchers
        (format t "Node matchers -----------------~%")
        (format t "~{   ~3@{~20a ~}~%~}" short-names)
        (format t "~{   ~2@{~30a ~}~%~}" long-names)))
    (format t "Narrowing matchers ---------~%")
    (narrowing-hint environment)
    (format t "Traversal matchers ---------~%")
    (traversal-hint environment)))


#+(or)
(dolist (m *traversal-matcher-rules*)
  (let ((some (remove-if-not (lambda (x) (eq (cadr m) (cadr x))) *all-matcher-rules*)))
    (when (> (length some) 1)
      (format t "matcher: ~a  ~a~%" (cadr m) (map 'list (lambda (x) (caddr x)) some)))))

(defgeneric super-class-matchers (x))

(defmethod super-class-matchers ((environments list))
  "Merge together the superclasses of a list of environments"
  (let (super-classes)
    (loop :for env :in environments
       :do (setf super-classes (union super-classes (super-class-matchers env))))
    super-classes))

(defmethod super-class-matchers ((node symbol))
  "Given a environment name as a keyword - generate a list of keyword environment names that
correspond to the environment names superclasses that are also part of the clang environment hierarchy"
  (let* ((node-class (cond
                       ;; I'm guessing that these first four are special cases
                       ;; that need to map to :cxxmethod-decl and :cxxrecord-decl
                       ;; because recordDecl 
                       ((eq node :method-decl) :cxxmethod-decl)
                       ((eq node :record-decl) :cxxrecord-decl)
                       ((eq node :cxxmethod-decl) :cxxmethod-decl)
                       ((eq node :cxxrecord-decl) :cxxrecord-decl)
                       (t node)))
         (name (symbol-name node-class))
         (symbol (intern name :cast)))
    (if (find-class symbol nil)
        (let* ((cpl (clos:class-precedence-list (find-class symbol)))
               (cast-pkg (find-package :cast))
               (cast-cpl (remove-if-not #'(lambda (x) (eq cast-pkg (symbol-package (name-of-class x)))) cpl)))
          (mapcar #'(lambda (x) (intern (symbol-name (name-of-class x)) :keyword)) cast-cpl))
        (signal 'wrong-matcher :node-type (identify-node-type node)))))

(defun narrowing-hint (environment)
  "For a list of valid environments give a hint for narrowing matchers that could narrow the number of matches"
  (check-type environment list)
  (handler-case (let ((supers (append (super-class-matchers environment) '(:*))))
                  (dolist (i *narrowing-matcher-rules*)
                    (when (member (car i) supers)
                      (format t "   ~a~%" (cdr i)))))
    (wrong-matcher (exception)
      (format t "Narrowing matchers aren't appropriate here - ~a~%" (wrong-matcher-node-type exception)))))

(defun traversal-hint (environment)
  "For a list of valid environments, give a hint for traversal matchers that will move us from there"
  (check-type environment list)
  (handler-case (let ((supers (append (super-class-matchers environment) '(:*))))
                  (dolist (i *traversal-matcher-rules*)
                    (when (member (car i) supers)
                      (format t "   ~a~%" (cdr i)))))
    (wrong-matcher (exception)
      (format t "Traversal matchers aren't appropriate here - ~a~%" (wrong-matcher-node-type exception)))))

(defun arg-for-cmd (node)
  (dolist (i (append *node-matcher-rules* *narrowing-matcher-rules* *traversal-matcher-rules*))
    (when (eq (cadr i) node)
      (format t "~a~%" i))))





#+(or)
(defun compile-matcher-arguments* (args diagnostics)
  (let ((arg-vec (make-array (length args))))
    (do* ((i 0 (1+ i))
          (rest args (cdr rest))
          (arg (car rest) (car rest)))
        ((null rest) arg-vec)
      (setf (elt arg-vec i)
            (cond
              ((consp arg) (ast-tooling:new-parser-value rest (ast-tooling:new-variant-value-matcher (compile-matcher* arg diagnostics))))
              ((stringp arg) (ast-tooling:new-parser-value rest (ast-tooling:new-variant-value-string arg)))
              ((integerp arg) (ast-tooling:new-parser-value rest (ast-tooling:new-variant-value-unsigned arg)))
              (t (error "Illegal matcher argument type ~a" arg)))))))


#+(or)
(defun compile-matcher* (sexp diagnostics)
  (cond
    ((eq (car sexp) :bind)
     ;; (bind {name} {matcher-sexp})
     (let* ((bind-name (cadr sexp))
            (body (caddr sexp))
            (bound-matcher-head (car body))
            (bound-matcher-arguments (compile-matcher-arguments* (cdr body) diagnostics)))
       (ast-tooling:construct-bound-matcher bound-matcher-head body (string bind-name) bound-matcher-arguments diagnostics)))
    (t (let* ((matcher-head (car sexp))
              (matcher-arguments (compile-matcher-arguments* (cdr sexp) diagnostics)))
         (assert matcher-head)
         (ast-tooling:construct-matcher matcher-head sexp matcher-arguments diagnostics)))))


(defparameter *hint-environment* nil)
(defun contains-hint-request-impl (sexp)
  "Look for hint requests and print the hint"
  (cond
    ((atom sexp) nil)
    ((eq (car sexp) :bind) (contains-hint-request-impl (caddr sexp)))
    ((eq (car sexp) :?)
     (provide-hint *hint-environment*)
     (throw 'got-hint-request t))
    (t
     (let ((*hint-environment* (identify-hint-environment *hint-environment* sexp)))
       (check-type *hint-environment* (or symbol list))
       (dolist (i (cdr sexp))
         (contains-hint-request-impl i))))))

(defun contains-hint-request (sexp)
  "Returns true if a hint-request was found in sexp and nil if not"
  (catch 'got-hint-request
    (contains-hint-request-impl sexp)
    nil))

(defvar *use-dynamic-ast-matcher-library* t)
(defun compile-matcher-safe (sexp)
  (let ((matcher-cform (cform-matcher sexp)))
;;;    (format t "Converted matcher to: ~a~%" matcher-cform)
    (ast-tooling:parse-dynamic-matcher matcher-cform)))
  

(defun compile-matcher (sexp)
  "* Arguments
- sexp :: The S-expression that describes an ASTMatcher.
* Description
Compile an ASTMatcher from an S-expression.
 eg: (compile-matcher '(:bind :field (:field-decl (:has-type (:record-decl (:has-name \"TinyStruct\"))))))).
Return nil if no matcher could be compiled."
  (if (contains-hint-request sexp)
      nil
      (compile-matcher-safe sexp)))

;;;
;;;  Convert my matchers back into C-style matchers
;;;

(defparameter +matcher-hash-table+
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (e ast-tooling:*matcher-names*)
      (setf (gethash (car e) ht) (cadr e)))
    ht))

(defun cform-matcher-name (sym)
  (let ((cm (gethash sym +matcher-hash-table+)))
    (or cm (error "Could not find c-matcher for ~a" cm)))) 

(defun cform-matcher (sexp)
  "* Arguments
- sexp :: An ASTMatcher in s-expression format.
* Description
Return a string representation of C++ code for the ASTMatcher."
  (cond
    ((typep sexp 'integer)
     (format nil "~a" sexp))
    ((typep sexp 'simple-string)
     (format nil "~s" sexp))
    ((typep sexp 'symbol)
     (format nil "\"~a\"" (symbol-name sexp)))
    ((and (consp sexp) (eq (car sexp) :bind))
     (format nil "~a.bind(~s)" (cform-matcher (caddr sexp)) (symbol-name (cadr sexp))))
    ((consp sexp)
     (with-output-to-string (sout)
       (format sout "~a(" (cform-matcher-name (car sexp)))
       (when (cdr sexp)
         (let ((args (mapcar (lambda (k) (cform-matcher k)) (cdr sexp) )))
         (format sout "~{~a~^, ~}" args)))
       (format sout ")")))
    (t (error "Add support to cform-matcher for ~a" sexp))))

