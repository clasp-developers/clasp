(provide :clang-tool)

(defpackage #:clang-tool
  (:use #:common-lisp #:core #:ast-tooling #:clang-ast)
  (:export
   #:with-compilation-tool-database
   #:main-pathname
   #:clang-database
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
   #:make-multitool
   #:multitool-add-matcher
   #:multitool-results
   #:code-match-callback
   #:mtag-node
   #:mtag-loc-start
   #:mtag-source
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
   #:load-asts))

(in-package :clang-tool)

(use-package :ast-tooling)
(use-package :clang-ast)




(defconstant +isystem-dir+ 
  #+target-os-darwin "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0"
  #+target-os-linux "/usr/include/clang/3.6/include"
  "Define the -isystem command line option for Clang compiler runs")

(defconstant +resource-dir+ 
  #+target-os-darwin "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0"
  #+target-os-linux "/usr/lib/llvm-3.6/bin/../lib/clang/3.6.0/include"
  #+(or)"/home/meister/Development/externals-clasp/build/release/lib/clang/3.6.2"
  "Define the -resource-dir command line option for Clang compiler runs")
(defconstant +additional-arguments+
  #+target-os-darwin (vector
                      "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0.0/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks")
  #-target-os-darwin (vector
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3/x86_64-redhat-linux"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3/tr1"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/lib/gcc/x86_64-redhat-linux/4.8.3/include"))






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
   (main-source-filename :initform "main.cc" :initarg :main-source-filename :accessor main-source-filename)
   (source-namestrings :initarg :source-namestrings :accessor source-namestrings)
   (arguments-adjuster-list :initform nil :initarg :arguments-adjuster-list :accessor arguments-adjuster-list)))

(defmethod initialize-instance :after ((obj compilation-tool-database) &rest args)
  "* Description
Initialize the source-namestrings using all filenames in the database if they haven't already been set."
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

(defun fix-path (root rel)
  (let* ((pnroot (pathname root))
         (pnrel (pathname rel))
         (merged (merge-pathnames pnrel pnroot))
         (abs #+(or)(uiop/filesystem:native-namestring merged) (namestring merged)))
    (or abs (error "Could not find absolute path for ~a + ~a" root rel))))

(defun ensure-directory (relpath)
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
           (let* ((fixed-path (fix-path root-directory (ensure-directory (subseq arg 2))))
                  (new-arg (concatenate 'string "-I" fixed-path)))
             (elt-set new-args i new-arg)))
          ((string= arg "../" :start1 0 :end1 3)
           (let ((fixed-path (fix-path root-directory arg)))
             (elt-set new-args i fixed-path)))
          (t #| do nothing |# ))))
    new-args))

(defun setup-default-arguments-adjusters (compilation-tool-database &key convert-relative-includes-to-absolute )
  "* Arguments
- compilation-tool-database :: The compilation-tool-database to add the arguments adjusters to.
- convert-relative-includes-to-absolute :: If T the main-pathname of the compilation-tool-database is used
as the root of the absolute includes, if nil relative includes are left alone.
Otherwise the value of convert-relative-includes-to-absolute is used.
* Description
Setup the default arguments adjusters."
  (push (ast-tooling:get-clang-syntax-only-adjuster) (arguments-adjuster-list compilation-tool-database))
  (push (ast-tooling:get-clang-strip-output-adjuster) (arguments-adjuster-list compilation-tool-database))
  (push (lambda (args)
          (concatenate 'vector
                       args
                       (vector "-isystem" +isystem-dir+
                               "-resource-dir" +resource-dir+)
                       +additional-arguments+))
        (arguments-adjuster-list compilation-tool-database))
  (cond
    ((eq convert-relative-includes-to-absolute t)
     (push (lambda (args)
             (convert-relative-includes-to-absolute args (main-pathname compilation-tool-database)))
           (arguments-adjuster-list compilation-tool-database)))
    (convert-relative-includes-to-absolute
     (push (lambda (args)
             (convert-relative-includes-to-absolute args convert-relative-includes-to-absolute))
           (arguments-adjuster-list compilation-tool-database)))
    (t #| Do nothing |#)))

(defun load-compilation-tool-database (pathname &key (main-source-filename "main.cc")
                                                  convert-relative-includes-to-absolute)
  "* Arguments
- pathname :: The name of the file that contains the compilation database in JSON format.
See http://clang.llvm.org/docs/JSONCompilationDatabase.html for the format
- main-source-filename :: The name of the source file that is considered the main source file.  Default is main.cc.
- convert-relative-includes-to-absolute :: If T the main-pathname of the compilation-tool-database is used
as the root of the absolute includes, if nil relative includes are left alone.
Otherwise the value of convert-relative-includes-to-absolute is used.
* Description
Load the compilation database and return it"
  (let* ((db (ast-tooling:jsoncompilation-database-load-from-file
              (namestring (probe-file pathname))))
         (all-files (map 'list #'identity (ast-tooling:get-all-files db)))
         (ctd (make-instance 'compilation-tool-database
                             :clang-database db
                             :main-source-filename main-source-filename)))
    (format t "Loaded database contains ~a source files~%" (length all-files))
    (setup-default-arguments-adjusters ctd :convert-relative-includes-to-absolute convert-relative-includes-to-absolute)
    ctd))

(defun main-pathname (&optional (compilation-tool-database *compilation-tool-database*))
  "* Arguments
- compilation-tool-database :: The compilation database.
* Return Value
A pathname.
* Description
Return the pathname of the directory that contains the main source file."
  (let ((source-namestrings (select-source-namestrings compilation-tool-database))
        (main-source-filename (pathname (main-source-filename compilation-tool-database))))
    (dolist (name source-namestrings)
      (let ((pn (pathname name)))
        (when (and (string= (pathname-name pn) (pathname-name main-source-filename))
                   (string= (pathname-type pn) (pathname-type main-source-filename)))
          (return-from main-pathname pn)))))
  (error "Could not find main filename"))


(defun select-source-namestrings (compilation-tool-database &optional (pattern ".*"))
  "* Arguments
- compilation-tool-database :: The compilation database.
- pattern :: A regex pattern for selecting file names from the database.
* Description
Select a subset (or all) source file names from the compilation database and return them as a list."
  (let ((re (core:make-regex pattern))
        (list-names (map 'list #'identity (ast-tooling:get-all-files (clang-database compilation-tool-database)))))
    (remove-if-not #'(lambda (x) (core:regex-matches re x)) list-names)))


(defparameter *match-refactoring-tool* nil)
(defparameter *run-and-save* nil)

(defparameter *match-counter* 0)
(defparameter *match-counter-limit* nil)

(defun advance-match-counter ()
  (setq *match-counter* (1+ *match-counter*))
  (when *match-counter-limit*
      (when (>= *match-counter* *match-counter-limit*)
        (throw 'match-counter-reached-limit *match-counter*))))

(defparameter *match-dump-tag* nil)
(defclass good-dump-match-callback (ast-tooling:match-callback) () )
(core:defvirtual ast-tooling:run ((self good-dump-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash *match-dump-tag* id-to-node-map))
         (context (context match))
         (source-manager (ast-tooling:source-manager match))
         (lang-options (get-lang-opts context))
         (begin (get-loc-start node))
         (_end (get-loc-end node))
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

(defclass dump-match-callback (ast-tooling:match-callback) () )
(core:defvirtual ast-tooling:run ((self dump-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash *match-dump-tag* id-to-node-map)))
    (format t "*match-dump-tag* = ~a~%" *match-dump-tag*)
    (cast:dump node)
      (advance-match-counter)))



(defclass count-match-callback (ast-tooling:match-callback) () )
(core:defvirtual ast-tooling:run ((self count-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (id-to-node-map (ast-tooling:idto-node-map nodes))
         (node (gethash :whole id-to-node-map)))
    (advance-match-counter)))




(defclass match-info ()
  ((id-to-node-map :initarg :id-to-node-map :accessor id-to-node-map)
   (ast-context :initarg :ast-context :accessor ast-context)
   (source-manager :initarg :source-manager :accessor source-manager)))

;;Requires a lambda CODE that takes a single match-info argument
(defclass code-match-callback (ast-tooling:match-callback)
  ((start-of-translation-unit-code :initarg :start-of-translation-unit-code :accessor start-of-translation-unit-code)
   (match-code :initarg :match-code :accessor match-code)
   (end-of-translation-unit-code :initarg :end-of-translation-unit-code :accessor end-of-translation-unit-code)))

(core:defvirtual ast-tooling:on-start-of-translation-unit ((self code-match-callback))
  (format t "on-start-of-translation-unit for code-match-callback of: ~a~%" self)
  (when (slot-boundp self 'start-of-translation-unit-code)
    (assert (start-of-translation-unit-code self))
    (funcall (start-of-translation-unit-code self))))

(core:defvirtual ast-tooling:run ((self code-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (match-info (make-instance 'match-info
                                    :id-to-node-map (ast-tooling:idto-node-map nodes)
                                    :ast-context (context match)
                                    :source-manager (ast-tooling:source-manager match))))
    (when (match-code self)
      (funcall (match-code self) match-info)
      (advance-match-counter))))

(core:defvirtual ast-tooling:on-end-of-translation-unit ((self code-match-callback))
  (format t "on-end-of-translation-unit for code-match-callback of: ~a~%" self)
  (when (slot-boundp self 'end-of-translation-unit-code)
    (assert (end-of-translation-unit-code self))
    (funcall (end-of-translation-unit-code self))))



(defclass source-loc-match-callback (code-match-callback)
  ((comments-regex-list :accessor comments-regex-list :initarg :comments-regex-list)))


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
          (dolist (comment-regex (comments-regex-list source-loc-match-callback))
            (when (core:regex-matches comment-regex comment)
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
                                    :ast-context (context match)
                                    :source-manager (ast-tooling:source-manager match))))
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
         (begin (get-loc-start node))
         (_end (get-loc-end node))
         (end (lexer-get-loc-for-end-of-token _end 0 (source-manager match-info) lang-options))
         (token-range (new-char-source-range-get-char-range begin end)))
    (multiple-value-bind (source invalid)
        (lexer-get-source-text token-range (source-manager match-info) lang-options)
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
  (if (cast:get-identifier node)
      (cast:get-name node)
      "NO-NAME"))

(defgeneric mtag-source-impl (match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:decl))
  (mtag-source-decl-stmt-impl match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:stmt))
  (mtag-source-decl-stmt-impl match-info node))

(defmethod mtag-source-impl (match-info (node clang-ast:qual-type))
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

(defun ploc-as-string (ploc)
  (declare (special *compilation-tool-database*))
  (when (ast-tooling:is-invalid ploc)
    (return-from ploc-as-string "<invalid source-location>"))
  (let* ((relative (pathname (ast-tooling:presumed-loc-get-filename ploc)))
         (absolute (make-pathname :name nil :type nil :defaults (main-pathname *compilation-tool-database*)))
         (pathname (merge-pathnames (make-pathname :host (pathname-host absolute)
                                                   :device (pathname-device absolute)
                                                   :defaults relative) absolute))
         (probed-file (probe-file pathname)))
    (if (null probed-file)
        (format nil "Could not locate ~a --> after merging ~a" relative pathname)
        (format nil "~a:~a:~a" (namestring probed-file) (ast-tooling:get-line ploc) (ast-tooling:get-column ploc)))))


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
- sloc :: The source location object.
* Description 
Return a string that describes the start of a source location."
  (let* ((node (mtag-node match-info tag))
         (begin-sloc (get-loc-start node)))
    (source-loc-as-string match-info begin-sloc)))



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

(defmacro mtag-replace (match-info tag replace-callback)
  "* Arguments
- match-info :: A match-info.
- tag :: A tag that is associated with a node in match-info.
- replace-callback :: A function that takes match-info and tag and returns a string.
* Description
Calls the replace-callback function (funcall replace-callback match-info tag) and generates a replacement
for the node corresponding to tag in match-info."
  (let ((rep-src-gs (gensym))
        (node-gs (gensym))
        (begin-gs (gensym))
        (end-gs (gensym))
        (rep-range-gs (gensym))
        (rep-gs (gensym))
        (replacements-gs (gensym)))
    `(let* ((,rep-src-gs (funcall ,replace-callback ,match-info ,tag))
	    (,node-gs (mtag-node ,match-info ,tag)) ;; (id-to-node-map match-info)))
	    (,begin-gs (get-loc-start ,node-gs))
	    (,end-gs (get-loc-end ,node-gs))
	    (,rep-range-gs (new-char-source-range-get-token-range ,begin-gs ,end-gs))
	    (,rep-gs (new-replacement (source-manager ,match-info) ,rep-range-gs ,rep-src-gs)))
       (if *match-refactoring-tool*
           (let ((,replacements-gs (ast-tooling:get-replacements *match-refactoring-tool*)))
             (ast-tooling:replacements-insert ,replacements-gs ,rep-gs))
           (format t "Replacing: ~a~%     with: ~a~%" (clang-tool:mtag-source ,match-info ,tag) ,rep-src-gs)))))


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
          (,begin-gs (get-loc-start ,node-gs))
          (,end-gs (get-loc-end ,node-gs))
          (,rep-range-gs (new-char-source-range-get-token-range ,begin-gs ,end-gs))
          (,rep-gs (new-replacement (source-manager match-info) ,rep-range-gs ,rep-src-gs)))
     (push ,rep-gs *match-results*))))


(defvar *asts* nil)



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
      (time 
       (multiple-value-bind (num asts)
           (ast-tooling:build-asts tool)
         (setq *asts* asts)
         (format t "Built asts: ~a~%" asts))))))

(defun batch-run-matcher (match-sexp &key compilation-tool-database callback run-and-save)
  (declare (type list match-sexp)
           (type ast-tooling:match-callback callback))
  (let* ((*match-refactoring-tool* (ast-tooling:new-refactoring-tool
                                    (clang-database compilation-tool-database)
                                    (source-namestrings compilation-tool-database))))
    (apply-arguments-adjusters compilation-tool-database *match-refactoring-tool*)
    (let ((*run-and-save* run-and-save)
          (matcher (compile-matcher `(:bind :whole ,match-sexp)))
          (match-finder (let ((mf (ast-tooling:new-match-finder)))
                          (ast-tooling:add-dynamic-matcher mf matcher callback)
                          mf))
          (factory (ast-tooling:new-frontend-action-factory match-finder)))
      (time (if (not run-and-save)
                (ast-tooling:clang-tool-run *match-refactoring-tool* factory)
                (ast-tooling:run-and-save *match-refactoring-tool* factory)))
      (format t "Number of matches ~a~%" *match-counter*))))



(defstruct multitool
  "Store multiple tools to run in one go across a bunch of source files."
  all-tools   ;; the list of all tools
  selected-tools  ;; the list of selected tools - if nil then use all-tools
  results
  arguments-adjuster )


(defstruct single-tool
  name
  initializer
  matcher
  callback
)

(defun multitool-add-matcher (mtool &key name matcher callback initializer)
  "Keep track of matchers and callbacks so they don't go out of scope while the tool is alive"
  (let ((tool (make-single-tool :name name
                                :initializer initializer
                                :matcher matcher
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


(defun batch-run-multitool (mtool &key compilation-tool-database run-and-save)
  (let* ((*match-refactoring-tool* (ast-tooling:new-refactoring-tool
                                    (clang-database compilation-tool-database)
                                    (source-namestrings compilation-tool-database))))
    (apply-arguments-adjusters compilation-tool-database *match-refactoring-tool*)
    (when (multitool-arguments-adjuster mtool)
      (ast-tooling:append-arguments-adjuster temp (multitool-arguments-adjuster mtool)))
    (let* ((*run-and-save* run-and-save)
           (tools (multitool-active-tools mtool))
           (match-finder (let ((mf (ast-tooling:new-match-finder)))
                           (dolist (tool tools)
                             (ast-tooling:add-dynamic-matcher mf (single-tool-matcher tool) (single-tool-callback tool)))
                           mf))
           (factory (ast-tooling:new-frontend-action-factory match-finder)))
      (dolist (tool tools)
        (let ((initializer (single-tool-initializer tool)))
          (when initializer (funcall (single-tool-initializer tool)))))
      (time (if run-and-save
                (ast-tooling:run-and-save *match-refactoring-tool* factory)
                (ast-tooling:clang-tool-run *match-refactoring-tool* factory)))
      (format t "Ran tools: ~a~%" (multitool-active-tool-names mtool))
      (format t "Number of matches ~a~%" *match-counter*))))

(defun sub-match-run (compiled-matcher node ast-context code)
  "* Arguments
- compiled-matcher :: The matcher that you want to run on the node and everything under it.
- node :: The node that you want to run the matcher on.
- ast-context :: The AST context of the node.
- code :: The code that should be run on the matches.
* Description
Run a matcher on a node and everything underneath it."
  (with-unmanaged-object (callback (make-instance 'code-match-callback :match-code code))
    (let ((sub-match-finder (ast-tooling:new-match-finder)))
      (ast-tooling:add-dynamic-matcher sub-match-finder compiled-matcher callback)
      (match sub-match-finder node ast-context))))

(defun run-matcher-on-loaded-asts (match-sexp &key callback counter-limit)
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
           (whole-matcher-sexp `(:bind :whole ,match-sexp))
           (matcher (compile-matcher whole-matcher-sexp))
           (match-finder (ast-tooling:new-match-finder)))
      (ast-tooling:add-dynamic-matcher match-finder matcher callback)
      (catch 'match-counter-reached-limit
        (map 'list #'(lambda (x) (match-ast match-finder (get-astcontext x))) *asts*))
      (format t "Number of matches ~a~%" *match-counter*))))

(defun match-count-loaded-asts (match-sexp &key limit &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: The maximum number of times to run the matcher.
* Description
Compile the match-sexp and run it on the loaded ASTs and count how many times it matches.
Limit the number of times you match."
  (with-unmanaged-object (callback (make-instance 'count-match-callback))
         (time (run-matcher-on-loaded-asts match-sexp
                            :callback callback
                            :counter-limit limit))))


(defun match-dump-loaded-asts (match-sexp &key limit (tag :whole) &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: The maximum number of times to run the matcher.
- tag :: A tag for the node to dump the AST of.
* Description
Dump the matches - if :tag is supplied then dump the given tag, otherwise :whole"
  (with-unmanaged-object (callback (make-instance 'dump-match-callback))
    (let ((*match-dump-tag* tag))
      (time (run-matcher-on-loaded-asts match-sexp
                         :callback callback
                         :counter-limit limit)))))

(defun match-comments-loaded-asts (match-sexp &key match-comments code &allow-other-keys)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- match-comments :: A regular expression to match to comments.
- code :: A function to run.
* Description
I'm guessing at what this function does!!!!!
Run the match-sexp on the loaded ASTs and for each match, extract the associated comments
and match them to the match-comments regex.  If they match, run the code."
  (with-unmanaged-object (callback (make-instance
                                    'source-loc-match-callback
                                    :comments-regex-list (if (atom match-comments)
                                                             (list (core:make-regex match-comments))
                                                             (mapcar #'(lambda (str) (core:make-regex str)) match-comments))
                                    :code code))
    (let ((*match-source-location* nil))
      (time (run-matcher-on-loaded-asts match-sexp
                         :callback callback))
      (format t "Matched the desired location: ~a~%" *match-source-location*))))


(defun match-run-loaded-asts (match-sexp &key limit callback)
  "* Arguments
- match-sexp :: A matcher in s-expression form.
- limit :: Limit the number of callback calls.
- callback :: The code evaluated for each match.
* Description
I'm guessing at what this function does!!!!!
Run the-code-match-callback (a functionRun the match-sexp on the loaded ASTs and for each match, extract the associated comments
and match them to the match-comments regex.  If they match, run the code."
  (time (run-matcher-on-loaded-asts match-sexp
                     :callback callback
                     :counter-limit limit)))

(defun batch-match-run (match-sexp &key compilation-tool-database limit the-code-match-callback run-and-save)
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
  (time (batch-run-matcher match-sexp
                           :compilation-tool-database compilation-tool-database
                           :callback the-code-match-callback
			   :run-and-save run-and-save)))


#| ----------------------------------------------------------------------

Code for representing ASTMatchers as s-expressions

|#

(defun keyword-everything (tree)
  (mapcar #'(lambda (l) (mapcar #'(lambda (x) (intern x :keyword)))) l) tree )


(defconstant +node-matchers+
  '((:CXXCTOR-INITIALIZER :CTOR-INITIALIZER :CXXCTOR-INITIALIZER)
    (:DECL :ACCESS-SPEC-DECL :ACCESS-SPEC-DECL)
    (:DECL :CLASS-TEMPLATE-DECL :CLASS-TEMPLATE-DECL)
    (:DECL :CLASS-TEMPLATE-SPECIALIZATION-DECL :CLASS-TEMPLATE-SPECIALIZATION-DECL)
    (:DECL :CONSTRUCTOR-DECL :CXXCONSTRUCTOR-DECL)
    (:DECL :DECL :DECL)
    (:DECL :DECLARATOR-DECL :DECLARATOR-DECL)
    (:DECL :DESTRUCTOR-DECL :CXXDESTRUCTOR-DECL)
    (:DECL :ENUM-CONSTANT-DECL :ENUM-CONSTANT-DECL)
    (:DECL :ENUM-DECL :ENUM-DECL)
    (:DECL :FIELD-DECL :FIELD-DECL)
    (:DECL :FRIEND-DECL :FRIEND-DECL)
    (:DECL :FUNCTION-DECL :FUNCTION-DECL)
    (:DECL :FUNCTION-TEMPLATE-DECL :FUNCTION-TEMPLATE-DECL)
    (:DECL :METHOD-DECL :CXXMETHOD-DECL)
    (:DECL :NAMED-DECL :NAMED-DECL)
    (:DECL :NAMESPACE-DECL :NAMESPACE-DECL)
    (:DECL :PARM-VAR-DECL :PARM-VAR-DECL)
    (:DECL :RECORD-DECL :CXXRECORD-DECL)
    (:DECL :UNRESOLVED-USING-VALUE-DECL :UNRESOLVED-USING-VALUE-DECL)
    (:DECL :USING-DECL :USING-DECL)
    (:DECL :VAR-DECL :VAR-DECL)
    (:NESTED-NAME-SPECIFIER-LOC :NESTED-NAME-SPECIFIER-LOC :NESTED-NAME-SPECIFIER-LOC)
    (:NESTED-NAME-SPECIFIER :NESTED-NAME-SPECIFIER :NESTED-NAME-SPECIFIER)
    (:QUAL-TYPE :QUAL-TYPE :QUAL-TYPE)
    (:STMT :ARRAY-SUBSCRIPT-EXPR :ARRAY-SUBSCRIPT-EXPR)
    (:STMT :ASM-STMT :ASM-STMT)
    (:STMT :BINARY-OPERATOR :BINARY-OPERATOR)
    (:STMT :BIND-TEMPORARY-EXPR :CXXBIND-TEMPORARY-EXPR)
    (:STMT :BOOL-LITERAL :CXXBOOL-LITERAL-EXPR)
    (:STMT :BREAK-STMT :BREAK-STMT)
    (:STMT :C-STYLE-CAST-EXPR :CSTYLE-CAST-EXPR)
    (:STMT :CALL-EXPR :CALL-EXPR)
    (:STMT :CASE-STMT :CASE-STMT)
    (:STMT :CAST-EXPR :CAST-EXPR)
    (:STMT :CATCH-STMT :CXXCATCH-STMT)
    (:STMT :CHARACTER-LITERAL :CHARACTER-LITERAL)
    (:STMT :COMPOUND-LITERAL-EXPR :COMPOUND-LITERAL-EXPR)
    (:STMT :COMPOUND-STMT :COMPOUND-STMT)
    (:STMT :CONDITIONAL-OPERATOR :CONDITIONAL-OPERATOR)
    (:STMT :CONST-CAST-EXPR :CXXCONST-CAST-EXPR)
    (:STMT :CONSTRUCT-EXPR :CXXCONSTRUCT-EXPR)
    (:STMT :CONTINUE-STMT :CONTINUE-STMT)
    (:STMT :DECL-REF-EXPR :DECL-REF-EXPR)
    (:STMT :DECL-STMT :DECL-STMT)
    (:STMT :DEFAULT-ARG-EXPR :CXXDEFAULT-ARG-EXPR)
    (:STMT :DEFAULT-STMT :DEFAULT-STMT)
    (:STMT :DELETE-EXPR :CXXDELETE-EXPR)
    (:STMT :DO-STMT :DO-STMT)
    (:STMT :DYNAMIC-CAST-EXPR :CXXDYNAMIC-CAST-EXPR)
    (:STMT :EXPLICIT-CAST-EXPR :EXPLICIT-CAST-EXPR)
    (:STMT :EXPR :EXPR)
    (:STMT :FLOAT-LITERAL :FLOATING-LITERAL)
    (:STMT :FOR-RANGE-STMT :CXXFOR-RANGE-STMT)
    (:STMT :FOR-STMT :FOR-STMT)
    (:STMT :FUNCTIONAL-CAST-EXPR :CXXFUNCTIONAL-CAST-EXPR)
    (:STMT :GOTO-STMT :GOTO-STMT)
    (:STMT :IF-STMT :IF-STMT)
    (:STMT :IMPLICIT-CAST-EXPR :IMPLICIT-CAST-EXPR)
    (:STMT :INIT-LIST-EXPR :INIT-LIST-EXPR)
    (:STMT :INTEGER-LITERAL :INTEGER-LITERAL)
    (:STMT :LABEL-STMT :LABEL-STMT)
    (:STMT :LAMBDA-EXPR :LAMBDA-EXPR)
    (:STMT :MATERIALIZE-TEMPORARY-EXPR :MATERIALIZE-TEMPORARY-EXPR)
    (:STMT :MEMBER-CALL-EXPR :CXXMEMBER-CALL-EXPR)
    (:STMT :MEMBER-EXPR :MEMBER-EXPR)
    (:STMT :NEW-EXPR :CXXNEW-EXPR)
    (:STMT :NULL-PTR-LITERAL-EXPR :CXXNULL-PTR-LITERAL-EXPR)
    (:STMT :NULL-STMT :NULL-STMT)
    (:STMT :OPERATOR-CALL-EXPR :CXXOPERATOR-CALL-EXPR)
    (:STMT :REINTERPRET-CAST-EXPR :CXXREINTERPRET-CAST-EXPR)
    (:STMT :RETURN-STMT :RETURN-STMT)
    (:STMT :STATIC-CAST-EXPR :CXXSTATIC-CAST-EXPR)
    (:STMT :STMT :STMT)
    (:STMT :STRING-LITERAL :STRING-LITERAL)
    (:STMT :SWITCH-CASE :SWITCH-CASE)
    (:STMT :SWITCH-STMT :SWITCH-STMT)
    (:STMT :TEMPORARY-OBJECT-EXPR :CXXTEMPORARY-OBJECT-EXPR)
    (:STMT :THIS-EXPR :CXXTHIS-EXPR)
    (:STMT :THROW-EXPR :CXXTHROW-EXPR)
    (:STMT :TRY-STMT :CXXTRY-STMT)
    (:STMT :UNARY-EXPR-OR-TYPE-TRAIT-EXPR :UNARY-EXPR-OR-TYPE-TRAIT-EXPR)
    (:STMT :UNARY-OPERATOR :UNARY-OPERATOR)
    (:STMT :UNRESOLVED-CONSTRUCT-EXPR :CXXUNRESOLVED-CONSTRUCT-EXPR)
    (:STMT :USER-DEFINED-LITERAL :USER-DEFINED-LITERAL)
    (:STMT :WHILE-STMT :WHILE-STMT)
    (:TYPE-LOC :TYPE-LOC :TYPE-LOC)
    (:TYPE :ARRAY-TYPE :ARRAY-TYPE)
    (:TYPE :ATOMIC-TYPE :ATOMIC-TYPE)
    (:TYPE :AUTO-TYPE :AUTO-TYPE)
    (:TYPE :BLOCK-POINTER-TYPE :BLOCK-POINTER-TYPE)
    (:TYPE :BUILTIN-TYPE :BUILTIN-TYPE)
    (:TYPE :COMPLEX-TYPE :COMPLEX-TYPE)
    (:TYPE :CONSTANT-ARRAY-TYPE :CONSTANT-ARRAY-TYPE)
    (:TYPE :DEPENDENT-SIZED-ARRAY-TYPE :DEPENDENT-SIZED-ARRAY-TYPE)
    (:TYPE :ELABORATED-TYPE :ELABORATED-TYPE)
    (:TYPE :FUNCTION-TYPE :FUNCTION-TYPE)
    (:TYPE :INCOMPLETE-ARRAY-TYPE :INCOMPLETE-ARRAY-TYPE)
    (:TYPE :L-VALUE-REFERENCE-TYPE :LVALUE-REFERENCE-TYPE)
    (:TYPE :MEMBER-POINTER-TYPE :MEMBER-POINTER-TYPE)
    (:TYPE :PAREN-TYPE :PAREN-TYPE)
    (:TYPE :POINTER-TYPE :POINTER-TYPE)
    (:TYPE :R-VALUE-REFERENCE-TYPE :RVALUE-REFERENCE-TYPE)
    (:TYPE :RECORD-TYPE :RECORD-TYPE)
    (:TYPE :REFERENCE-TYPE :REFERENCE-TYPE)
    (:TYPE :TEMPLATE-SPECIALIZATION-TYPE :TEMPLATE-SPECIALIZATION-TYPE)
    (:TYPE :TYPE :TYPE)
    (:TYPE :TYPEDEF-TYPE :TYPEDEF-TYPE)
    (:TYPE :UNARY-TRANSFORM-TYPE :UNARY-TRANSFORM-TYPE)
    (:TYPE :VARIABLE-ARRAY-TYPE :VARIABLE-ARRAY-TYPE)))
;;;
;;; narrowing matchers of the type :* ... :* require submatchers
;;; of the same type that they are within
;;;
(defconstant +narrowing-matchers+
  '((:* :ALL-OF :* :... :*)
    (:* :ANY-OF :* :... :*)
    (:* :ANYTHING)
    (:* :UNLESS :*)
    (:BINARY-OPERATOR :HAS-OPERATOR-NAME :STRING-NAME)
    (:CXXBOOL-LITERAL :EQUALS :VALUET-VALUE)
    (:CXXCONSTRUCT-EXPR :ARGUMENT-COUNT-IS :UNSIGNED-N)
    (:DECL :IS-IMPLICIT)
    (:CXXCTOR-INITIALIZER :IS-WRITTEN)
    (:CXXMETHOD-DECL :HAS-OVERLOADED-OPERATOR-NAME :STRINGREF-NAME)
    (:CXXMETHOD-DECL :IS-CONST)
    (:CXXMETHOD-DECL :IS-OVERRIDE)
    (:CXXMETHOD-DECL :IS-VIRTUAL)
    (:CXXOPERATOR-CALL-EXPR :HAS-OVERLOADED-OPERATOR-NAME :STRINGREF-NAME)
    (:CXXRECORD-DECL :IS-DERIVED-FROM :STRINGREF-BASENAME)
    (:CXXRECORD-DECL :IS-EXPLICIT-TEMPLATE-SPECIALIZATION)
    (:CXXRECORD-DECL :IS-SAME-OR-DERIVED-FROM :STRINGREF-BASENAME)
    (:CXXRECORD-DECL :IS-TEMPLATE-INSTANTIATION)
    (:CALL-EXPR :ARGUMENT-COUNT-IS :UNSIGNED-N)
    (:CHARACTER-LITERAL :EQUALS :VALUET-VALUE)
    (:COMPOUND-STMT :STATEMENT-COUNT-IS :UNSIGNED-N)
    (:CONSTANT-ARRAY-TYPE :HAS-SIZE :UNSIGNED-N)
    (:DECL-STMT :DECL-COUNT-IS :UNSIGNED-N)
    (:DECL :EQUALS-BOUND-NODE :STRING-ID)
    (:DECL :EQUALS-NODE :DECL*-OTHER)
    (:DECL :IS-PRIVATE)
    (:DECL :IS-PROTECTED)
    (:DECL :IS-PUBLIC)
    (:FLOATING-LITERAL :EQUALS :VALUET-VALUE)
    (:FUNCTION-DECL :IS-DEFINITION)
    (:FUNCTION-DECL :IS-EXPLICIT-TEMPLATE-SPECIALIZATION)
    (:FUNCTION-DECL :IS-EXTERN-C)
    (:FUNCTION-DECL :IS-TEMPLATE-INSTANTIATION)
    (:FUNCTION-DECL :PARAMETER-COUNT-IS :UNSIGNED-N)
    (:INTEGER-LITERAL :EQUALS :VALUET-VALUE)
    (:MEMBER-EXPR :IS-ARROW)
    (:NAMED-DECL :HAS-NAME :STRING-NAME)
    (:NAMED-DECL :MATCHES-NAME :STRING-REGEXP)
    (:QUAL-TYPE :AS-STRING :STRING-NAME)
    (:QUAL-TYPE :EQUALS-BOUND-NODE :STRING-ID)
    (:QUAL-TYPE :HAS-LOCAL-QUALIFIERS)
    (:QUAL-TYPE :IS-CONST-QUALIFIED)
    (:QUAL-TYPE :IS-INTEGER)
    (:STMT :EQUALS-BOUND-NODE :STRING-ID)
    (:STMT :EQUALS-NODE :STMT*-OTHER)
    (:TAG-DECL :IS-DEFINITION)
    (:TYPE :EQUALS-BOUND-NODE :STRING-ID)
    (:UNARY-EXPR-OR-TYPE-TRAIT-EXPR :OF-KIND :UNARY-EXPR-OR-TYPE-TRAIT-KIND)
    (:UNARY-OPERATOR :HAS-OPERATOR-NAME :STRING-NAME)
    (:VAR-DECL :IS-DEFINITION)
    (:VAR-DECL :IS-EXPLICIT-TEMPLATE-SPECIALIZATION)
    (:VAR-DECL :IS-TEMPLATE-INSTANTIATION)))

;;;
;;; traversal matchers of the type :* ... :* allow submatchers
;;; of any kind?????
;;;
(defconstant +traversal-matchers+
  '((:*	:each-Of	:* :... :*)
    (:*	:for-Each	:*)
    (:*	:for-Each-Descendant	:*)
    (:*	:has	:*)
    (:*	:has-Ancestor	:*)
    (:*	:has-Descendant	:*)
    (:*	:has-parent	:*)
    (:ARRAY-SUBSCRIPT-EXPR :HAS-BASE :EXPR)
    (:ARRAY-SUBSCRIPT-EXPR :HAS-INDEX :EXPR)
    (:ARRAY-TYPE-LOC :HAS-ELEMENT-TYPE-LOC :TYPE-LOC)
    (:ARRAY-TYPE :HAS-ELEMENT-TYPE :TYPE)
    (:ATOMIC-TYPE-LOC :HAS-VALUE-TYPE-LOC :TYPE-LOC)
    (:ATOMIC-TYPE :HAS-VALUE-TYPE :TYPE)
    (:AUTO-TYPE :HAS-DEDUCED-TYPE :TYPE)
    (:BINARY-OPERATOR :HAS-EITHER-OPERAND :EXPR)
    (:BINARY-OPERATOR :HAS-LHS :EXPR)
    (:BINARY-OPERATOR :HAS-RHS :EXPR)
    (:BLOCK-POINTER-TYPE-LOC :POINTEE-LOC :TYPE-LOC)
    (:BLOCK-POINTER-TYPE :POINTEE :TYPE)
    (:CXXCONSTRUCT-EXPR :HAS-ANY-ARGUMENT :EXPR)
    (:CXXCONSTRUCT-EXPR :HAS-ARGUMENT :UNSIGNED-N :EXPR)
    (:CXXCONSTRUCT-EXPR :HAS-DECLARATION :DECL)
    (:CXXCONSTRUCTOR-DECL :FOR-EACH-CONSTRUCTOR-INITIALIZER :CXXCTOR-INITIALIZER)
    (:CXXCONSTRUCTOR-DECL :HAS-ANY-CONSTRUCTOR-INITIALIZER :CXXCTOR-INITIALIZER)
    (:CXXCTOR-INITIALIZER :FOR-FIELD :FIELD-DECL)
    (:CXXCTOR-INITIALIZER :WITH-INITIALIZER :EXPR)
    (:CXXMEMBER-CALL-EXPR :ON :EXPR)
    (:CXXMEMBER-CALL-EXPR :ON-IMPLICIT-OBJECT-ARGUMENT :EXPR)
    (:CXXMEMBER-CALL-EXPR :THIS-POINTER-TYPE :DECL)
    (:CXXMETHOD-DECL :OF-CLASS :CXXRECORD-DECL)
    (:CXXRECORD-DECL :HAS-METHOD :CXXMETHOD-DECL)
    (:CXXRECORD-DECL :IS-DERIVED-FROM :NAMED-DECL)
    (:CXXRECORD-DECL :IS-SAME-OR-DERIVED-FROM :NAMED-DECL)
    (:CALL-EXPR :CALLEE :DECL)
    (:CALL-EXPR :HAS-ANY-ARGUMENT :EXPR)
    (:CALL-EXPR :HAS-ARGUMENT :UNSIGNED-N :EXPR)
    (:CALL-EXPR :HAS-DECLARATION :DECL)
    (:CASE-STMT :HAS-CASE-CONSTANT :EXPR)
    (:CAST-EXPR :HAS-SOURCE-EXPRESSION :EXPR)
    (:CLASS-TEMPLATE-SPECIALIZATION-DECL :HAS-ANY-TEMPLATE-ARGUMENT :TEMPLATE-ARGUMENT)
    (:CLASS-TEMPLATE-SPECIALIZATION-DECL :HAS-TEMPLATE-ARGUMENT :UNSIGNED-N :TEMPLATE-ARGUMENT)
    (:COMPLEX-TYPE-LOC :HAS-ELEMENT-TYPE-LOC :TYPE-LOC)
    (:COMPLEX-TYPE :HAS-ELEMENT-TYPE :TYPE)
    (:COMPOUND-STMT :HAS-ANY-SUBSTATEMENT :STMT)
    (:CONDITIONAL-OPERATOR :HAS-CONDITION :EXPR)
    (:CONDITIONAL-OPERATOR :HAS-FALSE-EXPRESSION :EXPR)
    (:CONDITIONAL-OPERATOR :HAS-TRUE-EXPRESSION :EXPR)
    (:DECL-REF-EXPR :HAS-DECLARATION :DECL)
    (:DECL-REF-EXPR :THROUGH-USING-DECL :USING-SHADOW-DECL)
    (:DECL-REF-EXPR :TO :DECL)
    (:DECL-STMT :CONTAINS-DECLARATION :UNSIGNED-N :DECL)
    (:DECL-STMT :HAS-SINGLE-DECL :DECL)
    (:DECLARATOR-DECL :HAS-TYPE-LOC :TYPE-LOC)
    (:DECL :HAS-DECL-CONTEXT :DECL)
    (:DO-STMT :HAS-BODY :STMT)
    (:DO-STMT :HAS-CONDITION :EXPR)
    (:ELABORATED-TYPE :HAS-QUALIFIER :NESTED-NAME-SPECIFIER)
    (:ELABORATED-TYPE :NAMES-TYPE :QUAL-TYPE)
    (:ENUM-TYPE :HAS-DECLARATION :DECL)
    (:EXPLICIT-CAST-EXPR :HAS-DESTINATION-TYPE :QUAL-TYPE)
    (:EXPR :HAS-TYPE :DECL)
    (:EXPR :HAS-TYPE :QUAL-TYPE)
    (:EXPR :IGNORING-IMP-CASTS :EXPR)
    (:EXPR :IGNORING-PAREN-CASTS :EXPR)
    (:EXPR :IGNORING-PAREN-IMP-CASTS :EXPR)
    (:FOR-STMT :HAS-BODY :STMT)
    (:FOR-STMT :HAS-CONDITION :EXPR)
    (:FOR-STMT :HAS-INCREMENT :STMT)
    (:FOR-STMT :HAS-LOOP-INIT :STMT)
    (:FUNCTION-DECL :HAS-ANY-PARAMETER :PARM-VAR-DECL)
    (:FUNCTION-DECL :HAS-PARAMETER :UNSIGNED-N :PARM-VAR-DECL)
    (:FUNCTION-DECL :RETURNS :QUAL-TYPE)
    (:IF-STMT :HAS-CONDITION :EXPR)
    (:IF-STMT :HAS-CONDITION-VARIABLE-STATEMENT :DECL-STMT)
    (:IMPLICIT-CAST-EXPR :HAS-IMPLICIT-DESTINATION-TYPE :QUAL-TYPE)
    (:INJECTED-CLASS-NAME-TYPE :HAS-DECLARATION :DECL)
    (:LABEL-STMT :HAS-DECLARATION :DECL)
    (:MEMBER-EXPR :HAS-DECLARATION :DECL)
    (:MEMBER-EXPR :HAS-OBJECT-EXPRESSION :EXPR)
    (:MEMBER-EXPR :MEMBER :VALUE-DECL)
    (:MEMBER-POINTER-TYPE-LOC :POINTEE-LOC :TYPE-LOC)
    (:MEMBER-POINTER-TYPE :POINTEE :TYPE)
    (:NESTED-NAME-SPECIFIER-LOC :HAS-PREFIX :NESTED-NAME-SPECIFIER-LOC)
    (:NESTED-NAME-SPECIFIER-LOC :LOC :NESTED-NAME-SPECIFIER)
    (:NESTED-NAME-SPECIFIER-LOC :SPECIFIES-TYPE-LOC :TYPE-LOC)
    (:NESTED-NAME-SPECIFIER :HAS-PREFIX :NESTED-NAME-SPECIFIER)
    (:NESTED-NAME-SPECIFIER :SPECIFIES-NAMESPACE :NAMESPACE-DECL)
    (:NESTED-NAME-SPECIFIER :SPECIFIES-TYPE :QUAL-TYPE)
    (:PAREN-TYPE :INNER-TYPE :TYPE)
    (:POINTER-TYPE-LOC :POINTEE-LOC :TYPE-LOC)
    (:POINTER-TYPE :POINTEE :TYPE)
    (:QUAL-TYPE :HAS-CANONICAL-TYPE :QUAL-TYPE)
    (:QUAL-TYPE :HAS-DECLARATION :DECL)
    (:QUAL-TYPE :POINTS-TO :DECL)
    (:QUAL-TYPE :REFERENCES :DECL)
    (:RECORD-TYPE :HAS-DECLARATION :DECL)
    (:REFERENCE-TYPE-LOC :POINTEE-LOC :TYPE-LOC)
    (:REFERENCE-TYPE :POINTEE :TYPE)
    (:STMT :ALIGN-OF-EXPR :UNARY-EXPR-OR-TYPE-TRAIT-EXPR)
    (:STMT :SIZE-OF-EXPR :UNARY-EXPR-OR-TYPE-TRAIT-EXPR)
    (:SWITCH-STMT :FOR-EACH-SWITCH-CASE :SWITCH-CASE)
    (:TAG-TYPE :HAS-DECLARATION :DECL)
    (:TEMPLATE-ARGUMENT :REFERS-TO-DECLARATION :DECL)
    (:TEMPLATE-ARGUMENT :REFERS-TO-TYPE :QUAL-TYPE)
    (:TEMPLATE-SPECIALIZATION-TYPE :HAS-DECLARATION :DECL)
    (:TEMPLATE-TYPE-PARM-TYPE :HAS-DECLARATION :DECL)
    (:* :FIND-ALL :*)
    (:TYPE-LOC :LOC :QUAL-TYPE)
    (:TYPEDEF-TYPE :HAS-DECLARATION :DECL)
    (:UNARY-EXPR-OR-TYPE-TRAIT-EXPR :HAS-ARGUMENT-OF-TYPE :QUAL-TYPE)
    (:UNARY-OPERATOR :HAS-UNARY-OPERAND :EXPR)
    (:UNRESOLVED-USING-TYPE :HAS-DECLARATION :DECL)
    (:USING-DECL :HAS-ANY-USING-SHADOW-DECL :USING-SHADOW-DECL)
    (:USING-SHADOW-DECL :HAS-TARGET-DECL :NAMED-DECL)
    (:VALUE-DECL :HAS-TYPE :DECL)
    (:VALUE-DECL :HAS-TYPE :QUAL-TYPE)
    (:VAR-DECL :HAS-INITIALIZER :EXPR)
    (:VARIABLE-ARRAY-TYPE :HAS-SIZE-EXPR :EXPR)
    (:WHILE-STMT :HAS-BODY :STMT)
    (:WHILE-STMT :HAS-CONDITION :EXPR))) 

(defconstant +all-matchers+ (append +node-matchers+ +narrowing-matchers+ +traversal-matchers+))

(defparameter +node-matcher-hints+ (make-hash-table :test #'eq))
(dolist (i +node-matchers+)
  (setf (gethash (car i) +node-matcher-hints+) (cdr i)))

(defparameter +narrowing-matcher-hints+ (make-hash-table :test #'eq))
(dolist (i +narrowing-matchers+)
  (setf (gethash (car i) +narrowing-matcher-hints+) (cdr i)))

(define-condition wrong-matcher (condition)
  ((node-type :initarg :node-type :accessor wrong-matcher-node-type )))

(defun identify-node-type (node)
  (or (find-if #'(lambda (x) (eq node (second x))) +narrowing-matchers+)
      (find-if #'(lambda (x) (eq node (second x))) +traversal-matchers+)))

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
                        +node-matchers+)))
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
                     +traversal-matchers+)))
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
                                   +narrowing-matchers+)))
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
    (when (position-if (lambda (x) (eq p (cadr x))) +node-matchers+)
      (return-from good t))
    (when (position-if (lambda (x) (eq p (cadr x))) +traversal-matchers+)
      (return-from good t))
    (when (position-if (lambda (x) (eq p (cadr x))) +narrowing-matchers+)
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
    (node-matcher-ambiguous-error (err)
      (error "Hint node environment is ambiguous prev-environment is ~a; node is ~a; sexp is ~a~%"
             prev-environment (car sexp)  sexp))))

(defun provide-hint (environment)
  (format t "Environment: ~a~%" environment)
  (check-type environment list)
  (let ((*print-circle* nil))
    (let* ((super-classes (super-class-matchers environment))
           (node-matcher-records (remove-if-not (lambda (x) (member (car x) super-classes)) +node-matchers+))
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
(dolist (m +traversal-matchers+)
  (let ((some (remove-if-not (lambda (x) (eq (cadr m) (cadr x))) +all-matchers+)))
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
                       ((eq node :method-decl) :cxxmethod-decl)
                       ((eq node :record-decl) :cxxrecord-decl)
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
                  (dolist (i +narrowing-matchers+)
                    (when (member (car i) supers)
                      (format t "   ~a~%" (cdr i)))))
    (wrong-matcher (exception)
      (format t "Narrowing matchers aren't appropriate here - ~a~%" (wrong-matcher-node-type exception)))))

(defun traversal-hint (environment)
  "For a list of valid environments, give a hint for traversal matchers that will move us from there"
  (check-type environment list)
  (handler-case (let ((supers (append (super-class-matchers environment) '(:*))))
                  (dolist (i +traversal-matchers+)
                    (when (member (car i) supers)
                      (format t "   ~a~%" (cdr i)))))
    (wrong-matcher (exception)
      (format t "Traversal matchers aren't appropriate here - ~a~%" (wrong-matcher-node-type exception)))))

(defun arg-for-cmd (node)
  (dolist (i (append +node-matchers+ +narrowing-matchers+ +traversal-matchers+))
    (when (eq (cadr i) node)
      (format t "~a~%" i))))






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


(defun compile-matcher (sexp)
  "* Arguments
- sexp :: The S-expression that describes an ASTMatcher.
* Description
Compile an ASTMatcher from an S-expression.
 eg: (compile-matcher '(:bind :field (:field-decl (:has-type (:record-decl (:has-name \"TinyStruct\"))))))).
Return nil if no matcher could be compiled."
  (if (contains-hint-request sexp)
      nil
      (let* ((diag (ast-tooling:new-diagnostics))
             (dyn-matcher (compile-matcher* sexp diag))
             (single-matcher (ast-tooling:get-single-matcher dyn-matcher)))
        (if (null single-matcher)
            (error "Error while constructing matcher - diagnostics:~% ~a" (to-string-full diag))
            single-matcher))))

;;;
;;;  Convert my matchers back into C-style matchers
;;;

(defparameter +matcher-names+
  '((:ACCESS-SPEC-DECL "accessSpecDecl")
    (:ALIGN-OF-EXPR "alignOfExpr")
    (:ALL-OF "allOf")
    (:ANY-OF "anyOf")
    (:ANYTHING "anything")
    (:ARGUMENT-COUNT-IS "argumentCountIs")
    (:ARRAY-SUBSCRIPT-EXPR "arraySubscriptExpr")
    (:ARRAY-TYPE "arrayType")
    (:AS-STRING "asString")
    (:ASM-STMT "asmStmt")
    (:ATOMIC-TYPE "atomicType")
    (:AUTO-TYPE "autoType")
    (:BINARY-OPERATOR "binaryOperator")
    (:BIND-TEMPORARY-EXPR "bindTemporaryExpr")
    (:BLOCK-POINTER-TYPE "blockPointerType")
    (:BOOL-LITERAL "boolLiteral")
    (:BREAK-STMT "breakStmt")
    (:BUILTIN-TYPE "builtinType")
    (:C-STYLE-CAST-EXPR "cStyleCastExpr")
    (:CALL-EXPR "callExpr")
    (:CALLEE "callee")
    (:CASE-STMT "caseStmt")
    (:CAST-EXPR "castExpr")
    (:CATCH-STMT "catchStmt")
    (:CHARACTER-LITERAL "characterLiteral")
    (:CLASS-TEMPLATE-DECL "classTemplateDecl")
    (:CLASS-TEMPLATE-SPECIALIZATION-DECL "classTemplateSpecializationDecl")
    (:COMPLEX-TYPE "complexType")
    (:COMPOUND-LITERAL-EXPR "compoundLiteralExpr")
    (:COMPOUND-STMT "compoundStmt")
    (:CONDITIONAL-OPERATOR "conditionalOperator")
    (:CONST-CAST-EXPR "constCastExpr")
    (:CONSTANT-ARRAY-TYPE "constantArrayType")
    (:CONSTRUCT-EXPR "constructExpr")
    (:CONSTRUCTOR-DECL "constructorDecl")
    (:CONTAINS-DECLARATION "containsDeclaration")
    (:CONTINUE-STMT "continueStmt")
    (:CTOR-INITIALIZER "ctorInitializer")
    (:DECL "decl")
    (:DECL-COUNT-IS "declCountIs")
    (:DECL-REF-EXPR "declRefExpr")
    (:DECL-STMT "declStmt")
    (:DECLARATOR-DECL "declaratorDecl")
    (:DEFAULT-ARG-EXPR "defaultArgExpr")
    (:DEFAULT-STMT "defaultStmt")
    (:DELETE-EXPR "deleteExpr")
    (:DEPENDENT-SIZED-ARRAY-TYPE "dependentSizedArrayType")
    (:DESTRUCTOR-DECL "destructorDecl")
    (:DO-STMT "doStmt")
    (:DYNAMIC-CAST-EXPR "dynamicCastExpr")
    (:EACH-OF "eachOf")
    (:ELABORATED-TYPE "elaboratedType")
    (:ENUM-CONSTANT-DECL "enumConstantDecl")
    (:ENUM-DECL "enumDecl")
    (:EQUALS "equals")
    (:EQUALS-BOUND-NODE "equalsBoundNode")
    (:EQUALS-NODE "equalsNode")
    (:EXPLICIT-CAST-EXPR "explicitCastExpr")
    (:EXPR "expr")
    (:FIELD-DECL "fieldDecl")
    (:FIND-ALL "findAll")
    (:FLOAT-LITERAL "floatLiteral")
    (:FOR-EACH "forEach")
    (:FOR-EACH-CONSTRUCTOR-INITIALIZER "forEachConstructorInitializer")
    (:FOR-EACH-DESCENDANT "forEachDescendant")
    (:FOR-EACH-SWITCH-CASE "forEachSwitchCase")
    (:FOR-FIELD "forField")
    (:FOR-RANGE-STMT "forRangeStmt")
    (:FOR-STMT "forStmt")
    (:FRIEND-DECL "friendDecl")
    (:FUNCTION-DECL "functionDecl")
    (:FUNCTION-TEMPLATE-DECL "functionTemplateDecl")
    (:FUNCTION-TYPE "functionType")
    (:FUNCTIONAL-CAST-EXPR "functionalCastExpr")
    (:GOTO-STMT "gotoStmt")
    (:HAS "has")
    (:HAS-ANCESTOR "hasAncestor")
    (:HAS-ANY-ARGUMENT "hasAnyArgument")
    (:HAS-ANY-CONSTRUCTOR-INITIALIZER "hasAnyConstructorInitializer")
    (:HAS-ANY-PARAMETER "hasAnyParameter")
    (:HAS-ANY-SUBSTATEMENT "hasAnySubstatement")
    (:HAS-ANY-TEMPLATE-ARGUMENT "hasAnyTemplateArgument")
    (:HAS-ANY-USING-SHADOW-DECL "hasAnyUsingShadowDecl")
    (:HAS-ARGUMENT "hasArgument")
    (:HAS-ARGUMENT-OF-TYPE "hasArgumentOfType")
    (:HAS-BASE "hasBase")
    (:HAS-BODY "hasBody")
    (:HAS-CANONICAL-TYPE "hasCanonicalType")
    (:HAS-CASE-CONSTANT "hasCaseConstant")
    (:HAS-CONDITION "hasCondition")
    (:HAS-CONDITION-VARIABLE-STATEMENT "hasConditionVariableStatement")
    (:HAS-DECL-CONTEXT "hasDeclContext")
    (:HAS-DECLARATION "hasDeclaration")
    (:HAS-DEDUCED-TYPE "hasDeducedType")
    (:HAS-DESCENDANT "hasDescendant")
    (:HAS-DESTINATION-TYPE "hasDestinationType")
    (:HAS-EITHER-OPERAND "hasEitherOperand")
    (:HAS-ELEMENT-TYPE "hasElementType")
    (:HAS-ELEMENT-TYPE-LOC "hasElementTypeLoc")
    (:HAS-FALSE-EXPRESSION "hasFalseExpression")
    (:HAS-IMPLICIT-DESTINATION-TYPE "hasImplicitDestinationType")
    (:HAS-INCREMENT "hasIncrement")
    (:HAS-INDEX "hasIndex")
    (:HAS-INITIALIZER "hasInitializer")
    (:HAS-LHS "hasLHS")
    (:HAS-LOCAL-QUALIFIERS "hasLocalQualifiers")
    (:HAS-LOOP-INIT "hasLoopInit")
    (:HAS-METHOD "hasMethod")
    (:HAS-NAME "hasName")
    (:HAS-OBJECT-EXPRESSION "hasObjectExpression")
    (:HAS-OPERATOR-NAME "hasOperatorName")
    (:HAS-OVERLOADED-OPERATOR-NAME "hasOverloadedOperatorName")
    (:HAS-PARAMETER "hasParameter")
    (:HAS-PARENT "hasParent")
    (:HAS-PREFIX "hasPrefix")
    (:HAS-QUALIFIER "hasQualifier")
    (:HAS-RHS "hasRHS")
    (:HAS-SINGLE-DECL "hasSingleDecl")
    (:HAS-SIZE "hasSize")
    (:HAS-SIZE-EXPR "hasSizeExpr")
    (:HAS-SOURCE-EXPRESSION "hasSourceExpression")
    (:HAS-TARGET-DECL "hasTargetDecl")
    (:HAS-TEMPLATE-ARGUMENT "hasTemplateArgument")
    (:HAS-TRUE-EXPRESSION "hasTrueExpression")
    (:HAS-TYPE "hasType")
    (:HAS-TYPE-LOC "hasTypeLoc")
    (:HAS-UNARY-OPERAND "hasUnaryOperand")
    (:HAS-VALUE-TYPE "hasValueType")
    (:HAS-VALUE-TYPE-LOC "hasValueTypeLoc")
    (:IF-STMT "ifStmt")
    (:IGNORING-IMP-CASTS "ignoringImpCasts")
    (:IGNORING-PAREN-CASTS "ignoringParenCasts")
    (:IGNORING-PAREN-IMP-CASTS "ignoringParenImpCasts")
    (:IMPLICIT-CAST-EXPR "implicitCastExpr")
    (:INCOMPLETE-ARRAY-TYPE "incompleteArrayType")
    (:INIT-LIST-EXPR "initListExpr")
    (:INNER-TYPE "innerType")
    (:INTEGER-LITERAL "integerLiteral")
    (:IS-ARROW "isArrow")
    (:IS-CONST "isConst")
    (:IS-CONST-QUALIFIED "isConstQualified")
    (:IS-DEFINITION "isDefinition")
    (:IS-DERIVED-FROM "isDerivedFrom")
    (:IS-EXPLICIT-TEMPLATE-SPECIALIZATION "isExplicitTemplateSpecialization")
    (:IS-EXTERN-C "isExternC")
    (:IS-IMPLICIT "isImplicit")
    (:IS-INTEGER "isInteger")
    (:IS-OVERRIDE "isOverride")
    (:IS-PRIVATE "isPrivate")
    (:IS-PROTECTED "isProtected")
    (:IS-PUBLIC "isPublic")
    (:IS-SAME-OR-DERIVED-FROM "isSameOrDerivedFrom")
    (:IS-TEMPLATE-INSTANTIATION "isTemplateInstantiation")
    (:IS-VIRTUAL "isVirtual")
    (:IS-WRITTEN "isWritten")
    (:L-VALUE-REFERENCE-TYPE "lValueReferenceType")
    (:LABEL-STMT "labelStmt")
    (:LAMBDA-EXPR "lambdaExpr")
    (:LOC "loc")
    (:MATCHES-NAME "matchesName")
    (:MATERIALIZE-TEMPORARY-EXPR "materializeTemporaryExpr")
    (:MEMBER "member")
    (:MEMBER-CALL-EXPR "memberCallExpr")
    (:MEMBER-EXPR "memberExpr")
    (:MEMBER-POINTER-TYPE "memberPointerType")
    (:METHOD-DECL "methodDecl")
    (:NAMED-DECL "namedDecl")
    (:NAMES-TYPE "namesType")
    (:NAMESPACE-DECL "namespaceDecl")
    (:NESTED-NAME-SPECIFIER "nestedNameSpecifier")
    (:NESTED-NAME-SPECIFIER-LOC "nestedNameSpecifierLoc")
    (:NEW-EXPR "newExpr")
    (:NULL-PTR-LITERAL-EXPR "nullPtrLiteralExpr")
    (:NULL-STMT "nullStmt")
    (:OF-CLASS "ofClass")
    (:OF-KIND "ofKind")
    (:ON "on")
    (:ON-IMPLICIT-OBJECT-ARGUMENT "onImplicitObjectArgument")
    (:OPERATOR-CALL-EXPR "operatorCallExpr")
    (:PARAMETER-COUNT-IS "parameterCountIs")
    (:PAREN-TYPE "parenType")
    (:PARM-VAR-DECL "parmVarDecl")
    (:POINTEE "pointee")
    (:POINTEE-LOC "pointeeLoc")
    (:POINTER-TYPE "pointerType")
    (:POINTS-TO "pointsTo")
    (:QUAL-TYPE "qualType")
    (:R-VALUE-REFERENCE-TYPE "rValueReferenceType")
    (:RECORD-DECL "recordDecl")
    (:RECORD-TYPE "recordType")
    (:REFERENCE-TYPE "referenceType")
    (:REFERENCES "references")
    (:REFERS-TO-DECLARATION "refersToDeclaration")
    (:REFERS-TO-TYPE "refersToType")
    (:REINTERPRET-CAST-EXPR "reinterpretCastExpr")
    (:RETURN-STMT "returnStmt")
    (:RETURNS "returns")
    (:SIZE-OF-EXPR "sizeOfExpr")
    (:SPECIFIES-NAMESPACE "specifiesNamespace")
    (:SPECIFIES-TYPE "specifiesType")
    (:SPECIFIES-TYPE-LOC "specifiesTypeLoc")
    (:STATEMENT-COUNT-IS "statementCountIs")
    (:STATIC-CAST-EXPR "staticCastExpr")
    (:STMT "stmt")
    (:STRING-LITERAL "stringLiteral")
    (:SWITCH-CASE "switchCase")
    (:SWITCH-STMT "switchStmt")
    (:TEMPLATE-SPECIALIZATION-TYPE "templateSpecializationType")
    (:TEMPORARY-OBJECT-EXPR "temporaryObjectExpr")
    (:THIS-EXPR "thisExpr")
    (:THIS-POINTER-TYPE "thisPointerType")
    (:THROUGH-USING-DECL "throughUsingDecl")
    (:THROW-EXPR "throwExpr")
    (:TO "to")
    (:TRY-STMT "tryStmt")
    (:TYPE "type")
    (:TYPE-LOC "typeLoc")
    (:TYPEDEF-TYPE "typedefType")
    (:UNARY-EXPR-OR-TYPE-TRAIT-EXPR "unaryExprOrTypeTraitExpr")
    (:UNARY-OPERATOR "unaryOperator")
    (:UNARY-TRANSFORM-TYPE "unaryTransformType")
    (:UNLESS "unless")
    (:UNRESOLVED-CONSTRUCT-EXPR "unresolvedConstructExpr")
    (:UNRESOLVED-USING-VALUE-DECL "unresolvedUsingValueDecl")
    (:USER-DEFINED-LITERAL "userDefinedLiteral")
    (:USING-DECL "usingDecl")
    (:VAR-DECL "varDecl")
    (:VARIABLE-ARRAY-TYPE "variableArrayType")
    (:WHILE-STMT "whileStmt")
    (:WITH-INITIALIZER "withInitializer")
    ))

(defparameter +matcher-hash-table+
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (e +matcher-names+)
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

