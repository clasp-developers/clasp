(in-package #:koga)

(defparameter +llvm-major-version+
  13
  "The required LLVM version.")

(defparameter +llvm-config-candidates+
  '("llvm-config-~a"
    "llvm-config-~a-64"
    "llvm-config~a"
    "llvm-config~a-64"
    "llvm-config"
    "llvm-config-64"
    #+darwin "/usr/local/opt/llvm/bin/llvm-config")
  "Candidate names for the llvm-config binary.")

(defparameter +scraper-headers+
  '(:init_functions_inc_h :init_classes_inc_h :source_info_inc_h
    :symbols_scraped_inc_h :enum_inc_h :initializers_inc_h :expose_inc_h
    :terminators_inc_h :pregcstartup_inc_h :c_wrappers)
  "The C macro names of the scraper headers used during a non-precise build.")

(defparameter +scraper-precise-headers+
  '(:init_functions_inc_h :init_classes_inc_h :source_info_inc_h
    :symbols_scraped_inc_h :enum_inc_h :initializers_inc_h :expose_inc_h
    :terminators_inc_h :pregcstartup_inc_h :c_wrappers :clasp_gc_filename)
  "The C macro names of the scraper headers used during a precise build.")

(defparameter +scraper-lisp-sources+
  '(:lisp_wrappers)
  "The C macro names of the scraper lisp sources.")

(defmethod configure-unit (configuration (unit (eql :llvm)))
  "Find the llvm-config binary and use it to configure LLVM."
  (with-accessors ((llvm-config llvm-config)
                   (llvm-version llvm-version)
                   (llvm-bindir llvm-bindir)
                   (llvm-includedir llvm-includedir)
                   (ar ar)
                   (cc cc)
                   (cxx cxx)
                   (nm nm))
      configuration
    (message :emph "Configuring LLVM")
    (if llvm-config
        (setf llvm-version (run-program-capture (list llvm-config "--version")))
        (setf llvm-config
              (loop for candidate in +llvm-config-candidates+
                    for name = (format nil candidate +llvm-major-version+)
                    for version = (run-program-capture (list name "--version"))
                    when (and version
                              (= +llvm-major-version+
                                 (first (uiop:parse-version version))))
                      do (setf llvm-version version)
                         (return name))))
    (unless llvm-config
      (message :err "Unable to find llvm-config compatible with major version ~a." +llvm-major-version+))
    (message :info "Using llvm-config binary: ~a" llvm-config)
    (unless llvm-bindir
      (setf llvm-bindir
            (uiop:ensure-directory-pathname (run-program-capture (list llvm-config "--bindir")))))
    (unless llvm-includedir
      (setf llvm-includedir
            (uiop:ensure-directory-pathname (run-program-capture (list llvm-config "--includedir")))))
    (unless ar
      (setf ar (merge-pathnames #P"llvm-ar" llvm-bindir)))
    (unless cc
      (setf cc (merge-pathnames #P"clang" llvm-bindir)))
    (unless cxx
      (setf cxx (merge-pathnames #P"clang++" llvm-bindir)))
    (unless nm
      (setf nm (merge-pathnames #P"llvm-nm" llvm-bindir)))
    (append-cflags configuration (format nil "-I~a" llvm-includedir))
    (append-ldflags configuration (run-program-capture (list llvm-config "--ldflags")))
    (append-ldlibs configuration (run-program-capture (list llvm-config "--system-libs")))
    (append-ldlibs configuration (run-program-capture (list llvm-config "--libs")))
    (when (ld configuration)
      (append-ldflags configuration (format nil "-fuse-ld=~(~a~)" (ld configuration))))
    (append-ldflags configuration "-pthread -lstdc++ -fvisibility=default -rdynamic")))

;; TODO This needs to be improved and made more automatic.
(defmethod configure-unit (configuration (unit (eql :clang)))
  "Find the clang libraries."
  (append-ldlibs configuration
                 (if (clang-cpp configuration)
                     "-lclang-cpp"
                     (format nil "-lclangASTMatchers -lclangDynamicASTMatchers -lclangIndex ~
-lclangTooling -lclangFormat -lclangToolingInclusions -lclangToolingCore -lclangBasic ~
-lclangCodeGen -lclangDriver -lclangFrontend -lclangFrontendTool -lclangCodeGen ~
-lclangRewriteFrontend -lclangARCMigrate -lclangStaticAnalyzerFrontend -lclangFrontend ~
-lclangDriver -lclangParse -lclangSerialization -lclangSema -lclangEdit ~
-lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangAnalysis -lclangAST -lclangRewrite ~
-lclangLex -lclangBasic"))))

(defparameter +pkg-config-candidates+
  '("pkg-config"))

(defmethod configure-unit (configuration (unit (eql :pkg-config)))
  "Find the pkg-config binary."
  (with-accessors ((pkg-config pkg-config))
      configuration
    (message :emph "Configuring pkg-config")
    (unless pkg-config
      (setf pkg-config
            (loop for candidate in +pkg-config-candidates+
                  for version = (run-program-capture (list candidate "--version"))
                  when version
                    do (return candidate))))
    (unless pkg-config
      (message :err "Unable to find pkg-config."))
    (message :info "Using pkg-config binary: ~a" pkg-config)))

(defmethod configure-unit (configuration (unit (eql :base))
                           &aux (app-config (cscrape:read-application-config #P"include/clasp/main/application.config")))
  "Add base cflags, ldflags and parse application.config to determine the scraper outputs."
  (message :emph "Configuring base")
  (append-cflags configuration
                 (format nil "-Wno-macro-redefined -Wno-deprecated-declarations ~
-Wno-deprecated-register -Wno-expansion-to-defined -Wno-return-type-c-linkage ~
-Wno-invalid-offsetof -Wno-#pragma-messages -Wno-inconsistent-missing-override ~
-Wno-error=c++11-narrowing -Wno-c++11-narrowing -Wno-deprecated-enum-enum-conversion ~
-Wno-deprecated-anon-enum-enum-conversion"))
  (loop for variant in (variants configuration)
        do (append-cflags variant (format nil "-I~a" (variant-bitcode-name variant))))
  (append-cflags configuration "-O3 -g -fPIC" :type :cxxflags :debug nil)
  (append-cflags configuration "-O3 -g -fPIC" :type :cflags :debug nil)
  (append-cflags configuration "-O0 -g" :type :cxxflags :debug t)
  (append-cflags configuration "-O0 -g" :type :cflags :debug t)
  (append-cflags configuration "-std=c++20" :type :cxxflags)
  #+darwin (append-cflags configuration "-stdlib=libc++" :type :cxxflags)
  #+darwin (append-cflags configuration "-I/usr/local/include/")
  #+linux (append-cflags configuration "-stdlib=libstdc++" :type :cxxflags)
  (when (address-sanitizer configuration)
    (append-cflags configuration "-fsanitize=address" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=address"))
  (when (memory-sanitizer configuration)
    (append-cflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1" :type :cflags)
    (append-cflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1"))
  (when (thread-sanitizer configuration)
    (append-cflags configuration "-fsanitize=thread" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=thread"))
  (setf (scraper-headers configuration)
        (mapcar (lambda (key)
                  (make-source (gethash key app-config) :variant))
                +scraper-headers+)
        (scraper-precise-headers configuration)
        (mapcar (lambda (key)
                  (make-source (gethash key app-config) :variant))
                +scraper-precise-headers+)
        (scraper-lisp-sources configuration)
        (mapcar (lambda (key)
                  (make-source (gethash key app-config) :variant))
                +scraper-lisp-sources+)))

(defmethod configure-unit (configuration (unit (eql :cpu-count)))
  "Use sysctl or nproc to determine the number of CPU cores and set the job count if it
has not been set."
  (unless (jobs configuration)
    (message :emph "Inspecting system to determine the number of cpu cores")
    (loop for command in #+bsd '("sysctl -n hw.physicalcpu" "sysctl -n hw.ncpu" "sysctl -n hw.ncpufound")
                         #-bsd '("nproc --all")
          for output = (run-program-capture command)
          when output
            do (message :info "Found ~a cpu cores. Setting the number of jobs to this value."
                        (setf (jobs configuration) (parse-integer output :junk-allowed t)))
               (return)
          finally (message :warn "Unknown number of cpu cores. Setting the number of jobs to ~a."
                           (setf (jobs configuration) 4))))) 
