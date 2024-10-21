(in-package #:koga)

(defparameter +llvm-major-version+
  '(15 . 19)
  "The required LLVM version.")

(defparameter +llvm-config-candidates+
  '("llvm-config-~a"
    "llvm-config-~a-64"
    "llvm-config~a"
    "llvm-config~a-64"
    "llvm-config"
    "llvm-config-64"
    #+darwin "/usr/local/opt/llvm/bin/llvm-config"
    #+darwin "/usr/local/opt/llvm@~a/bin/llvm-config"
    #+darwin "/opt/homebrew/opt/llvm@~a/bin/llvm-config")
  "Candidate names for the llvm-config binary.")

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
    (multiple-value-setq (llvm-config llvm-version)
                         (configure-program "llvm-config"
                                            (or llvm-config
                                                +llvm-config-candidates+)
                                            :major-version +llvm-major-version+
                                            :required t))
    (unless llvm-bindir
      (setf llvm-bindir
            (uiop:ensure-directory-pathname (run-program-capture (list llvm-config "--bindir")))))
    (unless llvm-includedir
      (setf llvm-includedir
            (uiop:ensure-directory-pathname (run-program-capture (list llvm-config "--includedir")))))
    (append-cflags configuration (format nil "-I~a" (normalize-directory llvm-includedir)))
    (append-cflags configuration "-Wno-vla-extension")
    (append-ldflags configuration (run-program-capture (list llvm-config "--ldflags")))
    (append-ldlibs configuration (run-program-capture (list llvm-config "--system-libs")))
    (append-ldlibs configuration (run-program-capture (list llvm-config "--libs")))
    #+freebsd (append-ldlibs configuration "-lexecinfo")
    (when (ld configuration)
      (append-ldflags configuration (format nil "-fuse-ld=~(~a~)" (ld configuration))))
    (append-ldflags configuration "-pthread -fvisibility=default -rdynamic")))

(defmethod configure-unit (configuration (unit (eql :ar)))
  "Find the ar binary."
  (with-accessors ((ar ar)
                   (llvm-bindir llvm-bindir))
      configuration
    (message :emph "Configuring ar")
    (setf ar (configure-program "ar"
                                (or ar (merge-pathnames #P"llvm-ar" llvm-bindir))
                                :required t))))

(defmethod configure-unit (configuration (unit (eql :cc)))
  "Find the cc binary."
  (with-accessors ((cc cc)
                   (llvm-bindir llvm-bindir))
      configuration
    (message :emph "Configuring cc")
    (setf cc (configure-program "cc"
                                (or cc (merge-pathnames #P"clang" llvm-bindir))
                                :required t))))

(defmethod configure-unit (configuration (unit (eql :cxx)))
  "Find the cxx binary."
  (with-accessors ((cxx cxx)
                   (llvm-bindir llvm-bindir))
      configuration
    (message :emph "Configuring cxx")
    (setf cxx (configure-program "cxx"
                                 (or cxx (merge-pathnames #P"clang++" llvm-bindir))
                                 :required t))))

(defmethod configure-unit (configuration (unit (eql :dis)))
  "Find the llvm-dis binary."
  (with-accessors ((dis dis)
                   (llvm-bindir llvm-bindir))
      configuration
    (message :emph "Configuring llvm-dis")
    (setf dis (configure-program "dis"
                                 (or dis (merge-pathnames #P"llvm-dis" llvm-bindir))
                                 :required t))))

(defmethod configure-unit (configuration (unit (eql :nm)))
  "Find the nm binary."
  (with-accessors ((nm nm)
                   (llvm-bindir llvm-bindir))
      configuration
    (message :emph "Configuring nm")
    (setf nm (configure-program "nm"
                                (or nm (merge-pathnames #P"llvm-nm" llvm-bindir))
                                :required t))))

(defmethod configure-unit (configuration (unit (eql :mpi)))
  (with-accessors ((cxx cxx)
                   (mpi mpi)
                   (mpicxx mpicxx))
      configuration
    (when mpi
      (message :emph "Configuring OpenMPI")
      (let ((mpicxx (configure-program "mpic++" (or mpicxx #P"mpic++") :required t)))
        (append-cflags configuration
                       (run-program-capture (format nil "OMPI_CXX=~a ~a --showme:compile" cxx mpicxx)))
        (append-ldlibs configuration
                       (run-program-capture (format nil "OMPI_CXX=~a ~a --showme:link" cxx mpicxx)))
        (append-ldlibs configuration "-lboost_mpi -lboost_serialization")))))

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

(defmethod configure-unit (configuration (unit (eql :pkg-config)))
  "Find the pkg-config binary."
  (with-accessors ((pkg-config pkg-config))
      configuration
    (message :emph "Configuring pkg-config")
    (setf pkg-config (configure-program "pkg-config"
                                        (or pkg-config #P"pkg-config")
                                        :required t))))

(defmethod configure-unit (configuration (unit (eql :git)))
  "Find the git binary."
  (with-accessors ((git git))
      configuration
    (message :emph "Configuring git")
    (setf git (configure-program "git"
                                 (or git #P"git")
                                 :required t))))

(defmethod configure-unit (configuration (unit (eql :objcopy)))
  "Find the objcopy binary."
  (with-accessors ((objcopy objcopy))
      configuration
    (message :emph "Configuring objcopy")
    (setf objcopy (configure-program "objcopy"
                                     (or objcopy #P"objcopy")
                                     :required #-darwin t #+darwin nil))))

(defmethod configure-unit (configuration (unit (eql :etags)))
  "Find the etags binary."
  (unless (reproducible-build configuration)
    (with-accessors ((etags etags))
        configuration
      (message :emph "Configuring etags")
      (setf etags (configure-program "etags"
                                     (or etags (list #P"etags")))))))

(defmethod configure-unit (configuration (unit (eql :ctags)))
  "Find the ctags binary."
  (unless (reproducible-build configuration)
    (with-accessors ((ctags ctags))
        configuration
      (message :emph "Configuring ctags")
      (setf ctags (configure-program "ctags" ctags :match "Ctags")))))

(defmethod configure-unit (configuration (unit (eql :xcode))
                           &aux (env (uiop:getenv-absolute-directory "XCODE_SDK"))
                                (def #P"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/"))
  (message :emph "Configuring XCode SDK")
  (with-accessors ((xcode-sdk xcode-sdk))
      configuration
    (when (and (not xcode-sdk)
               env)
      (message :info "Found XCODE_SDK environment variable, using that value.")
      (setf xcode-sdk env))
    (when (and (not xcode-sdk)
               (probe-file def))
      (message :info "Found XCode at the default path, using that value.")
      (setf xcode-sdk def))
    (when xcode-sdk
      (append-cflags configuration (format nil "-isysroot ~a" xcode-sdk)))))

(defmethod configure-unit (configuration (unit (eql :base)))
  "Add base cflags and ldflags."
  (message :emph "Configuring base")
  (append-cflags configuration
                 (format nil "-Wno-macro-redefined -Wno-deprecated-declarations ~
-Wno-deprecated-register -Wno-expansion-to-defined -Wno-return-type-c-linkage ~
-Wno-invalid-offsetof -Wno-#pragma-messages -Wno-inconsistent-missing-override ~
-Wno-error=c++11-narrowing -Wno-c++11-narrowing -Wno-deprecated-enum-enum-conversion ~
-Wno-deprecated-anon-enum-enum-conversion"))
  (loop for variant in (variants configuration)
        do (append-cflags variant (format nil "-I~a" (variant-bitcode-name variant)))
           (append-cflags variant (format nil "-I~a/generated" (variant-bitcode-name variant)))
           (append-ldflags variant (format nil "-L~a/lib" (variant-bitcode-name variant))))
  (append-cflags configuration "-O3 -g -fPIC" :type :cxxflags :debug nil)
  (append-cflags configuration "-O3 -g -fPIC" :type :cflags :debug nil)
  (append-cflags configuration "-O0 -g" :type :cxxflags :debug t)
  (append-cflags configuration "-O0 -g" :type :cflags :debug t)
  (append-cflags configuration "-fconstexpr-steps=10000000" :type :cxxflags)
  (append-cflags configuration "-std=gnu++20" :type :cxxflags)
  #+darwin (append-cflags configuration "-stdlib=libc++" :type :cxxflags)
  #+darwin (append-cflags configuration "-I/usr/local/include")
  #+linux (append-cflags configuration "-fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -fno-stack-protector -stdlib=libstdc++"
                                       :type :cxxflags)
  #+linux (append-cflags configuration "-fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -fno-stack-protector"
                                       :type :cflags)
  #+linux (append-ldlibs configuration "-ldl")
  (when (address-sanitizer configuration)
    (append-cflags configuration "-fsanitize=address" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=address"))
  (when (memory-sanitizer configuration)
    (append-cflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1" :type :cflags)
    (append-cflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=memory -fsanitize-memory-track-origins=1"))
  (when (thread-sanitizer configuration)
    (append-cflags configuration "-fsanitize=thread" :type :cxxflags)
    (append-ldflags configuration "-fsanitize=thread")))

(defmethod configure-unit (configuration (unit (eql :default-target)))
  "Configure default target."
  (message :emph "Configuring the default target")
  (with-accessors ((default-target default-target)
                   (default-stage default-stage))
      configuration
    (unless default-target
      (setf default-target (if (extensions configuration)
                               "eclasp-boehmprecise"
                               "cclasp-boehmprecise")))
    (loop with bitcode-name = (subseq default-target (1+ (position #\- default-target)))
          for variant in (variants configuration)
          when (equal bitcode-name (variant-bitcode-name variant))
           do (setf (variant-default variant) t))
    (setf default-stage
          (find (subseq default-target 0 (position #\- default-target))
                '(:iclasp :aclasp :bclasp :cclasp :eclasp :sclasp)
                :test #'string-equal))))

(defmethod configure-unit (configuration (unit (eql :cpu-count)))
  "Use sysctl or nproc to determine the number of CPU cores and set the job count if it
has not been set."
  (unless (jobs configuration)
    (message :emph "Inspecting system to determine the number of cpu cores")
    (loop for command in #+bsd '("sysctl -n hw.physicalcpu" "sysctl -n hw.ncpu" "sysctl -n hw.ncpufound")
                         #-bsd '("nproc --all")
          for output = (run-program-capture command)
          when output
            do (let ((count (parse-integer output :junk-allowed t)))
                 (message :info "Found ~a cpu cores. Setting the number of jobs to ~a."
                          count
                          (setf (jobs configuration)
                                (cond ((< count 2) 2)
                                      ((= count 2) 3)
                                      (t (+ 2 count))))))
               (return)
          finally (message :warn "Unknown number of cpu cores. Setting the number of jobs to ~a."
                           (setf (jobs configuration) 4))))) 

(defmethod configure-unit (configuration (unit (eql :describe)))
  "Update version number."
  (with-accessors ((version version)
                   (commit-short commit-short)
                   (commit-full commit-full))
      configuration
    (cond ((git-working-tree-p configuration)
           (message :emph "Updating version number.")
           (setf version (or (if (member :cando (extensions configuration))
                                 (format nil "~a-g~a"
                                         (git-describe configuration)
                                         (git-commit configuration :directory "extensions/cando/"
                                                     :short t))
                                 (git-describe configuration))
                             version)
                 commit-short (git-commit configuration :short t)
                 commit-full (git-commit configuration))
           (message :info "Set version number to ~A" version))
          ((not version)
           (message :err "Clasp version number is not defined and we are not in a git working tree!")))))

(defmethod configure-unit (configuration (unit (eql :jupyter)))
  "Configure Jupyter"
  (let ((jupyter-path-env (uiop:getenvp "JUPYTER_PATH")))
    (message :emph "Configuring Jupyter")
    (cond ((jupyter-path configuration)
           (message :info "Jupyter path already initialized."))
          (jupyter-path-env
           (message :info "Using JUPYTER_PATH environment variable to set Jupyter path.")
           (setf (jupyter-path configuration)
                 (uiop::ensure-directory-pathname jupyter-path-env)))
          (t
           (message :info "Deducing Jupyter path from share path.")
           (setf (jupyter-path configuration)
                 (merge-pathnames (make-pathname :directory '(:relative :up "jupyter"))
                                  (share-path configuration)))))))

(defmethod configure-unit (configuration (unit (eql :reproducible)))
  "Configure for a reproducible build."
  (cond ((reproducible-build configuration)
         (message :emph "Configuring reproducible build")
         (append-cflags configuration
                        (format nil "-ffile-prefix-map=..=~a"
                                (normalize-directory (root :install-share))))
         (loop for variant in (variants configuration)
               do (append-cflags variant
                                 (format nil "-ffile-prefix-map=~a=~a -ffile-prefix-map=~a=~a"
                                         (normalize-directory (make-pathname :directory (list :relative
                                                                                              (variant-bitcode-name variant)
                                                                                              "generated")))
                                         (normalize-directory (root :install-generated))
                                         (normalize-directory (make-pathname :directory (list :relative
                                                                                              (variant-bitcode-name variant)
                                                                                              "lib")))
                                         (normalize-directory (root :install-lib))))))
        (t
         (message :emph "Configuring non-reproducible build")
         (loop for variant in (variants configuration)
               do (append-ldflags variant
                                  (format nil "-Wl,--enable-new-dtags,-rpath,~a"
                                          (normalize-directory
                                           (uiop:ensure-absolute-pathname
                                            (merge-pathnames
                                             (make-pathname :directory (list :relative
                                                                             (variant-bitcode-name variant)
                                                                             "lib"))
                                             (root :build))
                                            (uiop:getcwd)))))))))

(defmethod configure-unit (configuration (unit (eql :asdf)))
  "Configure ASDF"
  (message :emph "Building ASDF sources")
  (run-program "./make-asdf.sh" :directory #P"src/lisp/modules/asdf/"))
