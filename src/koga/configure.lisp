(in-package #:koga)

(defparameter +core-features+
  '(:32-bit :64-bit :alpha :arm :arm64 :asdf :asdf2 :asdf3 :asdf3.1 :asdf3.2 :asdf3.3 :asdf-unicode
    :big-endian :bsd :darwin :freebsd :hppa :hppa64 :linux :little-endian :mips :netbsd :openbsd
    :ppc :ppc64 :sparc :sparc64 :sunos :unix :x86 :x86-64))

(defparameter *configuration*
  nil
  "The current configuration")

(defparameter *extensions*
  nil
  "The current extensions")

(defparameter *variant-gc*
  nil
  "The garbage collector of the current variant.")

(defparameter *variant-precise*
  nil
  "Is the current variant precise?")

(defparameter *variant-prep*
  nil
  "Is the current variant a prep?")

(defparameter *variant-debug*
  nil
  "Is the current variant a debug build?")

(defparameter *variant-default*
  nil
  "Is the current variant the default variant?")

(defparameter *variant-name*
  nil
  "The name of the current variant (without debugging suffix).")

(defparameter *variant-bitcode-name*
  nil
  "The full name of the current variant (with debugging suffix).")

(defparameter *variant-cflags*
  nil
  "cflags of the current variant.")

(defparameter *variant-cppflags*
  nil
  "cppflags of the current variant.")

(defparameter *variant-cxxflags*
  nil
  "cxxflags of the current variant.")

(defparameter *variant-ldflags*
  nil
  "ldflags of the current variant.")

(defparameter *variant-ldlibs*
  nil
  "ldlibs of the current variant.")

(defclass flags ()
  ((cflags :accessor cflags
           :initform nil
           :initarg :cflags
           :type (or null string)
           :documentation "Flags for the C compiler.")
   (cppflags :accessor cppflags
             :initform nil
             :initarg :cppflags
             :type (or null string)
             :documentation "Flags for the C preprocessor.")
   (cxxflags :accessor cxxflags
             :initform nil
             :initarg :cxxflags
             :type (or null string)
             :documentation "Flags for the C++ compiler.")
   (ldflags :accessor ldflags
            :initform nil
            :initarg :ldflags
            :type (or null string)
            :documentation "Linking flags that appear first on the call to ld.")
   (ldlibs :accessor ldlibs
           :initform nil
           :initarg :ldlibs
           :type (or null string)
           :documentation "Libraries or flags that appear last on the call to ld."))
  (:documentation "Flags associated with a configuration or a variant."))

(defclass variant (flags)
  ((gc :reader variant-gc
       :initarg :gc
       :type (member :boehm :mmtk :mps)
       :documentation "The garbage collector of the variant.")
   (precise :reader variant-precise
            :initarg :precise
            :initform nil
            :type boolean
            :documentation "Is the variant a precise build?")
   (prep :reader variant-prep
         :initarg :prep
         :initform nil
         :type boolean
         :documentation "Is the variant a prep build?")
   (debug :reader variant-debug
          :initarg :debug
          :initform nil
          :type boolean
          :documentation "Is the variant a debug build?")
   (default :accessor variant-default
            :initform nil
            :type boolean
            :documentation "Is the variant the default?"))
  (:documentation "Class that represents a variant."))

(defclass configuration (flags)
  ((build-mode :accessor build-mode ; TODO Add logic for :bitcode, :object and :fasl
               :initarg :build-mode
               :initform :faso
               :type (member :faso :bitcode :bytecode :object :fasoll :fasobc :fasl)
               :documentation "Define how clasp is built.
- :bitcode compiles to bitcode and thinLTO is used to link everything.
  This gives the fastest product but linking takes a long time.
- :object produces object files and regular linking is used.
  This is probably not as fast as bitcode (maybe a few percent slower)
  but it links fast.
- :faso generates faso files. This is good for development.")
   (build-path :accessor build-path
               :initarg :build-path
               :initform #P"build/"
               :type pathname
               :documentation "The directory where build files are to be put.")
   (parallel-build :accessor parallel-build
                   :initarg :parallel-build
                   :initform t
                   :type boolean
                   :documentation "Build clasp in parallel")
   (bin-path :accessor bin-path
             :initform #P"/usr/local/bin/"
             :initarg :bin-path
             :type pathname
             :documentation "The directory under which to install binaries.")
   (lib-path :accessor lib-path
             :initform #P"/usr/local/lib/clasp/"
             :initarg :lib-path
             :type pathname
             :documentation "The directory under which to install the Clasp libraries.")
   (share-path :accessor share-path
               :initform #P"/usr/local/share/clasp/"
               :initarg :share-path
               :type pathname
               :documentation "The directory under which to install shared Clasp files.")
   (jupyter-path :accessor jupyter-path
                 :initform nil
                 :initarg :jupyter-path
                 :type (or null pathname)
                 :documentation "The directory under which to install Jupyter files.")
   (package-path :accessor package-path
                 :initform nil
                 :initarg :package-path
                 :type (or null pathname)
                 :documentation "The directory used as an install location during packaging.")
   (extensions :accessor extensions
               :initarg :extensions
               :initform nil
               :type list
               :documentation "")
   (broken-stdlib :accessor broken-stdlib
                  :initarg :broken-stdlib
                  :initform nil
                  :type boolean
                  :documentation "If T if the C++ stdlib is broken for C++20.")
   (cst :accessor cst
        :initarg :cst
        :initform t
        :type boolean
        :documentation "If T use the CST build, otherwise use the AST build.")
   (mpi :accessor mpi
        :initarg :mpi
        :initform nil
        :type boolean
        :documentation "If T enable OpenMPI support.")
   (mpicxx :accessor mpicxx
           :initarg :mpicxx
           :initform nil
           :type (or null pathname)
           :documentation "Path to mpic++ binary.")
   (clang-cpp :accessor clang-cpp
              :initarg :clang-cpp
              :initform t
              :type boolean
              :documentation "If t use clang-cpp otherwise use the individual clang libraries.")
   (compile-file-parallel :accessor compile-file-parallel
                          :initarg :compile-file-parallel
                          :initform t
                          :type boolean
                          :documentation "Compile files in parallel.")
   (force-startup-external-linkage :accessor force-startup-external-linkage
                                   :initarg :force-startup-external-linkage
                                   :initform t
                                   :type boolean
                                   :documentation "Use external-linkage for StartUp functions")
   (unwinder :accessor unwinder ; TODO Add libunwind logic
             :initform :gcc
             :initarg :unwinder
             :type (member :gcc :llvm)
             :documentation "The libunwind to use.")
   (jobs :accessor jobs
         :initarg :jobs
         :initform nil
         :type (or null integer)
         :documentation "The number of concurrent jobs during aclasp, bclasp and clasp compilation.")
   (always-inline-mps-allocations :accessor always-inline-mps-allocations
                                  :initform t
                                  :initarg :always-inline-mps-allocations
                                  :type boolean)
   (address-sanitizer :accessor address-sanitizer
                      :initarg :address-sanitizer
                      :initform nil
                      :type boolean)
   (memory-sanitizer :accessor memory-sanitizer
                      :initarg :memory-sanitizer
                      :initform nil
                      :type boolean)
   (thread-sanitizer :accessor thread-sanitizer
                      :initarg :thread-sanitizer
                      :initform nil
                      :type boolean)
   (debug-dtree-interpreter :accessor debug-dtree-interpreter
                            :initarg :debug-dtree-interpreter
                            :initform nil
                            :type boolean
                            :documentation "Generate dtree interpreter log")
   (debug-dtrace-lock-probe :accessor debug-dtrace-lock-probe
                            :initarg :debug-dtrace-lock-probe
                            :initform nil
                            :type boolean
                            :documentation "Add a Dtrace probe for mutex lock acquisition")
   (debug-stackmaps :accessor debug-stackmaps
                    :initarg :debug-stackmaps
                    :initform nil
                    :type boolean
                    :documentation "print messages about stackmap registration")
   (debug-assert :accessor debug-assert
                 :initarg :debug-assert
                 :initform nil
                 :type boolean
                 :documentation "Turn on DEBUG_ASSERT")
   (debug-assert-type-cast :accessor debug-assert-type-cast
                           :initarg :debug-assert-type-cast
                           :initform nil
                           :type boolean
                           :documentation "Turn on type checking when passing arguments")
   (source-debug :accessor source-debug
                 :initarg :source-debug
                 :initform nil
                 :type boolean
                 :documentation "Allow LOG messages to print - works with CLASP_DEBUG environment variable")
   (debug-jit-log-symbols :accessor debug-jit-log-symbols
                          :initarg :debug-jit-log-symbols
                          :initform nil
                          :type boolean
                          :documentation "Generate a log of JITted symbols in /tmp/clasp-symbols-<pid>")
   (debug-guard :accessor debug-guard
                :initarg :debug-guard
                :initform nil
                :type boolean
                :documentation "Add guards around allocated objects")
   (debug-guard-validate :accessor debug-guard-validate
                         :initarg :debug-guard-validate
                         :initform nil
                         :type boolean
                         :documentation "Add quick checking of guards")
   (debug-guard-backtrace :accessor debug-guard-backtrace
                          :initarg :debug-guard-backtrace
                          :initform nil
                          :type boolean
                          :documentation "Add allocation backtraces to guards")
   (debug-guard-exhaustive-validate :accessor debug-guard-exhaustive-validate
                                    :initarg :debug-guard-exhaustive-validate
                                    :initform nil
                                    :type boolean
                                    :documentation "Add exhaustive, slow, checks of guards")
   (debug-trace-interpreted-closures :accessor debug-trace-interpreted-closures
                                     :initarg :debug-trace-interpreted-closures
                                     :initform nil
                                     :type boolean
                                     :documentation "")
   (debug-environments :accessor debug-environments
                       :initarg :debug-environments
                       :initform nil
                       :type boolean
                       :documentation "")
   (debug-release :accessor debug-release
                  :initarg :debug-release
                  :initform nil
                  :type boolean
                  :documentation "Turn off optimization for a few C++ functions; undef this to optimize everything")
   (debug-cache :accessor debug-cache
                :initarg :debug-cache
                :initform nil
                :type boolean
                :documentation "Debug the dispatch caches - see cache.cc")
   (debug-bitunit-container :accessor debug-bitunit-container
                            :initarg :debug-bitunit-container
                            :initform nil
                            :type boolean
                            :documentation "Prints debug info for bitunit containers")
   (debug-lexical-depth :accessor debug-lexical-depth
                        :initarg :debug-lexical-depth
                        :initform nil
                        :type boolean
                        :documentation "Generate tests for lexical closure depths")
   (debug-dynamic-binding-stack :accessor debug-dynamic-binding-stack
                                :initarg :debug-dynamic-binding-stack
                                :initform nil
                                :type boolean
                                :documentation "dynamic variable binding debugging")
   (debug-values :accessor debug-values
                 :initarg :debug-values
                 :initform nil
                 :type boolean
                 :documentation "turn on printing (values x y z) values when core:*debug-values* is not nil")
   (debug-ihs :accessor debug-ihs
              :initarg :debug-ihs
              :initform nil
              :type boolean
              :documentation "")
   (debug-track-unwinds :accessor debug-track-unwinds
                        :initarg :debug-track-unwinds
                        :initform nil
                        :type boolean
                        :documentation "Count cc_unwind calls and report in TIME")
   (debug-no-unwind :accessor debug-no-unwind
                    :initarg :debug-no-unwind
                    :initform nil
                    :type boolean
                    :documentation "Debug intrinsics that say they don't unwind but actually do")
   (debug-startup :accessor debug-startup
                  :initarg :debug-startup
                  :initform nil
                  :type boolean
                  :documentation "Generate per-thread logs in /tmp/dispatch-history/**  of the slow path of fastgf")
   (debug-rehash-count :accessor debug-rehash-count
                       :initarg :debug-rehash-count
                       :initform nil
                       :type boolean
                       :documentation "Keep track of the number of times each hash table has been rehashed")
   (debug-monitor :accessor debug-monitor
                  :initarg :debug-monitor
                  :initform nil
                  :type boolean
                  :documentation "generate logging messages to a file in /tmp for non-hot code")
   (debug-monitor-support :accessor debug-monitor-support
                          :initarg :debug-monitor-support
                          :initform nil
                          :type boolean
                          :documentation "Must be enabled with other options - do this automatically?")
   (debug-memory-profile :accessor debug-memory-profile
                         :initarg :debug-memory-profile
                         :initform nil
                         :type boolean
                         :documentation "Profile memory allocations total size and counter")
   (debug-bclasp-lisp :accessor debug-bclasp-lisp
                      :initarg :debug-bclasp-lisp
                      :initform nil
                      :type boolean
                      :documentation "Generate debugging frames for all bclasp code - like declaim")
   (debug-cclasp-lisp :accessor debug-cclasp-lisp
                      :initarg :debug-cclasp-lisp
                      :initform t
                      :type boolean
                      :documentation "Generate debugging frames for all cclasp code - like declaim (default on)")
   (debug-count-allocations :accessor debug-count-allocations
                            :initarg :debug-count-allocations
                            :initform nil
                            :type boolean
                            :documentation "count per-thread allocations of instances of classes")
   (debug-compiler :accessor debug-compiler
                   :initarg :debug-compiler
                   :initform nil
                   :type boolean
                   :documentation "Turn on compiler debugging")
   (debug-verify-modules :accessor debug-verify-modules
                         :initarg :debug-verify-modules
                         :initform nil
                         :type boolean
                         :documentation "Verify LLVM modules before using them")
   (debug-verify-transformations :accessor debug-verify-transformations
                                 :initarg :debug-verify-transformations
                                 :initform nil
                                 :type boolean
                                 :documentation "Verify BIR transformations before using them")
   (debug-long-call-history :accessor debug-long-call-history
                            :initarg :debug-long-call-history
                            :initform nil
                            :type boolean
                            :documentation "The GF call histories used to blow up - this triggers an error if they get too long")
   (debug-bounds-assert :accessor debug-bounds-assert
                        :initarg :debug-bounds-assert
                        :initform t
                        :type boolean
                        :documentation "check bounds")
   (debug-gfdispatch :accessor debug-gfdispatch
                     :initarg :debug-gfdispatch
                     :initform nil
                     :type boolean
                     :documentation "debug call history manipulation")
   (debug-fastgf :accessor debug-fastgf
                 :initarg :debug-fastgf
                 :initform nil
                 :type boolean
                 :documentation "generate slow gf dispatch logging and write out dispatch functions to /tmp/dispatch-history-**")
   (debug-slot-accessors :accessor debug-slot-accessors
                         :initarg :debug-slot-accessors
                         :initform nil
                         :type boolean
                         :documentation "GF accessors have extra debugging added to them")
   (debug-threads :accessor debug-threads
                  :initarg :debug-threads
                  :initform nil
                  :type boolean
                  :documentation "")
   (debug-stores :accessor debug-stores
                 :initarg :debug-stores
                 :initform nil
                 :type boolean
                 :documentation "insert a call to cc_validate_tagged_pointer everytime something is written to memory")
   (debug-ensure-valid-object :accessor debug-ensure-valid-object
                              :initarg :debug-ensure-valid-object
                              :initform nil
                              :type boolean
                              :documentation "Defines ENSURE_VALID_OBJECT(x)->x macro - sprinkle these around to run checks on objects")
   (debug-quick-validate :accessor debug-quick-validate
                         :initarg :debug-quick-validate
                         :initform nil
                         :type boolean
                         :documentation "quick/cheap validate if on and comprehensive validate if not")
   (debug-mps-size :accessor debug-mps-size
                   :initarg :debug-mps-size
                   :initform nil
                   :type boolean
                   :documentation "check that the size of the MPS object will be calculated properly by obj_skip")
   (debug-mps-underscanning :accessor debug-mps-underscanning
                            :initarg :debug-mps-underscanning
                            :initform nil
                            :type boolean
                            :documentation "Very expensive - does a mps_arena_collect/mps_arena_release for each allocation")
   (debug-dont-optimize-bclasp :accessor debug-dont-optimize-bclasp
                               :initarg :debug-dont-optimize-bclasp
                               :initform nil
                               :type boolean
                               :documentation "Optimize bclasp by editing llvm-ir")
   (debug-recursive-allocations :accessor debug-recursive-allocations
                                :initarg :debug-recursive-allocations
                                :initform nil
                                :type boolean
                                :documentation "Catch allocations within allocations - MPS hates these")
   (debug-alloc-alignment :accessor debug-alloc-alignment
                          :initarg :debug-alloc-alignment
                          :initform nil
                          :type boolean
                          :documentation "catch misaligned allocations")
   (debug-llvm-optimization-level-0 :accessor debug-llvm-optimization-level-0
                                    :initarg :debug-llvm-optimization-level-0
                                    :initform nil
                                    :type boolean
                                    :documentation "")
   (debug-slow :accessor debug-slow
               :initarg :debug-slow
               :initform nil
               :type boolean
               :documentation "Code runs slower due to checks - undefine to remove checks")
   (human-readable-bitcode :accessor human-readable-bitcode
                           :initarg :human-readable-bitcode
                           :initform nil
                           :type boolean
                           :documentation "")
   (debug-compile-file-output-info :accessor debug-compile-file-output-info
                                   :initarg :debug-compile-file-output-info
                                   :initform nil
                                   :type boolean
                                   :documentation "")
   (config-var-cool :accessor config-var-cool
                    :initarg :config-var-cool
                    :initform t
                    :type boolean
                    :documentation "mps setting")
   (ld :accessor ld ; TODO Add ld detection logic
       :initarg :ld
       :initform #+darwin nil #-darwin :gold
       :type (member nil :bfd :lld :gold :mold)
       :documentation "The linker to use")
   (ar :accessor ar
       :initarg :ar
       :initform nil
       :type (or null pathname)
       :documentation "The ar binary to use. If not set then llvm-config will be used to find llvm-ar.")
   (cc :accessor cc
       :initarg :cc
       :initform nil
       :type (or null pathname)
       :documentation "The cc binary to use. If not set then llvm-config will be used to find clang.")
   (cxx :accessor cxx
        :initarg :cxx
        :initform nil
        :type (or null pathname)
        :documentation "The cxx binary to use. If not set then llvm-config will be used to find clang++.")
   (git :accessor git
        :initarg :git
        :initform nil
        :type (or null pathname)
        :documentation "The git binary to use.")
   (lisp :accessor lisp
         :initarg :lisp
         :initform #+ccl #P"ccl"
                   #+ecl #P"ecl"
                   #+sbcl #P"sbcl"
                   #-(or ccl ecl sbcl) (error "Booting Clasp from implementation ~a is not currently supported." (lisp-implementation-type))
         :type (or null pathname)
         :documentation "The Lisp binary to bootstrap from.")
   (llvm-config :accessor llvm-config
                :initform nil
                :initarg :llvm-config
                :type (or null string pathname)
                :documentation "The llvm-config binary to use. If not set then configure will attempt to
find a compatible one.")
   (nm :accessor nm
       :initarg :nm
       :initform nil
       :type (or null pathname)
       :documentation "The nm binary to use. If not set then llvm-config will be used to find llvm-nm.")
   (objcopy :accessor objcopy
            :initarg :objcopy
            :initform nil
            :type (or null pathname)
            :documentation "The objcopy binary to use. This must be the GNU objcopy on Linux. llvm-objcopy
is not compatible with snapshots.")
   (pkg-config :accessor pkg-config
               :initform nil
               :initarg :pkg-config
               :type (or null pathname)
               :documentation "The pkg-config binary to use.")
   (etags :accessor etags
          :initform nil
          :initarg :etags
          :type (or null pathname)
          :documentation "The etags binary to use.")
   (ctags :accessor ctags
          :initform nil
          :initarg :ctags
          :type (or null pathname)
          :documentation "The ctags binary to use.")
   (jupyter :accessor jupyter
            :initform nil
            :initarg :jupyter
            :type boolean
            :documentation "Enable Jupyter and create Jupyter kernels.")
   (xcode-sdk :accessor xcode-sdk
              :initform nil
              :initarg :xcode-sdk
              :type (or null pathname)
              :documentation "XCode SDK Path")
   (default-target :accessor default-target
                   :initform nil
                   :initarg :default-target
                   :type (or null string)
                   :documentation "Default build target for Ninja")
   (commit-short :accessor commit-short
                 :initform nil
                 :initarg :commit-short
                 :type (or null string)
                 :documentation "The short commit hash of the source code.")
   (commit-full :accessor commit-full
                :initform nil
                :initarg :commit-full
                :type (or null string)
                :documentation "The full commit hash of the source code.")
   (version :accessor version
            :initform nil
            :initarg :version
            :type (or null string)
            :documentation "The version of the source code.")
   (update-version :accessor update-version
                   :initform nil
                   :initarg :update-version
                   :type boolean
                   :documentation "Use git describe to update the version and commit values in the version.sexp file and then exit.")
   (reproducible-build :accessor reproducible-build
                       :initform nil
                       :initarg :reproducible-build
                       :type boolean
                       :documentation "Use a reproducible build by remapping build paths, etc.")
   (extension-systems :accessor extension-systems
                      :initform nil
                      :type list
                      :documentation "")
   (target-systems :accessor target-systems
                   :initform (make-hash-table)
                   :type hash-table
                   :documentation "")
   (default-stage :accessor default-stage
                  :initform :cclasp
                  :type (member :iclasp :aclasp :bclasp :cclasp :eclasp :sclasp)
                  :documentation "Default stage for installation")
   (units :accessor units
          :initform '(:git :describe :cpu-count #+darwin :xcode :base :default-target :pkg-config
                      :clang :llvm :ar :cc :cxx :mpi :nm :etags :ctags :objcopy :jupyter :reproducible :asdf)
          :type list
          :documentation "The configuration units")
   (outputs :accessor outputs
            :initform (alexandria:plist-hash-table (list :generate-sif
                                                         (list (make-source #P"generate-sif.lisp" :build))
                                                         :generate-headers
                                                         (list (make-source #P"generate-headers.lisp" :variant)
                                                               :scraper)
                                                         :generate-vm-header
                                                         (list (make-source #P"generate-vm-header.lisp" :build))
                                                         :compile-systems
                                                         (list (make-source #P"compile-systems.lisp" :build))
                                                         :update-unicode
                                                         (list (make-source #P"update-unicode.lisp" :build))
                                                         :load-clasp
                                                         (list (make-source #P"load-clasp.lisp" :build))
                                                         :snapshot-clasp
                                                         (list (make-source #P"snapshot-clasp.lisp" :build))
                                                         :compile-clasp
                                                         (list (make-source #P"compile-clasp.lisp" :build))
                                                         :compile-module
                                                         (list (make-source #P"compile-module.lisp" :build))
                                                         :link-fasl
                                                         (list (make-source #P"link-fasl.lisp" :build))
                                                         :analyze-file
                                                         (list (make-source #P"analyze-file.lisp" :build))
                                                         :analyze-generate
                                                         (list (make-source #P"analyze-generate.lisp" :build))
                                                         :snapshot
                                                         (list (make-source #P"snapshot.lisp" :variant))
                                                         :clasprc
                                                         (list (make-source #P"clasprc.lisp" :variant))
                                                         :jupyter-kernel
                                                         (list (make-source #P"jupyter-kernel.lisp" :variant))
                                                         :ansi-test-subset
                                                         (list (make-source #P"ansi-test-subset.lisp" :build))
                                                         :bench
                                                         (list (make-source #P"bench.lisp" :build))
                                                         :ninja
                                                         (list (make-source #P"build.ninja" :build)
                                                               :bitcode :iclasp :cclasp :modules :eclasp
                                                               :eclasp-link :sclasp :install-bin :install-code
                                                               :clasp :regression-tests :analyzer :analyze
                                                               :tags :install-extension-code :vm-header)
                                                         :config-h
                                                         (list (make-source #P"config.h" :variant)
                                                               :scraper)
                                                         :version-h
                                                         (list (make-source #P"version.h" :variant))
                                                         :base-translations
                                                         (list (make-source #P"generated/base-translations.lisp" :variant)
                                                               :cclasp)
                                                         :extension-translations
                                                         (list (make-source #P"generated/extension-translations.lisp" :variant)
                                                               :extension-translations)
                                                         :base-immutable
                                                         (list (make-source #P"generated/base-immutable.lisp" :variant))
                                                         :extension-immutable
                                                         (list (make-source #P"generated/extension-immutable.lisp" :variant))
                                                         :compile-commands
                                                         (list (make-source #P"compile_commands.json" :variant)
                                                               :iclasp)))
            :type hash-table
            :documentation "The configuration outputs with the associated targets.")
   (targets :accessor targets
            :initform (make-hash-table)
            :type hash-table
            :documentation "The configuration targets and sources")
   (llvm-version :accessor llvm-version
                 :initform nil
                 :type (or null string)
                 :documentation "The version of the compatible LLVM discovered.")
   (llvm-bindir :accessor llvm-bindir
                :initform nil
                :type (or null pathname)
                :documentation "The bin directory of LLVM.")
   (llvm-includedir :accessor llvm-includedir
                    :initform nil
                    :type (or null pathname)
                    :documentation "The include directory of LLVM.")
   (features :accessor features
             :initform '(:non-base-chars-exist-p :package-local-nicknames :cdr-7 :cdr-6 :cdr-5
                         :threads :unicode :ieee-floating-point :clasp :ansi-cl :common-lisp)
             :type list
             :documentation "The anticipated value of *FEATURES* in the CLASP build.")
   (scraper-headers :accessor scraper-headers
                    :initform nil
                    :type list
                    :documentation "The headers generated by the scrapper.")
   (scraper-precise-headers :accessor scraper-precise-headers
                            :initform nil
                            :type list
                            :documentation "The headers generated by the scrapper for precise builds.")
   (scraper-lisp-sources :accessor scraper-lisp-sources
                         :initform nil
                         :type list
                         :documentation "The lisp files generated by the scrapper.")
   (variants :reader variants
             :initform (list (make-instance 'variant :gc :boehm)
                             (make-instance 'variant :gc :boehm :precise t)
                             (make-instance 'variant :gc :boehm :debug t)
                             (make-instance 'variant :gc :boehm :precise t :debug t)
                             (make-instance 'variant :gc :mps :prep t)
                             (make-instance 'variant :gc :mps :debug t :prep t))
             :type list
             :documentation "A list of the variants")
   (scripts :accessor scripts
            :initform nil)
   (repos :reader repos
          :initform (uiop:read-file-form #P"repos.sexp")))
  (:documentation "A class to encapsulate the configuration state."))

(defmethod initialize-instance :after ((instance configuration) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (package-path instance)
    (setf (package-path instance)
          (uiop:ensure-directory-pathname (package-path instance))))
  (setf (build-path instance)
        (uiop:ensure-directory-pathname (build-path instance))
        (bin-path instance)
        (uiop:ensure-directory-pathname (bin-path instance))
        (lib-path instance)
        (uiop:ensure-directory-pathname (lib-path instance))
        (share-path instance)
        (uiop:ensure-directory-pathname (share-path instance)))
  (when (jupyter-path instance)
    (setf (jupyter-path instance)
          (uiop:ensure-directory-pathname (jupyter-path instance))))
  (when (xcode-sdk instance)
    (setf (xcode-sdk instance)
          (uiop:ensure-directory-pathname (xcode-sdk instance))))
  (when (member :cando (extensions instance))
    (setf (gethash :clasp-sh (outputs instance))
          (list (make-source (make-pathname :name "clasp" :type :unspecific) :variant))))
  (loop for system in '(:asdf :asdf-package-system :uiop)
        for version = (asdf:component-version (asdf:find-system system))
        for expr = (if version (list system :version version) (list system))
        do (pushnew expr (gethash :cclasp (target-systems instance)))
           (pushnew expr (gethash :eclasp (target-systems instance))))
  (setf (features instance)
        (append (features instance)
                (remove-if (lambda (feature)
                             (not (member feature +core-features+)))
                           *features*))))

(defun build-name (name
                   &key common
                        (gc *variant-gc* gc-p)
                        (precise *variant-precise* precise-p)
                        (prep *variant-prep* prep-p)
                        (debug *variant-debug* debug-p))
  "Return a standardized name for a build step. name is downcased with
a suffix of the bitcode name. If common is non-NIL then \"common\" will
be used as the suffix. If the keys :gc, :precise, :prep or :debug are passed
then they will overide the current variant's corresponding property."
  (format nil "~(~a~)-~a" name
          (cond (common
                 "common")
                ((or gc-p precise-p prep-p debug-p)
                 (variant-bitcode-name (make-instance 'variant :gc gc :precise precise
                                                      :prep prep :debug debug)))
                (t
                 *variant-bitcode-name*))))

(defun file-faso-extension (configuration)
  "Return the file extension based on the build mode."
  (case (build-mode configuration)
    (:bytecode "faslbc")
    (:faso "faso")
    (:fasobc "fasobc")
    (:fasoll "fasoll")
    (otherwise "fasl")))

(defun module-fasl-extension (configuration)
  "Return the module extension, i.e. faso -> fasp, etc."
  (case (build-mode configuration)
    (:bytecode "faslbc")
    (:faso "fasp")
    (:fasobc "faspbc")
    (:fasoll "faspll")
    (otherwise "fasl")))

(defun image-fasl-extension (configuration)
  "Return the extension for the clasp image."
  (case (build-mode configuration)
    (:bytecode "faslbc")
    (:fasl "lfasl")
    (:faso "fasp")
    (:fasobc "faspbc")
    (:fasoll "faspll")
    (otherwise "fasl")))

(defun image-source (configuration extension &optional (root :variant-lib))
  "Return the name of an image based on a target name, the bitcode name
and the build mode."
  (make-source (format nil "images/~:[base~;extension~].~a"
                       extension (image-fasl-extension configuration))
               root))

(defun funcall-variant (configuration func
                        &key (debug nil debug-p) (gc nil gc-p)
                             (precise nil precise-p) (prep nil prep-p)
                        &allow-other-keys)
  "Apply a function to all variants that match the keys :debug, :gc
:precise and :prep. If non of these keys are specified then just apply
the function to the overall configuration."
  (if (or debug-p gc-p precise-p prep-p)
      (loop for variant in (variants configuration)
            when (and (or (not debug-p)
                          (eql debug (variant-debug variant)))
                      (or (not gc-p)
                          (eql gc (variant-gc variant)))
                      (or (not precise-p)
                          (eql precise (variant-precise variant)))
                      (or (not prep-p)
                          (eql prep (variant-prep variant))))
              do (funcall func variant))
      (funcall func configuration)))

(defun append-cflags (configuration flags &rest rest
                      &key type &allow-other-keys
                      &aux (trimmed-flags (string-trim " " flags)))
  "Append flags to the specified variant's (via funcall-variant) cflags. The key
:type can be used to only update a specific type of cflag (:cflags, :cxxflags,
:cppflags)."
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (when (or (null type)
                       (eql :cflags type))
               (setf (cflags object)
                     (format nil "~@[~a ~]~a" (cflags object) trimmed-flags)))
             (when (or (null type)
                       (eql :cxxflags type))
               (setf (cxxflags object)
                     (format nil "~@[~a ~]~a" (cxxflags object) trimmed-flags)))
             (when (or (null type)
                       (eql :cppflags type))
               (setf (cppflags object)
                     (format nil "~@[~a ~]~a" (cppflags object) trimmed-flags))))
          rest)))

(defun append-ldflags (configuration flags &rest rest
                       &aux (trimmed-flags (string-trim " " flags)))
  "Append flags to the specified variant's (via funcall-variant) ldflags."
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (setf (ldflags object)
                   (format nil "~@[~a ~]~a" (ldflags object) trimmed-flags)))
           rest)))

(defun append-ldlibs (configuration flags &rest rest
                       &aux (trimmed-flags (string-trim " " flags)))
  "Append flags to the specified variant's (via funcall-variant) ldlibs."
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (setf (ldlibs object)
                   (format nil "~@[~a ~]~a" (ldlibs object) trimmed-flags)))
           rest)))

(defun sources (target &rest sources)
  "Add the souces to the specified target."
  (loop for source in sources
        do (add-target-source *configuration* target source)))

(defun recurse (&rest paths)
  "Recurse into each path looking for cscript files to load."
  (loop with script-path = *script-path*
        for path in paths
        do (message :info "Looking for configure scripts in ~a" path)
           (loop for subpath in (directory (merge-pathnames (merge-pathnames "cscript.lisp" path) script-path))
                 for script = (enough-namestring subpath (uiop:getcwd))
                 do (message :info "Found script ~a" script)
                    (push script (scripts *configuration*)))))

(defgeneric configure-unit (configuration unit)
  (:documentation "Configure a specific unit"))

(defun variant-name (variant)
  "Return the variant name, i.e. boehm, boehmprecise, preciseprep, etc"
  (format nil "~:[~(~a~)~:[~;precise~]~;preciseprep~]"
          (variant-prep variant)
          (variant-gc variant)
          (variant-precise variant)))

(defun variant-bitcode-name (variant)
  "Return the fully qualified bitcode name. This is the variant name with a
-d suffix if debugging is enabled."
  (format nil "~a~:[~;-d~]" (variant-name variant) (variant-debug variant)))

(defun configure-library (configuration library &rest rest
                          &key required min-version max-version &allow-other-keys)
  "Configure a library"
  (message :info "Configuring library ~a" library)
  (flet ((failure (control-string &rest args)
           (apply #'message (if required :err :warn) control-string args)
           nil))
    (let ((version (run-program-capture (list (pkg-config configuration) "--modversion" library))))
      (cond ((not version)
             (failure "Module ~a not found." library))
            ((and min-version
                  (uiop:version< version min-version))
             (failure "Module ~a with a version of ~a is less then minimum version of ~a." library version min-version))
            ((and max-version
                  (uiop:version<= max-version version))
             (failure "Module ~a with a version of ~a is not less then maximum version of ~a." library version max-version))
            (t
             (apply #'append-cflags configuration
                                    (run-program-capture (list (pkg-config configuration) "--cflags" library))
                                    rest)
             (apply #'append-ldflags configuration
                                     (run-program-capture (list (pkg-config configuration) "--libs-only-L" library))
                                     rest)
             (apply #'append-ldlibs configuration
                                    (run-program-capture (list (pkg-config configuration) "--libs-only-l" library))
                                    rest)
             t)))))

(defun library (&rest rest)
  "Configure a library with the current configuration state."
  (apply #'configure-library *configuration* rest))

(defun framework (name)
  (append-ldlibs *configuration* (format nil "-framework ~a" name)))

(defun includes (&rest paths)
  "Add C include directories to the current configuration."
  (let ((*root-paths* (list* :build #P""
                             :code (make-pathname :directory '(:relative :up))
                             *root-paths*)))
    (append-cflags *configuration*
                   (format nil "~{-I~a~^ ~}" (mapcar (lambda (source)
                                                       (normalize-directory (resolve-source source)))
                                                     paths)))))

(defun systems (&rest rest)
  (loop for system in rest
        do (pushnew system (extension-systems *configuration*))))

(defun configure-program (name candidates
                          &key major-version (version-flag "--version") required match)
  "Configure a program by looking through a list of candidates and checking the version number
if provided."
  (loop for candidate in (if (listp candidates) candidates (list candidates))
        for path = (if (stringp candidate)
                       (format nil candidate major-version)
                       (namestring candidate))
        for version = (run-program-capture (list path version-flag))
        when (and version
                  (or (null major-version)
                      (and match
                           (search match version))
                      (= major-version
                         (first (uiop:parse-version version)))))
          do (message :info "Found ~a program with path ~a ~:[~;and version ~a~]"
                            name path major-version version)
             (return (values path version))
        finally (message (if required :err :warn)
                         "Unable to find ~a program~@[ compatible with major version ~a~]."
                         name major-version)
                (values nil nil)))

