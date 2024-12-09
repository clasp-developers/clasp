(in-package #:koga)

(defmethod make-output-stream (configuration (name (eql :ninja)) path)
  (declare (ignore configuration name))
  (ninja:make-line-wrapping-stream (ninja:make-timestamp-preserving-stream path)))

(defun wrap-with-env (configuration executable-name)
  (if (reproducible-build configuration)
      (format nil "LD_LIBRARY_PATH=~a ~a"
              (make-source "lib/" :variant) executable-name)
      executable-name))

(defun lisp-command (script &optional arguments)
  (concatenate 'string
               "$lisp "
               #+abcl "--noinform --batch --noinit "
               #+(or clasp sbcl) "--script "
               #+ecl "--norc --shell "
               #+ccl "--no-init --quiet --load "
               #+clisp "-norc "
               script
               #+sbcl " "
               #+(or abcl clasp ecl) " -- "
               #+ccl " --eval \"(quit)\" -- "
               arguments))  

(defmethod print-prologue (configuration (name (eql :ninja)) output-stream
                           &aux (clasp-quicklisp-directory (uiop:getenvp "CLASP_QUICKLISP_DIRECTORY")))
  (ninja:write-bindings output-stream
                        :quicklisp-client (make-source "dependencies/quicklisp-client/" :code)
                        :cflags (cflags configuration)
                        :cppflags (cppflags configuration)
                        :cxxflags (cxxflags configuration)
                        :ar (ar configuration)
                        :cc (cc configuration)
                        :cxx (cxx configuration)
                        :dis (dis configuration)
                        :ldflags (ldflags configuration)
                        :ldlibs (ldlibs configuration)
                        :lisp (lisp configuration)
                        :ldflags_fasl (if (uiop:os-macosx-p)
                                          "-flat_namespace -undefined dynamic_lookup -bundle"
                                          "-shared")
                        :objcopy (objcopy configuration)
                        :tags (or (ctags configuration)
                                  (etags configuration)))
  (terpri output-stream)
  (ninja:write-rule output-stream :tags
                    :command (if (ctags configuration)
                                 "$tags -e -I $identifiers -o $out $in"
                                 "$tags -o $out $in")
                    :restat 1
                    :description "Creating tags")
  (ninja:write-rule output-stream :install-file
                    :command #+bsd "install -C -m 644 $in $out"
                             #+linux "install -CT --mode=644 $in $out"
                    :restat 1
                    :description "Installing $in to $out")
  (ninja:write-rule output-stream :install-binary
                    :command #+bsd "install -C -m 755 $in $out"
                             #+linux "install -CT --mode=755 $in $out"
                    :restat 1
                    :description "Installing $in to $out")
  (ninja:write-rule output-stream :symbolic-link
                    :command "ln -s -f $target $out"
                    :restat 1
                    :description "Linking $out to $target")
  (ninja:write-rule output-stream :scrape-pp
                    :command "$cxx $variant-cppflags $cppflags -MD -MF $out.d -o$out -E -DSCRAPING $in"
                    :description "Preprocess $in for scraping"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :generate-sif
                    :command (lisp-command "generate-sif.lisp" "$out $in")
                    :restat 1
                    :description "Scraping $in")
  (ninja:write-rule output-stream :generate-headers
                    :command (lisp-command "${variant-path}generate-headers.lisp" "$precise $in")
                    :restat 1
                    :description "Creating headers from sif files")
  (ninja:write-rule output-stream :generate-vm-header
                    :command (lisp-command "generate-vm-header.lisp" "$out $in")
                    :restat 1
                    :description "Generating VM header from $in")
  (ninja:write-rule output-stream :compile-systems
                    :command "$clasp --norc --non-interactive --base --feature ignore-extensions --load compile-systems.lisp -- $out $systems"
                    :description "Compiling systems: $systems"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :analyze-generate
                    :command "$clasp --norc --non-interactive --base --feature ignore-extensions --load analyze-generate.lisp -- $sif $in"
                    :description "Analyzing clasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :analyze-file
                    :command "$clasp --norc --non-interactive --base --feature ignore-extensions --load analyze-file.lisp -- $out $in $log $database"
                    :depfile "$out.d"
                    :description "Analyzing $in")
  (ninja:write-rule output-stream :ar
                    :command "$ar rcsu $out $in"
                    :description "Creating archive $out")
  (ninja:write-rule output-stream :cc
                    :command "$cc $variant-cflags $cflags -c -MD -MF $out.d -o$out $in"
                    :description "Compiling $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :cxx
                    :command "$cxx $variant-cxxflags $cxxflags -c -MD -MF $out.d -o$out $in"
                    :description "Compiling $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :cxx-llvm
                    :command "$cxx $variant-cxxflags $cxxflags -c -emit-llvm -g -O3 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -o$out $in"
                    :description "")
  (ninja:write-rule output-stream :disassemble
                    :command "$dis -o $out $in"
                    :description "Dissassembling $in")
  (ninja:write-rule output-stream :trampoline
                    :command (lisp-command "trampoline.lisp" "$out $in")
                    :description "Creating trampoline $out")
  (ninja:write-rule output-stream :link
                    :command "$cxx $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :link-lib
                    :command #+darwin "$cxx -dynamiclib $variant-ldflags $ldflags -install_name @rpath/$libname -o$out $in $variant-ldlibs $ldlibs"
                             #-darwin "$cxx -shared $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :load-cclasp
                    :command "$clasp --norc --disable-mpi --ignore-image --feature clasp-min --load load-clasp.lisp -- base 0 $source"
                    :description "Loading clasp $name"
                    :pool "console")
  (ninja:write-rule output-stream :snapshot-cclasp
                    :command "$clasp --norc --disable-mpi --ignore-image --feature clasp-min --load snapshot-clasp.lisp -- $out base 0 $source"
                    :description "Snapshot clasp $name"
                    :pool "console")
  (ninja:write-rule output-stream :compile-cclasp
                    :command "$clasp --norc --disable-mpi --ignore-image --feature clasp-min --load compile-clasp.lisp -- base 0 $source"
                    :description "Compiling clasp $name"
                    :restat 1
                    :pool "console")
  (when (extensions configuration)
    (ninja:write-rule output-stream :load-eclasp
                      :command "$clasp --norc --disable-mpi --base --feature ignore-extension-systems --feature cclasp --load load-clasp.lisp -- extension $position $source"
                      :description "Loading eclasp"
                      :pool "console")
    (ninja:write-rule output-stream :snapshot-eclasp
                      :command "$clasp --norc --disable-mpi --base --feature ignore-extension-systems --feature cclasp --load snapshot-clasp.lisp -- $out extension $position $source"
                      :description "Snapshot eclasp"
                      :pool "console")
    (ninja:write-rule output-stream :compile-eclasp
                      :command "$clasp --norc --disable-mpi --base --feature ignore-extension-systems --feature cclasp --load compile-clasp.lisp -- extension $position $source"
                      :description "Compiling eclasp"
                      :restat 1
                      :pool "console"))
  (ninja:write-rule output-stream :compile-module
                    :command "$clasp --non-interactive --norc --disable-mpi --base --feature ignore-extensions --load compile-module.lisp -- $fasl $source"
                    :description "Compiling module $in")
  (ninja:write-rule output-stream :regression-tests
                    :command "$clasp --norc --non-interactive --base --feature ignore-extensions --load \"sys:src;lisp;regression-tests;run-all.lisp\""
                    :description "Running regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :cando-regression-tests
                    :command "$clasp --norc --non-interactive --load \"sys:extensions;cando;src;lisp;regression-tests;run-all.lisp\""
                    :description "Running Cando regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :bench
                    :command "$clasp --norc --base --feature ignore-extensions --load bench.lisp"
                    :description "Running benchmarks"
                    :pool "console")
  (ninja:write-rule output-stream :ansi-test
                    :command "$clasp --norc --base --feature ignore-extensions --load ansi-test.lisp"
                    :description "Running ANSI tests"
                    :pool "console")
  (ninja:write-rule output-stream :asdf-test
                    :command "bash asdf-test.bash $clasp $target"
                    :description "Running ASDF tests"
                    :pool "console")
  (ninja:write-rule output-stream :test-random-integer
                    :command "$clasp --norc --base --feature ignore-extensions --load \"../dependencies/ansi-test/run-random-type-tests.lisp\""
                    :description "Running pfdietz test-random-integer-forms"
                    :pool "console")
  (ninja:write-rule output-stream :link-fasl
                    :command "$clasp --norc --disable-mpi --ignore-image --feature clasp-min --load link-fasl.lisp -- $out $in"
                    :restat 1
                    :description "Linking $target")
  (ninja:write-rule output-stream "link-fasl-abc"
                    :command "$cxx $variant-ldflags $ldflags $ldflags-fasl -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :jupyter-system-kernel
                    :command (format nil
                                     "~:[CLASP_QUICKLISP_DIRECTORY=$quicklisp-client ~;~]$clasp --rc ${variant-path}clasprc.lisp --non-interactive --load ${variant-path}jupyter-kernel.lisp -- $name $bin-path $load-system 1"
                                     clasp-quicklisp-directory)
                    :description "Installing jupyter kernel for $name")
  (ninja:write-rule output-stream :jupyter-user-kernel
                    :command "$clasp --non-interactive --load ${variant-path}jupyter-kernel.lisp -- $name $bin-path $load-system 0"
                    :description "Installing jupyter kernel for $name")
  (ninja:write-rule output-stream :make-snapshot
                    :command (format nil
                                     "~:[CLASP_QUICKLISP_DIRECTORY=$quicklisp-client ~;~]$clasp --rc ${variant-path}clasprc.lisp --non-interactive $arguments --load ${variant-path}snapshot.lisp -- $out"
                                     clasp-quicklisp-directory)
                    :pool "console"
                    :description "Creating snapshot $out")
  (ninja:write-rule output-stream :update-unicode
                    :command (lisp-command "update-unicode.lisp" "$source")
                    :description "Updating unicode tables"))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :trampoline)) (source cc-source)
     &aux (ll (make-source-output source :type "ll"))
          (bc (make-source-output source :type "bc"))
          (header (make-source "trampoline.h" :variant-generated))
          (installed-header (make-source "include/trampoline.h" :package-share)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :cxx-llvm
                     :variant-cxxflags *variant-cxxflags*
                     :inputs (list source)
                     :outputs (list ll))
  (ninja:write-build output-stream :disassemble
                     :inputs (list ll)
                     :outputs (list bc))
  (ninja:write-build output-stream :trampoline
                     :inputs (list bc)
                     :outputs (list header))
  (when *variant-default*
    (ninja:write-build output-stream :install-file
                                :inputs (list header)
                                :outputs (list installed-header)))
  (list :outputs header))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :tags)) sources
     &key &allow-other-keys
     &aux (identifiers (make-source ".identifiers" :code))
          (outputs (list (make-source "TAGS" :code))))
  (when (and *variant-default*
             (not (reproducible-build configuration))
             (or (etags configuration)
                 (ctags configuration)))
    (ninja:write-build output-stream :tags
                       :inputs (append (remove-if (lambda (source)
                                                    (not (or (typep source 'lisp-source)
                                                             (typep source 'h-source)
                                                             (typep source 'c-source)
                                                             (typep source 'cc-source))))
                                                  sources)
                                       (if *variant-precise*
                                           (scraper-precise-headers configuration)
                                           (scraper-headers configuration))
                                       (list (make-source "config.h" :variant)
                                             (make-source "version.h" :variant)))
                       :implicit-inputs (list identifiers)
                       :identifiers (resolve-source identifiers)
                       :outputs outputs)
    (ninja:write-build output-stream :phony
                       :inputs outputs
                       :outputs '("tags"))))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :install-code)) source
     &aux (output (make-source (source-path source) :package-share)))
  (declare (ignore configuration))
  (ninja:write-build output-stream (if (shebangp (source-path source))
                                       :install-binary
                                       :install-file)
                     :inputs (list source)
                     :outputs (list output))
  (list :outputs output))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :install-code)) sources
     &key outputs &allow-other-keys)
  (declare (ignore configuration))
  (ninja:write-build output-stream :phony
                     :inputs (list* (make-source "include/trampoline.h" :package-share)
                                    (make-source "include/config.h" :package-share)
                                    (make-source "include/virtualMachine.h" :package-share)
                                    outputs)
                     :outputs (list "install_code")))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :install-extension-code)) source
     &aux (output (make-source (source-path source) :package-share)))
  (when (member :cando (extensions configuration))
    (ninja:write-build output-stream (if (shebangp (source-path source))
                                         :install-binary
                                         :install-file)
                       :inputs (list source)
                       :outputs (list output))
    (list :outputs output)))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :install-extension-code)) sources
     &key outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list "install_extension_code")))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :install-bin)) source
     &aux (output (make-source (file-namestring (source-path source)) :package-bin)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :install-binary
                     :inputs (list source)
                     :outputs (list output))
  (list :outputs output))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :install-bin)) sources
     &key outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list "install_bin")))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :vm-header)) sources
     &key &allow-other-keys
     &aux (header (make-source "virtualMachine.h" :variant-generated))
          (installed-header (make-source "include/virtualMachine.h" :package-share)))
  (ninja:write-build output-stream :generate-vm-header
                     :inputs sources
                     :outputs (list header))
  (when *variant-default*
    (ninja:write-build output-stream :install-file
                                :inputs (list header)
                                :outputs (list installed-header))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :libclasp))
     (source c-source))
  (let ((o (make-source-output source :type "o")))
    (ninja:write-build output-stream :cc
                       :variant-cflags *variant-cflags*
                       :inputs (list source)
                       :order-only-inputs (if *variant-precise*
                                              (scraper-precise-headers configuration)
                                              (scraper-headers configuration))
                       :outputs (list o))
    (list :objects o)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :libclasp))
     (source cc-source))
  (let ((pp (make-source-output source :type "pp"))
        (sif (make-source-output source :type "sif"))
        (o (make-source-output source :type "o")))
    (ninja:write-build output-stream :scrape-pp
                       :variant-cppflags *variant-cppflags*
                       :inputs (list source)
                       :order-only-inputs (list (make-source "virtualMachine.h" :variant-generated)
                                                (make-source "trampoline.h" :variant-generated))
                       :outputs (list pp))
    (ninja:write-build output-stream :generate-sif
                       :inputs (list pp)
                       :outputs (list sif))
    (ninja:write-build output-stream :cxx
                       :variant-cxxflags *variant-cxxflags*
                       :inputs (list source)
                       :order-only-inputs (list* (make-source "virtualMachine.h" :variant-generated)
                                                 (make-source "trampoline.h" :variant-generated)
                                                 (if *variant-precise*
                                                     (scraper-precise-headers configuration)
                                                     (scraper-headers configuration)))
                       :outputs (list o))
    (list :objects o
          :sifs sif)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source cc-source))
  (let ((o (make-source-output source :type "o")))
    (ninja:write-build output-stream :cxx
                       :variant-cxxflags *variant-cxxflags*
                       :inputs (list source)
                       :order-only-inputs (list* (make-source "virtualMachine.h" :variant-generated)
                                                 (make-source "trampoline.h" :variant-generated)
                                                 (if *variant-precise*
                                                     (scraper-precise-headers configuration)
                                                     (scraper-headers configuration)))
                       :outputs (list o))
    (list :objects o)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :libclasp))
     (source sif-source))
  (declare (ignore configuration name output-stream target))
  (list :sifs source))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :libclasp)) sources
     &key objects sifs &allow-other-keys
     &aux (generated (append (if *variant-precise*
                                 (scraper-precise-headers configuration)
                                 (scraper-headers configuration))
                             (scraper-lisp-sources configuration)))
          (products (mapcar (lambda (source)
                              (make-source (source-path source) :package-share))
                            generated))
          (libclasp-name (lib-filename configuration "libclasp" :dynamic t))
          (libclasp (make-source libclasp-name :variant-lib))
          (libclasp-installed (make-source libclasp-name (if (static-linking-p configuration)
                                                             :package-lib
                                                             :package-dylib)))
          (libclasp-pc-installed (make-source "libclasp.pc" :package-pkgconfig))
          (filtered-sifs (if *variant-precise*
                             (sort sifs
                                   (lambda (x y)
                                     (and (eq x :code)
                                          (eq y :variant)))
                                   :key #'source-root)
                             (remove-if (lambda (x)
                                          (eq :code (source-root x)))
                                        sifs))))
  (ninja:write-build output-stream :generate-headers
                     :inputs filtered-sifs
                     :precise (if *variant-precise* "1" "0")
                     :variant-path *variant-path*
                     :outputs generated)
  (ninja:write-build output-stream :phony
                     :outputs (list (build-name "generated"))
                     :inputs generated)
  (if (static-linking-p configuration)
      (ninja:write-build output-stream :ar
                         :inputs objects
                         :outputs (list libclasp))
      (ninja:write-build output-stream :link-lib
                         :variant-ldflags *variant-ldflags*
                         :variant-ldlibs *variant-ldlibs*
                         :libname libclasp-name
                         :inputs objects
                         :outputs (list libclasp)))
  (ninja:write-build output-stream :phony
                     :inputs (list libclasp)
                     :outputs (list (build-name target)))
  (when *variant-default*
    (loop for input in generated
          for output in products
          do (ninja:write-build output-stream :install-file
                                :inputs (list input)
                                :outputs (list output)))
    (ninja:write-build output-stream :install-file
                                :inputs (list (make-source "config.h" :variant))
                                :outputs (list (make-source "include/config.h" :package-share)))
    (ninja:write-build output-stream :install-file
                       :inputs (list libclasp)
                       :outputs (list libclasp-installed))
    (unless (static-linking-p configuration)
      (ninja:write-build output-stream :install-file
                         :inputs (list (make-source "libclasp.pc" :build))
                         :outputs (list libclasp-pc-installed)))
    (ninja:write-build output-stream :phony
                       :inputs (list* libclasp-installed
                                      (if (static-linking-p configuration)
                                          products
                                          (cons libclasp-pc-installed
                                                products)))
                       :outputs (list "install_lib"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp)) sources
     &key objects sifs &allow-other-keys
     &aux (exe (make-source "iclasp" :variant))
          (exe-installed (make-source "iclasp" :package-bin))
          (libiclasp-name (lib-filename configuration "libiclasp"))
          (libiclasp (make-source libiclasp-name :variant-lib))
          (libiclasp-installed (make-source libiclasp-name :package-lib))
          (libclasp (make-source (lib-filename configuration "libclasp" :dynamic t) :variant-lib))
          (symlink (make-source (if (member :cando (extensions configuration))
                                    "cando"
                                    "clasp")
                                :variant))
          (symlink-installed (make-source (if (member :cando (extensions configuration))
                                              "cando"
                                              "clasp")
                                          :package-bin))
          (clasp-sh (make-source "clasp" :variant))
          (clasp-sh-installed (make-source "clasp" :package-bin))
          (cleap-symlink (make-source "cleap" :variant))
          (cleap-symlink-installed (make-source "cleap" :package-bin)))
  (ninja:write-build output-stream :ar
                     :inputs objects
                     :outputs (list libiclasp))
  (ninja:write-build output-stream :link
                     :variant-ldflags *variant-ldflags*
                     :variant-ldlibs (concatenate 'string
                                                  (if (static-linking-p configuration)
                                                      #+darwin "-Wl,-all_load -lclasp -Wl,-noall_load "
                                                      #-darwin "-Wl,-whole-archive -lclasp -Wl,-no-whole-archive"
                                                      "-lclasp ")
                                                  *variant-ldlibs*)
                     :inputs objects
                     :order-only-inputs (list libclasp)
                     :outputs (list exe))
  (ninja:write-build output-stream :symbolic-link
                     :inputs (list exe)
                     :target (file-namestring (source-path exe))
                     :outputs (list symlink))
  (when (member :cando (extensions configuration))
    (ninja:write-build output-stream :symbolic-link
                       :inputs (list exe)
                       :target (file-namestring (source-path exe))
                       :outputs (list cleap-symlink)))
  (ninja:write-build output-stream :phony
                     :inputs (append (list exe symlink libiclasp)
                                     (when (member :cando (extensions configuration))
                                       (list cleap-symlink))
                                     (when (and *variant-default*
                                                (not (reproducible-build configuration))
                                                (or (etags configuration)
                                                    (ctags configuration)))
                                       (list "tags")))
                     :outputs (list (build-name target)))
  (when *variant-default*
    (ninja:write-build output-stream :install-binary
                       :inputs (list exe)
                       :outputs (list exe-installed))
    (ninja:write-build output-stream :install-file
                       :inputs (list libiclasp)
                       :outputs (list libiclasp-installed))
    (ninja:write-build output-stream :symbolic-link
                       :inputs (list exe-installed)
                       :target (file-namestring (source-path exe-installed))
                       :outputs (list symlink-installed))
    (when (member :cando (extensions configuration))
      (ninja:write-build output-stream :symbolic-link
                         :inputs (list exe-installed)
                         :target (file-namestring (source-path exe-installed))
                         :outputs (list cleap-symlink-installed))
      (ninja:write-build output-stream :install-binary
                                       :inputs (list clasp-sh)
                                       :outputs (list clasp-sh-installed)))
    (ninja:write-build output-stream :phony
                                     :inputs (append (list "install_code"
                                                           "install_extension_code"
                                                           "install_bin"
                                                           "install_lib"
                                                           libiclasp-installed
                                                           exe-installed
                                                           symlink-installed)
                                                     (when (member :cando (extensions configuration))
                                                       (list clasp-sh-installed cleap-symlink-installed)))
                                     :outputs (list "install_iclasp"))))

(defun make-kernel-source-list (configuration sources)
  (if (reproducible-build configuration)
      (format nil "~{\"~/ninja:escape/\" ~a~^ ~}"
              (mapcan (lambda (source)
                        (list (source-logical-namestring source)
                              (make-source-output source
                                                  :root (if (eq (source-root source) :variant-generated)
                                                            :install-generated
                                                            :install-share))))
                      sources))
      (format nil "~{\"~/ninja:escape/\"~^ ~}"
              (mapcar #'source-logical-namestring sources))))

(defun jupyter-kernel-path (configuration name &key system)
  (declare (ignore configuration))
  (merge-pathnames (make-pathname :directory (list :relative
                                                   "kernels"
                                                   name)
                                  :name "kernel"
                                  :type "json")
                   (if system
                       (root :package-jupyter)
                       #+darwin (merge-pathnames (make-pathname :directory '(:relative "Library" "Jupyter"))
                                                 (uiop:getenv-pathname "HOME" :ensure-directory t))
                       #-darwin (merge-pathnames (make-pathname :directory '(:relative "jupyter"))
                                                 (uiop:xdg-data-home)))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) (source lisp-source))
  (let* ((image (image-source configuration nil))
         (name (pathname-name (source-path source)))
         (module-name (format nil "modules/~a.~a"
                              name (fasl-extension configuration)))
         (output (make-source module-name :variant-lib))
         (install-output (make-source module-name :package-lib))
         (iclasp (make-source "iclasp" :variant))
         (clasp-with-env (wrap-with-env configuration iclasp)))
    (ninja:write-build output-stream :compile-module
                       :clasp clasp-with-env
                       :inputs (list source)
                       :implicit-inputs (list iclasp image)
                       :source (format nil "\"~/ninja:escape/\""
                                       (source-logical-namestring source))
                       :fasl (format nil "\"~/ninja:escape/\""
                                     (source-logical-namestring output))
                       :outputs (list output))
    (when *variant-default*
      (ninja:write-build output-stream :install-file
                         :inputs (list output)
                         :outputs (list install-output)))
    (list :outputs output
          :install-outputs install-output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (let* ((vimage (image-source configuration nil))
         (vimage-installed (image-source configuration nil :package-lib))
         (iclasp (make-source "iclasp" :variant))
         (clasp-with-env (wrap-with-env configuration iclasp)))
    (ninja:write-build output-stream :load-cclasp
                       :clasp clasp-with-env
                       :source (make-kernel-source-list configuration sources)
                       :inputs sources
                       :implicit-inputs (list iclasp
                                              (make-source "tools-for-build/character-names.sexp" :code))
                       :outputs (list (build-name "load_cclasp")))
    (ninja:write-build output-stream :compile-cclasp
                       :clasp clasp-with-env
                       :source (make-kernel-source-list configuration sources)
                       :inputs sources
                       :implicit-inputs (list iclasp
                                              (make-source "tools-for-build/character-names.sexp" :code))
                       :outputs (mapcar (lambda (x)
                                         (make-source-output x
                                                             :type (fasl-extension configuration)
                                                             :root (if (eq (source-root x) :variant-generated)
                                                                       :variant-lib-generated
                                                                       :variant-lib)))
                                       sources))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:bytecode :bytecode-faso :faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :clasp clasp-with-env
                       :target "cclasp"
                       :inputs (mapcar (lambda (x)
                                         (make-source-output x
                                                             :type (fasl-extension configuration)
                                                             :root (if (eq (source-root x) :variant-generated)
                                                                       :variant-lib-generated
                                                                       :variant-lib)))
                                       sources)
                       :implicit-inputs (list iclasp)
                       :outputs (list vimage))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "iclasp")
                                     vimage
                                     (build-name "modules"))
                       :outputs (list (build-name "cclasp")))
    (when (jupyter configuration)
      (let ((kernels (loop for name in (if (member :cando (extensions configuration))
                                           (list "clasp" "cando")
                                           (list "clasp"))
                           for clasp = (make-source name :variant)
                           for build-name = (build-name name)
                           for output = (jupyter-kernel-path configuration
                                                             (format nil "~a_~a"
                                                                     (if (equal name "clasp")
                                                                         "common-lisp"
                                                                         "cando")
                                                                     build-name))
                           do (ninja:write-build output-stream :jupyter-user-kernel
                                                 :outputs (list output)
                                                 :inputs (list (build-name "cclasp"))
                                                 :variant-path *variant-path*
                                                 :name build-name
                                                 :bin-path clasp
                                                 :load-system 1
                                                 :clasp clasp-with-env)
                           collect output)))
        (ninja:write-build output-stream :phony
                           :inputs kernels
                           :outputs (list (build-name "jupyter_cclasp")))
        (unless (eq :sclasp (default-stage configuration))
          (ninja:write-build output-stream :phony
                             :inputs (list (build-name "jupyter_cclasp"))
                             :outputs (list (build-name "jupyter"))))))
    (when *variant-default*
      (let ((kernels (when (jupyter configuration)
                       (loop with system = (not (uiop:subpathp (share-path configuration)
                                                               (uiop:getenv-absolute-directory "HOME")))
                             for name in (if (member :cando (extensions configuration))
                                             (list "clasp" "cando")
                                             (list "clasp"))
                             for clasp = (make-source name :variant)
                             for output = (jupyter-kernel-path configuration
                                                               (if (equal name "clasp")
                                                                   "common-lisp_clasp"
                                                                   "cando_cando")
                                                               :system system)
                             do (ninja:write-build output-stream (if system
                                                                     :jupyter-system-kernel
                                                                     :jupyter-user-kernel)
                                                   :outputs (list output)
                                                   :inputs (list (build-name "cclasp"))
                                                   :name name
                                                   :variant-path *variant-path*
                                                   :bin-path name
                                                   :load-system 1
                                                   :clasp clasp-with-env)
                             collect output))))
        (ninja:write-build output-stream :install-file
                           :inputs (list vimage)
                           :outputs (list vimage-installed))
        (ninja:write-build output-stream :phony
                           :inputs (list* "install_iclasp"
                                          "install_modules"
                                          vimage-installed
                                          kernels)
                           :outputs (list "install_cclasp"))))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) sources
     &key outputs install-outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list (build-name "modules")))
  (when *variant-default*
    (ninja:write-build output-stream :phony
                       :inputs install-outputs
                       :outputs (list "install_modules"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :eclasp)) sources
     &key &allow-other-keys)
  (when (extensions configuration)
    (let* ((cimage (image-source configuration nil))
          (eimage (image-source configuration t))
          (eimage-installed (image-source configuration t :package-lib))
          (iclasp (make-source "iclasp" :variant))
           (clasp-with-env (wrap-with-env configuration iclasp))
          (eclasp-sources (member #P"src/lisp/kernel/stage/extension/0-begin.lisp" sources :key #'source-path :test #'equal)))
      (ninja:write-build output-stream :load-eclasp
                         :clasp clasp-with-env
                         :source (make-kernel-source-list configuration sources)
                         :inputs sources
                         :position (- (length sources) (length eclasp-sources))
                         :source (make-kernel-source-list configuration eclasp-sources)
                         :inputs eclasp-sources
                         :implicit-inputs (list iclasp cimage (build-name "cclasp")
                                                (make-source "tools-for-build/character-names.sexp" :code))
                         :outputs (list (build-name "load_eclasp")))
      (ninja:write-build output-stream :compile-eclasp
                         :clasp clasp-with-env
                         :image cimage
                         :position (- (length sources) (length eclasp-sources))
                         :source (make-kernel-source-list configuration eclasp-sources)
                         :inputs eclasp-sources
                         :implicit-inputs (list iclasp cimage (build-name "cclasp")
                                                (make-source "tools-for-build/character-names.sexp" :code))
                         :outputs (mapcar (lambda (x)
                                            (make-source-output x
                                                                :type (fasl-extension configuration)
                                                                :root (if (eq (source-root x) :variant-generated)
                                                                          :variant-lib-generated
                                                                          :variant-lib)))
                                          eclasp-sources))
      (ninja:write-build output-stream (case (build-mode configuration)
                                         ((:bytecode :bytecode-faso :faso :fasoll :fasobc) :link-fasl)
                                         (otherwise "link-fasl-abc"))
                         :variant-ldflags *variant-ldflags*
                         :variant-ldlibs *variant-ldlibs*
                         :clasp clasp-with-env
                         :target "eclasp"
                         :inputs (mapcar (lambda (x)
                                           (make-source-output x
                                                               :type (fasl-extension configuration)
                                                               :root (if (eq (source-root x) :variant-generated)
                                                                         :variant-lib-generated
                                                                         :variant-lib)))
                                         sources)
                         :implicit-inputs (list iclasp)
                         :outputs (list eimage))
      (ninja:write-build output-stream :phony
                         :inputs (list eimage (build-name "cclasp"))
                         :outputs (list (build-name "eclasp")))
      (when *variant-default*
        (ninja:write-build output-stream :install-file
                           :inputs (list eimage)
                           :outputs (list eimage-installed))
        (ninja:write-build output-stream :phony
                           :inputs (list "install_cclasp"
                                          eimage-installed)
                           :outputs (list "install_eclasp"))))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :regression-tests)) sources
     &key &allow-other-keys
     &aux (clasp (wrap-with-env configuration (make-source "iclasp" :variant))))
  (ninja:write-build output-stream :regression-tests
                     :clasp clasp
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "test")))
  (ninja:write-build output-stream :bench
                     :clasp clasp
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "bench")))
  (ninja:write-build output-stream :ansi-test
                     :clasp clasp
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "ansi-test")))
  (ninja:write-build output-stream :asdf-test
                     :clasp clasp
                     :target "t"
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "asdf-test")))
  (ninja:write-build output-stream :asdf-test
                     :clasp clasp
                     :target "u"
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "asdf-test-upgrade")))
  (ninja:write-build output-stream :test-random-integer
                     :clasp clasp
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "test-random-integer")))
  (when (member :cando (extensions configuration))
    (ninja:write-build output-stream :cando-regression-tests
                       :clasp clasp
                       :inputs (list (build-name "eclasp"))
                       :outputs (list (build-name "cando-test"))))
  (when *variant-default*
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "test"))
                       :outputs (list "test"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "bench"))
                       :outputs (list "bench"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "ansi-test"))
                       :outputs (list "ansi-test"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "ansi-test-subset"))
                       :outputs (list "ansi-test-subset"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "asdf-test"))
                       :outputs (list "asdf-test"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "asdf-test-upgrade"))
                       :outputs (list "asdf-test-upgrade"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "test-random-integer"))
                       :outputs (list "test-random-integer"))
    (when (member :cando (extensions configuration))
      (ninja:write-build output-stream :phony
                         :inputs (list (build-name "cando-test"))
                         :outputs (list "cando-test")))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :analyzer)) sources
     &key &allow-other-keys)
  (ninja:write-build output-stream :compile-systems
                     :clasp (wrap-with-env configuration (make-source "iclasp" :variant))
                     :inputs sources
                     :implicit-inputs (list (build-name "cclasp"))
                     :systems "clasp-analyzer"
                     :outputs (list (make-source "analyzer.stub" :variant-lib))))                                     

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :analyze))
     (source c-source)
     &aux (output (make-source-output source :type "sa"))
          (database (make-source (format nil "boehm~:[~;-d~]/compile_commands.json"
                                         *variant-debug*)
                                 :build)))
  (declare (ignore configuration name target))
  (unless (or *variant-prep* *variant-precise* *variant-debug*)
    (ninja:write-build output-stream :analyze-file
                       :clasp (wrap-with-env configuration (make-source "iclasp" :variant))
                       :inputs (list source)
                       :implicit-inputs (list (build-name "cclasp")
                                              (build-name "generated" :gc :boehm)
                                              database
                                              (make-source "analyzer.stub" :variant-lib))                                    
                       :database database
                       :log (make-source-output source :type "log")
                       :outputs (list output))
    (list :outputs output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :analyze)) sources
     &key outputs &allow-other-keys
     &aux (sif (make-source (if (member :cando (extensions configuration))
                                             "src/analysis/clasp_gc_cando.sif"
                                             "src/analysis/clasp_gc.sif")
                                         :code)))              
  (unless (or *variant-prep* *variant-precise* *variant-debug*)
    (ninja:write-build output-stream :analyze-generate
                       :clasp (wrap-with-env configuration (make-source "iclasp" :variant))
                       :inputs outputs
                       :implicit-inputs (list (build-name "cclasp")
                                              (build-name "generated" :gc :boehm)
                                              (make-source "analyzer.stub" :variant-lib))                                    
                       :sif sif
                       :outputs (list (build-name "analyze")))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "analyze"))
                       :outputs (list "analyze"))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :sclasp))
     (source c-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :sclasp))
     (source cc-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :sclasp)) sources
     &key objects &allow-other-keys
     &aux (cclasp (build-name (if (extensions configuration) :eclasp :cclasp)))
          (iclasp (make-source "iclasp" :variant))
          (clasp-with-env (wrap-with-env configuration iclasp)))
  (declare (ignore objects))
  (flet ((snapshot (name &key ignore-extensions)
           (let* ((executable (make-source (build-name name) :variant))
                  (symlink (make-source name :variant))
                  (symlink-installed (make-source name :package-bin))
                  (scleap-symlink (make-source "scleap" :variant))
                  (scleap-symlink-installed (make-source "scleap" :package-bin))
                  (installed (make-source (build-name name) :package-bin))
                  (build-outputs (list executable symlink))
                  (install-outputs (list installed symlink-installed))
                  (system (not (uiop:subpathp (share-path configuration)
                                              (uiop:getenv-absolute-directory "HOME"))))
                  (kernel (jupyter-kernel-path configuration
                                               (if (equal name "sclasp")
                                                   "common-lisp_sclasp"
                                                   "cando_scando")
                                               :system system))
                  (user-kernel (jupyter-kernel-path configuration
                                                    (format nil "~a_~a"
                                                            (if (equal name "sclasp")
                                                                "common-lisp"
                                                                "cando")
                                                            (build-name name)))))
             (ninja:write-build output-stream :make-snapshot
                                :clasp clasp-with-env
                                :arguments (when ignore-extensions
                                             "--base --feature ignore-extensions")
                                :variant-path *variant-path*
                                :inputs (list cclasp)
                                :outputs (list executable))
             (ninja:write-build output-stream :symbolic-link
                                :inputs (list executable)
                                :target (file-namestring (source-path executable))
                                :outputs (list symlink))
             (when (equal name "scando")
               (ninja:write-build output-stream :symbolic-link
                                  :inputs (list executable)
                                  :target (file-namestring (source-path executable))
                                  :outputs (list scleap-symlink))
               (push scleap-symlink build-outputs))
             (when (jupyter configuration)
               (ninja:write-build output-stream :jupyter-user-kernel
                                                :outputs (list user-kernel)
                                                :inputs (list executable)
                                                :name (build-name name)
                                                :variant-path *variant-path*
                                                :bin-path symlink
                                                :load-system 0
                                                :clasp executable))
             (when *variant-default*
               (ninja:write-build output-stream :install-binary
                                  :inputs (list executable)
                                  :outputs (list installed))
               (ninja:write-build output-stream :symbolic-link
                                  :inputs (list installed)
                                  :target (file-namestring (source-path installed))
                                  :outputs (list symlink-installed))
               (when (equal name "scando")
                 (ninja:write-build output-stream :symbolic-link
                                    :inputs (list installed)
                                    :target (file-namestring (source-path executable))
                                    :outputs (list scleap-symlink-installed))
                 (push scleap-symlink-installed install-outputs))
               (when (jupyter configuration)
                 (ninja:write-build output-stream (if system
                                                      :jupyter-system-kernel
                                                      :jupyter-user-kernel)
                                    :outputs (list kernel)
                                    :implicit-inputs (list executable)
                                    :name name
                                    :bin-path name
                                    :variant-path *variant-path*
                                    :load-system 0
                                    :clasp executable)
                 (push kernel install-outputs)))
             (list build-outputs install-outputs user-kernel))))
    (let ((outputs (if (member :cando (extensions configuration))
                       (list (snapshot "scando")
                             (snapshot "sclasp" :ignore-extensions t))
                       (list (snapshot "sclasp")))))
      (ninja:write-build output-stream :phony
                         :inputs (list* (build-name :cclasp)
                                        (mapcan #'first outputs))
                         :outputs (list (build-name :sclasp)))
      (when (jupyter configuration)
        (ninja:write-build output-stream :phony
                           :inputs (mapcar #'third outputs)
                           :outputs (list (build-name "jupyter_sclasp")))
        (when (eq :sclasp (default-stage configuration))
          (ninja:write-build output-stream :phony
                             :inputs (list (build-name "jupyter_cclasp")
                                           (build-name "jupyter_sclasp"))
                             :outputs (list (build-name "jupyter")))))
      (when *variant-default*
        (ninja:write-build output-stream :phony
                           :inputs (list* (if (extensions configuration)
                                              "install_eclasp"
                                              "install_cclasp")
                                          (mapcan #'second outputs))
                           :outputs (list "install_sclasp"))))))

(defmethod print-epilogue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-build output-stream :update-unicode
                     :source (format nil "~a ~a" 
                                     (make-source "src/core/character-generated.cc" :code)
                                     (make-source "tools-for-build/character-names.sexp" :code))
                     :outputs (list "update-unicode"))
  (ninja:write-build output-stream :phony
                     :inputs (list (format nil "install_~(~a~)" (default-stage configuration)))
                     :outputs (list "install"))
  (ninja:write-default output-stream (default-target configuration)))
  
