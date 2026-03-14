(in-package #:koga)

(defmethod make-output-stream (configuration (name (eql :ninja)) path)
  (declare (ignore configuration name))
  (ninja:make-line-wrapping-stream (ninja:make-timestamp-preserving-stream path)))

(defun wrap-with-env (configuration executable-name)
  (if (reproducible-build configuration)
      (format nil
              #+darwin "DYLD_LIBRARY_PATH=~a ~a"
              #-darwin "LD_LIBRARY_PATH=~a ~a"
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
                        :gtags (gtags configuration)
                        :tags (or (ctags configuration)
                                  (etags configuration)))
  (terpri output-stream)
  (ninja:write-rule output-stream :gtags
                    :command "$gtags -C ../"
                    :restat 1
                    :description "Creating gtags")
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
  (ninja:write-rule output-stream :generate-lisp-info
                    :command "$clasp -n -- $out <generate-lisp-info.lisp >/dev/null"
                    :description "Generating info from Clasp runtime"
                    :restat 1)
  (ninja:write-rule output-stream :generate-encodings
                    :command (lisp-command "generate-encodings.lisp" "$out $in")
                    :description "Generating character encoding tables"
                    :restat 1)
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
                    :command #+darwin (if (reproducible-build configuration)
                                          "$cxx -dynamiclib $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                                          "$cxx -dynamiclib $variant-ldflags $ldflags -install_name @rpath/$libname -o$out $in $variant-ldlibs $ldlibs")
                             #-darwin "$cxx -shared $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :compile-bytecode-image
                    :command (lisp-command "compile-bytecode-image.lisp" "$in --output $out --sources $sources")
                    :description "Building Clasp bytecode image"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-native-image
                    :command "$clasp --norc  --non-interactive --disable-mpi --feature ignore-extensions --image $image --load compile-native-image.lisp --quit -- $in --output $out --sources $sources --cfasls $cfasls"
                    :description "Compiling Clasp native image"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-lisp
                    :command "$clasp --norc --non-interactive --disable-mpi --feature ignore-extension-systems --image $image --load compile-lisp.lisp --quit -- $in --output $out --sources $sources")
  (when (extensions configuration)
    (ninja:write-rule output-stream :snapshot-extension
                      :command "$clasp --norc --disable-mpi --base --feature ignore-extension-systems --load snapshot-clasp.lisp -- $out extension $position $source"
                      :description "Snapshot extension"
                      :pool "console")
    (ninja:write-rule output-stream :compile-extension
                      :command "$clasp --norc --disable-mpi --base --feature ignore-extension-systems --load compile-clasp.lisp -- extension $position $source"
                      :description "Compiling extension"
                      :restat 1
                      :pool "console"))
  (ninja:write-rule output-stream :compile-module
                    :command "$clasp --non-interactive --norc --disable-mpi --image $image --feature ignore-extensions --load compile-module.lisp -- $fasl $source"
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
  (ninja:write-rule output-stream :link-image
                    :command (lisp-command "link-image.lisp" "$out $in")
                    :restat 1
                    :description "Linking $out")
  (ninja:write-rule output-stream "link-fasl-abc"
                    :command "$cxx $variant-ldflags $ldflags $ldflags-fasl -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
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
       (tag-output (make-source "TAGS" :code))
       (gtag-output (make-source "GTAGS" :code))
       (outputs nil))
  (when (and *variant-default*
             (not (reproducible-build configuration))
             (or (etags configuration)
                 (ctags configuration)
                 (gtags configuration)))
    (when (or (etags configuration)
              (ctags configuration))
      (push tag-output outputs)
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
                         :outputs (list tag-output)))
    (when (gtags configuration)
      (push gtag-output outputs)
      (ninja:write-build output-stream :gtags
                         :order-only-inputs (append (remove-if (lambda (source)
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
                         :outputs (list gtag-output)))
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

(defun source-fasl (source &key (type "fasl"))
  (make-source-output source
                      :type type
                      :root (if (eq (source-root source) :variant-generated)
                                :variant-lib-generated
                                :variant-lib)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) (source lisp-source))
  (let* ((image (image-source configuration :mode :bytecode))
         (name (pathname-name (source-path source)))
         (module-name (format nil "modules/~a.~a"
                              name (fasl-extension (build-mode configuration))))
         (output (make-source module-name :variant-lib))
         (install-output (make-source module-name :package-lib))
         (iclasp (make-source "iclasp" :variant))
         (clasp-with-env (wrap-with-env configuration iclasp)))
    (ninja:write-build output-stream :compile-module
                       :clasp clasp-with-env
                       :image (image-source configuration :mode :bytecode)
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
    (configuration (name (eql :ninja)) output-stream (target (eql :base)) sources
     &key &allow-other-keys)
  (let* ((features.sexp (make-source "features.sexp" :variant-generated))
         (runtime-packages.lisp (make-source "runtime-packages.lisp"
                                             :variant-generated))
         (cxx-classes.lisp (make-source "cxx-classes.lisp" :variant-generated))
         (runtime-functions.lisp (make-source "runtime-functions.lisp"
                                              :variant-generated))
         (runtime-variables.lisp (make-source "runtime-variables.lisp"
                                              :variant-generated))
         (runtime-info.lisp (make-source "runtime-info.lisp" :variant-generated))
         (type-map.lisp (make-source "type-map.lisp" :variant-generated))
         (fli-specs.lisp (make-source "fli-specs.lisp" :variant-generated))
         (generated-encodings.lisp
           (make-source "generated-encodings.lisp" :variant-generated))
         (bytecode-image (image-source configuration :mode :bytecode))
         (native-image (image-source configuration :mode :native))
         (image (image-source configuration))
         (image-installed (image-source configuration :root :package-lib))
         (iclasp (make-source "iclasp" :variant))
         (clasp-with-env (wrap-with-env configuration iclasp))
         (fasls (mapcar #'source-fasl sources))
         (outputs ; interleaved fasls and cfasls
           (loop for source in sources
                 collect (source-fasl source)
                 collect (source-fasl source :type "cfasl"))))
    (ninja:write-build output-stream :generate-lisp-info
                       :outputs (list features.sexp runtime-packages.lisp
                                      cxx-classes.lisp runtime-functions.lisp
                                      runtime-variables.lisp runtime-info.lisp
                                      type-map.lisp fli-specs.lisp)
                       :clasp clasp-with-env
                       :implicit-inputs (list iclasp))
    (ninja:write-build output-stream :generate-encodings
                       :inputs (list (make-source "tools-for-build/encodingdata.txt" :code))
                       :outputs (list generated-encodings.lisp))
    (ninja:write-build output-stream :compile-bytecode-image
                       :inputs (list* (make-source "tools-for-build/character-names.sexp"
                                                   :code)
                                      features.sexp
                                      sources)
                       :sources (make-kernel-source-list
                                   configuration sources)
                       :outputs outputs)
    (ninja:write-build output-stream :link-image
                       :inputs fasls
                       :outputs (list bytecode-image))
    (when (eq (build-mode configuration) :native)
      (let ((nfasls (loop for source in sources
                          collect (source-fasl source :type "nfasl")))
            (cfasls
              (format nil "~{\"~/ninja:escape/\"~^ ~}"
                      (loop for source in sources
                            collect (source-fasl source
                                                 :type "cfasl")))))
        (ninja:write-build output-stream :compile-native-image
                           :clasp clasp-with-env
                           :source (make-kernel-source-list configuration sources)
                           :inputs (list* (make-source "tools-for-build/character-names.sexp"
                                                       :code)
                                          features.sexp
                                          sources)
                           :sources (make-kernel-source-list configuration sources)
                           :cfasls cfasls
                           :implicit-inputs (list iclasp bytecode-image
                                                  (build-name "modules"))
                           :image bytecode-image
                           :outputs nfasls)
        (ninja:write-build output-stream :link-image
                           :clasp clasp-with-env
                           :outputs (list native-image)
                           :inputs nfasls)))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "iclasp")
                                     image
                                     (build-name "modules"))
                       :outputs (list (build-name "base")))
    (when *variant-default*
      (ninja:write-build output-stream :install-file
                         :inputs (list image)
                         :outputs (list image-installed))
      (ninja:write-build output-stream :phony
                         :inputs (list "install_iclasp"
                                       "install_modules"
                                       image-installed)
                         :outputs (list "install_base")))))

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
    (configuration (name (eql :ninja)) output-stream (target (eql :extension)) sources
     &key &allow-other-keys)
  (when (extensions configuration)
    (let* ((bytecode-base-image (image-source configuration :mode :bytecode))
           (base-image (image-source configuration))
           (extension-image (image-source configuration :extension t))
           (extension-image-installed (image-source configuration :extension t :root :package-lib))
           (iclasp (make-source "iclasp" :variant))
           (clasp-with-env (wrap-with-env configuration iclasp))
           (fasls (loop for source in sources
                        when (typep source 'lisp-source)
                          collect (source-fasl source
                                               :type (fasl-extension (build-mode configuration)))))
           (extension-sources (loop with extension = nil
                                    for source in sources
                                    when (and extension (typep source 'lisp-source))
                                      collect source
                                    when (typep source 'mark-source)
                                      do (setf extension t)))
           (extension-fasls (mapcar (lambda (x)
                                      (source-fasl x :type (fasl-extension (build-mode configuration))))
                                    extension-sources)))
      (ninja:write-build output-stream :compile-lisp
                         :clasp clasp-with-env
                         :image bytecode-base-image
                         :inputs extension-sources
                         :sources (make-kernel-source-list configuration extension-sources)
                         :implicit-inputs (list iclasp bytecode-base-image)
                         :outputs extension-fasls)
      (ninja:write-build output-stream :link-image
                         :inputs (list* base-image fasls)
                         :implicit-inputs (list iclasp)
                         :outputs (list extension-image))
      (ninja:write-build output-stream :phony
                         :inputs (list extension-image (build-name "base"))
                         :outputs (list (build-name "extension")))
      (when *variant-default*
        (ninja:write-build output-stream :install-file
                           :inputs (list extension-image)
                           :outputs (list extension-image-installed))
        (ninja:write-build output-stream :phony
                           :inputs (list "install_base"
                                         extension-image-installed)
                           :outputs (list "install_extension"))))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :regression-tests)) sources
     &key &allow-other-keys
     &aux (clasp (wrap-with-env configuration (make-source "iclasp" :variant))))
  (ninja:write-build output-stream :regression-tests
                     :clasp clasp
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "test")))
  (ninja:write-build output-stream :bench
                     :clasp clasp
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "bench")))
  (ninja:write-build output-stream :ansi-test
                     :clasp clasp
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "ansi-test")))
  (ninja:write-build output-stream :asdf-test
                     :clasp clasp
                     :target "t"
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "asdf-test")))
  (ninja:write-build output-stream :asdf-test
                     :clasp clasp
                     :target "u"
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "asdf-test-upgrade")))
  (ninja:write-build output-stream :test-random-integer
                     :clasp clasp
                     :inputs (list (build-name "base"))
                     :outputs (list (build-name "test-random-integer")))
  (when (member :cando (extensions configuration))
    (ninja:write-build output-stream :cando-regression-tests
                       :clasp clasp
                       :inputs (list (build-name "extension"))
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
                     :implicit-inputs (list (build-name "base"))
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
                       :implicit-inputs (list (build-name "base")
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
                       :implicit-inputs (list (build-name "base")
                                              (build-name "generated" :gc :boehm)
                                              (make-source "analyzer.stub" :variant-lib))                                    
                       :sif sif
                       :outputs (list (build-name "analyze")))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "analyze"))
                       :outputs (list "analyze"))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :snapshot))
     (source c-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :snapshot))
     (source cc-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :snapshot)) sources
     &key objects &allow-other-keys
     &aux (image (build-name (if (extensions configuration) :extension :base)))
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
                                              (uiop:getenv-absolute-directory "HOME")))))
             (ninja:write-build output-stream :make-snapshot
                                :clasp clasp-with-env
                                :arguments (when ignore-extensions
                                             "--base --feature ignore-extensions")
                                :variant-path *variant-path*
                                :inputs (list image)
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
                 (push scleap-symlink-installed install-outputs)))
             (list build-outputs install-outputs))))
    (let ((outputs (if (member :cando (extensions configuration))
                       (list (snapshot "scando")
                             (snapshot "snapshot" :ignore-extensions t))
                       (list (snapshot "snapshot")))))
      (ninja:write-build output-stream :phony
                         :inputs (list* (build-name :base)
                                        (mapcan #'first outputs))
                         :outputs (list (build-name :snapshot)))
      (when *variant-default*
        (ninja:write-build output-stream :phony
                           :inputs (list* (if (extensions configuration)
                                              "install_extension"
                                              "install_base")
                                          (mapcan #'second outputs))
                           :outputs (list "install_snapshot"))))))

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
  
