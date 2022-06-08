(in-package #:koga)

(defmethod make-output-stream (configuration (name (eql :ninja)) path)
  (declare (ignore configuration name))
  (ninja:make-line-wrapping-stream (ninja:make-timestamp-preserving-stream path)))

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
                        :ldflags (ldflags configuration)
                        :ldlibs (ldlibs configuration)
                        :lisp "sbcl" ;(first (uiop:raw-command-line-arguments))
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
                    :command "$lisp --script generate-sif.lisp $out $in"
                    :restat 1
                    :description "Scraping $in")
  (ninja:write-rule output-stream :generate-headers
                    :command "$lisp --script generate-headers.lisp $precise $variant-path $in"
                    :restat 1
                    :description "Creating headers from sif files")
  (ninja:write-rule output-stream :static-analyzer
                    :command "$clasp --norc --non-interactive --feature ignore-extensions --load ${variant-path}static-analyzer.lisp -- $sif $in"
                    :description "Analyzing clasp"
                    :restat 1
                    :pool "console")
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
  (ninja:write-rule output-stream :link
                    :command "$cxx $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :bc
                    :command "$cxx $variant-cxxflags $cxxflags -emit-llvm -c -MD -MF $out.d -o$out $in"
                    :description "Creating bitcode for $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :run-aclasp
                    :command "$clasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --feature jit-log-symbols --load run-aclasp.lisp -- $source"
                    :description "Compiling aclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-aclasp
                    :command "$clasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load compile-aclasp.lisp -- $source"
                    :description "Compiling aclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-bclasp
                    :command "$clasp --norc --type image --disable-mpi --image $image --load compile-bclasp.lisp -- $source"
                    :description "Compiling bclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-cclasp
                    :command "$clasp --norc --type image --disable-mpi --image $image --load compile-cclasp.lisp -- $source"
                    :description "Compiling cclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-module
                    :command "$clasp --non-interactive --norc --type image --disable-mpi --image $image --feature ignore-extensions --load compile-module.lisp -- $out $in"
                    :description "Compiling module $in")
  (ninja:write-rule output-stream :regression-tests
                    :command "$clasp --norc --non-interactive --feature ignore-extensions --load \"sys:src;lisp;regression-tests;run-all.lisp\""
                    :description "Running regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :ansi-test
                    :command "$clasp --norc --feature ignore-extensions --load \"../dependencies/ansi-test/doit-clasp.lsp\""
                    :description "Running ANSI tests"
                    :pool "console")
  (ninja:write-rule output-stream :test-random-integer
                    :command "$clasp --norc --feature ignore-extensions --load \"../dependencies/ansi-test/run-random-type-tests.lisp\""
                    :description "Running pfdietz test-random-integer-forms"
                    :pool "console")
  (ninja:write-rule output-stream :link-fasl
                    :command "$clasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load link-fasl.lisp -- $out $in"
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
                    :command "$lisp --script update-unicode.lisp $source"
                    :description "Updating unicode tables"))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :tags)) sources
     &key &allow-other-keys
     &aux (identifiers (make-source ".identifiers" :code))
          (outputs (list (make-source "TAGS" :code))))
  (when (and *variant-default*
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
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list "install_code")))

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

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
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
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source cc-source))
  (let ((pp (make-source-output source :type "pp"))
        (sif (make-source-output source :type "sif"))
        (o (make-source-output source :type "o")))
    (ninja:write-build output-stream :scrape-pp
                       :variant-cppflags *variant-cppflags*
                       :inputs (list source)
                       :outputs (list pp))
    (ninja:write-build output-stream :generate-sif
                       :inputs (list pp)
                       :outputs (list sif))
    (ninja:write-build output-stream :cxx
                       :variant-cxxflags *variant-cxxflags*
                       :inputs (list source)
                       :order-only-inputs (if *variant-precise*
                                              (scraper-precise-headers configuration)
                                              (scraper-headers configuration))
                       :outputs (list o))
    (list :objects o
          :sifs sif)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source sif-source))
  (declare (ignore configuration name output-stream target))
  (list :sifs source))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp)) sources
     &key objects sifs &allow-other-keys
     &aux (generated (append (if *variant-precise*
                                 (scraper-precise-headers configuration)
                                 (scraper-headers configuration))
                             (scraper-lisp-sources configuration)))
          (products (mapcar (lambda (source)
                              (make-source (source-path source) :package-share))
                            generated))
          (exe (make-source (build-name target) :variant))
          (exe-installed (make-source (build-name target) :package-bin))
          (lib (make-source "libclasp.a" :variant-lib))
          (lib-installed (make-source "libclasp.a" :package-lib))
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
          (cleap-symlink-installed (make-source "cleap" :package-bin))
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
  (ninja:write-build output-stream :ar
                     :inputs objects
                     :outputs (list lib))
  (ninja:write-build output-stream :link
                     :variant-ldflags *variant-ldflags*
                     :variant-ldlibs *variant-ldlibs*
                     :inputs objects
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
                     :inputs (append (list exe symlink lib)
                                     (when (member :cando (extensions configuration))
                                       (list cleap-symlink))
                                     (when (and *variant-default*
                                                (or (etags configuration)
                                                    (ctags configuration)))
                                       (list "tags")))
                     :outputs (list (build-name target)))
  (when *variant-default*
    (loop for input in generated
          for output in products
          do (ninja:write-build output-stream :install-file
                                :inputs (list input)
                                :outputs (list output)))
    (ninja:write-build output-stream :install-binary
                       :inputs (list exe)
                       :outputs (list exe-installed))
    (ninja:write-build output-stream :install-file
                       :inputs (list lib)
                       :outputs (list lib-installed))
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
                                     :inputs (append (list "install_bitcode"
                                                           "install_code"
                                                           "install_bin"
                                                           exe-installed
                                                           lib-installed
                                                           symlink-installed)
                                                     (when (member :cando (extensions configuration))
                                                       (list clasp-sh-installed cleap-symlink-installed))
                                                     products)
                                     :outputs (list "install_iclasp"))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream
     (target (eql :bitcode)) (source cc-source)
     &aux (bitcode-name (format nil "~a-~a-cxx.bc" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (bitcode-output (make-source bitcode-name :variant-bitcode))
          (bitcode-install (make-source bitcode-name :package-bitcode))
          (bitcode-nd-name (format nil "~a-~a-no-debug-info-cxx.bc" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (bitcode-nd-output (make-source bitcode-nd-name :variant-bitcode))
          (bitcode-nd-install (make-source bitcode-nd-name :package-bitcode))
          (object (make-source-output source :type "o"))
          (archive-name (format nil "~a-~a-cxx.a" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (archive-output (make-source archive-name :variant-lib))
          (archive-install (make-source archive-name :package-lib))
          (object-nd (make-source (make-pathname :directory (pathname-directory (source-path source))
                                                 :name (concatenate 'string
                                                                    (pathname-name (source-path source))
                                                                    "-no-debug-info")
                                                 :type "o")
                                  :variant))
          (archive-nd-name (format nil "~a-~a-no-debug-info-cxx.a" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (archive-nd-output (make-source archive-nd-name :variant-lib))
          (archive-nd-install (make-source archive-nd-name :package-lib))
          (headers (if *variant-precise*
                       (scraper-precise-headers configuration)
                       (scraper-headers configuration)))
          (no-debug-flags (remove-flag "-g" *variant-cxxflags*)))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags *variant-cxxflags*
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list bitcode-output))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags no-debug-flags
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list bitcode-nd-output))
  (ninja:write-build output-stream :ar
                     :inputs (list object)
                     :outputs (list archive-output))
  (ninja:write-build output-stream :cxx
                     :variant-cxxflags no-debug-flags
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list object-nd))
  (ninja:write-build output-stream :ar
                     :inputs (list object-nd)
                     :outputs (list archive-nd-output))
  (when *variant-default*
    (ninja:write-build output-stream :install-file
                       :inputs (list bitcode-output)
                       :outputs (list bitcode-install))
    (ninja:write-build output-stream :install-file
                       :inputs (list bitcode-nd-output)
                       :outputs (list bitcode-nd-install))
    (ninja:write-build output-stream :install-file
                       :inputs (list archive-output)
                       :outputs (list archive-install))
    (ninja:write-build output-stream :install-file
                       :inputs (list archive-nd-output)
                       :outputs (list archive-nd-install)))
  (list :install-outputs (list bitcode-install bitcode-nd-install
                               archive-install archive-nd-install)
        :outputs (list bitcode-output bitcode-nd-output
                       archive-output archive-nd-output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream
     (target (eql :bitcode)) sources
     &key install-outputs outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list (build-name "bitcode")))
  (when *variant-default*
    (ninja:write-build output-stream :phony
                       :inputs install-outputs
                       :outputs (list "install_bitcode"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream
     (target (eql :aclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (iclasp (make-source (build-name :iclasp) :variant))
        (aimage-installed (image-source configuration :aclasp :package-fasl))
        (*root-paths* (list* :variant-stage-bitcode (merge-pathnames (make-pathname :directory (list :relative
                                                                                                     (format nil "aclasp-~a-bitcode"
                                                                                                                 *variant-name*)))
                                                                     (root :variant-lib))
                             *root-paths*)))
    (ninja:write-build output-stream :run-aclasp
                       :clasp iclasp
                       :source (format nil "~{~/ninja:escape/~^ ~}"
                                       (mapcar (lambda (source)
                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                  (source-path source)))
                                               sources))
                       :inputs sources
                       :implicit-inputs (list iclasp
                                              (build-name "bitcode"))
                       :outputs (list (build-name "rclasp")))
    (ninja:write-build output-stream :compile-aclasp
                       :clasp iclasp
                       :source (format nil "~{~/ninja:escape/~^ ~}"
                                       (mapcar (lambda (source)
                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                  (source-path source)))
                                               sources))
                       :inputs sources
                       :implicit-inputs (list iclasp
                                              (build-name "bitcode"))
                       :outputs (make-source-outputs sources
                                                     :type (file-faso-extension configuration)
                                                     :root :variant-stage-bitcode))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :clasp iclasp
                       :target "aclasp"
                       :inputs (make-source-outputs sources
                                                    :type (file-faso-extension configuration)
                                                    :root :variant-stage-bitcode)
                       :implicit-inputs (list iclasp)
                       :outputs (list aimage))
    (ninja:write-build output-stream :phony
                       :inputs (list aimage (build-name :iclasp))
                       :outputs (list (build-name :aclasp)))
    (when *variant-default*
      (ninja:write-build output-stream :install-file
                         :inputs (list aimage)
                         :outputs (list aimage-installed))
      (ninja:write-build output-stream :phony
                         :inputs (list "install_iclasp"
                                       aimage-installed)
                         :outputs (list "install_aclasp")))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :bclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (bimage (image-source configuration :bclasp))
        (bimage-installed (image-source configuration :bclasp :package-fasl))
        (iclasp (make-source (build-name :iclasp) :variant))
        (*root-paths* (list* :variant-stage-bitcode (merge-pathnames (make-pathname :directory (list :relative
                                                                                                     (format nil "bclasp-~a-bitcode"
                                                                                                                 *variant-name*)))
                                                                     (root :variant-bitcode))
                             *root-paths*)))
    (ninja:write-build output-stream :compile-bclasp
                       :clasp iclasp
                       :image aimage
                       :source (format nil "~{~/ninja:escape/~^ ~}"
                                           (mapcar (lambda (source)
                                                     (merge-pathnames (make-pathname :type :unspecific)
                                                                      (source-path source)))
                                                   sources))
                       :inputs sources
                       :implicit-inputs (list iclasp aimage)
                       :outputs (make-source-outputs sources
                                                     :type (file-faso-extension configuration)
                                                     :root :variant-stage-bitcode))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :clasp iclasp
                       :target "bclasp"
                       :inputs (make-source-outputs sources
                                                    :type (file-faso-extension configuration)
                                                    :root :variant-stage-bitcode)
                       :implicit-inputs (list (make-source (build-name :iclasp) :variant))
                       :outputs (list bimage))
    (ninja:write-build output-stream :phony
                       :inputs (list bimage (build-name :aclasp))
                       :outputs (list (build-name :bclasp)))
    (when *variant-default*
      (ninja:write-build output-stream :install-file
                         :inputs (list bimage)
                         :outputs (list bimage-installed))
      (ninja:write-build output-stream :phony
                         :inputs (list "install_aclasp"
                                       bimage-installed)
                         :outputs (list "install_bclasp")))))

(defun jupyter-kernel-path (configuration name &key system)
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

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (let ((bimage (image-source configuration :bclasp))
        (cimage (image-source configuration :cclasp))
        (cimage-installed (image-source configuration :cclasp :package-fasl))
        (iclasp (make-source (build-name :iclasp) :variant))
        (*root-paths* (list* :variant-stage-bitcode (merge-pathnames (make-pathname :directory (list :relative
                                                                                                     (format nil "cclasp-~a-bitcode"
                                                                                                                 *variant-name*)))
                                                                     (root :variant-bitcode))
                             *root-paths*)))
    (ninja:write-build output-stream :compile-cclasp
                       :clasp iclasp
                       :image bimage
                       :source (format nil "~{~/ninja:escape/~^ ~}"
                                       (mapcar (lambda (source)
                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                  (source-path source)))
                                               sources))
                       :inputs sources
                       :implicit-inputs (list iclasp bimage
                                              (make-source "tools-for-build/character-names.sexp" :code))
                       :outputs (make-source-outputs sources
                                                     :type (file-faso-extension configuration)
                                                     :root :variant-stage-bitcode))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :clasp iclasp
                       :target "cclasp"
                       :inputs (make-source-outputs sources
                                                    :type (file-faso-extension configuration)
                                                    :root :variant-stage-bitcode)
                       :implicit-inputs (list iclasp)
                       :outputs (list cimage))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "bclasp")
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
                                                 :clasp clasp)
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
                                                   :clasp clasp)
                             collect output))))
        (ninja:write-build output-stream :install-file
                           :inputs (list cimage)
                           :outputs (list cimage-installed))
        (ninja:write-build output-stream :phony
                           :inputs (list* "install_bclasp"
                                          "install_modules"
                                          cimage-installed
                                          kernels)
                           :outputs (list "install_cclasp"))))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) (source lisp-source))
  (let* ((image (image-source configuration :cclasp))
         (name (pathname-name (source-path source)))
         (module-name (format nil "cclasp-~a-bitcode/src/lisp/modules/~a/~a.~a"
                              *variant-name* name name
                              (module-fasl-extension configuration)))
         (output (make-source module-name :variant-lib))
         (install-output (make-source module-name :package-lib))
         (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-module
                       :clasp iclasp
                       :image image
                       :inputs (list source)
                       :implicit-inputs (list iclasp image)
                       :outputs (list output))
    (when *variant-default*
      (ninja:write-build output-stream :install-file
                         :inputs (list output)
                         :outputs (list install-output)))
    (list :outputs output
          :install-outputs install-output)))

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
    (configuration (name (eql :ninja)) output-stream (target (eql :regression-tests)) sources
     &key &allow-other-keys)
  (ninja:write-build output-stream :regression-tests
                     :clasp (make-source (build-name :iclasp) :variant)
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "test")))
  (ninja:write-build output-stream :ansi-test
                     :clasp (make-source (build-name :iclasp) :variant)
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "ansi-test")))
  (ninja:write-build output-stream :test-random-integer
                     :clasp (make-source (build-name :iclasp) :variant)
                     :inputs (list (build-name "cclasp"))
                     :outputs (list (build-name "test-random-integer")))
  (when *variant-default*
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "test"))
                       :outputs (list "test"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "ansi-test"))
                       :outputs (list "ansi-test"))
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "test-random-integer"))
                       :outputs (list "test-random-integer"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :static-analyzer)) sources
     &key &allow-other-keys)
  (unless (or *variant-prep* *variant-precise*)
    (ninja:write-build output-stream :static-analyzer
                       :clasp (make-source (build-name :iclasp) :variant)
                       :variant-path *variant-path*
                       :inputs (list (make-source (format nil "preciseprep~:[~;-d~]/compile_commands.json"
                                                          *variant-debug*)
                                                          :build))
                       :implicit-inputs (list (build-name "cclasp")
                                              (build-name "generated" :prep t :gc :mps))
                       :outputs (list (build-name "analyze"))
                       :sif (make-source (if (member :cando (extensions configuration))
                                             "src/analysis/clasp_gc_cando.sif"
                                             "src/analysis/clasp_gc.sif")
                                         :code))
    (unless *variant-debug*
      (ninja:write-build output-stream :phony
                         :inputs (list (build-name "analyze"))
                         :outputs (list "analyze")))))

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
     &aux (cclasp (build-name :cclasp))
          (iclasp (make-source (build-name :iclasp) :variant)))
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
                                :clasp iclasp
                                :arguments (when ignore-extensions
                                             "--feature ignore-extensions")
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
                           :inputs (list* "install_cclasp"
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
  
