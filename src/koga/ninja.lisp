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
                        :etags (etags configuration))
  (terpri output-stream)
  (ninja:write-rule output-stream :etags
                    :command "$etags -o $out $in"
                    :restat 1
                    :description "Creating etags")
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
                    :command "$clasp --non-interactive --feature ignore-extensions --load ${variant-path}static-analyzer.lisp -- $sif $in"
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
                    :command "$clasp --norc --non-interactive --feature ignore-extensions --load \"sys:regression-tests;run-all.lisp\""
                    :description "Running regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :link-fasl
                    :command "$clasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load link-fasl.lisp -- $out $in"
                    :restat 1
                    :description "Linking $target")
  (ninja:write-rule output-stream "link-fasl-abc"
                    :command "$cxx $variant-ldflags $ldflags $ldflags-fasl -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :jupyter-kernel
                    :command (format nil
                                     "~:[CLASP_QUICKLISP_DIRECTORY=$quicklisp-client ~;~]$clasp --non-interactive --load jupyter-kernel.lisp -- $name $bin-path $load-system $system"
                                     clasp-quicklisp-directory)
                    :description "Installing jupyter kernel for $name")
  (ninja:write-rule output-stream :make-snapshot
                    :command (format nil
                                     "~:[CLASP_QUICKLISP_DIRECTORY=$quicklisp-client ~;~]$clasp --non-interactive $arguments --load snapshot.lisp -- $out"
                                     clasp-quicklisp-directory)
                    :pool "console"
                    :description "Creating snapshot $out"))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :etags)) sources
     &key &allow-other-keys)
  (when (and *variant-default* (etags configuration))
    (ninja:write-build output-stream :etags
                       :inputs (append (remove-if (lambda (source)
                                                    (not (or (typep source 'h-source)
                                                             (typep source 'c-source)
                                                             (typep source 'cc-source))))
                                                  sources)
                                       (if *variant-precise*
                                           (scraper-precise-headers configuration)
                                           (scraper-headers configuration))
                                       (list (make-source "config.h" :variant)
                                             (make-source "version.h" :variant)))
                       :outputs (list (make-source "TAGS" :code)))))

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
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (file-namestring (source-path source)) :package-startup)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :install-file
                     :inputs (list source)
                     :outputs (list output))
  (list :outputs output))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) sources
     &key outputs &allow-other-keys)
  (declare (ignore configuration sources))
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list "install_load")))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (file-namestring (source-path source)) :variant-startup)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :install-file
                     :inputs (list source)
                     :outputs (list output))
  (list :loads output))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) sources
     &key loads &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :inputs loads
                     :outputs (list (build-name "load"))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source c-source))
  (declare (ignore configuration))
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
        (o (make-source-output source :type "o"))
        (flags (format nil "-I~a" *variant-path*)))
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
  (ninja:write-build output-stream :phony
                     :inputs (list* exe symlink lib
                                    (when (and *variant-default*
                                               (etags configuration))
                                      (list (make-source "TAGS" :code))))
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
      (ninja:write-build output-stream :install-binary
                                       :inputs (list clasp-sh)
                                       :outputs (list clasp-sh-installed)))
    (ninja:write-build output-stream :phony
                                     :inputs (append (list "install_bitcode"
                                                           "install_code"
                                                           "install_load"
                                                           exe-installed
                                                           lib-installed
                                                           symlink-installed)
                                                     (when (member :cando (extensions configuration))
                                                       (list clasp-sh-installed))
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
                       (jupyter-path configuration)
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
                       :implicit-inputs (list iclasp bimage)
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
                                     (build-name "modules")
                                     (build-name "load"))
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
                           do (ninja:write-build output-stream :jupyter-kernel
                                                 :outputs (list output)
                                                 :inputs (list (build-name "cclasp"))
                                                 :name build-name
                                                 :bin-path clasp
                                                 :load-system 1
                                                 :system 0
                                                 :clasp clasp)
                           collect output)))
        (ninja:write-build output-stream :phony
                           :inputs kernels
                           :outputs (list (build-name "jupyter_cclasp")))
        (unless (eq :dclasp (default-stage configuration))
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
                             do (ninja:write-build output-stream :jupyter-kernel
                                                   :outputs (list output)
                                                   :inputs (list (build-name "cclasp"))
                                                   :name name
                                                   :bin-path name
                                                   :load-system 1
                                                   :system (if system 1 0)
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
     &key outputs install-outputs &allow-other-keys
     &aux (executable (build-name :cclasp)))
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
  (when *variant-default*
    (ninja:write-build output-stream :phony
                       :inputs (list (build-name "test"))
                       :outputs (list "test"))))

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
                       :sif (make-source "src/clasp_gc.sif" :code))
    (unless *variant-debug*
      (ninja:write-build output-stream :phony
                         :inputs (list (build-name "analyze"))
                         :outputs (list "analyze")))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :dclasp))
     (source c-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :dclasp))
     (source cc-source))
  (declare (ignore configuration name output-stream target))
  (list :objects (make-source-output source :type "o")))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :dclasp)) sources
     &key objects &allow-other-keys
     &aux (cclasp (build-name :cclasp))
          (iclasp (make-source (build-name :iclasp) :variant)))
  (flet ((snapshot (name &key ignore-extensions)
           (let* ((executable (make-source (build-name name) :variant))
                  (symlink (make-source name :variant))
                  (symlink-installed (make-source name :package-bin))
                  (installed (make-source (build-name name) :package-bin))
                  (link (make-source name :package-bin))
                  (build-outputs (list executable symlink))
                  (install-outputs (list installed symlink-installed))
                  (system (not (uiop:subpathp (share-path configuration)
                                              (uiop:getenv-absolute-directory "HOME"))))
                  (kernel (jupyter-kernel-path configuration
                                               (if (equal name "dclasp")
                                                   "common-lisp_dclasp"
                                                   "cando_dcando")
                                               :system system))
                  (user-kernel (jupyter-kernel-path configuration
                                                    (format nil "~a_~a"
                                                            (if (equal name "dclasp")
                                                                "common-lisp"
                                                                "cando")
                                                            (build-name name)))))
             (declare (ignorable object))
             (ninja:write-build output-stream :make-snapshot
                                :clasp iclasp
                                :arguments (when ignore-extensions
                                             "--feature ignore-extensions")
                                :inputs (list cclasp)
                                :outputs (list executable))
             (ninja:write-build output-stream :symbolic-link
                                :inputs (list executable)
                                :target (file-namestring (source-path executable))
                                :outputs (list symlink))
             (when (jupyter configuration)
               (ninja:write-build output-stream :jupyter-kernel
                                                :outputs (list user-kernel)
                                                :inputs (list executable)
                                                :name (build-name name)
                                                :bin-path symlink
                                                :load-system 0
                                                :system 0
                                                :clasp executable))
             (when *variant-default*
               (ninja:write-build output-stream :install-binary
                                  :inputs (list executable)
                                  :outputs (list installed))
               (ninja:write-build output-stream :symbolic-link
                                  :inputs (list installed)
                                  :target (file-namestring (source-path installed))
                                  :outputs (list symlink-installed))
               (when (jupyter configuration)
                 (ninja:write-build output-stream :jupyter-kernel
                                    :outputs (list kernel)
                                    :implicit-inputs (list executable)
                                    :name name
                                    :bin-path name
                                    :load-system 0
                                    :system (if system 1 0)
                                    :clasp executable)
                 (push kernel install-outputs)))
             (list build-outputs install-outputs user-kernel))))
    (let ((outputs (if (member :cando (extensions configuration))
                       (list (snapshot "dclasp" :ignore-extensions t)
                             (snapshot "dcando"))
                       (list (snapshot "dclasp")))))
      (ninja:write-build output-stream :phony
                         :inputs (list* (build-name :cclasp)
                                        (mapcan #'first outputs))
                         :outputs (list (build-name :dclasp)))
      (when (jupyter configuration)
        (ninja:write-build output-stream :phony
                           :inputs (mapcar #'third outputs)
                           :outputs (list (build-name "jupyter_dclasp")))
        (when (eq :dclasp (default-stage configuration))
          (ninja:write-build output-stream :phony
                             :inputs (list (build-name "jupyter_cclasp")
                                           (build-name "jupyter_dclasp"))
                             :outputs (list (build-name "jupyter")))))
      (when *variant-default*
        (ninja:write-build output-stream :phony
                           :inputs (list* "install_cclasp"
                                          (mapcan #'second outputs))
                           :outputs (list "install_dclasp"))))))

(defmethod print-epilogue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-build output-stream :phony
                     :inputs (list (format nil "install_~(~a~)" (default-stage configuration)))
                     :outputs (list "install"))
  (ninja:write-default output-stream (default-target configuration)))
  
