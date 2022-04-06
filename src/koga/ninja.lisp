(in-package #:koga)

(defmethod make-output-stream (configuration (name (eql :ninja)) path)
  (declare (ignore configuration name))
  (ninja:make-line-wrapping-stream (ninja:make-timestamp-preserving-stream path)))

(defmethod print-prologue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-bindings output-stream
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
                        :objcopy (objcopy configuration))
  (terpri output-stream)
  (ninja:write-rule output-stream :install-file
                    :command #+bsd "install -C --mode=644 $in $out"
                             #+linux "install -CT --mode=644 $in $out"
                    :restat 1
                    :description "Installing $in to $out")
  (ninja:write-rule output-stream :install-binary
                    :command #+bsd "install -C --mode=755 $in $out"
                             #+linux "install -CT --mode=755 $in $out"
                    :restat 1
                    :description "Installing $in to $out")
  (ninja:write-rule output-stream :install-binary-with-link
                    :command #+bsd "install -C --mode=755 $in $out && ln -s -f $target $link"
                             #+linux "install -CT --mode=755 $in $out && ln -s -f $target $link"
                    :restat 1
                    :description "Installing $in to $out")
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
                    :command "$iclasp --non-interactive --feature ignore-extensions --load ${variant-path}static-analyzer.lisp -- $sif $in"
                    :description "Analyzing clasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :ar
                    :command "$ar ru $out $in"
                    :description "Creating archive for $in")
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
  (ninja:write-rule output-stream :compile-aclasp
                    :command "$iclasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load compile-aclasp.lisp -- $source"
                    :description "Compiling aclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-bclasp
                    :command "$iclasp --norc --type image --disable-mpi --image $image --load compile-bclasp.lisp -- $source"
                    :description "Compiling bclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-cclasp
                    :command "$iclasp --norc --type image --disable-mpi --image $image --load compile-cclasp.lisp -- $source"
                    :description "Compiling cclasp"
                    :restat 1
                    :pool "console")
  (ninja:write-rule output-stream :compile-module
                    :command "$iclasp --non-interactive --norc --type image --disable-mpi --image $image --feature ignore-extensions --load compile-module.lisp -- $out $in"
                    :description "Compiling module $in")
  (ninja:write-rule output-stream :regression-tests
                    :command "$iclasp --non-interactive --feature ignore-extensions --load \"sys:regression-tests;run-all.lisp\""
                    :description "Running regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :link-fasl
                    :command "$iclasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load link-fasl.lisp -- $out $in"
                    :restat 1
                    :description "Linking $target")
  (ninja:write-rule output-stream "link-fasl-abc"
                    :command "$cxx $variant-ldflags $ldflags $ldflags-fasl -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :make-snapshot
                    :command "$iclasp --non-interactive $arguments --load snapshot.lisp -- $out"
                    :description "Creating snapshot $out")
  (ninja:write-rule output-stream :make-snapshot-object
                    :command "$objcopy --input-target binary --output-target elf64-x86-64 --binary-architecture i386 $in $out --redefine-sym _binary_${mangled-name}_end=_binary_snapshot_end  --redefine-sym _binary_${mangled-name}_start=_binary_snapshot_start  --redefine-sym _binary_${mangled-name}_size=_binary_snapshot_size"
                    :description "Creating object from snapshot $in"))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :install-code)) source
     &aux (output (make-source (source-path source) :install-clasp)))
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
                     :outputs (list (build-name "install_code" :common t))))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (format nil "extension-startup-loads/~a"
                                       (file-namestring (source-path source)))
                               :install-bin)))
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
                     :outputs (list (build-name "install_load" :common t))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (format nil "extension-startup-loads/~a"
                                       (file-namestring (source-path source)))
                               :variant)))
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
          (outputs (list (make-source (build-name target) :variant))))
  (ninja:write-build output-stream :generate-headers
                     :inputs (sort sifs
                                   (lambda (x y)
                                     (and (eq x :code)
                                          (eq y :variant)))
                                   :key #'source-root)
                     :precise (if *variant-precise* "1" "0")
                     :variant-path *variant-path*
                     :sources (format nil "~{~/ninja:escape/~^ ~}"
                                      (mapcar #'source-path sifs))
                     :outputs generated)
  (ninja:write-build output-stream :phony
                     :outputs (list (build-name "generated"))
                     :inputs generated)
  (ninja:write-build output-stream :link
                     :variant-ldflags *variant-ldflags*
                     :variant-ldlibs *variant-ldlibs*
                     :inputs objects
                     :outputs outputs)
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list (build-name target))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream
     (target (eql :bitcode)) (source cc-source)
     &aux (bitcode-name (format nil "fasl/~a-~a-cxx.bc" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (bitcode-output (make-source bitcode-name :variant))
          (bitcode-install (make-source bitcode-name :install-variant))
          (bitcode-nd-name (format nil "fasl/~a-~a-no-debug-info-cxx.bc" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (bitcode-nd-output (make-source bitcode-nd-name :variant))
          (bitcode-nd-install (make-source bitcode-nd-name :install-variant))
          (object (make-source-output source :type "o"))
          (archive-name (format nil "fasl/~a-~a-cxx.a" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (archive-output (make-source archive-name :variant))
          (archive-install (make-source archive-name :install-variant))
          (object-nd (make-source (make-pathname :directory (pathname-directory (source-path source))
                                                 :name (concatenate 'string
                                                                    (pathname-name (source-path source))
                                                                    "-no-debug-info")
                                                 :type "o")
                                  :variant))
          (archive-nd-name (format nil "fasl/~a-~a-no-debug-info-cxx.a" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (archive-nd-output (make-source archive-nd-name :variant))
          (archive-nd-install (make-source archive-nd-name :install-variant))
          (headers (if *variant-precise*
                       (scraper-precise-headers configuration)
                       (scraper-headers configuration)))
          (no-debug-flags (remove-flag "-g" *variant-cxxflags*)))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags *variant-cxxflags*
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list bitcode-output))
  (ninja:write-build output-stream :install-file
                     :inputs (list bitcode-output)
                     :outputs (list bitcode-install))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags no-debug-flags
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list bitcode-nd-output))
  (ninja:write-build output-stream :install-file
                     :inputs (list bitcode-nd-output)
                     :outputs (list bitcode-nd-install))
  (ninja:write-build output-stream :ar
                     :inputs (list object)
                     :outputs (list archive-output))
  (ninja:write-build output-stream :install-file
                     :inputs (list archive-output)
                     :outputs (list archive-install))
  (ninja:write-build output-stream :cxx
                     :variant-cxxflags no-debug-flags
                     :inputs (list source)
                     :order-only-inputs headers
                     :outputs (list object-nd))
  (ninja:write-build output-stream :ar
                     :inputs (list object-nd)
                     :outputs (list archive-nd-output))
  (ninja:write-build output-stream :install-file
                     :inputs (list archive-nd-output)
                     :outputs (list archive-nd-install))
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
  (ninja:write-build output-stream :phony
                     :inputs install-outputs
                     :outputs (list (build-name "install_bitcode"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream
     (target (eql :aclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-aclasp
                       :iclasp iclasp
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
                                                     :root (format nil "fasl/aclasp-~a-bitcode/"
                                                                   *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "aclasp"
                       :inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                    :root (format nil "fasl/aclasp-~a-bitcode/"
                                                                       *variant-bitcode-name*))
                       :implicit-inputs (list iclasp)
                       :outputs (list aimage))
    (ninja:write-build output-stream :phony
                       :inputs (list aimage)
                       :outputs (list (build-name :aclasp)))
    (ninja:write-build output-stream :install-file
                       :inputs (list aimage)
                       :outputs (list (image-source configuration :aclasp :install-variant)))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :bclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (bimage (image-source configuration :bclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-bclasp
                       :iclasp iclasp
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
                                                     :root (format nil "fasl/bclasp-~a-bitcode/"
                                                                   *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "bclasp"
                       :inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                    :root (format nil "fasl/bclasp-~a-bitcode/"
                                                                       *variant-bitcode-name*))
                       :implicit-inputs (list (make-source (build-name :iclasp) :variant))
                       :outputs (list bimage))
    (ninja:write-build output-stream :phony
                       :inputs (list bimage)
                       :outputs (list (build-name :bclasp)))
    (ninja:write-build output-stream :install-file
                       :inputs (list bimage)
                       :outputs (list (image-source configuration :bclasp :install-variant)))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (let ((bimage (image-source configuration :bclasp))
        (cimage (image-source configuration :cclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-cclasp
                       :iclasp iclasp
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
                                                     :root (format nil "fasl/cclasp-~a-bitcode/"
                                                                   *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "cclasp"
                       :inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                    :root (format nil "fasl/cclasp-~a-bitcode/"
                                                                       *variant-bitcode-name*))
                       :implicit-inputs (list iclasp)
                       :outputs (list cimage))
    (ninja:write-build output-stream :install-file
                       :inputs (list cimage)
                       :outputs (list (image-source configuration :cclasp :install-variant)))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) (source lisp-source))
  (let* ((image (image-source configuration :cclasp))
         (name (pathname-name (source-path source)))
         (module-name (format nil "fasl/cclasp-~a-bitcode/src/lisp/modules/~a/~a.~a"
                              *variant-bitcode-name* name name
                              (module-fasl-extension configuration)))
         (output (make-source module-name :variant))
         (install-output (make-source module-name :install-variant))
         (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-module
                       :iclasp iclasp
                       :image image
                       :inputs (list source)
                       :implicit-inputs (list iclasp image)
                       :outputs (list output))
    (ninja:write-build output-stream :install-file
                       :inputs (list output)
                       :outputs (list install-output))
    (list :outputs output
          :install-outputs install-output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) sources
     &key outputs install-outputs &allow-other-keys
     &aux (executable (build-name :cclasp)))
  (ninja:write-build output-stream :phony
                     :inputs outputs
                     :outputs (list (build-name "modules")))
  (ninja:write-build output-stream :phony
                     :inputs install-outputs
                     :outputs (list (build-name "install_modules"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :clasp)) sources
     &key &allow-other-keys
     &aux (executable (build-name :cclasp))
          (iclasp (make-source (build-name :iclasp) :variant))
          (inst-iclasp (make-source (build-name :iclasp) :install-bin))
          (generated (append (if *variant-precise*
                                 (scraper-precise-headers configuration)
                                 (scraper-headers configuration))
                             (scraper-lisp-sources configuration)))
          (products (mapcar (lambda (source)
                              (make-source (source-path source) :install-variant))
                            generated)))
  (ninja:write-build output-stream :phony
                     :inputs (list (build-name "modules")
                                            (build-name "load"))
                     :outputs (list executable))
  (ninja:write-build output-stream :regression-tests
                     :iclasp iclasp
                     :inputs (list executable)
                     :outputs (list (format nil "test-~a" *variant-bitcode-name*)))
  (ninja:write-build output-stream :install-binary-with-link
                     :inputs (list iclasp)
                     :outputs (list inst-iclasp)
                     :link (resolve-source (make-source (if (member :cando (extensions configuration))
                                                            "ccando"
                                                            "cclasp")
                                           :install-bin))
                     :target (source-path inst-iclasp))
  (loop for input in generated
        for output in products
        do (ninja:write-build output-stream :install-file
                              :inputs (list input)
                              :outputs (list output)))
  (ninja:write-build output-stream :phony
                     :inputs (list* inst-iclasp
                                    (build-name "install_bitcode")
                                    (image-source configuration :aclasp :install-variant)
                                    (image-source configuration :bclasp :install-variant)
                                    (image-source configuration :cclasp :install-variant)
                                    (build-name "install_modules")
                                    (build-name "install_code" :common t)
                                    (build-name "install_load" :common t)
                                    products)
                     :outputs (list (build-name "install_cclasp")))
  (unless (or *variant-prep* *variant-precise*)
    (ninja:write-build output-stream :static-analyzer
                       :iclasp iclasp
                       :variant-path *variant-path*
                       :inputs (list (make-source (format nil "preciseprep~:[~;-d~]/compile_commands.json"
                                                                       *variant-debug*)
                                                           :build))
                       :implicit-inputs (list executable
                                              (build-name "generated" :prep t :gc :mps))
                       :outputs (list (build-name "analyze"))
                       :sif (make-source "src/clasp_gc.sif" :code))))

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
           (let ((snapshot (make-source (format nil "generated/~a.snapshot" name) :variant))
                 (object (make-source (format nil "generated/~a.o" name) :variant))
                 (executable (make-source (build-name name) :variant)))
             (declare (ignorable object))
             (ninja:write-build output-stream :make-snapshot
                                :iclasp iclasp
                                :arguments (when ignore-extensions
                                             "--feature ignore-extensions")
                                :inputs (list cclasp)
                                :outputs (list snapshot))
             #+darwin (ninja:write-build output-stream :link
                                :variant-ldflags (format nil "-sectcreate __CLASP __clasp ~a ~a"
                                                         snapshot
                                                         *variant-ldflags*)
                                :variant-ldlibs *variant-ldlibs*
                                :inputs objects
                                :implicit-inputs (list snapshot)
                                :outputs (list executable))
             #-darwin (ninja:write-build output-stream :make-snapshot-object
                                         :mangled-name (substitute-if #\_
                                                                      (complement #'alphanumericp)
                                                                      (namestring (resolve-source snapshot)))
                                         :inputs (list snapshot)
                                         :outputs (list object))
             #-darwin (ninja:write-build output-stream :link
                                :variant-ldflags *variant-ldflags*
                                :variant-ldlibs *variant-ldlibs*
                                :inputs (cons object objects)
                                :outputs (list executable))
             (list executable
                   (make-source (build-name name) :install-bin)
                   (make-source name :install-bin)))))
    (let ((executables (if (member :cando (extensions configuration))
                            (list (snapshot "dclasp" :ignore-extensions t)
                                  (snapshot "dcando"))
                            (list (snapshot "dclasp")))))
      (ninja:write-build output-stream :phony
                         :inputs (mapcar #'first executables)
                         :outputs (list (build-name :dclasp)))
      (loop for (executable installed link) in executables
            do (ninja:write-build output-stream :install-binary-with-link
                                  :link link
                                  :target (source-path installed)
                                  :inputs (list executable)
                                  :outputs (list installed)))
      (ninja:write-build output-stream :phony
                         :inputs (list* (build-name "install_cclasp")
                                        (mapcar #'second executables))
                         :outputs (list (build-name "install_dclasp")))
      (ninja:write-build output-stream :phony
                         :inputs (list (build-name "install_dclasp"))
                         :outputs (list (build-name "install"))))))

(defmethod print-epilogue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-default output-stream (default-target configuration)))

