;;; SPDX-FileCopyrightText: Copyright (c) 2024 The Clasp Authors
;;; SPDX-License-Identifier: MIT
;;;
;;; (Of course, Guix is GPL-3.0-or-later.)

;; Usage:
;;
;;   guix shell -D -f guix/clasp.scm -- COMMAND # Clasp-suitable environment
;;   guix build -f guix/clasp.scm               # Build the package
;;   guix package -f guix/clasp.scm             # Install the package
;;
;; Unfortunately, for now the package and the above commands require the
;; cloned dependencies to be already present, which can be accomplished
;; as follows:
;;
;;   guix shell --pure git nss-certs sbcl -- ./koga
;;
;; though Koga will error out after downloading the dependencies, when
;; trying to configure Clasp.

(use-modules
 (ice-9 ftw)
 (srfi srfi-1)
 (srfi srfi-11)
 (git bindings)
 (git describe)
 (guix build-system gnu)
 (guix gexp)
 (guix git-download)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (gnu packages base)
 (gnu packages boost)
 (gnu packages certs)
 (gnu packages compression)
 (gnu packages elf)
 (gnu packages gcc)
 (gnu packages libunwind)
 (gnu packages lisp)
 (gnu packages llvm)
 (gnu packages multiprecision)
 (gnu packages ninja)
 (gnu packages pkg-config)
 (gnu packages pretty-print)
 (gnu packages version-control))

(define (git-checkout-description checkout)
  (libgit2-init!)
  (let-values (((commit description) (describe-checkout checkout)))
    description))

(define (scandir-absolute name)
  (let ((contents (scandir name (lambda (file)
                                  (not (or (equal? "." file)
                                           (equal? ".." file)))))))
    (if (not contents) '()
        (map-in-order (lambda (file) (string-append name "/" file))
                      contents))))

(define clasp-cl
  (let* ((source-dir (dirname (dirname (current-filename))))
         (version (git-checkout-description source-dir))
         (external-dirs               ; dirs containing nested Git repos
          (list (string-append source-dir "/src/lisp/kernel/contrib")
                (string-append source-dir "/dependencies")
                (string-append source-dir "/extensions")))
         (git-dirs
          (apply append
                 (list (string-append source-dir "/src/bdwgc")
                       (string-append source-dir "/src/libatomic_ops")
                       (string-append source-dir "/src/lisp/modules/asdf"))
                 (map-in-order scandir-absolute external-dirs)))
         (descend-dirs (append git-dirs external-dirs))
         (predicates (map-in-order git-predicate (cons source-dir git-dirs))))
    (package
      (name "clasp-cl")
      (version version)
      (source
       (local-file source-dir (git-file-name "clasp-cl" version)
                   #:recursive? #t
                   #:select?
                   (lambda (file stat)
                     (or (any (lambda (dir) (string=? dir file)) descend-dirs)
                         (any (lambda (pred) (pred file stat)) predicates)))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags
        #~(let ((clang #$(this-package-input "clang-toolchain")))
            (list "--build-mode=bytecode-faso"
                  "--reproducible-build"
                  (string-append "--bin-path=" #$output "/bin/")
                  (string-append "--lib-path=" #$output "/lib/clasp/")
                  (string-append "--share-path=" #$output "/share/clasp/")
                  (string-append "--dylib-path=" #$output "/lib/")
                  (string-append "--pkgconfig-path=" #$output "/lib/pkgconfig/")
                  ;; Without --cc and --cxx, Clang is searched for in
                  ;; LLVM's /bin.
                  (string-append "--cc=" clang "/bin/clang")
                  (string-append "--cxx=" clang "/bin/clang++")
                  (string-append
                   "--ld=" #$(this-package-native-input "lld") "/bin/ld.lld")
                  (string-append
                   "--ldflags="
                   "-Wl,-rpath," clang "/lib"
                   " -Wl,-rpath," (ungexp (this-package-input "gcc") "lib") "/lib"
                   " -Wl,-rpath," #$(this-package-input "fmt") "/lib"
                   " -Wl,-rpath," #$(this-package-input "gmp") "/lib"
                   " -Wl,-rpath," #$(this-package-input "libelf") "/lib")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-paths
              (lambda _
                (substitute* '("dependencies/quicklisp-client/asdf.lisp"
                               "src/lisp/modules/asdf/uiop/run-program.lisp"
                               "src/lisp/modules/asdf/uiop/launch-program.lisp"
                               "src/lisp/regression-tests/extensions.lisp")
                  (("\"/bin/sh\"")
                   (string-append "\"" (which "sh") "\"")))))
            (delete 'bootstrap)
            (replace 'configure
              (lambda* (#:key (configure-flags '()) #:allow-other-keys)
                (setenv "HOME" "/tmp")
                (apply invoke "sbcl" "--script" "koga" "--skip-sync"
                       "--jobs=1"       ; overridden in build phase
                       configure-flags)))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (let ((jobs (if parallel-build?
                                (number->string (parallel-job-count))
                                "1")))
                  (setenv "CLASP_BUILD_JOBS" jobs)
                  (invoke "ninja" "-C" "build" "-j" jobs))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?            ; always serial
                  (invoke "ninja" "-C" "build" "test"))))
            (replace 'install
              (lambda _
                (invoke "ninja" "-C" "build" "install"))))))
      (native-inputs
       (list binutils
             coreutils
             git
             lld
             ninja
             pkg-config
             sbcl
             ;; for koga --archive
             gzip nss-certs tar))
      (inputs
       (list boost
             clang-toolchain-18
             fmt
             (list gcc "lib")
             gmp
             libelf
             libunwind
             libunwind-headers))
      (home-page "https://clasp-developers.github.io/")
      (synopsis "Common Lisp implementation based on LLVM")
      (description "Clasp is a Common Lisp implementation based on LLVM that is
designed to interoperate with C++ libraries.")
      ;; Various files are licensed under permissive licenses.
      (license license:lgpl2.1+))))

clasp-cl
