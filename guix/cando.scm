;;; SPDX-FileCopyrightText: Copyright (c) 2024 The Clasp Authors
;;; SPDX-License-Identifier: MIT
;;;
;;; (Of course, Guix is GPL-3.0-or-later.)

;; Usage:
;;
;;   guix shell -D -f guix/cando.scm -- COMMAND # Cando-suitable environment
;;   guix build -f guix/cando.scm               # Build the package
;;   guix package -f guix/cando.scm             # Install the package
;;
;; Unfortunately, for now the package and the above commands require the
;; cloned dependencies to be already present, which can be accomplished
;; as follows:
;;
;;   guix shell --pure git nss-certs sbcl -- ./koga --extensions=cando,seqan-clasp
;;
;; though Koga will error out after downloading the dependencies, when
;; trying to configure Clasp.

(include "clasp.scm")

(use-modules
 (guix utils)
 (gnu packages maths)
 (gnu packages xml))

(define cando
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
         (descend-dirs `(,@git-dirs
                         ,@external-dirs))
         (predicates (map-in-order git-predicate (cons source-dir git-dirs))))
    (package
      (inherit clasp-cl)
      (name "cando")
      (version version)
      (source
       (local-file source-dir (git-file-name "cando" version)
                   #:recursive? #t
                   #:select?
                   (lambda (file stat)
                     (or (any (lambda (dir) (string=? dir file)) descend-dirs)
                         (any (lambda (pred) (pred file stat)) predicates)))))
      (arguments
       (substitute-keyword-arguments (package-arguments clasp-cl)
         ((#:configure-flags flags '())
          #~(cons*
             "--extensions=cando"
             ;; Had trouble simplifying this using find.
             ;; This is prepended because Koga keeps the first instance
             ;; of repeated options.
             (string-append
              "--ldflags="
              "-Wl,-rpath," #$(this-package-input "clang-toolchain") "/lib"
              " -Wl,-rpath," (ungexp (this-package-input "gcc") "lib") "/lib"
              " -Wl,-rpath," #$(this-package-input "fmt") "/lib"
              " -Wl,-rpath," #$(this-package-input "gmp") "/lib"
              " -Wl,-rpath," #$(this-package-input "libelf") "/lib"
              " -Wl,-rpath," #$(this-package-input "expat") "/lib"
              " -Wl,-rpath," #$(this-package-input "netcdf") "/lib")
             #$flags))))
      (inputs
       (modify-inputs (package-inputs clasp-cl)
         (prepend expat netcdf)))
      (home-page "https://github.com/cando-developers/cando/")
      (synopsis "Computer-aided nanostructure design and optimization")
      (description "Cando is a computational chemistry environment for building
and designing functional molecules and materials.")
      ;; Different components are variously licensed.
      (license
       (list license:gpl2+ license:lgpl2.0+ license:lgpl3+ license:expat)))))

cando
