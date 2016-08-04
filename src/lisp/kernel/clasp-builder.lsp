;;
;; Clasp builder code
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package :core))


(defun recursive-remove-from-list (item list)
  (if list
      (if (equal item (car list))
          (recursive-remove-from-list item (cdr list))
          (list* (car list) (recursive-remove-from-list item (cdr list))))
      nil))
(export 'recursive-remove-from-list)

(export '(compile-aclasp source-files-aclasp bitcode-files-aclasp))
(defun source-files-aclasp ()
  (bformat t "%s\n" (source-file-names :min-start :cmp)))
(defun bitcode-files-aclasp ()
  (with-aclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun compile-aclasp (&key clean (link-type :bc) (target-backend (default-target-backend)) (system *system-files*))
  (aclasp-features)
  (if clean (clean-system :min-start :no-prompt t))
  (if (out-of-date-bitcodes :min-start :cmp)
      (progn
        (load-system :start :cmp-pre-epilogue :system system)
        (let* ((*target-backend* target-backend)
               (files (out-of-date-bitcodes :min-start :cmp-pre-epilogue :system system))
               (files-with-epilogue (out-of-date-bitcodes :min-start :cmp :system system)))
          (with-compilation-unit ()
            (compile-system files :reload t)
            (if files-with-epilogue (compile-system (bitcode-pathnames :cmp-pre-epilogue :cmp :system system) :reload nil)))
          (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                (all-bitcode (bitcode-pathnames :min-start :cmp)))
            (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                (progn
                  (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                  (if (not (eq link-type :bc))
                      (let ((exec-pathname (build-pathname nil link-type)))
                        (bformat t "Linking aclasp %s\n" (string-downcase (string link-type)))
                        (cmp:llvm-link exec-pathname
                                       :lisp-bitcode-files (list cl-bitcode-pathname)
                                       :link-type link-type))))))))))
                   

(defun remove-stage-features ()
  (setq *features* (recursive-remove-from-list :ecl-min *features*))
  (setq *features* (recursive-remove-from-list :clos *features*))
  (setq *features* (recursive-remove-from-list :aclasp *features*))
  (setq *features* (recursive-remove-from-list :bclasp *features*))
  (setq *features* (recursive-remove-from-list :cclasp *features*)))

(export '(aclasp-features with-aclasp-features))
(defun aclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :aclasp :ecl-min *features*)))
(core:fset 'with-aclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :aclasp :ecl-min *features*)))
                     ,@body)))
            t)


(export '(compile-link-aclasp))
(defun compile-link-aclasp (&key clean)
  (with-aclasp-features
      (if clean (clean-system :init :no-prompt t))
    (compile-aclasp)))

(export '(bclasp-features with-bclasp-features))
(defun bclasp-features()
  (remove-stage-features)
  (setq *features* (list* :clos :bclasp *features*)))
(core:fset 'with-bclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :bclasp :clos *features*)))
                     ,@body)))
            t)

(export '(cclasp-features with-cclasp-features))
(defun cclasp-features ()
  (remove-stage-features)
  (setq *features* (list* :clos :cclasp *features*)))
(core:fset 'with-cclasp-features
            #'(lambda (whole env)
                (let* ((body (cdr whole)))
                  `(let ((*features* (list* :cclasp :clos *features*)))
                     ,@body)))
            t)

(export '(load-bclasp-source))
(defun load-bclasp-source ()
  (bclasp-features)
  (let ((*target-backend* (default-target-backend)))
    (load-system :start :all :interp t )))

(export '(compile-bclasp source-files-bclasp bitcode-files-bclasp))
(defun source-files-bclasp ()
  (bformat t "%s\n" (source-file-names :init :all)))
(defun bitcode-files-bclasp ()
  (with-bclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun compile-bclasp (&key clean (link-type :bc))
  (bclasp-features)
  (setq *system-files* (expand-build-file-list *build-files*))
  (if clean (clean-system :init :no-prompt t))
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :all)
        (progn
          (load-system :init :all :interp t ) ;; was :start
          (let ((files (out-of-date-bitcodes :init :all)))
            (compile-system files)
            (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                  (all-bitcode (bitcode-pathnames :init :all)))
              (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                  (progn
                    (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                    (if (not (eq link-type :bc))
                        (let ((exec-pathname (build-pathname nil link-type)))
                          (bformat t "Linking bclasp %s\n" (string-downcase (string link-type)))
                          (cmp:llvm-link exec-pathname
                                         :lisp-bitcode-files (list cl-bitcode-pathname)
                                         :link-type link-type)))))))))))

(export '(compile-cclasp recompile-cclasp source-files-cclasp bitcode-files-cclasp))
(defun source-files-cclasp ()
  (bformat t "%s\n" (source-file-names :init :cclasp)))
(defun bitcode-files-cclasp ()
  (with-cclasp-features (bformat t "%s\n" (namestring (build-common-lisp-bitcode-pathname)))))
(defun recompile-cclasp (&key clean (link-type :bc))
  (if clean (clean-system :init :no-prompt t))
  (let ((files (out-of-date-bitcodes :init :cclasp)))
    (compile-system files)
    (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
          (all-bitcode (bitcode-pathnames :init :cclasp)))
      (if (out-of-date-target cl-bitcode-pathname all-bitcode)
          (progn
            (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
            (if (not (eq link-type :bc))
                (let ((exec-pathname (build-pathname nil link-type)))
                  (bformat t "Linking cclasp %s\n" (string-downcase (string link-type))
                           (cmp:llvm-link exec-pathname
                                          :lisp-bitcode-files (list cl-bitcode-pathname)
                                          :link-type link-type)))))))))

(defun compile-cclasp (&key clean (link-type :executable))
  (cclasp-features)
  (setq *system-files* (expand-build-file-list *build-files*))
  (if clean (clean-system :init :no-prompt t))
  (let ((*target-backend* (default-target-backend)))
    (if (out-of-date-bitcodes :init :cclasp)
        (time
         (progn
           (load-system :bclasp :cclasp :interp t )
           (let ((files (out-of-date-bitcodes :init :cclasp)))
             (compile-system files)
             (let ((cl-bitcode-pathname (build-common-lisp-bitcode-pathname))
                   (all-bitcode (bitcode-pathnames :init :cclasp)))
               (if (out-of-date-target cl-bitcode-pathname all-bitcode)
                   (progn
                     (cmp:link-bitcode-modules cl-bitcode-pathname all-bitcode)
                     (if (not (eq link-type :bc))
                         (let ((exec-pathname (build-pathname nil link-type)))
                           (bformat t "Linking cclasp executable %s\n" (string link-type))
                           (cmp:llvm-link exec-pathname
                                          :lisp-bitcode-files (list cl-bitcode-pathname)
                                          :link-type link-type))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Setup the build system for ASDF
;;
;;

(defun compile-addons ()
  ;; Build serve-event and asdf
  (core:compile-kernel-file #P"src/lisp/modules/serve-event/serve-event" :force-recompile t)
  (core:compile-kernel-file #P"src/lisp/modules/asdf/build/asdf" :force-recompile t))

(defun link-addons ()
  (cmp:llvm-link (core:build-pathname #P"src/lisp/modules/serve-event/serve-event" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"src/lisp/modules/serve-event/serve-event" :bc)))
  (cmp:llvm-link (core:build-pathname #P"src/lisp/modules/asdf/asdf" :fasl)
                 :lisp-bitcode-files (list (core:build-pathname #P"src/lisp/modules/asdf/build/asdf" :bc))))
(export '(compile-addons link-addons))
