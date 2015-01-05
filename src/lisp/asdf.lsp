;;
;; Load the ASDF system
;;
(load "sys:kernel;asdf;build;asdf.fasl")
(asdf/find-system:initialize-source-registry #P"sys:clasp-asdf-registry.conf")
