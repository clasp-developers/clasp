;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
(load "../util/system")
(load "defsys")
(load "cmpinit")
(setq compiler:*cc* (concatenate 'STRING compiler:*cc* " -I../h -I../../linux/h"))
(rename-package 'clos 'old-clos)
(setq si:*gc-verbose* nil)
(sbt:build-system clos :compile :force)
;(setq *print-circle* t)
;(allocate 'cons 800 t)
(quit)