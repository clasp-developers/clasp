(si:print "Startup initCommonLisp.l")
(si:make-package "COMMON-LISP" :use :core :nicknames '("CL" "cl"))
(si:import 'si:import)
(import 'si:*package*)
(import 'si:setq)
(import 'si:load)
;(load "macros.l")
(use-package 'CL)


