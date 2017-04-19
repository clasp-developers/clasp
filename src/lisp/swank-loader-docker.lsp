
(load #P"/home/app/slime/swank-loader.lisp")
(swank-loader:init
 :delete nil  ; delete any existing SWANK packages
 :reload nil  ; reload SWANK, even if the SWANK package already exists
 :load-contribs nil)			; load all contribs

(mp:process-run-function
 'swank-main
 (swank:create-server
  :port 4005
  :interface "0.0.0.0"))

 
