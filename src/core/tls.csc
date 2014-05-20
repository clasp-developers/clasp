;
;
;defClass A () ()
;    method a (self)
;        println "a"
;
;;
;; Make sure the B is a subclass of A
;;
;defClass B A ()
;    method b (self)
;        println "b"
;
;defClass C B ()
;    method c (self)
;        println "c"
;
;defClass M Atom ()
;    method aaa (self)
;        println "aaa"
;
;

local lm (loadArchive "_m.cxml")
local lb (loadArchive "_b.cxml")

println "-------------------------- describing u:M"
describe u:M
println "-------------------------- describing u:C"
describe u:C


println [ "lm class=%s" % [lm className] ]
println [ "lb class=%s" % [lb className] ]


saveArchive lm "_lm.cxml"
saveArchive lb "_lb.cxbl"
