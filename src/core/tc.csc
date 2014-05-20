
defClass A () (_A)
    method __init__ (self &key (valA 1) )
	println [ "A class __init__ called with arg: %d" % valA ]
        setq [self slot _A] valA


defClass B A ( _B )
    method __init__ ( self &key (valA 2) (valB 1) )
	println [ "B class __init__ called with arg: %d" % valB ]
        setq [self slot _B] valB
	println [ "self class = %s" % [ self className ] ]
	callAncestorMethod self :valA valA

    method dump ( self )
        println [ "_B = %d" % [self slot _B] ]
        println [ "_A = %d" % [self slot _A] ]

global b (B :valB 3 :valA 100)
[b dump]
