
defun a ( a &optional ( b 78 ) &rest rest &key c (d 23) )
    println [ "----------" ]
    println [ "a=%s" % a ]
    println [ "b=%s" % b ]
    println [ "c=%s" % c ]
    println [ "d=%s" % d ]
    println [ "rest=%s" % (repr rest) ]


defClass A () ( val )
    method __init__ ( self a )
        println [ "init a = %d" % a ]
	[ self setf val a ]
	println [ "_val = %s " % [ self getf val ] ]

    method set_val ( self v )
        [ self setf val v ]

    method ma ( self a &optional ( b 178 ) &rest rest &key c (d 123) )
	println [ "----------" ]
	println [ "a=%s" % a ]
	println [ "b=%s" % b ]
	println [ "c=%s" % c ]
	println [ "d=%s" % d ]
	println [ "rest=%s" % (repr rest) ]
	println [ "_val = %s" % [ self getf val ] ]
        

a 1
a 1 2
a 1 2 :c 3
a 1 2 :c 4 :d 5

describe A

setq b (A 23)
println "Describing object b"
[b set_val 20]
[b ma 1 ]
[b ma 1 2 ]
[b ma 1 2 :c 10]
[b ma 1 2 :c 10 :d 20]

describe b

println [ "before setting slot = %s" % (slot b val) ]
setq (slot b val) 1000
println [ "after setting slot = %s" % (slot b val) ]

save b "_out.cxml"
