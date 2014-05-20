
local a (Table)
[ a appendField :b Int ]
[ a appendField :a String ]
[ a appendWrite (list :a "val100" :b 100 ) ]
[ a appendWrite (list :a "val5" :b 5 ) ]
[ a appendWrite (list :a "val3" :b 3 ) ]
[ a appendWrite (list :a "val20" :b 20 ) ]
println [ "contents = \n%s" % (contentsAsString a) ]
sortTableEntriesUsingComparisonFunction a (lambda ( x y ) [ [ x read :b ] < [ y read :b] ] )
println [ "after sort contents = \n%s" % (contentsAsString a) ]
sortTableEntriesUsingComparisonFunction a (lambda ( x y ) [ [ y read :b ] < [ x read :b] ] )
println [ "after second sort contents = \n%s" % (contentsAsString a) ]
