import mbb

print "Reading MOE file"
agg = mbb.new_RPAggregate()
agg.open("phe.moe")

loop = mbb.Loop()
loop.loopTopAggregateGoal(agg,mbb.ATOMS)
while ( loop.advanceLoopAndProcess() ):
    a = loop.getAtom()
    print "atom = ", a.getName()

driver = mbb.TorsionDriver()
driver.setAggregateAndRoot( agg, "NH" )

driver.addTorsion( "NH", "CA", 3 )
driver.addTorsion( "CA", "CB", 3 )
driver.addTorsion( "CB", "CG", 3 )
driver.addTorsion( "CA", "CO", 3 )

driver.prepareToDriveTorsions()

print "Driver=",driver

x = 0
update = 999
while ( update != 0 ):
    x = x + 1
    update = driver.advanceToNextConformation()
    print "at:%d advance=%d"%(x,update)

