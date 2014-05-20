
from mbb import *

db = BuilderDatabase()

a = Recepticle(db)


astr = '''
<Recepticle cap="gly">@AminoAcids</Recepticle>
'''
axml = QuickDomFromString(astr)


a.parseFromXml(axml)

if ( "gly"==a.getCap() ):
    print "OK"
else:
    print "BAD"


pxml = QuickDomFromString('''
<InPlug name="inplug0" x0="aa">
  <Recepticle cap="gly">@AminoAcids</Recepticle>
  <Recepticle cap="val">@AminoAcids</Recepticle>
  <Recepticle cap="tyr">@AminoAcids</Recepticle>
</InPlug>
''')


db = BuilderDatabase()
plug = Plug(db)
plug.parseFromXml(pxml)
print plug.asXml().asString()

ams = list(plug.recepticleIterator)
print ams
