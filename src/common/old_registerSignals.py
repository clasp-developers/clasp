import StringIO
import sys
import re


class	OneSignal:
    def __init__(self,sigName,sigNum):
	print "Signal(%60s) #(%4d)"%(sigName,sigNum)
	self._SignalName = sigName
	self._SignalNumber = sigNum

    def declareConstant(self,fout):
        fout.write("extern const uint %s;\n"%(self._SignalName))

    def defineConstant(self,fout):
        fout.write("const uint %s = %d;\n"%(self._SignalName,self._SignalNumber))

    def exposeConstantForBoostPython(self,fout):
        fout.write("    .value(\"%s\",%s)\n"%(self._SignalName,self._SignalName))
       
    def exposeConversionIdToString(self,fout):
        fout.write("        case %3d: { return \"%s\"; break; }\n"%(self._SignalNumber,self._SignalName)) 

    def exposeConversionStringToId(self,fout):
        fout.write("    if ( name == \"%s\" ) { return %d; }\n"%(self._SignalName,self._SignalNumber)) 


dec = re.compile('\w*PUBLISH_SIGNAL\s*\(\s*([a-zA-Z0-9:_]*)\w*\)\w*;')

outFileName = sys.argv[1]

fout = StringIO.StringIO()

fout.write("// start\n" )
fout.flush()

fileNames = sys.argv[2:]

signals = []
for fileName in fileNames:
    fin = open(fileName,"r")
    ln = 0
    for l in fin.readlines():
	ln += 1
	line = l.strip()
#	print("%s\n"%line)
	decmatch = dec.match(line)
	if ( decmatch!= None ):
	    gr = decmatch.groups()
	    o = OneSignal(gr[0],len(signals)+1)
	    signals.append(o)
    fin.close()


fout.write("#ifdef	DECLARE_SIGNAL_CONSTANTS\n")
for sig in signals:
    sig.declareConstant(fout)
fout.write("#endif\n\n\n\n")
fout.write("#ifdef	DEFINE_SIGNAL_CONSTANTS\n")
for sig in signals:
    sig.defineConstant(fout)
fout.write("#endif\n\n\n\n")
fout.write("#ifdef	SIGNAL_EXPOSE_TO_BOOST_PYTHON\n")
for sig in signals:
    sig.exposeConstantForBoostPython(fout)
fout.write("#endif\n\n\n\n")

fout.write("#ifdef	SIGNAL_EXPOSE_CONVERSION_FUNCTIONS\n")
fout.write("""
string signalIdToName(uint sig)
{
    switch ( sig )
    {
""")
for sig in signals:
    sig.exposeConversionIdToString(fout)
fout.write("""        default: { return "undefinedSignal"; break; }
    }
};

""")

fout.write("""
uint signalNameToId(const string& name )
{
""")
for sig in signals:
    sig.exposeConversionStringToId(fout)
fout.write("""    return -1;

}
""")


fout.write("#endif\n")


newSignalsContent = fout.getvalue()

fout.close()


fout = open(outFileName,"r")
oldSignalsContent = fout.read()
fout.close()

if ( oldSignalsContent == newSignalsContent ):
    print("%s will be unchanged --> NOT UPDATED"%outFileName)
else:
    print("Changes in signals were made, UPDATING %s"%outFileName)
    fout = open(outFileName,"w")
    fout.write(newSignalsContent)
    fout.close()

