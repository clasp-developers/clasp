


class FitStatus:
    def __init__(self):
	self._Builder = None
	pass

    def setBuilder(self,aBuilder):
	self._Builder = aBuilder

    def setStep(self,aStep):
	self._Step = aStep

    def setError(self,spectralError):
	self._SpectralError = spectralError

    def asXml(self):
	node = QuickDomNode("FitStatus")
	bnode = self._Builder.asXml()
	node.addChild(bnode)
	node.addAttributeDouble("spectralError",self._SpectralError,9,5)
	node.addAttributeInt("step",self._Step)



