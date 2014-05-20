

from mbb import *

from reportlab.lib.enums import TA_CENTER
from reportlab.graphics.shapes import *
from reportlab.pdfbase.pdfmetrics import *
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.rl_config import defaultPageSize
from reportlab.lib.units import inch
PAGE_HEIGHT=defaultPageSize[1]; PAGE_WIDTH=defaultPageSize[0]
styles = getSampleStyleSheet()


from reportlab.pdfgen import canvas
from reportlab.lib import colors
from reportlab.graphics.charts.lineplots import LinePlot
from reportlab.graphics import renderPDF
from reportlab.graphics.widgets.markers import makeMarker


class	ReportGraph:
    def __init__(self):
	self._Title = ""
	self._Data = []

    def setTitle(self,title):
	self._Title = title

    def addData(self,nf):
	self._Data.append(nf)

    def oneDrawing(self):
	allColors = [colors.black,colors.red,colors.green,colors.blue,colors.magenta,colors.yellow]
	height = 320
	lc = LinePlot()
	drawing = Drawing(450,height)
	drawing.add(String(100,height*0.92,self._Title,fontSize=20))
	lc.x = 50
	lc.y = 50
	lc.height = height*0.75
	lc.width = 350
	allData = []
	minX = 9999999.9
	maxX = -9999999.9
	for nf in self._Data:
	    allData.append(nf.getXYData())
	    if ( minX > nf.getLowX() ): minX = nf.getLowX()
	    if ( maxX < nf.getHighX() ): maxX = nf.getHighX()
	firstNf = self._Data[0]
	lc.data = allData
	idx = 0
	clr = 0
	for d in allData:
#	    lc.lines[idx].symbol = makeMarker("FilledCircle")
	    lc.lines[idx].strokeColor = allColors[clr]
	    idx += 1
	    clr += 1
	    if ( clr>=len(allColors) ): clr = 0
	lc.joinedLines = 1
	lc.xValueAxis.valueMin = minX
	lc.xValueAxis.valueMax = maxX
	lc.xValueAxis.labelTextFormat = '%3d'
	drawing.add(lc)
	drawing.add(String(350,20,firstNf.getXAxisName(),fontSize=16))
	yax = String(0,0,firstNf.getYAxisName(),fontSize=16)
	g = Group(yax)
	g.rotate(90)
	g.translate(170,-20)
	drawing.add(g)
	return drawing


class	ReportPage:
    def __init__(self):
	self._PageTitle = ""
	self._Graphs = []

    def setTitle(self,t):
	self._PageTitle = t

    def addGraph(self,graph):
	self._Graphs.append(graph)

    def renderToCanvas(self,canvas):
	canvas.setFont("Helvetica",20)
	canvas.drawCentredString(4.25*inch,10.5*inch,self._PageTitle)
	drawing = self._Graphs[0].oneDrawing()
	renderPDF.draw(drawing,canvas,inch,5.5*inch,showBoundary=True)
	if ( len(self._Graphs)>1 ):
	    drawing = self._Graphs[1].oneDrawing()
	    renderPDF.draw(drawing,canvas,inch,1*inch,showBoundary=True)
	canvas.showPage()




class	OptimizeReport:
    def __init__(self):
	self._Page = 1
	self._Header = "use setHeader to change me"
	self._Comment = "use setComment to change me"
	self._FirstPageGraph = ("First page graph",None)
	self._Canvas = None
	self._PageData = []
	pass

    def setHeader(self,head):
	self._Header = head

    def setComment(self,cmt):
	self._Comment = cmt

    def setFirstPageGraph(self,title,data):
	self._FirstPageGraph = (title,data)


    def addPageData(self,pageData):
	self._PageData.append(pageData)

    def startReport(self):
	self._Page = 1
	

    def drawHeader(self):
	self._Canvas.setFont("Helvetica",16)
	self._Canvas.drawString(inch,11*inch,self._Header)
	self._Canvas.drawRightString(8*inch,0.5*inch,"Page: %d"%self._Page)
	self._Page += 1


    def plotFirstPage(self,summary,summaryTitle,summaryGraph):
	self.drawHeader()
	textobject = self._Canvas.beginText()
	textobject.setTextOrigin(inch,10.8*inch)
	textobject.setFont("Courier", 10 )
	textobject.textLines(summary)
	self._Canvas.drawText(textobject)
	lc = LinePlot()
	if ( summaryGraph != None ):
	    drawing = Drawing(400,350)
	    lc.x = 50
	    lc.y = 50
	    lc.height = 250
	    lc.width = 300
	    nf = summaryGraph
	    lc.data = [nf.getXYData()]
	    lc.joinedLines = 1
	    lc.lines[0].symbol = makeMarker("FilledCircle")
	    lc.xValueAxis.valueMin = nf.getLowX()
	    lc.xValueAxis.valueMax = nf.getHighX()
	    lc.xValueAxis.labelTextFormat = '%3d'
	    drawing.add(lc)
	    drawing.add(String(200,20,nf.getXAxisName(),fontSize=16))
	    drawing.add(String(150,330,summaryTitle,fontSize=16))
	    yax = String(0,0,nf.getYAxisName(),fontSize=16)
	    g = Group(yax)
	    g.rotate(90)
	    g.translate(200,-30)
	    drawing.add(g)
	    renderPDF.draw(drawing,self._Canvas,inch,inch,showBoundary=True)
	self._Canvas.showPage()




    def generateReport(self,file ):
	self._Canvas = canvas.Canvas(file)
	self.startReport()
	self.plotFirstPage(self._Comment,self._FirstPageGraph[0],self._FirstPageGraph[1])
	for page in self._PageData:
	    self.drawHeader()
	    page.renderToCanvas(self._Canvas)
	self._Canvas.save()

   		


if ( __name__ == "__main__" ):

    comment = """
//summary point: 66
//var      rotx0 =   22.32 scale: 10.00 range: -Inf +Inf
//var      roty0 =   35.24 scale: 10.00 range: -Inf +Inf
//var   logrotyk =   -2.12 scale:  1.00 range: -Inf +Inf
//var      rotz0 =   -2.48 scale: 10.00 range: -Inf +Inf
//var   logrotzk =    0.34 scale:  1.00 range: -Inf +Inf
//var     rotyy0 =    0.00 scale: 10.00 range: -Inf +Inf
//var     rotzz0 =    0.00 scale: 10.00 range: -Inf +Inf
//var   logrotxk =    0.00 scale:  1.00 range: -Inf +Inf
//var  logrotyyk =    0.00 scale:  1.00 range: -Inf +Inf
//var  logrotzzk =    0.00 scale:  1.00 range: -Inf +Inf
//var   logdistk =    0.00 scale:  1.00 range: -Inf +Inf
//var      dist0 =    0.00 scale:  0.10 range: -Inf +Inf
//done

//energy = 0.003042340234
"""

    nf = NumericalFunction()
    nf.setXAxisName("iteration")
    nf.setYAxisName("error function")
    nf.setXYData([
(1.000000, 0.342594 ),
( 2.000000, 0.343426 ),
( 3.000000, 0.299291 ),
( 4.000000, 0.283568 ),
( 5.000000, 0.278841 ),
( 6.000000, 0.278638 ),
( 7.000000, 0.282761 ),
( 8.000000, 0.279919 ),
( 9.000000, 0.278908 ),
( 10.00000, 0.278635 ),
( 11.00000, 0.279853 ),
( 12.00000, 0.279861 ),
( 13.00000, 0.279112 ),
( 14.00000, 0.274873 ),
( 15.00000, 0.256160 ),
( 16.00000, 0.336991 ),
( 17.00000, 0.256315 ),
( 18.00000, 0.240475 ),
( 19.00000, 0.258847 ),
( 20.00000, 0.240296 ),
( 21.00000, 0.239939 ),
( 22.00000, 0.239001 ),
( 23.00000, 0.239689 ),
( 24.00000, 0.239889 ),
( 25.00000, 0.241600 ),
( 26.00000, 0.893973 )
])

  
    nf2 = NumericalFunction()
    nf2.setXAxisName("iteration")
    nf2.setYAxisName("error function")
    nf2.setXYData([
(1.000000, 0.342594 ),
( 2.000000, 0.343426 ),
( 3.000000, 0.299291 ),
( 4.000000, 0.283568 ),
( 5.000000, 0.278841 ),
( 6.000000, 0.278638 ),
( 7.000000, 0.282761 ),
( 8.000000, 0.279919 ),
( 9.000000, 0.278908 ),
( 10.00000, 0.278635 ),
( 11.00000, 0.279853 ),
( 12.00000, 0.279861 ),
( 13.00000, 0.279112 ),
( 14.00000, 0.274873 ),
( 15.00000, 0.256160 ),
( 16.00000, 0.336991 ),
( 17.00000, 0.256315 ),
( 18.00000, 0.240475 ),
( 19.00000, 0.258847 ),
( 20.00000, 0.240296 ),
( 21.00000, 0.239939 ),
( 22.00000, 0.239001 ),
( 23.00000, 0.239689 ),
( 24.00000, 0.239889 ),
( 25.00000, 0.241600 ),
( 26.00000, 0.893973 )
])
    nf2.scaleYValues(0.6)
    onePageData = [("spectrum",[nf,]),("spectrum2",[nf,nf2])] 
    myData = [("title line",onePageData),]
    m = OptimizeReport()
    m.setHeader("Optimizer report: cycle(%d)"%1)
    m.setComment(comment)
    m.setFirstPageGraph( "results", nf )

    page = ReportPage()
    page.setTitle("page 2 data")

    graph = ReportGraph()
    graph.setTitle("top graph")
    graph.addData(nf)
    page.addGraph(graph)
    graph = ReportGraph()
    graph.setTitle("bottom graph")
    graph.addData(nf)
    graph.addData(nf2)
    page.addGraph(graph)

    m.addPageData(page)

    m.generateReport("_report.pdf")

