# setup.py

from distutils.core import setup
import py2exe


opts = {
	"py2exe": {
		"excludes":"OpenGL,OpenGL.GL,OpenGL.GLU,OpenGL.GLUT",
		"packages":"encodings"
	}
}



setup(
	options = opts,
	windows = [ {
		"script": "candoEdit.py",
		"icon_resources": [(1,"cando.ico")]
	}],
	data_files = [  ]
)



