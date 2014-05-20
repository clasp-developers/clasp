#! /bin/gawk -f
/InitPython/	{ printf( "void	%s; // %s\n", $1, FILENAME );}

