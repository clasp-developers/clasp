#! /bin/gawk -f
/InitPython/	{ printf( "	%s; // %s\n", $1, FILENAME );}

