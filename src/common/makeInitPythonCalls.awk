#! /bin/gawk -f
/InitPython[a-zA-Z0-9_]*/	{
    		    fn = $1;
    		    printf( " LOG(\"%s\");\t", fn );
    		    printf( "	%s; \n", fn, FILENAME );
		}

