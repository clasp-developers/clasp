#! /bin/gawk -f
BEGIN	{
	    num = 0;
	}

/^InitClasses/	{ 
		inits[num] = $0;
		num = num + 1;
	}

END	{
	    for ( i=0; i<num; i++ ) {
		printf( "/*1*/ extern void %s;\n", inits[i] );
	    }
	    for ( i=0; i<num; i++ ) {
		printf( "/*9*/	%s;\n", inits[i] );
	    }
	    
	}

