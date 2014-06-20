/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Stack.h"


StackT* Stack_new() {


	StackT* ret = NULL;

	/* allocate memory for the stack */
	ret = (StackT*) malloc( sizeof( StackT ) );
	assert( ret != NULL );

	ret->n = 0;
	ret->s = 0;
	ret->stack = 0;

	return ret;
}

void Stack_push( StackT* stack, StackEntryT* entry ) {


	/* realloc ? */
	if ( stack->n >= stack->s )
	{
		stack->s= ( stack->s > 0 ) ? ( 2* stack->s ) : 10;
		stack->stack= (StackEntryT*) realloc( stack->stack,
			stack->s * sizeof(StackEntryT) );
		assert( NULL != stack->stack );
	}

	stack->stack[stack->n] = *entry;

	++(stack->n);

	return;
}

StackEntryT Stack_pop( StackT* stack ) {


	if( stack->n < 1 )
	{
		fprintf( stderr, "ERROR: Stack is empty\n" );
		exit( 1 );
	}

	--(stack->n);

	return stack->stack[stack->n];
}

void Stack_delete( StackT *stack ) {

	if ( 0 != stack ) {
	
		if ( 0 != stack->stack ) {
		
			free( stack->stack );
		}
		
		free( stack );
	}
}


void writeFileIOBuffer( int cpuid, processT* proc, const char* outputFile ) {


	FILE *pf;
	char file[128];


	sprintf( file, "%s.%i.io", outputFile, cpuid );

	pf= fopen( file, "wb" );
	assert(pf);

	fwrite( proc->fileIObuffer, sizeof(uint64_t), proc->fileIObufferpos, pf );

	proc->fileIObufferpos= 0;
}


void readFileIOBuffer( int cpuid, processT* proc, const char* outputFile ) {


	FILE *pf;
	char file[128];


	sprintf( file, "%s.%i.io", outputFile, cpuid );

	pf= fopen( file, "rb" );
	if( NULL != pf ) {

		fseek( pf, proc->filepos * sizeof(uint64_t), SEEK_SET );
	
		proc->fileIObufferlen= fread( proc->fileIObuffer, sizeof(uint64_t), FILEIOBUFFERSIZE, pf );
	
		proc->fileIObufferpos= 0;
		proc->filepos+= proc->fileIObufferlen;
	
		fclose( pf );
	}
}
