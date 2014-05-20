/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef _STACK_H
#define _STACK_H

#include <assert.h>
#include "Hash.h"

#include <OTF_Writer.h>


struct struct_StackEntry
{
	int state;
};
typedef struct struct_StackEntry StackEntryT;

struct struct_Stack
{
	/* Stack entries */
	StackEntryT* stack;

	/* Number of entries */
	int n;

	/* Size of the stack */
	int s;
};
typedef struct struct_Stack StackT;

#define FILEIOBUFFERSIZE 2048
typedef struct {
	char *name;
	StackT *stack;
	
	uint64_t fileIObegin; /* writing - timestamp of the last beginfileio */
	
	uint64_t filepos; /* reading - current position in the *.io file */

	uint32_t fileIObufferpos;
	uint32_t fileIObufferlen; /* reading - determines the end of the buffer */
	uint64_t fileIObuffer[FILEIOBUFFERSIZE]; /*1st= timestamp, 2nd= duration*/
}processT;

typedef struct {
	uint32_t id;
	uint32_t size;
	uint32_t *procs;
	char* name;
} ProcessGroup;

typedef struct {
	OTF_Writer* writer;
	processT **processes;
	int processcount;
	int *threadnums;
	ProcessGroup *processgroups;
	unsigned int processgroupcount;
	Hash *pghash; /* processgroup-mapping */
	uint32_t *reservedIds;
	int reservedIdsc;

	char *outputFile;

	int ioonly;

	uint64_t handleid; /* needed for fileio */
}fcbT;


StackT* Stack_new( void );

void Stack_delete( StackT *stack );

void Stack_push( StackT* stack, StackEntryT* entry );

StackEntryT Stack_pop( StackT* stack );


/* write out the io buffer */
void writeFileIOBuffer( int cpuid, processT* proc, const char* outputFile );

/* read from the io buffer */
void readFileIOBuffer( int cpuid, processT* proc, const char* outputFile );

#endif /* _STACK_H */
