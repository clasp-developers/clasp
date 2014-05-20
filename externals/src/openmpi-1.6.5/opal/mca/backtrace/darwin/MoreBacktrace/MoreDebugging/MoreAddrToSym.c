/*
	File:		MoreAddrToSym.c

	Contains:	Code for mapping addresses to their symbolic names.

	Written by:	DTS

	Copyright:	Copyright (c) 2006 by Apple Computer, Inc., All Rights Reserved.

	Disclaimer:	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc.
				("Apple") in consideration of your agreement to the following terms, and your
				use, installation, modification or redistribution of this Apple software
				constitutes acceptance of these terms.  If you do not agree with these terms,
				please do not use, install, modify or redistribute this Apple software.

				In consideration of your agreement to abide by the following terms, and subject
				to these terms, Apple grants you a personal, non-exclusive license, under Apple’s
				copyrights in this original Apple software (the "Apple Software"), to use,
				reproduce, modify and redistribute the Apple Software, with or without
				modifications, in source and/or binary forms; provided that if you redistribute
				the Apple Software in its entirety and without modifications, you must retain
				this notice and the following text and disclaimers in all such redistributions of
				the Apple Software.  Neither the name, trademarks, service marks or logos of
				Apple Computer, Inc. may be used to endorse or promote products derived from the
				Apple Software without specific prior written permission from Apple.  Except as
				expressly stated in this notice, no other rights or licenses, express or implied,
				are granted by Apple herein, including but not limited to any patent rights that
				may be infringed by your derivative works or by other works in which the Apple
				Software may be incorporated.

				The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
				WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
				WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
				PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
				COMBINATION WITH YOUR PRODUCTS.

				IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
				CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
				GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
				ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
				OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT
				(INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN
				ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Change History (most recent first):

$Log: MoreAddrToSym.c,v $
Revision 1.2  2006/01/22 22:47:13  eskimo1
Significant rewrite to account for changes in MoreBacktrace.  Still not as functional as I'd like.

Revision 1.1  2003/04/04 15:02:57  eskimo1
First checked in.  This code still has bugs, but I've written enough code that checking in is a good idea.


*/

/////////////////////////////////////////////////////////////////

// Our Prototypes

#include "MoreAddrToSym.h"

// Mac OS Interfaces

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
#include <stdlib.h>
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/stab.h>

// MIB Prototypes

/////////////////////////////////////////////////////////////////

extern int  MoreAToSCreate(size_t count, MoreAToSSymInfo *symbols[])
	// See comments in header.
{
	int err;
	
	assert(symbols != NULL);
	assert(*symbols == NULL);
	
	err = 0;
	*symbols = calloc(count, sizeof(MoreAToSSymInfo));
	if (*symbols == NULL) {
		err = ENOMEM;
	}
	
	assert( (err == 0) == (*symbols != NULL) );
	
	return err;
}

extern void MoreAToSDestroy(size_t count, MoreAToSSymInfo symbols[])
	// See comments in header.
{
	size_t index;

	if (symbols != NULL) {
		for (index = 0; index < count; index++) {
			free( (void *) symbols[index].symbolName );
		}
		free(symbols);
	}
}

static int ReplaceSymbolIfBetter(
	MoreAToSSymInfo *	existingSymbol, 
	MoreAToSSymbolType	symbolType, 
	const char *		symbolName, 
	MoreAToSOffset		symbolOffset
)
	// Check to see whether the symbolic information defined by symbolType, 
	// symbolName, and symbolOffset is 'better' than the symbolic information 
	// in existingSymbol.  If so, replace the old information with the new. 
	// In this case, 'better' is defined as being having a smaller 
	// symbolOffset, that is, we've found a symbol that's closer to the 
	// requested address.
	//
	// The idea behind this code is that one day I'll come up with multiple 
	// ways to get at symbolic information, and then I'll only keep the best 
	// result.  However, I don't really use this functionality yet.
{
	int		err;
	bool	replace;
	
	assert(existingSymbol != NULL);
	assert(symbolType     != kMoreAToSNoSymbol);
	assert(symbolName     != NULL);
	assert( (existingSymbol->symbolType == kMoreAToSNoSymbol) == (existingSymbol->symbolName == NULL) );
	
	err = 0;
	
	if (existingSymbol->symbolType == kMoreAToSNoSymbol) {
		replace = true;
	} else {
		replace = (symbolOffset < existingSymbol->symbolOffset);
	}
	
	if (replace) {
		char *	tmp;
		
		tmp = strdup(symbolName);
		if (tmp == NULL) {
			err = ENOMEM;
		} else {
			free( (void *) existingSymbol->symbolName );
			
			existingSymbol->symbolType   = symbolType;
			existingSymbol->symbolName   = tmp;
			existingSymbol->symbolOffset = symbolOffset;
		}
	}
	
	return err;
}

// FindOwnerOfPC and GetFunctionName countesy of Ed Wynne.  Don't ask me 
// how it works, or I'll start to whimper.  Actually, I have a fairly good idea 
// how it works, and a fairly good idea how to fix its more serious problems, 
// but I just don't have time at the moment.

static const struct mach_header *FindOwnerOfPC(unsigned int pc)
{
	unsigned int				count,index,offset,cmdex;
	struct segment_command *	seg;
	struct load_command	*		cmd;
	const struct mach_header *	header;
	
	count = _dyld_image_count();
	for (index = 0;index < count;index += 1)
	{
		header = _dyld_get_image_header(index);
		offset = _dyld_get_image_vmaddr_slide(index);
		cmd = (struct load_command*)((char*)header + sizeof(struct mach_header));
		for (cmdex = 0;cmdex < header->ncmds;cmdex += 1,cmd = (struct load_command*)((char*)cmd + cmd->cmdsize))
		{
			switch(cmd->cmd)
			{
				case LC_SEGMENT:
					seg = (struct segment_command*)cmd;
					if ((pc >= (seg->vmaddr + offset)) && (pc < (seg->vmaddr + offset + seg->vmsize)))
						return header;
					break;
			}
		}
	}
	
	return NULL;
}

static const char *GetFunctionName(unsigned int pc,unsigned int *offset, bool *publicSymbol)
{
	struct segment_command	*seg_linkedit = NULL;
	struct segment_command	*seg_text = NULL;
	struct symtab_command	*symtab = NULL;
	struct load_command		*cmd;
	const struct mach_header*header;
	unsigned int			vm_slide,file_slide;
	struct nlist			*sym,*symbase;
	char					*strings,*name;
	unsigned int			base,index;
	
	header = FindOwnerOfPC(pc);
	if (header != NULL)
	{
		cmd = (struct load_command*)((char*)header + sizeof(struct mach_header));
		for (index = 0;index < header->ncmds;index += 1,cmd = (struct load_command*)((char*)cmd + cmd->cmdsize))
		{
			switch(cmd->cmd)
			{
				case LC_SEGMENT:
					if (!strcmp(((struct segment_command*)cmd)->segname,SEG_TEXT))
						seg_text = (struct segment_command*)cmd;
					else if (!strcmp(((struct segment_command*)cmd)->segname,SEG_LINKEDIT))
						seg_linkedit = (struct segment_command*)cmd;
					break;
				
				case LC_SYMTAB:
					symtab = (struct symtab_command*)cmd;
					break;
			}
		}
		
		if ((seg_text == NULL) || (seg_linkedit == NULL) || (symtab == NULL))
		{
			*offset = 0;
			return NULL;
		}
		
		vm_slide = (unsigned long)header - (unsigned long)seg_text->vmaddr;
		file_slide = ((unsigned long)seg_linkedit->vmaddr - (unsigned long)seg_text->vmaddr) - seg_linkedit->fileoff;
		symbase = (struct nlist*)((unsigned long)header + (symtab->symoff + file_slide));
		strings = (char*)((unsigned long)header + (symtab->stroff + file_slide));
		
		// Look for a global symbol.
		for (index = 0,sym = symbase;index < symtab->nsyms;index += 1,sym += 1)
		{
			if (sym->n_type != N_FUN)
				continue;
			
			name = sym->n_un.n_strx ? (strings + sym->n_un.n_strx) : NULL;
			base = sym->n_value + vm_slide;
			
			for (index += 1,sym += 1;index < symtab->nsyms;index += 1,sym += 1)
				if (sym->n_type == N_FUN)
					break;
			
			if ((pc >= base) && (pc <= (base + sym->n_value)) && (name != NULL) && (strlen(name) > 0))
			{
				*offset = pc - base;
				*publicSymbol = true;
				return strdup(name);
			}
		}
		
		// Look for a reasonably close private symbol.
		for (name = NULL,base = 0xFFFFFFFF,index = 0,sym = symbase;index < symtab->nsyms;index += 1,sym += 1)
		{
			if ((sym->n_type & 0x0E) != 0x0E)
				continue;
			
			if ((sym->n_value + vm_slide) > pc)
				continue;
			
			if ((base != 0xFFFFFFFF) && ((pc - (sym->n_value + vm_slide)) >= (pc - base)))
				continue;
			
			name = sym->n_un.n_strx ? (strings + sym->n_un.n_strx) : NULL;
			base = sym->n_value + vm_slide;
		}
		
		*offset = pc - base;
		*publicSymbol = false;
		return (name != NULL) ? strdup(name) : NULL;
	}
	
	*offset = 0;
	return NULL;
}

extern int MoreAToSCopySymbolNamesUsingDyld(
	size_t			count, 
	MoreAToSAddr 	addresses[],
	MoreAToSSymInfo symbols[]
)
	// See comments in header.
{
	int 	err;
	size_t 	index;
	
	assert(addresses != NULL);
	assert(symbols != NULL);
	
	err = 0;
	for (index = 0; index < count; index++) {
		const char * 		thisSymbol;
		const char * 		cleanSymbol;
		unsigned int 		thisSymbolOffset;
		bool				thisSymbolPublic;
		MoreAToSSymbolType	thisSymbolType;
		
		thisSymbol = NULL;
		if (addresses[index] != 0) {		// NULL is never a useful symbol
			thisSymbol = GetFunctionName( (unsigned int) addresses[index], &thisSymbolOffset, &thisSymbolPublic);
		}
		if (thisSymbol != NULL) {
		
			// Mach-O symbols virtually always start with '_'.  If there's one there, 
			// let's strip it.
			
			if (thisSymbol[0] == '_') {
				cleanSymbol = &thisSymbol[1];
			} else {
				cleanSymbol = thisSymbol;
			}
			
			if (thisSymbolPublic) {
				thisSymbolType = kMoreAToSDyldPubliSymbol;
			} else {
				thisSymbolType = kMoreAToSDyldPrivateSymbol;
			}
			
			err = ReplaceSymbolIfBetter(&symbols[index], thisSymbolType, cleanSymbol, thisSymbolOffset);
		}
		
		free( (void *) thisSymbol);
		
		if (err != 0) {
			break;
		}
	}
	
	return err;
}
