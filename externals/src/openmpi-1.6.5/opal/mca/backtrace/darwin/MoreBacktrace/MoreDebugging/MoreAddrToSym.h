/*
	File:		MoreAddrToSym.h

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

$Log: MoreAddrToSym.h,v $
Revision 1.2  2006/01/22 22:47:15  eskimo1
Significant rewrite to account for changes in MoreBacktrace.  Still not as functional as I'd like.

Revision 1.1  2003/04/04 15:03:00  eskimo1
First checked in.  This code still has bugs, but I've written enough code that checking in is a good idea.


*/

#pragma once

/////////////////////////////////////////////////////////////////

// MoreIsBetter Setup

#include "MoreSetup.h"

// Mac OS Interfaces

#include <stdint.h>
#include <stdlib.h>

/////////////////////////////////////////////////////////////////

#ifdef __cplusplus
extern "C" {
#endif

// IMPORTANT:
// This code is currently broken in two ways:
// 
// o If the binary has debugging symbols, you often get a debugging 
//   symbol rather than a real symbol.  I believe this is caused by 
//   handling of N_FUN in the GetFunctionName routine.  Furthermore, 
//   I believe that it did work previous but something changed in the 
//   Mach-O headers to break it.  This warrants serious investigation. 
//
//   You can work around this problem by removing the debug symbols 
//   using:
//
//   $ strip -S YourBinaryName
//
// o It doesn't handle 64-bit Mach-O binaries.  This is a simple 
//   limitation of the code, and it should be relatively easy to fix.
//
// I intend to fix both of these problems with a future rewrite.  
// Until then, I've decided to live with them.

typedef uint64_t MoreAToSAddr;
typedef uint64_t MoreAToSOffset;

enum MoreAToSSymbolType {
	kMoreAToSNoSymbol = 0,
	kMoreAToSDyldPubliSymbol,
	kMoreAToSDyldPrivateSymbol
};
typedef enum MoreAToSSymbolType MoreAToSSymbolType;

struct MoreAToSSymInfo {
	MoreAToSSymbolType	symbolType;
	const char *		symbolName;
	MoreAToSOffset		symbolOffset;
};
typedef struct MoreAToSSymInfo MoreAToSSymInfo;

extern int  MoreAToSCreate(size_t count, MoreAToSSymInfo *symbols[]);
	// Creates a blank MoreAToSSymInfo array with count entries.
	// You must dispose of it using MoreAToSDestroy.
	// 
	// symbols must not be NULL
	// *symbols must be NULL
	// Returns 0 on success, an errno-style error code otherwise
	// On success, *symbols will not be NULL
	// On error, *symbols will be NULL
	
extern void MoreAToSDestroy(size_t count, MoreAToSSymInfo symbols[]);
	// Destroys a MoreAToSDestroy array (blank or filled in) of 
	// count entries.

extern int MoreAToSCopySymbolNamesUsingDyld(
	size_t			count, 
	MoreAToSAddr 	addresses[],
	MoreAToSSymInfo symbols[]
);
	// Given count values in the addresses array, works out the symbolic 
	// information for those values and returns it in the symbols array. 
	//
	// Returns 0 on success, an errno-style error code otherwise
	//
	// Note that not being able to map an address to a symbol is not 
	// considered an error.  Rather, you get NULL back in the symbolName
	// field of the corresponding symbols array element.

#ifdef __cplusplus
}
#endif
