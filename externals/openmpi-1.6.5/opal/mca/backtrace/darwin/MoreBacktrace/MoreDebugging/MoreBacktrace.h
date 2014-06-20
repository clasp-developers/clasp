/*
	File:		MoreBacktrace.h

	Contains:	Code for generating backtraces.

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

$Log: MoreBacktrace.h,v $
Revision 1.4  2006/01/22 22:46:48  eskimo1
Complete rewrite to account for architecture independence.

Revision 1.3  2003/04/09 22:48:15  eskimo1
Added comments.

Revision 1.2  2003/04/09 21:58:18  eskimo1
Removed the "uncertain PC" flag.  I may need to bring it back later, but for now I never use it so there's no point declaring it.

Revision 1.1  2003/04/04 15:03:08  eskimo1
First checked in.  This code still has bugs, but I've written enough code that checking in is a good idea.


*/

#pragma once

/////////////////////////////////////////////////////////////////

// MoreIsBetter Setup

#include "MoreSetup.h"

// Mac OS Interfaces

// Put <mach/mach.h> inside extern "C" guards for the C++ build 
// because the Mach header files don't always have them.

#if defined(__cplusplus)
	extern "C" {
#endif

#include <mach/mach.h>

#if defined(__cplusplus)
	}
#endif

#include <stdint.h>

/////////////////////////////////////////////////////////////////

#ifdef __cplusplus
extern "C" {
#endif

/////////////////////////////////////////////////////////////////

#if TARGET_RT_MAC_CFM
	#error MoreBacktrace no longer supports CFM builds.
#endif

/////////////////////////////////////////////////////////////////

/*	Overview
	--------
	This module implements a number of backtrace routines. All 
	of the routines are implemented in terms of a common core. 
	The code is structured in a very generic way.  For example, if 
	you were running on a version of Mach that support inter-machine 
	messaging, it would be feasible to do a backtrace of a PowerPC 
	program from program executing a completely different instruction 
	set architecture (ISA).
	
	IMPORTANT:
	The current code does not support cross-architecture backtraces 
	because of technical limitations (specifically, I haven't had 
	time to implement cross-architecture symbol translation, which 
	is required for cross-architecture backtraces to work properly; 
	see the comments for InitSigTrampAddress in the implementation 
	file for the details).
	
	Backtraces are inherently processor-specific.  Internal to this 
	module is a ISA later than adapts the backtrace for various ISA. 
	Currently it supports PowerPC (32-bit), PowerPC (64-bit), and 
	Intel (32-bit).
	
	If you're curious about how stack frames work for each ISA, check 
	out the comments in the implementation file.  The comments in the 
	header focus on how you use these routines.
*/

/*	Changes Since Previous Version
	------------------------------
	If you used the previous version of this code, please note the 
	following changes:
	
	o Everything has changed (-:
	
	I completely rewrote this code to support multiple architectures. 
	By including support for PowerPC (64-bit), I was forced to 
	eliminate my dependencies on CoreServices (which isn't available 
	to PowerPC (64-bit) on Mac OS X 10.4.x), which means now I depend 
	solely on the System framework.  Also, because I had to support 
	Intel, which requires Mach-O, I decided to drop support for CFM. 
	My theory is that anyone who wants to adopt the new version of this 
	module is doing so because they're porting to Intel, and those folks 
	have to leave their CFM build behind.
	
	The good news is that the new implementation is very similar 
	in spirit to the old, and it should be very easy for you to 
	adopt the new code.
*/

// Within this module, I treat all addresses as 64-bit.  That way 
// the code doesn't change for 64-bit and 32-bit architectures.  
//
// Note:
// I could have made this type scale with the current pointer size, 
// but my eventual goal is to support cross-architecture backtraces, 
// which means I wanted a size that's at least as big as the 'largest' 
// architecture I support.

typedef uint64_t MoreBTAddr;

// The following flags provide information about a specific frame in 
// a backtrace.
//
// IMPORTANT:
// kMoreBTFrameBadMask is set for the last frame in the backtrace 
// (where we've run off the end of the stack), but it can also be set 
// for intermediate frames (where we've detected a frameless leaf 
// routine, either at the top of the stack or as part of crossing 
// a signal frame).

typedef int MoreBTFlags;
enum {
	kMoreBTFrameBadMask      = 0x0001,	// this frame pointer is bad
	kMoreBTPCBadMask         = 0x0002,	// this PC is bad
	kMoreBTSignalHandlerMask = 0x0004	// this frame is a signal handler
};

// The end result of a backtrace is an array of MoreBTFrame 
// structures describing a particular frame in the backtrace.  
//
// Note:
// The PC points to the code that's using the frame.  It is not 
// the return address for that code.  On architectures where the 
// frame holds the return address (Intel, but not PowerPC), I do 
// the appropriate corrections.

struct MoreBTFrame {
	MoreBTAddr		pc;					// PC for this function invocation
	MoreBTAddr		fp;					// frame pointer for this function invocation
	MoreBTFlags		flags;				// various flags, see above
};
typedef struct MoreBTFrame MoreBTFrame;

/*	Common Parameters
	-----------------
	All of the backtrace routines accept certain common parameters.
	
	  o function result -- This is an errno-style error code.
	
	  o stackBottom and stackTop -- These define the extent of the stack 
		which you are tracing.  If this information isn't handy, supply 
		0 for both.  Supplying meaningful values can reduce the number 
		of bogus frames reported if the stack is corrupt.
	
	  o frameArray and frameArrayCount -- These define an array of stack 
		frames that the routines fill out.  You can supply NULL and 0 
		(respectively) if you're not interested in getting the actual 
		frame data (typically you do this to get the count of the number 
		of frames via frameCount).  The routines do not fail if this 
		buffer is exhausted.  Instead they simply return as many frames 
		as they can and continue tracing, returning an accurate value 
		for frameCount.
		
	  o frameCountPtr -- You can use this to get back an accurate count of 
		the number of frames in the stack.  If you're not interested 
		in this information, you can pass NULL.
	
	IMPORTANT:
	Because of the above, on return, *frameCountPtr can be larger than 
	frameArrayCount.
	
	The following assertions apply to these common parameters.
	
	o On entry, stackBottom and stackTop must both be zero, or stackBottom 
	  must be strictly less than stackTop.
	
	o On entry, if frameArrayCount is not zero, frameArray must be not NULL.
	
	o On entry, frameCountPtr must not be NULL.
	
	o On return, *frameCountPtr will be the number of frames that were 
	  found.  On success, this is likely to be a meaningful number.  On 
	  error, this just indicates how far we got.
	  
	o On return, if frameArrayCount is not NULL, frame data has been placed 
	  into the frame array.  The number of valid entries is 
	  min(frameArray, *frameArrayCount).
*/

extern int MoreBacktraceMachSelf(
	MoreBTAddr		stackBottom, 
	MoreBTAddr		stackTop,
	MoreBTFrame *	frameArray, 
	size_t			frameArrayCount, 
	size_t *		frameCountPtr
);
	// Does a backtrace of the current thread.  All of the parameters 
	// are described by the "Common Parameters" section above.

extern int MoreBacktraceMachThread(
	task_t			task, 
	thread_t		thread,
	MoreBTAddr		stackBottom, 
	MoreBTAddr		stackTop,
	MoreBTFrame *	frameArray, 
	size_t			frameArrayCount, 
	size_t *		frameCountPtr
);
	// Does a backtrace of a particular thread within a particular tasks. 
	// The common parameters (stackBottom and later) are described by the 
	// "Common Parameters" section above.  The task parameter must a 
	// task control port; for a thread in the current task, use 
	// mach_task_self to get this value.  The thread parameter is the thread 
	// to sample.  It's generally inappropriate to use mach_thread_self 
	// (call MoreBacktraceMachSelf instead), but it does work.
	//
	// On entry, task must not be MACH_PORT_NULL.
	// On entry, thread must not be MACH_PORT_NULL.

#ifdef __cplusplus
}
#endif
