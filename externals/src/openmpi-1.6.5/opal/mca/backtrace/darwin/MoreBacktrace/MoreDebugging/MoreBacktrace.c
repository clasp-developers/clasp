/*
	File:		MoreBacktrace.c

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

$Log: MoreBacktrace.c,v $
Revision 1.6  2006/01/23 18:32:52  eskimo1
Correct uninitialised variable pointed out by the compiler.

Revision 1.5  2006/01/23 00:59:36  eskimo1
Added lots of comments describing the stack structure for each architecture.  Also filled in one of the blank with regards stack offsets on older systems.

Revision 1.4  2006/01/22 22:46:46  eskimo1
Complete rewrite to account for architecture independence.

Revision 1.3  2003/04/09 22:48:11  eskimo1
Added comments.

Revision 1.2  2003/04/09 22:30:14  eskimo1
Lots of changes.  Rewrote the core to work properly.  We now handle leaf routines correctly in the common cases, and document the cases that we don't handle.  Also added lots of comments.

Revision 1.1  2003/04/04 15:03:04  eskimo1
First checked in.  This code still has bugs, but I've written enough code that checking in is a good idea.


*/

/////////////////////////////////////////////////////////////////

// Our Prototypes

#include "opal_config.h"

#include "MoreBacktrace.h"

// Mac OS Interfaces

#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/utsname.h>
#include <mach-o/arch.h>

#if defined(__cplusplus)
	extern "C" {
#endif

// Some extra Mach interfaces that we don't need in the public header.
// Again, we need C++ guards.

#ifdef HAVE_MACH_MACH_VM_H
#include <mach/mach_vm.h>
#endif
#include <mach/vm_region.h>

// We want both PowerPC and Intel thread state information.
// By default, the system only gives us the one that's appropriate 
// for our machine.  So we include both here.

#include <mach/ppc/thread_status.h>
#include <mach/i386/thread_status.h>

#if defined(__cplusplus)
	}
#endif

/* OMPI CHANGE: This is a total hack, but in OS X 10.5 (Leopard), they
 * renamed all the registers in the thread state structure if
 * __DARWIN_UNIX03 is defined.  So adapt. 
 */
#if !defined(HAVE_PPC_THREAD_STATE_T_SRR0)
#define srr0 __srr0
#define lr   __lr
#define r1   __r1
#define eip  __eip
#define ebp  __ebp
#define esp  __esp
#endif

/////////////////////////////////////////////////////////////////

// A new architecture will require substantial changes to this file.

#if ! (TARGET_CPU_PPC || TARGET_CPU_PPC64 || TARGET_CPU_X86 )
	#error MoreBacktrace: What architecture?
#endif

/////////////////////////////////////////////////////////////////
#pragma mark ***** Generic Utilities

struct OSRelease {
	int major;
	int minor;
	int bug;
};
typedef struct OSRelease OSRelease;

static int GetOSRelease(OSRelease *releasePtr)
	// Get the Darwin OS release using uname.  I can't use gestaltSystemVersion 
	// because it's part of CoreServices, and CoreServices is not available 
	// to 64-bit programs on Mac OS X 10.4.x.
{
	int				err;
	struct utsname	names;
	int				scanResult;
	
	assert(releasePtr != NULL);
	
	err = uname(&names);
	if (err < 0) {
		err = errno;
	}
	if (err == 0) {
		// Parse the three dot separated components of the release string. 
		// If we don't get exactly three, we've confused and we error.
		
		scanResult = sscanf(names.release, "%d.%d.%d", &releasePtr->major, &releasePtr->minor, &releasePtr->bug);
		if (scanResult != 3) {
			err = EINVAL;
		}
	}
	
	assert( (err == 0) == (releasePtr->major != 0) );
	
	return err;
}

/////////////////////////////////////////////////////////////////
#pragma mark ***** Architecture Specification

typedef struct MoreBTContext MoreBTContext;
	// forward declaration

// Architecture Callbacks -- Called by the core to do architecture-specific tasks.

typedef int  (*MoreBTHandleLeafProc)(MoreBTContext *context, MoreBTAddr *pcPtr, MoreBTAddr *fpPtr);
	// This callback is called by the core to start a backtrace. 
	// It should extract the first PC and frame from the thread state 
	// in the context and return them to the core.  Also, if the 
	// routine detects a frameless leaf routine, it should add a 
	// dummy frame for that routine (by calling AddFrame).
	//
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, pcPtr will not be NULL.
	// Returns an errno-style error code,
	// On entry, fpPtr will not be NULL.
	// On success, *pcPtr must be the PC of the first non-leaf frame.
	// On success, *fpPtr must be the frame pointer of the first non-leaf frame.

typedef bool (*MoreBTValidPCProc)(MoreBTContext *context, MoreBTAddr pc);
	// This callback is called by the core to check whether a PC address 
	// is valid.  This is architecture-specific; for example, on PowerPC a 
	// PC value must be a multiple of 4, whereas an Intel an instruction 
	// can start at any address.
	//
	// At a minimum, an implementation is expected to check the PC's alignment 
	// and read the instruction at the PC.
	//
	// IMPORTANT:
	// The core code assumes that (MoreBTAddr) -1 is never a valid PC. 
	// If that isn't true for your architecture, you'll need to eliminate 
	// that assumption from the core.
	// 
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, pc can be any value.
	// Returns true if the PC looks reasonably valid, false otherwise.

typedef int  (*MoreBTGetFrameNextPCProc)(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr nextFrame, MoreBTAddr *nextPCPtr);
	// This callback is called by the core to get the PC associated with 
	// the next frame.  This is necessary because different architectures 
	// store the PC in different places.  Specifically, PowerPC stores 
	// the PC of a frame in that frame, whereas Intel stores the PC of 
	// a frame in the previous frame (that is, the PC value in the frame 
	// is actually a return address).
	//
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, thisFrame will be a valid frame.
	// On entry, nextFrame will be the valid frame following thisFrame.
	// On entry, nextPCPtr will not be NULL.
	// Returns an errno-style error code.
	// On success, *nextPCPtr must be the PC associated with nextFrame.

typedef int  (*MoreBTCrossSignalFrameProc)(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr *nextPCPtr, MoreBTAddr *nextFramePtr);
	// This callback is called by the core when it detects a cross signal 
	// frame and wants to cross that frame in an architecture-specific 
	// manner.  The code gets a pointer to the cross-signal handler frame 
	// and is expected to return the PC and frame pointer of the first 
	// non-leaf frame on the other side.  Furthermore, it must detect if 
	// the first frame on the other side is a leaf frame and add a 
	// dummy frame for that routine (by calling AddFrame) before returning.
	//
	// An implementation does not have to check the validity of the 
	// returned PC and frame.  The core will do that for you.
	// 
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, thisFrame will be a valid cross-signal handler frame.
	// On entry, nextPCPtr will not be NULL.
	// On entry, nextFramePtr will not be NULL.
	// Returns an errno-style error code.
	// On success, *nextPCPtr must be the PC of the next non-leaf frame.
	// On success, *nextFramePtr must be the PC of the next non-leaf frame.

// Architecture Specification Structure -- Aggregates all of the information 
// associated with a particular architecture.

struct MoreBTArchInfo {

	// Information to identify the architecture
	
    cpu_type_t					cputype;					// per NXGetLocalArchInfo in <mach-o/arch.h>
    cpu_subtype_t				cpusubtype;					// per NXGetLocalArchInfo in <mach-o/arch.h>
	bool						is64Bit;

	// Misc information about the architecture
	
	enum NXByteOrder			byteOrder;					// per <architecture/byte_order.h>
	MoreBTAddr					frameAlignMask;				// mask to detect frame misalignment
															// FP & frameAlignMask must be 0 for a valid frame
	// Architecture-specific backtrace callbacks
	
	MoreBTHandleLeafProc		handleLeaf;					// described in detail above
	MoreBTValidPCProc			validPC;					// described in detail above
	MoreBTGetFrameNextPCProc	getFrameNextPC;				// described in detail above
	MoreBTCrossSignalFrameProc	crossSignalFrame;			// described in detail above
	
	// Specification of how to call thread_get_state
	
	thread_state_flavor_t		stateFlavor;
	mach_msg_type_number_t		stateCount;
};
typedef struct MoreBTArchInfo MoreBTArchInfo;

static bool TaskIs64Bits(task_t task)
	// Returns true if the specified task if a 64-bit task.  
	// The current implementation works by looking to see 
	// if the task uses any address space above the 4 GB boundary. 
	// This is less than ideal, but it's the best I can think of
	// right now.
{
	bool result;
	
	assert(task != MACH_PORT_NULL);

	// If mach_vm_region is NULL, we're running prior to 10.4, where no 
	// task can be 64-bit.
	
	result = false;
#ifdef HAVE_MACH_VM_REGION
	if ( mach_vm_region != NULL ) {
		kern_return_t					err;
		mach_vm_address_t				addr;
		mach_vm_size_t					size;
		vm_region_basic_info_data_64_t	info;
		mach_msg_type_number_t			count;
		mach_port_t						junkPort;
		
		// Look for a VM region above 4 GB.  In practice this generally 
		// picks up the stack.  However, at a minimum, this should always 
		// pick up the comm page.  If such a region exists, the task must 
		// be 64-bit.
		
		addr = 0x0000000100000000LL;
		count = VM_REGION_BASIC_INFO_COUNT_64;
		junkPort = MACH_PORT_NULL;
		err = mach_vm_region(
			task,
			&addr,
			&size,
			VM_REGION_BASIC_INFO_64,
			(vm_region_info_t) &info,
			&count,
			&junkPort
		);
		if (err == KERN_SUCCESS) {
			assert(addr >= 0x0000000100000000LL);
			assert(count == VM_REGION_BASIC_INFO_COUNT_64);
			result = true;
		}

		assert(junkPort == MACH_PORT_NULL);
	}
#endif /* HAVE_MACH_VM_REGION */
	return result;
}

static const MoreBTArchInfo kArchitectures[4];
	// forward declaration

static const MoreBTArchInfo * GetTaskArch(task_t task)
	// Returns a pointer to the architecture associated with the specific 
	// task, or NULL if it's an architecture we don't know about. 
	//
	// This is one place that trips up cross-architecture backtraces. 
	// For this to work properly, we have to work out the architecture 
	// of the remote task.  There's no API to do this.  The standard 
	// approach is to find dyld in the remote task and see what 
	// architecture it is (from its Mach image header).
	//
	// However, as I've not yet written code to find dyld (what an 
	// ugly kludge that is), I've installed a less than ideal solution, 
	// which is to assume that the remote task is the same architecture 
	// as the local task, modulo 32- vs 64-bit differences.  This is 
	// fine for most uses.
{
	const NXArchInfo *		localArch;
	bool					targetIs64Bits;
	const MoreBTArchInfo *	result;
	
	assert(task != MACH_PORT_NULL);

	localArch      = NXGetLocalArchInfo();
	assert(localArch != NULL);				// in debug builds, we want to know if this fails
	
	targetIs64Bits = TaskIs64Bits(task);
	
	result = NULL;
	if (localArch != NULL) {
		const MoreBTArchInfo *	thisArch;

		// Look through the architecture array for an architecture 
		// that matches the local architecture, but with the correct 
		// address space size (as determined by TaskIs64Bits, above). 
		// Also, we prefer architectures with an exact CPU subtype 
		// match, but we'll accept those with a 0 CPU subtype.
		
		thisArch = &kArchitectures[0];
		while ( (thisArch->cputype != 0) && (result == NULL) ) {
			if (   (thisArch->cputype == localArch->cputype)
				&& ((thisArch->cpusubtype == 0) || (thisArch->cpusubtype == localArch->cpusubtype))
				&& (thisArch->is64Bit == targetIs64Bits) 
			   ) {
				result = thisArch;
			} else {
				thisArch += 1;
			}
		}
	}

	return result;
}

/////////////////////////////////////////////////////////////////
#pragma mark ***** Backtrace Core

// Memory Read Callback

typedef int (*MoreBTReadBytesProc)(MoreBTContext *context, MoreBTAddr src, void *dst, size_t size);
	// This function pointer is called by the core backtrace code 
	// when it needs to read memory.  The callback should do a safe 
	// read of size bytes from src into the buffer specified by 
	// dst.  By "safe" we mean that the routine should return an error 
	// if the read can't be done (typically because src is a pointer to 
	// unmapped memory).
	//
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, src can be any value.
	// On entry, dst will not be NULL.
	// On entry, size will be greater than 0.
	// Returns an errno-style error code.
	// On success, the routine has copied size bytes of data from the address src 
	// in the remote task to the address dst in the local task.
	// On error, the value at dst is unspecified.
	//
	// Note:
	// In previous versions of MoreBacktrace, I supported alternative ways to 
	// read bytes from the target.  For example, I could use the Carbon 
	// exception handler mechanism <CoreServices/MachineExceptions.h> to read 
	// data from the current task in a safe fashion.  This was useful because 
	// it would work on both Mac OS X and traditional Mac OS.  Now that I 
	// require Mac OS X and Mach-O, I can just use the Mach routines to do 
	// my reading.  However, I've left the abstraction layer in place, 
	// Just In Case (tm).

struct MoreBTContext {

	// Internal parameters that are set up by the caller 
	// of the core backtrace code.
	
	const MoreBTArchInfo *		arch;
	task_t						task;
	const void *				threadState;			// architecture-specific current thread state
														// for example, on Intel this is i386_thread_state_t
	MoreBTReadBytesProc			readBytes;				// described in detail above
	void *						readBytesRefCon;		// currently unused

	// Stuff worked out internally by InitContext.
	
	bool						swapBytes;				// true if target and current task have different byte orders
	MoreBTAddr					sigTrampLowerBound;		// address of _sigtramp in target
	MoreBTAddr					sigTrampUpperBound;
	
	// Parameters from client.
	
	MoreBTAddr		stackBottom;
	MoreBTAddr		stackTop;
	MoreBTFrame *	frameArray;				// array contents filled out by core
	size_t			frameArrayCount;
	size_t			frameCountOut;			// returned by core
};

static bool ValidateContext(const MoreBTContext *context)
{
	return (context != NULL)
		&& (context->arch != NULL)
		&& (context->task != MACH_PORT_NULL)
		&& (context->readBytes != NULL)
		&& (context->sigTrampLowerBound != 0)
		&& (context->sigTrampLowerBound < context->sigTrampUpperBound)
		&& (context->stackBottom <= context->stackTop)
		&& ((context->frameArrayCount == 0) || (context->frameArray != NULL))
		&& (context->threadState != NULL);
}

static int ReadAddr(MoreBTContext *context, MoreBTAddr addr, MoreBTAddr *valuePtr)
	// Reads an address (that is, a pointer) from the target task, 
	// returning an error if the memory is unmapped.
	//
	// On entry, context will be a valid context (as determined by ValidateContext).
	// On entry, addr can be any value.
	// On entry, valuePtr must not be NULL.
	// Returns an errno-style error code.
	// On success, *valuePtr will be the value of the pointer stored at addr in 
	// the target task.
{
	int			err;
	MoreBTAddr	value;
	
	assert(ValidateContext(context));
	assert(valuePtr != NULL);
	
	if (context->arch->is64Bit) {

		// Read directly into value, and then swap all 8 bytes.
		
		err = context->readBytes(context, addr, &value, sizeof(value));
		if (err == 0) {
			if (context->swapBytes) {
				value = OSSwapInt64(value);
			}
		}
	} else {
		uint32_t	tmpAddr;
		
		// Read into a temporary address, swap 4 bytes, then copy that 
		// into value.  tmpAddr is unsigned, so we zero fill the top 
		// 32 bits.
		
		err = context->readBytes(context, addr, &tmpAddr, sizeof(tmpAddr));
		if (err == 0) {
			if (context->swapBytes) {
				tmpAddr = OSSwapInt32(tmpAddr);
			}
			value = tmpAddr;
		}
	}
	if (err == 0) {
		*valuePtr = value;
	}
	
	return err;
}

static void AddFrame(MoreBTContext *context, MoreBTAddr pc, MoreBTAddr fp, MoreBTFlags flags)
	// Adds a frame to the end of the output array with the 
	// value specified by pc, fp, and flags.
	//
	// On entry, context will be a valid context (as determined by ValidateContext).
{
	// Only actually output the frame if the client supplied an array 
	// and we we haven't filled it up yet.
	
	assert(ValidateContext(context));
	
	if ( (context->frameArray != NULL) && (context->frameCountOut < context->frameArrayCount) ) {
		MoreBTFrame *	frameOutPtr;

		frameOutPtr = &context->frameArray[context->frameCountOut];
		frameOutPtr->pc    = pc;
		frameOutPtr->fp    = fp;
		frameOutPtr->flags = flags;
	}
	
	// Always increment the frame count.
	
	context->frameCountOut += 1;	
}

static int InitSigTrampAddress(MoreBTContext *context)
	// Fills out the sigTrampLowerBound and sigTrampUpperBound fields of 
	// the context to be the address and bound of the _sigtramp routine.  
	// I need this information to be able to detect and cross signal 
	// frames correctly.
	//
	// Doing this correctly requires the ability to map symbols to addresses 
	// in the remote task, something for which I haven't yet written the code.  
	// So, we just make some risky assumptions:
	//
	// o _sigtramp is at the same address in the target task as it is in 
	//   our task.  This would fail if the target task was a different 
	//   architecture, so we outlaw that.  It can also fail if System 
	//   frameworks was relocated in the target task, something that's 
	//   possible and, with my current code base, hard to detect.
	//
	// o I assume a fixed length of 0x100 for the _sigtramp code.  Needless to 
	//   say, this is completely bogus.
	//
	// On entry, context isn't yet a fully valid context because the 
	// sigtramp fields haven't been set up yet.
	// Returns an errno-style error code.
	// On success, context is a valid context.
{
	int	err;
	extern void _sigtramp(void);
	
	assert(context != NULL);
	
	if ( context->arch != GetTaskArch(mach_task_self()) ) {
		fprintf(stderr, "MoreBacktrace: Cross architecture backtrace not supported because of problems finding _sigtramp.\n");
		err = ENOTSUP;
	} else {
		context->sigTrampLowerBound = (uintptr_t) &_sigtramp;
		context->sigTrampUpperBound = context->sigTrampLowerBound + 0x100;
		err = 0;
	}
	
	assert( (err != 0) || ValidateContext(context) );

	return err;
}

static OSRelease gOSRelease;

static int InitContext(
	MoreBTContext *			context,
	const MoreBTArchInfo *	arch,
	task_t					task,
	const void *			threadState,
	MoreBTReadBytesProc		readBytes,
	void *					readBytesRefCon,
	MoreBTAddr				stackBottom, 
	MoreBTAddr				stackTop,
	MoreBTFrame *			frameArray, 
	size_t					frameArrayCount
)
	// Initialises a MoreBTContext to appropriate values based on 
	// the input parameters and various default values and values 
	// derived from the input parameters.
	//
	// On entry, context must not be NULL (but it's not yet a valid context).
	// On entry, arch must not be NULL.
	// On entry, task must not be MACH_PORT_NULL.
	// On entry, threadState must not be NULL.
	// On entry, readBytes must not be NULL.
	// Returns an errno-style error code.
	// On success, context will be a valid context.
{
	int err;

	assert(context != NULL);
	assert(arch != NULL);
	assert(task != MACH_PORT_NULL);
	assert(threadState != NULL);
	assert(readBytes != NULL);
		
	memset(context, 0, sizeof(*context));
	
	// We don't check these input parameters here.  Instead the 
	// check is done by the ValidateContext call below.
	
	context->stackBottom     = stackBottom;
	context->stackTop        = stackTop;
	context->frameArray      = frameArray;
	context->frameArrayCount = frameArrayCount;
	
	context->arch            = arch;
	context->task            = task;
	context->threadState     = threadState;
	context->readBytes       = readBytes;
	context->readBytesRefCon = readBytesRefCon;
	
	context->swapBytes = (arch->byteOrder != NXGetLocalArchInfo()->byteorder);

	err = 0;
	if (gOSRelease.major == 0) {
		err = GetOSRelease(&gOSRelease);
	}
	if (err == 0) {
		err = InitSigTrampAddress(context);
	}

	assert( (err != 0) || ValidateContext(context) );
	
	return err;
}

static int BacktraceCore(MoreBTContext *context)
	// The core backtrace code.  This routine is called by all of the various 
	// exported routines.  It implements the core backtrace functionality. 
	// All of the parameters to this routine are contained within 
	// the context.  This routine traces back through the stack (using the 
	// readBytes callback in the context to actually read memory) creating 
	// a backtrace.
{
	int				err;
	MoreBTAddr		thisPC;
	MoreBTAddr		thisFrame;
	MoreBTAddr		lowerBound;
	MoreBTAddr		upperBound;
	bool			stopNow;
	
	assert(ValidateContext(context));
	
	lowerBound = context->stackBottom;
	upperBound = context->stackTop;
	if (upperBound == 0) {
		if (context->arch->is64Bit) {
			// This actually generates a theoretical off-by-one error (a fp of 
			// 0xFFFFFFFF FFFFFFFF is falsely considered invalid), but that's 
			// not a problem in practice.
			upperBound = 0xFFFFFFFFFFFFFFFFLL;
		} else {
			upperBound = 0x0000000100000000LL;
		}
	}
	
	// If you supply bounds, they must make sense.
	
	assert(upperBound >= lowerBound);

	// Handle any leaf frames, and also return to us the initial 
	// PC and FP.

	assert(context->frameCountOut == 0);			// set up by memset in InitContext
	err = context->arch->handleLeaf(context, &thisPC, &thisFrame);
	
	// Handle the normal frames.
	
	if (err == 0) {
		stopNow = false;
		do {
			MoreBTFrame *   frameOutPtr;
			MoreBTFrame		tmpFrameOut;
			MoreBTAddr 		nextFrame;
			MoreBTAddr 		nextPC;
			
			// Output to a tmpFrameOut unless the client has supplied 
			// a buffer and there's sufficient space left in it.
			//
			// IMPORTANT:
			// You can't just add the frame information (possibly by calling 
			// AddFrame) at the end of this loop, because the crossSignalFrame 
			// callback may add its own frame, and we have to make sure that 
			// this frame is allocated before that one.
			
			if ( (context->frameArray != NULL) && (context->frameCountOut < context->frameArrayCount) ) {
				frameOutPtr = &context->frameArray[context->frameCountOut];
			} else {
				frameOutPtr = &tmpFrameOut;
			}
			context->frameCountOut += 1;

			// Record this entry.
			
			frameOutPtr->pc    = thisPC;
			frameOutPtr->fp    = thisFrame;
			frameOutPtr->flags = 0;
			
			// Now set the flags to indicate the validity of specific information. 
			
			// Check the validity of the PC.  Don't set the err here; a bad PC value 
			// does not cause us to quit the backtrace.
			
			if ( ! context->arch->validPC(context, thisPC) ) {
				frameOutPtr->flags |= kMoreBTPCBadMask;
			} else {
				// On PowerPC I used to report the address of the call, 
				// rather than the return address.  That was easy: I just 
				// decremented the returned PC by 4.  However, this is 
				// much harder on Intel, where instructions are of variable 
				// length.  So, I decided to do what Apple's tools do, 
				// and just report the return address.
			}
			
			// Check the validity of the frame pointer.  A bad frame pointer *does* 
			// cause us to stop tracing.
			
			if (	(thisFrame == 0) 
				 || (thisFrame & context->arch->frameAlignMask) 
				 || (thisFrame < lowerBound) 
				 || (thisFrame >= upperBound) 
			   ) {
				frameOutPtr->flags |= kMoreBTFrameBadMask;
				stopNow = true;
			}

			if ( (err == 0) && ! stopNow) {
				
				// Move to the next frame, either by crossing a signal handler frame 
				// or by the usual mechanism.
				
				if (	!(frameOutPtr->flags & kMoreBTPCBadMask) 
					  && ( thisPC >= context->sigTrampLowerBound ) 
					  && ( thisPC <  context->sigTrampUpperBound ) 
				   ) {

					// If this frame is running in _sigtramp, get nextPC and nextFrame 
					// by delving into the signal handler stack block.

					frameOutPtr->flags |= kMoreBTSignalHandlerMask;
					err = context->arch->crossSignalFrame(context, thisFrame, &nextPC, &nextFrame);

				} else {
				
					// Read the next frame pointer.  A failure here causes us to quit 
					// backtracing.  Note that we set kMoreBTFrameBadMask in frameOutPtr 
					// because, if we can't read the contents of the frame pointer, the 
					// frame pointer itself must be bad.
					
					err = ReadAddr(context, thisFrame, &nextFrame);
					if (err != 0) {
						frameOutPtr->flags |= kMoreBTFrameBadMask;
						nextFrame = (MoreBTAddr) -1;
						// No need to set stopNow because err != 0 will 
						// terminate loop.
					}
					
					// Also get the PC of the next frame, or set it to dummy value if 
					// there is no next frame or we can't get the PC from that frame.

					if (	(frameOutPtr->flags & kMoreBTFrameBadMask) 
						 || (context->arch->getFrameNextPC(context, thisFrame, nextFrame, &nextPC) != 0) 
					   ) {
						nextPC = (MoreBTAddr) -1;		// an odd value, to trigger above check on next iteration
					}
				}

				// Set up for the next iteration.
				
				if (err == 0) {
					lowerBound = thisFrame;
					thisPC     = nextPC;
					thisFrame  = nextFrame;
				}
			}
		} while ( (err == 0) && ! stopNow );
	}

	assert(ValidateContext(context));
	
	return err;
}

#pragma mark ***** Mach Infrastructure

static int MachReadBytes(MoreBTContext *context, MoreBTAddr src, void *dst, size_t size)
	// A memory read callback for Mach.  This simply calls through 
	// to the Mach [mach_]vm_read primitive, which does more-or-less 
	// what we want.
	//
	// See the description of MoreBTReadBytesProc for information about 
	// the parameters.
{
	int						err;
	int						junk;
	vm_offset_t				dataRead;
	mach_msg_type_number_t	sizeRead;
	
	assert(ValidateContext(context));
	assert(dst != NULL);
	assert(size > 0);

	// I used to use mach_vm_read_overwrite, which has a better semantic match for 
	// what I'm trying to do than mach_vm_read, but it has some serious problems 
	// on some systems (at least Mac OS X 10.4.4 on PowerPC G4 and G5).  So I've 
	// reverting to using [mach_]vm_read, which means I have to vm_deallocate 
	// the space afterwards.  Such is life, kerplunk.
	
#ifdef HAVE_MACH_VM_READ
	if (mach_vm_read != NULL) {
		err = mach_vm_read(
			context->task,
			src,
			size,
			&dataRead,
			&sizeRead
		);
	} else 
#endif /* HAVE_MACH_VM_READ */
        {
		#if MORE_DEBUG
			// If I'm running 32-bits, vm_read's arguments are only 32-bits, 
			// and thus an attempt to read a 64-bit address is bad.  This 
			// should never happen because systems that support 64-bit 
			// addresses also support mach_vm_read.  But if it does happen, 
			// I want to know about it (and investigate what's going on).
			
			if ( ! TaskIs64Bits(mach_task_self()) ) {
				assert( (src & 0xFFFFFFFF00000000LL) == 0 );
				assert( ((src + size) & 0xFFFFFFFF00000000LL) == 0 );
			}
		#endif
		
		err = vm_read(
			context->task,
			(vm_address_t) src,
			size,
			&dataRead,
			&sizeRead
		);
	}
	if (err == 0) {
		if (sizeRead != size)  {
			err = KERN_FAILURE;
		} else {
			memcpy(dst, (void *) dataRead, size);
		}
		
		// Note that I can use vm_deallocate instead of mach_vm_deallocate because 
		// I know that the thing I'm deallocating is in the my address space, and 
		// thus vm_deallocate, whose parameters scale with the caller's address space, 
		// is just fine.  mach_vm_deallocate would work just as well, but that would 
		// put another unnecessary dependency on Mac OS X 10.4.
		
		junk = vm_deallocate(mach_task_self(), dataRead, sizeRead);
		assert(junk == 0);
	}
	return err;
}

static int MoreBacktraceMach(
	const MoreBTArchInfo *	arch,
	task_t					task, 
	const void *			threadState,
	MoreBTAddr				stackBottom, 
	MoreBTAddr				stackTop,
	MoreBTFrame *			frameArray, 
	size_t					frameArrayCount, 
	size_t *				frameCountPtr
)
	// Common code for the two exported backtrace routines. 
	// Backtraces a given task, of a given architecture, starting 
	// with the specified thread state.  The other parameters 
	// are directly from the client.
	//
	// Returns an errno-style error code.
{
	int				err;
	MoreBTContext	context;

	assert(arch != NULL);
	assert(task != MACH_PORT_NULL);
	assert(threadState != NULL);
	assert( ((stackBottom == 0) && (stackBottom == stackTop)) || (stackBottom < stackTop) );
	assert( (frameArrayCount == 0) || (frameArray != NULL) );
	assert( frameCountPtr != NULL );
	
	// Create the context, do the backtrace, and return the frame count.
	
	err = InitContext(
		&context,
		arch,
		task,
		threadState,
		MachReadBytes,
		NULL,
		stackBottom,
		stackTop,
		frameArray,
		frameArrayCount
	);
	if (err == 0) {
		err = BacktraceCore(&context);
	}
	*frameCountPtr = context.frameCountOut;

	return err;
}

#pragma mark ***** CPU Specific

#pragma mark - PowerPC

/*	PowerPC Stack Frame Basics
	--------------------------
	
	
						Offset	Size	Purpose
						------	----	-------
	low memory
	fp == sp == r1 ->	0		X		pointer to next frame
						X		X		place to save CR
						2X		X		place to save LR
						3X		2X		reserved
						5X		X		place to save TOC (CFM only)
	high memory
	
	
						where X is the address size (4 bytes for 32-bits, 
						8 bytes for 64-bits)
					
	To get from one frame to the next, you have to indirect an offset 
	of 0.  To extract the PC from a frame (which, notably, is the 
	address of the code running in that frame, not a return address), you 
	have to indirect an offset of 2X bytes (8 or 16).
	
	There's enough commonality between 32- and 64-bit PowerPC architectures 
	that it's easy to handle them both with the same code.
*/

static bool PowerPCIsSystemCall(MoreBTContext *context, MoreBTAddr pc)
	// Using the PC from the thread state, walk back through 
	// the code stream for 3 instructions looking for a "sc" instruction. 
	// If we find one, it's almost certain that we're in a system call 
	// frameless leaf routine.
{
	int			err;
	bool		isSystemCall;
	int			count;
	uint8_t		inst[4];
	
	isSystemCall = false;
	count = 0;
	do {
		err = context->readBytes(context, pc, &inst, sizeof(inst));
		if (err == 0) {
			isSystemCall = (inst[0] == 0x44)		// PPC "sc" instruction
			            && (inst[1] == 0x00)		// PPC instructions are always big 
						&& (inst[2] == 0x00)		// endian, so we compare it byte at 
						&& (inst[3] == 0x02);		// time for endian neutrality
						
		}
		if ( (err == 0) && ! isSystemCall ) {
			count += 1;
			pc -= sizeof(inst);
		}
	} while ( (err == 0) && ! isSystemCall && (count < 3) );
	err = 0;
	
	return isSystemCall;
}

static int PowerPCHandleLeaf(MoreBTContext *context, MoreBTAddr *pcPtr, MoreBTAddr *framePtr)
	// This is the handleLeaf routine for the PowerPC 
	// architecture.  See the description of MoreBTHandleLeafProc 
	// for a detailed discussion of its parameters.
	//
	// The top most frame may be in a weird state because of the 
	// possible variations in the routine prologue.  There are a 
	// variety of combinations, such as:
	//
	// 1. a normal routine, with its return address stored in 
	//    its caller's stack frame
	//
	// 2. a system call routine, which is a leaf routine with 
	//    no frame and the return address is in LR
	//
	// 3. a leaf routine with no frame, where the return address 
	//    is in LR
	//
	// 4. a leaf routine with no frame that accesses a global, where 
	//    the return address is in r0
	//
	// 5. a normal routine that was stopped midway through 
	//    constructing its prolog, where the return address is 
	//    typically in r0
	//
	// Of these, 1 and 2 are most common, and they're the cases I 
	// handle.  General support for all of the cases requires the 
	// ability to accurately determine the start of the routine 
	// which is not something that I can do with my current 
	// infrastructure.
	//
	// Note that don't handle any cases where the return address is 
	// in r0, although r0 is available as part of the threadState 
	// if I need it in the future.
{
	int			err;
	MoreBTAddr	pc;
	MoreBTAddr	lr;
	
	// Get the pc and lr from the thread state.
	
	if (context->arch->is64Bit) {
		pc = ((const ppc_thread_state64_t *) context->threadState)->srr0;
		lr = ((const ppc_thread_state64_t *) context->threadState)->lr;
	} else {
		pc = ((const ppc_thread_state_t *) context->threadState)->srr0;
		lr = ((const ppc_thread_state_t *) context->threadState)->lr;
	}

	// If we find that we're in a system call frameless leaf routine, 
	// add a dummy stack frame (with no frame, because the frame actually 
	// belows to frameArray[1]).

	err = 0;
	if ( PowerPCIsSystemCall(context, pc) ) {

		AddFrame(context, pc, 0, kMoreBTFrameBadMask);

		pc = lr;
	}

	// Pass the initial pc and frame back to the caller.
	
	*pcPtr = pc;
	if (context->arch->is64Bit) {
		*framePtr = ((const ppc_thread_state64_t *) context->threadState)->r1;
	} else {
		*framePtr = ((const ppc_thread_state_t *) context->threadState)->r1;
	}

	return err;
}

static bool  PowerPCValidPC(MoreBTContext *context, MoreBTAddr pc)
	// This is the validPC routine for the PowerPC 
	// architecture.  See the description of 
	// MoreBTValidPCProc for a detailed discussion 
	// of its parameters.
	//
	// PowerPC instructions must be word aligned.  Also, I check that 
	// it's possible to read the instruction.  I don't do anything 
	// clever like check that the resulting value is a valid instruction.
{
	uint32_t	junkInst;
	
	return ((pc & 0x03) == 0) && (context->readBytes(context, pc, &junkInst, sizeof(junkInst)) == 0);
}

static int PowerPCGetFrameNextPC(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr nextFrame, MoreBTAddr *nextPCPtr)
	// This is the getFrameNextPC routine for the PowerPC 
	// architecture.  See the description of 
	// MoreBTGetFrameNextPCProc for a detailed discussion 
	// of its parameters.
{
	MoreBTAddr	offset;
	
	if ( context->arch->is64Bit ) {
		offset = 16;
	} else {
		offset = 8;
	}
	
	return ReadAddr(context, nextFrame + offset, nextPCPtr);
}

/*	PowerPC Signal Stack Frames
	---------------------------
	In the current Mac OS X architecture, there is no guaranteed reliable 
	way to backtrace a PowerPC signal stack frame.  The problem is that the 
	kernel pushes a variable amount of data on to the stack when it invokes the 
	user space signal trampoline (_sigtramp), and the only handle to the 
	information about how much data was pushed is passed in a register 
	parameter to _sigtramp.  _sigtramp stashes that value away in a 
	non-volatile register.  So, when _sigtramp calls the user-supplied 
	signal handler, there's no way to work out where that register 
	ends up being saved.
	
	Thus, we devolve into guesswork.  It turns out that the offset from 
	the stack of the kernel data to the information we need (the place 
	where the interrupted thread's registers were stored) is a (relatively) 
	constant for any given system release.  So, we can just simply add the 
	appropriate offset to the frame pointer and grab the data we need.
	
	On recent systems (10.3 and later) this fails if the signal handle 
	requests 'dual contexts', that is, it requests both 32- and 64-bit 
	PowerPC registers.  In that case, the size of the pushed data changes, 
	and that affects the relative alignment of the data and the stack 
	pointer, and things break.  I don't know of any way to work around 
	this <rdar://problem/4411774>.
	
	Finally, these constant vary from release to release. 
	This code handles the significant cases that I know about (Mac OS X 10.1.x 
	and earlier, Mac OS X 10.2, and Mac OS 10.3 and later), but there's no 
	guarantee that this offset won't change again in the future.

	When the kernel invokes the user space signal trampoline, it pushes 
	the following items on to the stack.
	
	Mac OS X 10.1.x
	---------------
					Size	Purpose
					----	-------
	low memory
					0x030   bytes for C linkage
					0x040 	bytes for saving PowerPC parameters
					0x0c0	ppc_saved_state
					0x110	ppc_float_state
					0x018	struct sigcontext
					0x0e0	red zone
	high memory
					The previous frame's SP is at offset 0x00C within 
					ppc_saved_state, which makes it equal to 
					0x030 + 0x040 + 0x00C, or 0x07C.  The offset to 
					the previous PC (0x84) follows from that.
				                   			 
	Mac OS X 10.2.x
	---------------
					Size	Purpose
					----	-------
	low memory
					0x030   bytes for C linkage
					0x040 	bytes for saving PowerPC parameters
					0x008	alignment padding
					0x408   struct mcontext, comprised of:
								 0x020 ppc_exception_state_t
								 0x0A0 ppc_thread_state_t
								 0x108 ppc_float_state_t
								 0x240 ppc_vector_state_t
					0x040	siginfo_t
					0x020	ucontext
					0x0e0	red zone
	high memory	
					The previous frame's SP is at offset 0x00C within 
					ppc_thread_state_t, which it equal to 
					0x030 + 0x040 + 0x008 + 0x020 + 0x00C, or 0x0A4. 
					The offsets to the previous PC and LR (0x98 and 0x128) 
					follow from that.

	Mac OS X 10.3.x and 10.4.x
	--------------------------
					Size, 32	Size, 64	Purpose
					--------	--------	-------
	low memory
					align16		align32		alignment
					0x030		0x030		bytes for C linkage
					0x040		0x040		bytes for saving PowerPC parameters
					0x008		0x018		alignment
					0x040		0x068		siginfo_t, user_siginfo_t
					0x020		0x038		ucontext64
					0x408	   [0x408]		mcontext
				   [0x498]		0x498		mcontext64
				    align16		align32		alignment
					0x0e0		0x140		redzone
	high memory	
					Some things to note about the above diagram:

					o The items in square brackets are only pushed if the signal 
					  handler requests dual contexts.

					o For a 64-bit process, the kernel aligns the stack to a 
					  32 byte boundary, even though the runtime architecture 
					  only requires a 16 byte boundary.
					  
					o The final alignment is done last, but the space that it 
					  creates is effectively created between the parameter save 
					  area and the [user_]siginfo_t because the C linkage area 
					  and param save areas are both defined to be a fixed offset 
					  from the frame pointer.
					
					On 32-bit, the previous PC is stored at offset 0x18 within 
					the siginfo_t and the previous SP is stored at offset 0x024. 
					So the total offset is 0x030 + 0x040 + 0x008 + 0x018/0x024, 
					or 0x090 and 0x09C, respectively.
					
					On 64-bit, the previous PC is stored at offset 0x018 within 
					the user_siginfo_t and the previous SP is stored at offset 0x030. 
					So the total offset is 0x030 + 0x040 + 0x018 + 0x018/0x030, 
					or 0x0A0 and 0x0B8, respectively.
					
					To get the previous LR (necessary for tracing through 
					frameless leaf routines that are interrupted by a signal, 
					most notably system calls), you have to delve even further up  
					the stack, into the mcontext structures.  I won't bore you
					with the details.
*/

static int PowerPCCrossSignalFrame(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr *nextPCPtr, MoreBTAddr *nextFramePtr)
	// This is the crossSignalFrame routine for the PowerPC 
	// architecture.  See the description of MoreBTCrossSignalFrameProc 
	// for a detailed discussion of its parameters.
{
	int	err;
	MoreBTAddr	nextFrame;
	MoreBTAddr	offsetToPC;
	MoreBTAddr	offsetToFP;
	MoreBTAddr  offsetToLR;
	
	assert(gOSRelease.major != 0);

	if (context->arch->is64Bit) {
		offsetToPC = 0xa0;
		offsetToFP = 0xb8;
		offsetToLR = 0x260;
	} else {
		if ( gOSRelease.major < 6 ) {				// Darwin 6 == Mac OS X 10.2
			// 10.0 through 10.1.x
			assert(false);							// these values haven't been tested
			offsetToPC = 0x84;
			offsetToFP = 0x7c;
			// offsetToLR = ?;
		} else if ( gOSRelease.major < 7 ) {		// Darwin 7 == Mac OS X 10.3
			// Mac OS X 10.2.x
			assert(false);							// these values haven't been tested
			offsetToPC = 0x98;
			offsetToFP = 0xa4;
			offsetToLR = 0x128;
			// What about G5 10.2.x systems?  It's probably the same 
			// as 10.3, but I'm not sure and, even if I was, I have no 
			// idea how to detect such a system at runtime.
		} else {
			// Mac OS X 10.3 and later
			offsetToPC = 0x90;
			offsetToFP = 0x9c;
			offsetToLR = 0x188;
		}
	}
	
	// Read the address of the frame below the _sigtramp frame, because 
	// that where all the action is.
	
	err = ReadAddr(context, thisFrame, &nextFrame);
	
	// Go grab the saved PC and SP.
	
	if (err == 0) {
		err = ReadAddr(context, nextFrame + offsetToPC, nextPCPtr);
	}
	if (err == 0) {
		err = ReadAddr(context, nextFrame + offsetToFP, nextFramePtr);
	}
	
	// If the PC is a system call, add a dummy leaf for that PC 
	// and then get the next frame's PC from LR.
	
	if ( (err == 0) && PowerPCIsSystemCall(context, *nextPCPtr) ) {
		AddFrame(context, *nextPCPtr, 0, kMoreBTFrameBadMask);
		
		err = ReadAddr(context, nextFrame + offsetToLR, nextPCPtr);
	}
	
	return err;
}

#ifdef CPU_TYPE_X86
#pragma mark - Intel

/*	Intel Stack Frame Basics
	------------------------
	
					Offset	Size	Purpose
					------	----	-------
	low memory
	sp == esp ->    -??     ??		general work area
					-??		??		local variables
	fp == ebp ->	0		4		pointer to next frame
					4		4		return address
					-??		??		parameters
	high memory

	The stack frame on Intel is remarkably traditional.  Two registers 
	are used to manage the stack: esp points to the bottom of the stack, 
	and ebp points to the stack frame itself.  The memory at offset 0 
	stores the address of the next stack frame.  The memory at offset 4 
	stores the saved PC for the next stack frame (that is, the return 
	address for this stack frame).
*/

static bool IntelIsSystemCall(MoreBTContext *context, MoreBTAddr pc)
	// Using the PC from the thread state, look back in the code 
	// stream to see if the previous bytes look something like a 
	// system call.  This is a heuristic rather than solid design. 
	// Because Intel instructions are of variable length, there's no 
	// guarantee that these bytes are part of some other instruction. 
	// Still, it works most of the time.
	//
	// The instruction's were looking for are the two system call 
	// primitives on Mac OS X:
	//
	// o INT 81 is used for Mach system calls
	// o sysenter is used by BSD system calls
	// 
	// We detect INT 81 simply by looking for its bytes.  It's no 
	// so easy to detect sysenter, because the PC we get is an 
	// address in the specific system call, which actually calls 
	// another routine (_sysenter_trap) to do the sysenter.  
	// We look for the CALL disp32 instruction and, if we see, 
	// work out the address that it calls.  We then get the 
	// instructions from that address.  If that looks like a 
	// sysenter, we're probably looking at a system call.
{
	int	err;
	bool		isSystemCall;
	uint8_t		buf[5];
	uint32_t	sysEnterOffset;

	isSystemCall = false;
	err = context->readBytes(context, pc - sizeof(buf), buf, sizeof(buf));
	if (err == 0) {
		isSystemCall = ( buf[3] == 0xcd && buf[4] == 0x81);				// INT 81
		
		if ( ! isSystemCall && (buf[0] == 0xe8) ) {						// CALL disp32
			// Get the disp32.
			
			sysEnterOffset = (buf[1] | (buf[2] << 8) | (buf[3] << 16) | (buf[4] << 24));

			// Read the instructions at that offset from the PC and see if they're 
			// the standard _sysenter_trap code.
			//
			// It's a happy coincidence that the size of the _sysenter_trap code is 
			// 5 bytes, which is also the size of the buffer that I have lying around 
			// to read the instructions in front of the PC.  The upshot is that I can 
			// reuse buf rather than needing a second one.
			
			err = context->readBytes(context, pc + sysEnterOffset, buf, sizeof(buf));
			if (err == 0) {
				isSystemCall = (buf[0] == 0x5a)								// pop      %edx
							&& (buf[1] == 0x89)	&& (buf[2] == 0xe1)			// mov      %esp,%ecx
							&& (buf[3] == 0x0f) && (buf[4] == 0x34);		// sysenter
			}
		}
	}
	return isSystemCall;
}

static int IntelHandleLeaf(MoreBTContext *context, MoreBTAddr *pcPtr, MoreBTAddr *framePtr)
	// This is the handleLeaf routine for the Intel 
	// architecture.  See the description of MoreBTHandleLeafProc 
	// for a detailed discussion of its parameters.
	// 
	// I don't have the experience or the time to fully analyse 
	// the leaf routine problem for Intel.  Rather, I just implemented 
	// a simple system call check, much like I did on PowerPC.  This 
	// seems to be effective in the cases that I care about.
{
	int			err;
	MoreBTAddr	pc;
	
	pc = ((const i386_thread_state_t *) context->threadState)->eip;

	// If the PC is a system call, add a dummy leaf for that PC 
	// and then get the next frame's PC from the top of stack.

	err = 0;
	if ( IntelIsSystemCall(context, pc) ) {
		AddFrame(context, pc, 0, kMoreBTFrameBadMask);

		err = ReadAddr(context, ((const i386_thread_state_t *) context->threadState)->esp, &pc);
	}
	if (err == 0) {
		*pcPtr = pc;
		*framePtr = ((const i386_thread_state_t *) context->threadState)->ebp;
	}
	
	return err;
}

static bool  IntelValidPC(MoreBTContext *context, MoreBTAddr pc)
	// This is the validPC routine for the Intel 
	// architecture.  See the description of 
	// MoreBTValidPCProc for a detailed discussion 
	// of its parameters.
	//
	// Intel instructions are not aligned in any way.  All, I can do 
	// is check for known bad values ((MoreBTAddr) -1 is used as a 
	// known bad value by the core) and check that I can read at least 
	// byte of instruction from the address.
{
	uint8_t	junkInst;
	
	return (pc != (MoreBTAddr) -1) && (context->readBytes(context, pc, &junkInst, sizeof(junkInst)) == 0);
}

static int IntelGetFrameNextPC(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr nextFrame, MoreBTAddr *nextPCPtr)
	// This is the getFrameNextPC routine for the Intel 
	// architecture.  See the description of 
	// MoreBTGetFrameNextPCProc for a detailed discussion 
	// of its parameters.
	//
	// This is very easy on Intel, because it's the return address, 
	// which is at a fixed offset in the frame.
{
	return ReadAddr(context, thisFrame + 4, nextPCPtr);	
}

/*	Intel Signal Stack Frames
	-------------------------
	Cross signal stack frames is much more reliable on Intel.  The parameters 
	to _sigtramp are stored on the stack, and you can reliably pick them up 
	from there.

					Size	Purpose
					----	-------
	low memory
	
	frame  ->		0x004	pre-signal frame pointer
					0x018	struct sigframe
					0x020?	pad
					0x258	struct mcontext
								0x00c	i386_exception_state_t
								0x040	i386_thread_state_t
								0x20c	i386_float_state_t
					0x040	siginfo_t
					0x020	struct ucontext
	high memory
	
	Things to note about the above:
	
	o The kernel aligns the stack such that the catcher field of the 
	  sigframe structure is aligned on a 16 byte boundary.  This means that 
	  there's a variable amount of pad between sigframe and mcontext.  
	  This isn't a problem because the sigframe structure contains a 
	  field (sinfo) that's a pointer to the siginfo_t.
	
	The sinfo field of the sigframe structure is at offset 0x10.  Once you 
	account for the pre-signal frame pointer that's pushed on to the stack 
	by _sigtramp, you need to go 0x14 bytes up the frame to get the sinfo 
	field, which is a pointer to a siginfo_t structure.  The kernel places 
	the pre-signal PC and SP in fields in that structure (si_addr and 
	pad[0], offset 0x18 and 0x24 respectively).
	
	Finally, if we detect a frameless leaf routine past the signal frame, 
	we extract its return address from the top of stack.
*/

static int IntelCrossSignalFrame(MoreBTContext *context, MoreBTAddr thisFrame, MoreBTAddr *nextPCPtr, MoreBTAddr *nextFramePtr)
	// This is the crossSignalFrame routine for the Intel 
	// architecture.  See the description of 
	// MoreBTCrossSignalFrameProc for a detailed discussion 
	// of its parameters.
{
	int	err;
	MoreBTAddr	sigInfo;
	MoreBTAddr	preSignalSP;
	
	// Get the siginfo_t pointer from the parameters to _sigtramp 
	// (the sinfo field of sigframe).
	
	err = ReadAddr(context, thisFrame + 0x14, &sigInfo);
	
	// Get the previous PC from si_addr field of siginfo_t.
	
	if (err == 0) {
		err = ReadAddr(context, sigInfo + 0x18, nextPCPtr);
	}
	
	// Get the previous frame by simply reading from the frame pointer. 
	// Because of the way things work, this ends up being correct.
	
	if (err == 0) {
		err = ReadAddr(context, thisFrame, nextFramePtr);
	}
	
	// Finally, if we detect a leaf routine, add a dummy frame for it 
	// and then get the pre-signal SP (from the pad[0] of siginfo_t) 
	// and, assuming that the top word on the stack is a return address, 
	// use it for the next PC.
	
	if ( (err == 0) && IntelIsSystemCall(context, *nextPCPtr) ) {
		AddFrame(context, *nextPCPtr, 0, kMoreBTFrameBadMask);
		
		err = ReadAddr(context, sigInfo + 0x24, &preSignalSP);

		if (err == 0) {
			err = ReadAddr(context, preSignalSP, nextPCPtr);
		}
	}
	
	return err;
}
#endif /* #ifdef CPU_TYPE_X86 */

// kArchitectures is an array of all the architectures we support.  
// Things to notes:
//
// o GetTaskArch processes this in a forward direction.  If you 
//   list a more-specific architecture, you should list it before 
//   the less-specific one.
//
// o The table is terminated by a NULL architecture, signified by 
//   a 0 in the cputype field.
//
// See the comments near MoreBTArchInfo for a detailed description of 
// each field.

static const MoreBTArchInfo kArchitectures[] = {
	{	// PowerPC
		CPU_TYPE_POWERPC,			// cputype
		0,							// subcputype
		false,						// is64Bit
		NX_BigEndian,				// byteOrder
		15,							// frameAlignMask
		PowerPCHandleLeaf,			// handleLeaf
		PowerPCValidPC,				// validPC
		PowerPCGetFrameNextPC,		// getFrameNextPC
		PowerPCCrossSignalFrame,	// crossSignalFrame
		PPC_THREAD_STATE,			// stateFlavor
		PPC_THREAD_STATE_COUNT		// stateCount
	},
	{	// PowerPC64
		CPU_TYPE_POWERPC,			// cputype
		0,							// subcputype
		true,						// is64Bit
		NX_BigEndian,				// byteOrder
		15,							// frameAlignMask
		PowerPCHandleLeaf,			// handleLeaf
		PowerPCValidPC,				// validPC
		PowerPCGetFrameNextPC,		// getFrameNextPC
		PowerPCCrossSignalFrame,	// crossSignalFrame
		PPC_THREAD_STATE64,			// stateFlavor
		PPC_THREAD_STATE64_COUNT	// stateCount
	},
#ifdef CPU_TYPE_X86
	{	// Intel
		CPU_TYPE_X86,				// cputype
		0,							// subcputype
		false,						// is64Bit
		NX_LittleEndian,			// byteOrder
		3,							// frameAlignMask
									// Apple's i386 API requires that the stack be 16 byte aligned, 
									// but it says nothing about the frame.  It turns out that the 
									// frame is typically 8 byte aligned, but I can't find any 
									// documentation that requires that, so I'm only checking 4 byte 
									// alignment.
		IntelHandleLeaf,			// handleLeaf
		IntelValidPC,				// validPC
		IntelGetFrameNextPC,		// getFrameNextPC
		IntelCrossSignalFrame,		// crossSignalFrame
		i386_THREAD_STATE,			// stateFlavor
		i386_THREAD_STATE_COUNT		// stateCount
	}
#endif /* #ifdef CPU_TYPE_X86 */
	 /* null terminator */
	
};

#pragma mark ***** Public Interface

extern int MoreBacktraceMachThread(
	task_t			task, 
	thread_t		thread,
	MoreBTAddr		stackBottom, 
	MoreBTAddr		stackTop,
	MoreBTFrame *	frameArray, 
	size_t			frameArrayCount, 
	size_t *		frameCountPtr
)
	// See comments in header.
{
	int						err;
	const MoreBTArchInfo *	arch;
	mach_msg_type_number_t 	stateCount;
	void *					threadState;

	assert(task != MACH_PORT_NULL);
	assert(thread != MACH_PORT_NULL);
	assert( ((stackBottom == 0) && (stackBottom == stackTop)) || (stackBottom < stackTop) );
	assert( (frameArrayCount == 0) || (frameArray != NULL) );
	assert( frameCountPtr != NULL );

	threadState = NULL;
	
	// Get the architecture of the task, and us that to allocate enough 
	// space for the thread's state.
	
	err = 0;
	arch = GetTaskArch(task);
	if (arch == NULL) {
		err = EINVAL;
	}
	if (err == 0) {
		stateCount = arch->stateCount;
		
		threadState = malloc(stateCount * sizeof(int));
		if (threadState == NULL) {
			err = ENOMEM;
		}
	}
	
	// Get the thread state.
	
	if (err == 0) {
		err = thread_get_state(thread, arch->stateFlavor, (thread_state_t) threadState, &stateCount);
	}
	
	// Do the backtrace.
	
	if (err == 0) {
		err = MoreBacktraceMach(
			arch,
			task,
			threadState,
			stackBottom,
			stackTop,
			frameArray, 
			frameArrayCount, 
			frameCountPtr
		);
	}
	
	// Clean up.
	
	free(threadState);
	
	return err;
}

// InitThreadState is a macro that initialises an architecture-specific 
// thread state structure from the current CPU registers.  This only 
// initialises enough fields to support a backtrace.

#if TARGET_CPU_PPC

#if 0 /* OMPI CHANGE */
	#define InitThreadState(threadState)	\
		do {								\
			uint32_t tmpPC = 0;				\
			uint32_t tmpFP = 0;				\
			asm {							\
					bl		next ;			\
			next:	mflr	tmpPC ;			\
					mr		tmpFP,sp		\
			}								\
			((ppc_thread_state_t *) threadState)->srr0 = tmpPC;		\
			((ppc_thread_state_t *) threadState)->r1   = tmpFP;		\
		} while (0)

#else /* OMPI CHANGE */
	#define InitThreadState(threadState)	                                \
		do {								\
			uint32_t tmpPC = 0;				        \
			uint32_t tmpFP = 0;				        \
                        asm("\tmflr %0\n"                                  \
                            "\tmr %1,r1"                                        \
                            : "=r"(tmpPC), "=r"(tmpFP));                        \
			((ppc_thread_state_t *) threadState)->srr0 = tmpPC;	\
			((ppc_thread_state_t *) threadState)->r1   = tmpFP;	\
		} while (0)

#endif /* OMPI CHANGE */

#elif TARGET_CPU_PPC64

#if 0 /* OMPI CHANGE */
	#define InitThreadState(threadState)	\
		do {								\
			uint64_t tmpPC = 0;				\
			uint64_t tmpFP = 0;				\
			asm {							\
					bl		next ;			\
			next:	mflr	tmpPC ;			\
					mr		tmpFP,sp		\
			}								\
			((ppc_thread_state64_t *) threadState)->srr0 = tmpPC;	\
			((ppc_thread_state64_t *) threadState)->r1   = tmpFP;	\
		} while (0)

#else /* OMPI CHANGE */
	#define InitThreadState(threadState)	                                \
		do {								\
			uint64_t tmpPC = 0;				        \
			uint64_t tmpFP = 0;				        \
                        asm("\tmflr %0\n"                                  \
                            "\tmr %1,r1"                                        \
                            : "=r"(tmpPC), "=r"(tmpFP));                        \
			((ppc_thread_state64_t *) threadState)->srr0 = tmpPC;	\
			((ppc_thread_state64_t *) threadState)->r1   = tmpFP;	\
		} while (0)

#endif /* OMPI CHANGE */

#elif TARGET_CPU_X86

	// Have to use bizarr-o GCC syntax because the compiler is barfing on the 
	// block syntax, but only for Intel.  *sigh*
	
	#define InitThreadState(threadState)			\
		do {										\
			uint32_t tmpPC = 0;						\
			uint32_t tmpFP = 0;						\
			asm( "\tcall Lnext\nLnext: pop %0\n"	\
			     "\tmov %%ebp,%1"					\
				 : "=r" (tmpPC) , "=r" (tmpFP) );	\
			((i386_thread_state_t *) threadState)->eip = tmpPC;		\
			((i386_thread_state_t *) threadState)->ebp = tmpFP;		\
		} while (0)
		
#else
	#error What architecture?
#endif

extern int MoreBacktraceMachSelf(
	MoreBTAddr		stackBottom, 
	MoreBTAddr		stackTop,
	MoreBTFrame *	frameArray, 
	size_t			frameArrayCount, 
	size_t *		frameCountPtr
)
	// See comments in header.
{
	int						err;
	const MoreBTArchInfo *	arch;
	void *					threadState;

	assert( ((stackBottom == 0) && (stackBottom == stackTop)) || (stackBottom < stackTop) );
	assert( (frameArrayCount == 0) || (frameArray != NULL) );
	assert( frameCountPtr != NULL );

	threadState = NULL;
	
	// Get the architecture of the current task, and us that to allocate 
	// enough space for our thread's state.

	err = 0;
	arch = GetTaskArch(mach_task_self());
	if (arch == NULL) {
		err = EINVAL;
	}
	if (err == 0) {
		threadState = calloc(arch->stateCount, sizeof(int));
		if (threadState == NULL) {
			err = ENOMEM;
		}
	}
	
	// Initialise the thread state, then do the backtrace.
	
	if (err == 0) {
		InitThreadState(threadState);

		err = MoreBacktraceMach(
			GetTaskArch(mach_task_self()),
			mach_task_self(),
			threadState,
			stackBottom, 
			stackTop,
			frameArray, 
			frameArrayCount, 
			frameCountPtr
		);
	}
	
	// Clean up.
	
	free(threadState);
	
	return err;
}
