/*
    File: sc.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
/* sc.h: STACK CONTEXT
 *
 * $Id$
 * Copyright (c) 2012 Ravenbrook Limited.  See end of file for license.
 *
 * Provides a context to hold the registers and stack pointer
 * 
 * This file provide wrappers for using setjmp or some similar mechanism
 * to save the current callee-saves on the stack.
 *
 * See http://info.ravenbrook.com/mail/2012/08/03/14-36-35/0/ and the rest of
 * the thread for the origin of the idea.
 * 
 * TODO: Make StackScan take a StackContext
 */

#ifndef sc_h
#define sc_h

#include "mpm.h"

     
/* StackContext -- holds the registers including a stack pointer
 *
 * This contains the callee-save registers and the stack pointer.
 *
 * This is used to save the registers after or on entry to the arena so that
 * they can be scanned.
 */

/* STACK_CONTEXT_SAVE - save the callee-saves and stack pointer
 * 
 * This macro saves the callee-saves and stack pointer for the
 * current function into the passed StackContext.  The StackContext
 * is no longer valid after the function returns.
 *
 * This needs to be a macro because the compiler may need to do
 * setjmp magic.
 */
 
/* StackContextStackTop - return the stack top from the stack context.
 * 
 * We assume the stack is full.  In other words the stack top points at
 * a word that contains a potential Ref.
 */


/* Mac OS X on 32-bit Intel built with Clang or GCC */

#if defined(MPS_PF_XCI3LL) || defined(MPS_PF_XCI3GC)

#include <setjmp.h>

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;

/* See the implementation of _setjmp in
 * <http://www.opensource.apple.com/source/Libc/Libc-825.24/i386/sys/_setjmp.s> */

#define JB_ESP 36 /* offset into the jmp_buf in bytes as defined in _setjmp.s */

#define STACK_CONTEXT_SAVE(sc) ((void)_setjmp((sc)->jumpBuffer))

#define StackContextSP(sc) ((Addr *)(sc)->jumpBuffer[JB_ESP/sizeof(int)])

/* On MacOS X the stackPointer can end up pointing above the StackContext
 * which we assume to be stored on the stack because it is no longer
 * needed once we have _longjmp()ed back. So take the minimum of the 
 * SP and the base of the StackContext structure. */
#define StackContextStackTop(sc) \
  (StackContextSP(sc) < (Addr*)(sc) ? StackContextSP(sc) : (Addr*)(sc))


/* Mac OS X on 64-bit Intel build with Clang or GCC */

#elif defined(MPS_PF_XCI6LL) || defined(MPS_PF_XCI6GC)

#include <setjmp.h>

/* We could use getcontext() from libunwind but that produces
 * deprecation warnings.  See
 * <http://stackoverflow.com/questions/3592914/how-can-i-implement-cooperative-lightweight-threading-with-c-on-mac-os-x>
 */

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;

/* See the implementation of _setjmp in
 * <http://www.opensource.apple.com/source/Libc/Libc-825.24/x86_64/sys/_setjmp.s> */

#define STACK_CONTEXT_SAVE(sc) ((void)_setjmp((sc)->jumpBuffer))

#define JB_RSP 16 /* offset into the jmp_buf in bytes as defined in _setjmp.s */

/* jmp_buf is an int[] but the stack pointer is 8 bytes so we need a cast */
/* FIXME: possible aliasing problem */
#define StackContextSP(sc) \
  (*(Addr **)((char *)(sc)->jumpBuffer+JB_RSP))

/* On MacOS X the stackPointer can end up pointing above the StackContext
 * which we assume to be stored on the stack because it is no longer
 * needed once we have _longjmp()ed back. So take the minimum of the 
 * SP and the base of the StackContext structure. */
#define StackContextStackTop(sc) \
  (StackContextSP(sc) < (Addr*)(sc) ? StackContextSP(sc) : (Addr*)(sc))


/* Windows on 32-bit Intel with Microsoft Visual Studio */

#elif defined(MPS_PF_W3I3MV)

#include <setjmp.h>

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;

#define STACK_CONTEXT_SAVE(sc) ((void)setjmp((sc)->jumpBuffer))

#define StackContextStackTop(sc) \
  ((Addr *)((_JUMP_BUFFER *)(sc)->jumpBuffer)->Esp)


/* Windows on 64-bit Intel with Microsoft Visual Studio */

#elif defined(MPS_PF_W3I6MV)

#include <setjmp.h>

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;

#define STACK_CONTEXT_SAVE(sc) ((void)setjmp((sc)->jumpBuffer))

#define StackContextStackTop(sc) \
  ((Addr *)((_JUMP_BUFFER *)(sc)->jumpBuffer)->Rsp)


#else

/* TODO: implement this on other platforms in a safer way.
 * Potentially the callee saves from the calling function could be spilled
 * underneath the jmp_buf so returning the address of the jmp_buf for the
 * stack top is not completely safe.  
 */

#include <setjmp.h>

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;

#define STACK_CONTEXT_SAVE(sc) ((void)setjmp((sc)->jumpBuffer))

#define StackContextStackTop(sc) ((Addr *)(sc)->jumpBuffer)


#endif /* platform defines */

#endif /* sc_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2012 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
