"Unwinding" means usually nonlocal exits out of a function, but also local exits when more complicated than changing the instruction pointer. In Common Lisp the RETURN-FROM, GO, and THROW special operators do this. SIGNAL (and WARN and ERROR) do as well, but in Clasp and most other implementations those are implemented in terms of the special operators. In C++ nonlocal exits are done with the `throw` operator.

Clasp's problem is to implement Common Lisp semantics while respecting C++ semantics. This means that:

* If a C++ function unwinds, and between the throw and the handler there are Lisp frames, any UNWIND-PROTECT cleanup forms must be executed, and special variable bindings undone.
* If a Lisp function unwinds, and there are C++ frames in between, any automatic variables' destructors must be executed, as well as `catch (...)` blocks.

# Implementation intro

Clasp accomplishes this with two implementations of unwinding. First, there is the comprehensive implementation in terms of C++ semantics, i.e. throwing and catching. Secondly, we have our own unwinder, built on top of C setjmp/longjmp. Because the C++ unwinder has extremely poor performance characteristics for our purposes, we use our own unwinder whenever possible, i.e. whenever we do not need to worry about frames between the entry and exit containing uncooperative C++ code.

Specifically, the C++ unwinder works, generally, by the unwinder finding and using information tables in the executable to determine what cleanups and catches exist in each frame. This entails looking at the executable file and running the DWARF virtual machine to get the information, which can cause enormous slowdowns - in some cases I have seen the C++ unwinder work thousands of times slower than the SJLJ unwinder. On Linux, this additionally causes inter-thread contention, as the GNU C++ runtime uses a global flag to make sure this process does not take place concurrently with dlopen.

Our unwinder works by storing information about the dynamic context (i.e. live exit points, unwind-protect cleanups, and special variable bindings) in a thread local variable, _DynEnv (part of the ThreadLocalState). Upon entry to a new dynamic context, we must push a new dynamic environment onto the stack stored therein, and we must clean this dynamic environment up upon exit. This entails a runtime cost (unlike C++), but means that the unwinder has much less work to do, as it merely consults the _DynEnv.

In order to support both unwinders, we generate code for both all the time (with an exception for "simple" unwinds, explained below).

# CL semantics review

First, CL semantics: RETURN-FROM/BLOCK and GO/TAGBODY are very similar, being lexical; when RETURN-FROM/BLOCK is being referred to below, GO/TAGBODY works pretty much the same way. THROW/CATCH are instead dynamic. What this means is that a THROW will unwind _to the nearest dynamically enclosing CATCH_, whereas a RETURN-FROM or GO will unwind _to the lexically corresponding BLOCK or TAGBODY_. A function that evaluates a RETURN-FROM or GO to some BLOCK not in the same function is a closure, over an implicit variable marking where to return to. This means, for example, that in

```
(defun foo (n &optional f)
  (if (zerop n)
      (funcall f)
      (foo (1- n) (or f (lambda () (return-from foo)))))
  (print "hello world"))
```

a call like `(foo n)` will print nothing, for any positive `n` greater than zero: The outermost FOO block is returned from, since that's what the function closes over.

# Role of the compiler

Cleavir is intelligent enough to distinguish local and nonlocal unwinds. An unwind is "nonlocal" essentially if it crosses a function boundary, so for example `(block nil (return 4))` can be local as there is no function boundary between the BLOCK and RETURN-FROM. These local unwinds are represented as simple JUMP instructions that do not implicate the runtime and are not further describe here.

Additionally, in some cases Cleavir can identify "simple" nonlocal unwinds. These cross function boundaries, but the dynamic contexts between the entry and exit point are statically understood by the compiler. For example, in `(block nil (mapcar (lambda (x) (if x ... (return x))) ...)`, there is a nonlocal exit, but the compiler can tell that there are never any interposed cleanups or special variable bindings. These "simple" unwinds are handled as simple setjmp-longjmp pairs that do not further interact with the runtime here; they do not require unwind tables to be generated, or even an addition to the dynamic environment object to be made. Simplicity determination is done by `cleavir-bir-transformations:simple-unwinding-p`. Check the definition of that function in Cleavir for more information.

# Our unwinder

As described above, our unwinder works by storing information about the dynamic context in a frame local variable. The implementation lives in src/core/unwind.cc and include/clasp/core/unwind.h.

Dynamic environments are Lisp-accessible objects, essentially for the purpose of debugging. UNWIND-PROTECT cleanup and special variable binding dynamic environments are allocated on the stack for efficiency; BLOCK and RETURN-FROM environments are allocated on the heap, so that out of extent unwinds can hopefully be detected (more on those below).

The main entry points to the unwinder are defined in unwind.h. `sjlj_unwind` initializes an unwinding, and `sjlj_continue_unwinding` continues an already-preceding unwind operation after an UNWIND-PROTECT cleanup has executed. Cleavir uses intrinsics with similar names which essentially just call those functions.

A special dynamic environment object, the `UnknownDynEnv_O`, is an indication that there is uncooperative C++ code on the stack. Unwinding can only be safely completed through these frames (dynamic environments) by reverting to the C++ unwinder. `sjlj_unwind` searches for these dynamic environments before it jumps, and if it finds any, reverts to the C++ unwinder by executing a C++ `throw` operation.

# C++ unwinder

Because C++ semantics are like ERROR/HANDLER-CASE with additional concerns due to static typing and manual memory allocation, the unwinding system is kind of harrowing.

CL semantics are not like C++ exceptions, which presents most of the problem. Lisp THROW/CATCH, by contrast, is pretty close to C++. Since it's also rare in Lisp, we just implement these in Lisp. We expand `(throw ...)` and `(catch ...)` forms into calls to `core:throw-function` and `core:catch-function`, respectively, and these take thunks as arguments. THROW will perform a C++ throw of an exception of type CatchThrow, and CATCH catches it. See core/compiler.cc and core/exceptions.h CLASP_BEGIN_CATCH and CLASP_END_CATCH macros for more details.

So, RETURN-FROM/BLOCK. Besides that these don't match C++ semantics, we put more effort into their performance, since they're fairly ubiquitous in Lisp.

Now for actual nonlocal unwinds. When we hit a BLOCK for one, at runtime, we save the current frame pointer using `llvm.frameaddress`. This is stored as what any closures RETURN-FROM-ing to the BLOCK use to find the correct frame. We also coordinate between the BLOCK and RETURN-FROM at compile time a small integer, the "go index", indicating where in the function to go upon returning; this is actually only relevant for TAGBODY, since returning "to" the same TAGBODY could put you at multiple different go tags.

When we run the RETURN-FROM, we call the intrinsic `cc_sjlj_unwind`. This (in the c++ case) constructs an exception of type Unwind that stores the frame pointer and go index, and then `throw`s it.

## Cleanups

Sometimes we want to run code that executes during unwinding. In Lisp this is the case with UNWIND-PROTECT and with special variable bindings, which must be undone on the way out. We represent these with an UNWIND-PROTECT and BIND instructions, respectively.

Because UNWIND-PROTECT can execute arbitrary code, during unwinding we may want to execute arbitrary Lisp code. This code can of course itself unwind - and this unwinding can either be constrained to the cleanup code by handlers, or exit as well. This must be kept in mind: more than one unwinding can be in progress simultaneously, within the same thread.

## LLVM and Landing pads

LLVM represents exception handling constructs in a very C++ centered way. If a call could unwind, and in such a way that the caller might want to handle the exception, you have to use an `invoke` instruction rather than a `call`. The `invoke` has a second basic block to which the runtime will pass control during unwinding. This block must begin with a `landingpad` instruction, which indicates which types it handles. Calling a function with an actual `call` tells LLVM that the unwinder runtime can ignore this frame as it unwinds from that function.

A Lisp BLOCK can "handle" an Unwind "exception" by transferring control back into its function. As such, all calls within the dynamic extent of a BLOCK, and in the same function, must be `invoke`s to a landing pad that can handle Unwinds. If we don't handle an exception, we "resume" it.

The landing pad needs to test the type of the exception because I don't know. Something about LLVMM merging them during inlining. This means that both the personality function (below) and the code test exception types.

To get the type info we have to do some really unfortunate bullshit. You see, while C++ has a typeid operator to get a std::type_info object for a class, the exception machinery actually wants a pointer to a std::type_info, and I don't think there's any way to get those standardly. So instead, we make sure the exception class has a vtable so that a type_info* is accessible, and then access the mangled symbol directly. The name is hardcoded in the `*exceptions*` variable defined in cmpintrinsics.lisp. This is very bad but I don't know any workaround.

For BLOCK, TAGBODY, UNWIND-PROTECT, and special variable binding forms, we generate landing pads for any calls in their dynamic extents. We use two kinds of landing pads, called "maybe entry" and "never entry". The former is used when there is any possibility that the call could be returning to the frame in question: so, a call to a function that could hypothetically have a nonlocal RETURN-FROM, from a frame that has a BLOCK or TAGBODY that could be returned from. We don't presently have the capability to determine very well when no entry is possible - for example, even a C++ function may eventually call something that calls `cl:error` - so we mostly use the first when there are any nonlocal BLOCK/TAGBODY.

The processing after a landing pad can get kind of complicated. Consider the case of a function that has `(block a (unwind-protect (block b (let ((f ...)) ... (f))) ...))`. If `f` does a nonlocal exit, it's either returning to b or returning to a or returning somewhere higher on the stack. The landing pad will first check whether the exception is our Unwind class and for this frame. If not, it performs the unwind-protect cleanup and then resumes. If it is, it checks the go index to see if the `b` block is the destination. If so, it goes there, if not, it jumps to a block that performs the unwind-protect cleanup and then checks the go-index to see if `a` is the destination. If it is it jumps, if not it signals a bug, as we found that this frame is being returned to, but not a sane unwind index.

In a bit more detail. If the exception is not an Unwind we resume it without further examination. If it is an Unwind we actually handle it, using __cxa_begin_catch and __cxa_end_catch (below). These must be carefully managed to ensure that the exception is not freed early. If it is an Unwind but for the wrong frame, we rethrow rather than resume, so that the C++ rethrow informs __cxa_end_catch not to free the thing.

Convoluted. All this is in landing-pad.lisp.

## Itanium ABI

C++ runtimes generally follow the so-called Itanium ABI for exception handling, and use a "zero cost" implementation. This "zero cost" means that, as long as you don't actually throw exceptions, your C++ code will be just as fast as if there was no exception code whatsover. For example, entering a `try` block, or exiting it normally without unwinding, has no runtime cost. The downside of this is that if you do actually throw an exception, it will be slow.

Briefly, this is accomplished by keeping a table of information pertinent to the unwinder (e.g. types caught by handlers, where to jump to to enter a `catch` block, etc.) in a way that it can be determined just from an instruction pointer in the function (such as a return address on the call stack). The unwinder finds a customizable "personality function" for each frame, which finds this table and parses it. This is expensive - enough that it has caused us performance problems, because we unwind more than C++ does. On Linux, finding this table involves grabbing a mutex, so if you try to unwind in multiple threads simultaneously they will contend and you will suffer.

The unwinder runtime is accessible through calls to a few functions. To briefly summarize points relevant to us:

* When you throw an exception, memory is allocated on the heap by __cxa_allocate_exception, the exception object is copy-constructed into it, and then the unwinder is started with __cxa_throw. If there is no handler, __cxa_throw calls std::terminate, which kills the program. We don't call these functions directly, in favor of just using C++'s throw operator, but it's good to know these things.
* __cxa_begin_catch is called when you enter a catch block. It increments the reference count (yes) of the exception being handled, and pushes it to the front of a global (thread-local) stack of exceptions, so that if a `throw;` is executed the runtime knows what exception to rethrow.
* __cxa_end_catch must be called when you exit a catch block - either normally or abnormally. It pops this exception stack and reduces the reference count of the exception. If the count is zero, the exception is deallocated.

The exception must be heap-allocated, or at least allocated outside the control stack, because we are unwinding the control stack; and additionally because, again, more than one unwinding can be occurring simultaneously. The exception stack is only necessary for C++'s `throw;` operator. We don't really need it, but do need to keep things arranged so that the exception is eventually deallocated, and at the right time.

When we have a BLOCK or TAGBODY, our landing pad checks the type, then calls __cxa_begin_catch, then matches the frame pointer. If it matches, we get the go index, call __cxa_end_catch, then jump into the function and we're done unwinding. If it doesn't, we rethrow to a landing pad that calls __cxa_end_catch.

# Out of extent returns

An out-of-extent return is a RETURN-FROM or GO in which the corresponding BLOCK or TAGBODY has already exited. In the Lisp standard, this is undefined behavior. We would like to be nice and signal an error from the frame in which the exit was attempted.

In our unwinder, this situation can be detected simply by checking whether the destination dynamic environment is still present on the stack. In C++, it is more complicated.

The Itanium ABI documentation mentions the possibility of a "resumptive" exception handling regime, in which the runtime decides to cease throwing an exception before doing so. This sounds ideal for our purpose here. Unfortunately, this is not actually possible to implement due to C++ semantics. As mentioned, the C++ throw operator we use terminates the program if it cannot find a handler, so you might think if we just use the ABI more directly we can avoid this. Technically true, but the problem is the semantics of `catch (...)` blocks in C++. A C++ frame that catches any exception will catch Lisp exceptions - fine so far - but then if it rethrows them, it will again terminate if it can't find a handler. Itanium considers `catch (...)` a handler, so we can't detect the case of not having an actual handler through Itanium. Great.

We instead use libunwind more directly and check, before the C++ throw, whether our destination frame is actually on the stack. If it is we proceed with the throw, otherwise we signal the error. This is carried out by the frame_check function in debugger.cc.
