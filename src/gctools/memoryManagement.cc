/*
    File: memoryManagement.cc
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
//

#include <llvm/Support/ErrorHandling.h>
#include <clasp/core/foundation.h>
#include <clasp/gctools/gcalloc.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/debugger.h>
#include <clasp/gctools/telemetry.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/memoryManagement.h>
//#include "main/allHeaders.cc"

#ifdef _TARGET_OS_LINUX
#include <signal.h>
#endif

namespace gctools {
std::vector<Immediate_info> get_immediate_info() {
  std::vector<Immediate_info> info;
  info.push_back(Immediate_info(kind_fixnum,"FIXNUM"));
  info.push_back(Immediate_info(kind_single_float,"SINGLE_FLOAT"));
  info.push_back(Immediate_info(kind_character,"CHARACTER"));
  info.push_back(Immediate_info(kind_cons,"CONS"));
  if ( (info.size()+1) != kind_first_general ) {
    printf("get_immediate_info does not set up all of the immediate types\n");
    abort();
  }
  return info;
};
};

namespace gctools {

GCStack _ThreadLocalStack;
const char *_global_stack_marker;
size_t _global_stack_max_size;
/*! Keeps track of the next available header KIND value */
kind_t global_next_header_kind = (kind_t)KIND_max+1;

#if 0
    HeapRoot* 	rooted_HeapRoots = NULL;
    StackRoot* 	rooted_StackRoots = NULL;
#endif
};

#ifdef USE_BOEHM
#include "boehmGarbageCollection.cc"
#endif

#if defined(USE_MPS)
#include "mpsGarbageCollection.cc"
#endif

namespace gctools {

size_t global_alignup_sizeof_header;

MonitorAllocations global_monitorAllocations;

void monitorAllocation(kind_t k, size_t sz) {
  printf("%s:%d monitor allocation of %s with %zu bytes\n", __FILE__, __LINE__, obj_name(k), sz);
  if (global_monitorAllocations.counter >= global_monitorAllocations.start && global_monitorAllocations.counter < global_monitorAllocations.end) {
    core::core__clib_backtrace(global_monitorAllocations.backtraceDepth);
  }
  global_monitorAllocations.counter++;
}

void handle_signals(int signo) {
  //
  // Indicate that a signal was caught and handle it at a safe-point
  //
  SET_SIGNAL(signo);
  telemetry::global_telemetry_search->flush();
  if (signo == SIGABRT && core::global_debuggerOnSIGABRT) {
    printf("%s:%d Trapped SIGABRT - starting debugger\n", __FILE__, __LINE__);
    core::LispDebugger debugger(_Nil<core::T_O>());
    debugger.invoke();
  }
}

void fatal_error_handler(void *user_data, const std::string &reason, bool gen_crash_diag) {
  printf("Hit a fatal error in llvm/clang: %s\n", reason.c_str());
  printf("Clasp is terminating via abort(0)\n");
  abort();
}

#ifdef USE_BOEHM
void clasp_warn_proc(char *msg, GC_word arg) {
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
}
#endif

void setupSignals() {
  if (signal(SIGINT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGINT signal-handler with kernel\n");
  }
  if (signal(SIGCHLD, handle_signals) == SIG_ERR) {
    printf("failed to register SIGCHLD signal-handler with kernel\n");
  }
  if (signal(SIGABRT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGABRT signal-handler with kernel\n");
  }
  llvm::install_fatal_error_handler(fatal_error_handler, NULL);
}

gc::GCStack *threadLocalStack() {
  return &_ThreadLocalStack;
}

int handleFatalCondition() {
  int exitCode = 0;
  try {
    throw;
  } catch (core::ExitProgram &ee) {
    // Do nothing
    //            printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    exitCode = ee.getExitResult();
  } catch (core::TerminateProgramIfBatch &ee) {
    // Do nothing
    printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
  } catch (core::Condition &ee) {
    IMPLEMENT_MEF(BF("Figure out what to do if we catch a Condition"));
    //        printf("Caught Condition at %s:%d - %s\n", __FILE__, __LINE__, ee.message().c_str() );
    //        printf("Stack trace:\n%s", ee.conditionObject()->getStackTraceDump().c_str() );
  } catch (core::CatchThrow &ee) {
    _lisp->print(BF("%s:%d Uncaught THROW frame[%s] - this should NEVER happen - the stack should never be unwound unless there is a CATCH clause that matches the THROW") % __FILE__ % __LINE__ % ee.getFrame());
  } catch (core::Unwind &ee) {
    _lisp->print(BF("At %s:%d - Unwind caught frame: %d index: %d") % __FILE__ % __LINE__ % ee.getFrame() % ee.index());
  } catch (core::HardError &ee) {
    _lisp->print(BF("At %s:%d - HardError caught: %s") % __FILE__ % __LINE__ % ee.message());
  }
#if 0
        catch ( ... )
        {
            _lisp->print(BF("Unknown exception in main - everything should be caught lower down %s:%d") % __FILE__ % __LINE__);
        }
#endif
  return exitCode;
}

kind_t next_header_kind()
{
    kind_t next = global_next_header_kind;
    ++global_next_header_kind;
    return next;
}

core::Fixnum ensure_fixnum(kind_t val)
{
  if ( val > most_positive_fixnum || val < most_negative_fixnum ) {
    SIMPLE_ERROR(BF("The value %lz cannot be converted into a FIXNUM") % val );
  }
  return (core::Fixnum)val;
}

CL_LAMBDA();
CL_DOCSTRING(R"doc(Return the next available header KIND value and increment the global variable global_next_header_kind)doc");
CL_DEFUN core::Fixnum gctools__next_header_kind()
{
    kind_t next = global_next_header_kind;
    ++global_next_header_kind;
    return ensure_fixnum(next);
}


int startupGarbageCollectorAndSystem(MainFunctionType startupFn, int argc, char *argv[], size_t stackMax, bool mpiEnabled, int mpiRank, int mpiSize) {
  int stackMarker = 0;
  gctools::_global_stack_marker = (const char*)&stackMarker;
  gctools::_global_stack_max_size = stackMax;
//  printf("%s:%d       global_stack_marker = %p\n", __FILE__, __LINE__, gctools::_global_stack_marker );
  global_alignup_sizeof_header = AlignUp(sizeof(Header_s));

  { // Debugging info
    size_t alignment = Alignment();
#if 0
    printf("%s:%d Alignment() = %lu\n", __FILE__, __LINE__, alignment);
#ifdef USE_MPS
    printf("%s:%d Align(1) = %lu\n", __FILE__, __LINE__, Align(1));
    printf("%s:%d Align(Alignment()) = %lu\n", __FILE__, __LINE__, Align(Alignment()));
#endif
    printf("%s:%d Alignup(1) = %lu\n", __FILE__, __LINE__, AlignUp(1));
    printf("%s:%d Alignup(Alignment()) = %lu\n", __FILE__, __LINE__, AlignUp(Alignment()));
    printf("%s:%d global_alignup_sizeof_header = %lu\n", __FILE__, __LINE__, global_alignup_sizeof_header );
#endif
  }

  build_kind_field_layout_tables();
  
  setupSignals();

  telemetry::global_telemetry_search = new telemetry::Telemetry();
  telemetry::initialize_telemetry_functions();

  char *clasp_telemetry_mask_string = getenv("CLASP_TELEMETRY_MASK");
  telemetry::global_clasp_telemetry_file = getenv("CLASP_TELEMETRY_FILE");

  if (clasp_telemetry_mask_string) {
    printf("CLASP_TELEMETRY_MASK= %s\n", clasp_telemetry_mask_string);
    size_t mask = std::stoi(clasp_telemetry_mask_string);
    telemetry::global_telemetry_search->set_mask(mask);
  }
  if (telemetry::global_clasp_telemetry_file) {
    printf("CLASP_TELEMETRY_FILE= %s\n", telemetry::global_clasp_telemetry_file);
    telemetry::global_telemetry_search->open_write(telemetry::global_clasp_telemetry_file);
  }

#if defined(USE_MPS)
  int exitCode = gctools::initializeMemoryPoolSystem(startupFn, argc, argv, mpiEnabled, mpiRank, mpiSize);
#endif

#if defined(USE_BOEHM)
  GC_set_all_interior_pointers(1); // tagged pointers require this
                                   //printf("%s:%d Turning on interior pointers\n",__FILE__,__LINE__);
  GC_set_warn_proc(clasp_warn_proc);
  //  GC_enable_incremental();
  GC_init();
  _ThreadLocalStack.allocateStack(gc::thread_local_cl_stack_min_size);
  core::ThreadLocalState thread_local_state;
  my_thread = &thread_local_state;
  int exitCode = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
#endif
  telemetry::global_telemetry_search->close();
  return exitCode;
}
};
