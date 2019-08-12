// Dealing with the FPU environment. Check the header for more info.

#include <clasp/core/foundation.h>
#include <clasp/core/fp_env.h>
#include <stdbool.h>

void init_float_traps(void) {
#ifdef CLASP_FPT_SUPPORT
  clasp_feenableexcept(CLASP_FPT_INIT_EXCEPT);
#endif
}

namespace core {

SYMBOL_EXPORT_SC_(CorePkg, floatTrapsSupportedP);

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Are float traps supported?");
CL_DEFUN bool core__float_traps_supported_p() {
#ifdef CLASP_FPT_SUPPORT
  return 1;
#else
  return 0;
#endif
}

SYMBOL_EXPORT_SC_(CorePkg, getFloatTraps);

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Get current floating point traps state.");
CL_DEFUN int core__get_float_traps() {
#ifdef CLASP_FPT_SUPPORT
  return clasp_fegetexcept();
#else
  return 0; // FIXME: warn
#endif
}

} // namespace core
