# VampirTrace instrumentation for C
BEGIN_INSTRUMENT_SECTION
  file="*" line=1 code="#ifdef TAUINST_NOSRC\n#  define _TAUINST_FILE NULL\n#else\n#  define _TAUINST_FILE \"@FILE@\"\n#endif\n#include <vt_user.h>" lang="c"
  entry code="VT_User_start__(\"@ROUTINE@\", _TAUINST_FILE, @BEGIN_LINE@);" lang="c"
  exit  code="VT_User_end__(\"@ROUTINE@\");" lang="c"
END_INSTRUMENT_SECTION

# VampirTrace instrumentation for C++
BEGIN_INSTRUMENT_SECTION
  file="*" line=1 code="#ifdef TAUINST_NOSRC\n#  define _TAUINST_FILE NULL\n#else\n#  define _TAUINST_FILE \"@FILE@\"\n#endif\n#include <vt_user.h>" lang="c++"
  decl code="VT_Tracer<sizeof(int)> vt_tracer__(\"@ROUTINE@\", _TAUINST_FILE, @BEGIN_LINE@);" lang="c++"
END_INSTRUMENT_SECTION

# VampirTrace instrumentation for Fortran
BEGIN_INSTRUMENT_SECTION
  file="*" line=1 code="#ifdef TAUINST_NOSRC\n#  define _TAUINST_FILE \"\"\n#else\n#  define _TAUINST_FILE \"@FILE@\"\n#endif" lang="fortran"
  entry code="call VT_User_start__(\"@ROUTINE@\", _TAUINST_FILE, @BEGIN_LINE@)" lang="fortran"
  exit  code="call VT_User_end__(\"@ROUTINE@\")" lang="fortran"
END_INSTRUMENT_SECTION
