@ECHO OFF
REM Script to compile/link a C file with ECL
REM (Michael Goffioul)

set CMDTYPE=%1
set CMDARGS=
:LOOP
shift
IF NOT "%1" == "" (
	set CMDARGS=%CMDARGS% %1
	goto LOOP
)

IF %CMDTYPE% == --compile (
	cl @ECL_CFLAGS@ -I@includedir@ %CMDARGS%
	GOTO END
) ELSE IF %CMDTYPE% == --link (
	cl %CMDARGS% @LDFLAGS@ @libdir@/ecl.lib
	GOTO END
)

ECHO Usage: %0 [OPTIONS] [ARGS*]
ECHO Options:
ECHO      [--compile]
ECHO      [--link]

:END
