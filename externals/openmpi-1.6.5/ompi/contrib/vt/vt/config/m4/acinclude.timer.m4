AC_DEFUN([ACVT_TIMER],
[
	timer=

	AC_REQUIRE([ACVT_PLATFORM])

	case $PLATFORM in
	linux)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		timer=TIMER_GETTIMEOFDAY

		case $host_cpu in
			i*86 | x86* | powerpc*)
				AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. TSC)])
				timer=TIMER_CYCLE_COUNTER
				;;
			ia64)
				AC_CHECK_HEADERS([asm/intrinsics.h],
				[
					AC_MSG_CHECKING([for __getReg(_IA64_REG_AR_ITC)])
					AC_TRY_LINK([#include <asm/intrinsics.h>],
[
  volatile long long r = (long long)__getReg(_IA64_REG_AR_ITC);
],
					[
						AC_MSG_RESULT([yes])
						AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. ITC)])
						timer=TIMER_CYCLE_COUNTER
					], [AC_MSG_RESULT([no])])
				])
				;;
		esac
		;;
	macos)
		AC_DEFINE([TIMER_CYCLE_COUNTER], [1], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		timer=TIMER_CYCLE_COUNTER
		;;
	altix)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		timer=TIMER_CLOCK_GETTIME

		mmtimer_h_found=no
		AC_CHECK_HEADERS([linux/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([sn/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([mmtimer.h], [mmtimer_h_found=yes])])])
		AS_IF([test x"$mmtimer_h_found" = "xyes"],
		[
			AC_CHECK_FILE([/dev/mmtimer],
			[
				AC_DEFINE([TIMER_MMTIMER], [2], [Intel Multimedia Timer])
				timer=TIMER_MMTIMER
			])
		])
		;;
	bgl)
		AC_DEFINE([TIMER_RTS_GET_TIMEBASE], [1], [Use `rts_get_timebase' function])
		timer=TIMER_RTS_GET_TIMEBASE
		;;
	bgp | bgq)
		AC_DEFINE([TIMER_GET_TIMEBASE], [1], [Use `GetTimeBase' function])
		timer=TIMER_GET_TIMEBASE
		;;
	ibm)
		AC_DEFINE([TIMER_POWER_REALTIME], [1], [IBM Power family Real-Time-Clock])
		AC_DEFINE([TIMER_SWITCH_CLOCK], [2], [Hardware Switch-Clock (it's necessary to link your application with '-lswclock')])
		timer=TIMER_POWER_REALTIME
		;;
	sun)
		AC_DEFINE([TIMER_GETHRTIME], [1], [gethrtime])
		timer=TIMER_GETHRTIME
		;;
	necsx)
		AC_DEFINE([TIMER_SYSSX_HGTIME], [1], [NEC SX HGTIME])
		timer=TIMER_SYSSX_HGTIME
		;;
	crayt3e)
		AC_DEFINE([TIMER_CRAY_RTCLOCK],[1], [CRAY Real-Time-Clock])
		timer=TIMER_CRAY_RTCLOCK
		;;
	crayx1)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		AC_DEFINE([TIMER_RTC], [2], [RTC (DOES NOT WORK YET WITH FORTRAN CODES)])
		timer=TIMER_GETTIMEOFDAY
		;;
	crayxt | crayxe)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_CYCLE_COUNTER], [2], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [3], [Use `gettimeofday' function])
		timer=TIMER_CYCLE_COUNTER

		AS_IF([test $PLATFORM = "crayxt"],
		[
			AC_TRY_COMPILE([],
[
#ifndef __LIBCATAMOUNT__
#  error "__LIBCATAMOUNT__ not defined"
#endif
],
			[AC_CHECK_HEADERS([catamount/dclock.h],
			[AC_CHECK_HEADERS([catamount/data.h],
			[
				AC_DEFINE([TIMER_DCLOCK], [4], [Use `dclock' function])
				timer=TIMER_DCLOCK
			])])])
		])
		;;
	origin)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		timer=TIMER_CLOCK_GETTIME
		;;
	sicortex)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		timer=TIMER_GETTIMEOFDAY
		;;
	generic)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		timer=TIMER_GETTIMEOFDAY
		;;
	esac

	AC_DEFINE_UNQUOTED([TIMER], [$timer], [Use timer (see below)])
	AC_MSG_NOTICE([selected timer: $timer])

	case $timer in
		TIMER_RTS_GET_TIMEBASE | TIMER_GET_TIMEBASE | TIMER_SYSSX_HGTIME | TIMER_GETTIMEOFDAY)
			timer_is_global=yes
			timer_is_global_def=1
			;;
		*)
			timer_is_global=no
			timer_is_global_def=0
			;;
	esac
	AC_DEFINE_UNQUOTED([TIMER_IS_GLOBAL], [$timer_is_global_def],
	[Define to 1 if the selected timer is global (doesn't need synchronization)])
	AC_MSG_NOTICE([global timer: $timer_is_global])
])

