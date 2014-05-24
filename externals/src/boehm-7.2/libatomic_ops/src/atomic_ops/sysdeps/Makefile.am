#General sysdep utility headers, followed by the arch-specific ones
nobase_sysdep_HEADERS= generic_pthread.h \
	  atomic_load_store.h \
	  aligned_atomic_load_store.h \
	  acquire_release_volatile.h \
	  char_acquire_release_volatile.h \
	  char_atomic_load_store.h \
	  short_acquire_release_volatile.h \
	  short_aligned_atomic_load_store.h \
	  short_atomic_load_store.h \
	  int_acquire_release_volatile.h \
	  int_aligned_atomic_load_store.h \
	  int_atomic_load_store.h \
	  all_acquire_release_volatile.h \
	  all_aligned_atomic_load_store.h \
	  all_atomic_load_store.h \
	  read_ordered.h \
	  ordered_except_wr.h \
	  ordered.h \
	  ao_t_is_int.h \
	  test_and_set_t_is_ao_t.h \
	  test_and_set_t_is_char.h \
	  emul_cas.h \
	  standard_ao_double_t.h \
	  README \
	\
	  armcc/arm_v6.h \
	\
	  gcc/alpha.h gcc/arm.h gcc/avr32.h gcc/cris.h \
	  gcc/hexagon.h gcc/hppa.h gcc/ia64.h gcc/m68k.h \
	  gcc/mips.h gcc/powerpc.h gcc/s390.h \
	  gcc/sh.h gcc/sparc.h gcc/x86.h gcc/x86_64.h \
	\
	  hpc/hppa.h hpc/ia64.h \
	\
	  ibmc/powerpc.h \
	\
	  icc/ia64.h \
	\
	  msftc/arm.h msftc/common32_defs.h msftc/x86.h \
	  msftc/x86_64.h \
	\
	  sunc/sparc.h sunc/x86.h sunc/x86_64.h

sysdepdir= ${includedir}/atomic_ops/sysdeps

# A few architectures require special .S files
EXTRA_DIST = sunc/sparc.S
