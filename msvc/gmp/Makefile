srcdir = ..\..\src\gmp

#
# Change for other configurations
#
!if "$(BITS)" == "64"
YASM_TARGET=win64
YASM_FLAGS=-D PIC
D_WIN64=/D "WIN64"
!if "$(MPN_TYPE)" == ""
MPN_TYPE=amd64
!endif
BITS=64
!else
YASM_TARGET=win32
YASM_FLAGS=
D_WIN64=
BITS=32
!if "$(MPN_TYPE)" == ""
MPN_TYPE=p3
!endif
!endif

!if "$(YASM)" == ""
YASM=yasm-1.2.0-win32.exe
!endif

# for amd
MPN_AMD64_C_SOURCES = \
    mpn\generic\add.c \
    mpn\generic\add_1.c \
    mpn\generic\add_n.c \
    mpn\generic\addmul_1.c \
    mpn\generic\addsub_n.c \
    mpn\generic\bdivmod.c \
    mpn\generic\cmp.c \
    mpn\generic\dc_divrem_n.c \
    mpn\generic\dive_1.c \
    mpn\generic\diveby3.c \
    mpn\generic\divis.c \
    mpn\generic\divrem.c \
    mpn\generic\divrem_1.c \
    mpn\generic\divrem_2.c \
    mpn\generic\dump.c \
    mpn\generic\fib2_ui.c \
    mpn\generic\gcd.c \
    mpn\generic\gcd_1.c \
    mpn\generic\gcdext.c \
    mpn\generic\get_d.c \
    mpn\generic\get_str.c \
    mpn\generic\jacbase.c \
    mpn\generic\lshift.c \
    mpn\generic\mod_1.c \
    mpn\generic\mod_34lsub1.c \
    mpn\generic\mode1o.c \
    mpn\generic\mul.c \
    mpn\generic\mul_1.c \
    mpn\generic\mul_basecase.c \
    mpn\generic\mul_fft.c \
    mpn\generic\mul_n.c \
    mpn\generic\mullow_basecase.c \
    mpn\generic\mullow_n.c \
    mpn\generic\perfsqr.c \
    .\mpn\generic\popham2.c \
    mpn\generic\pow_1.c \
    mpn\generic\pre_divrem_1.c \
    mpn\generic\pre_mod_1.c \
    mpn\generic\random.c \
    mpn\generic\random2.c \
    mpn\generic\rootrem.c \
    mpn\generic\rshift.c \
    mpn\generic\sb_divrem_mn.c \
    mpn\generic\scan0.c \
    mpn\generic\scan1.c \
    mpn\generic\set_str.c \
    mpn\generic\sizeinbase.c \
    mpn\generic\sqr_basecase.c \
    mpn\generic\sqrtrem.c \
    mpn\generic\sub.c \
    mpn\generic\sub_1.c \
    mpn\generic\sub_n.c \
    mpn\generic\submul_1.c \
    mpn\generic\tdiv_qr.c

# for p4
MPN_P4_C_SOURCES = \
    mpn\generic\add.c \
    mpn\generic\add_1.c \
    mpn\generic\addsub_n.c \
    mpn\generic\bdivmod.c \
    mpn\generic\cmp.c \
    mpn\generic\dc_divrem_n.c \
    mpn\generic\diveby3.c \
    mpn\generic\divis.c \
    mpn\generic\divrem.c \
    mpn\generic\divrem_2.c \
    mpn\generic\dump.c \
    mpn\generic\fib2_ui.c \
    mpn\generic\gcd.c \
    mpn\generic\gcd_1.c \
    mpn\generic\gcdext.c \
    mpn\generic\get_d.c \
    mpn\generic\get_str.c \
    mpn\generic\jacbase.c \
    mpn\generic\mul.c \
    mpn\generic\mul_fft.c \
    mpn\generic\mul_n.c \
    mpn\generic\mullow_basecase.c \
    mpn\generic\mullow_n.c \
    mpn\generic\perfsqr.c \
    mpn\generic\pow_1.c \
    mpn\generic\pre_divrem_1.c \
    mpn\generic\pre_mod_1.c \
    mpn\generic\random.c \
    mpn\generic\random2.c \
    mpn\generic\rootrem.c \
    mpn\generic\sb_divrem_mn.c \
    mpn\generic\scan0.c \
    mpn\generic\scan1.c \
    mpn\generic\set_str.c \
    mpn\generic\sizeinbase.c \
    mpn\generic\sqrtrem.c \
    mpn\generic\sub.c \
    mpn\generic\sub_1.c \
    mpn\generic\tdiv_qr.c

# for p3
MPN_P3_C_SOURCES = \
    mpn\generic\add.c \
    mpn\generic\add_1.c \
    mpn\generic\addsub_n.c \
    mpn\generic\bdivmod.c \
    mpn\generic\cmp.c \
    mpn\generic\dc_divrem_n.c \
    mpn\generic\diveby3.c \
    mpn\generic\divis.c \
    mpn\generic\divrem.c \
    mpn\generic\divrem_2.c \
    mpn\generic\dump.c \
    mpn\generic\fib2_ui.c \
    mpn\generic\gcd.c \
    mpn\generic\gcd_1.c \
    mpn\generic\gcdext.c \
    mpn\generic\get_d.c \
    mpn\generic\get_str.c \
    mpn\generic\jacbase.c \
    mpn\generic\mul.c \
    mpn\generic\mul_fft.c \
    mpn\generic\mul_n.c \
    mpn\generic\mullow_basecase.c \
    mpn\generic\mullow_n.c \
    mpn\generic\perfsqr.c \
    mpn\generic\pow_1.c \
    mpn\generic\random.c \
    mpn\generic\random2.c \
    mpn\generic\rootrem.c \
    mpn\generic\sb_divrem_mn.c \
    mpn\generic\scan0.c \
    mpn\generic\scan1.c \
    mpn\generic\set_str.c \
    mpn\generic\sizeinbase.c \
    mpn\generic\sqrtrem.c \
    mpn\generic\sub.c \
    mpn\generic\sub_1.c \
    mpn\generic\tdiv_qr.c

MPN_GC_SOURCES = \
    mpn\generic\add.c \
    mpn\generic\add_1.c \
    mpn\generic\add_n.c \
    mpn\generic\addmul_1.c \
    .\mpn\generic\addsub_n.c \
    mpn\generic\bdivmod.c \
    mpn\generic\cmp.c \
    mpn\generic\dc_divrem_n.c \
    mpn\generic\dive_1.c \
    mpn\generic\diveby3.c \
    mpn\generic\divis.c \
    mpn\generic\divrem.c \
    .\mpn\generic\divrem_1.c \
    .\mpn\generic\divrem_2.c \
    mpn\generic\dump.c \
    mpn\generic\fib2_ui.c \
    mpn\generic\gcd.c \
    mpn\generic\gcd_1.c \
    mpn\generic\gcdext.c \
    mpn\generic\get_d.c \
    mpn\generic\get_str.c \
    mpn\generic\jacbase.c \
    mpn\generic\lshift.c \
    mpn\generic\mod_1.c \
    mpn\generic\mod_34lsub1.c \
    mpn\generic\mode1o.c \
    mpn\generic\mul.c \
    mpn\generic\mul_1.c \
    mpn\generic\mul_basecase.c \
    mpn\generic\mul_fft.c \
    mpn\generic\mul_n.c \
    mpn\generic\mullow_basecase.c \
    mpn\generic\mullow_n.c \
    mpn\generic\perfsqr.c \
    .\mpn\generic\popham2.c \
    mpn\generic\pow_1.c \
    mpn\generic\pre_divrem_1.c \
    mpn\generic\pre_mod_1.c \
    mpn\generic\random.c \
    mpn\generic\random2.c \
    mpn\generic\rootrem.c \
    mpn\generic\rshift.c \
    mpn\generic\sb_divrem_mn.c \
    mpn\generic\scan0.c \
    mpn\generic\scan1.c \
    mpn\generic\set_str.c \
    mpn\generic\sizeinbase.c \
    mpn\generic\sqr_basecase.c \
    mpn\generic\sqrtrem.c \
    mpn\generic\sub.c \
    mpn\generic\sub_1.c \
    mpn\generic\sub_n.c \
    mpn\generic\submul_1.c \
    mpn\generic\tdiv_qr.c

MPF_SOURCES = \
    mpf\abs.c \
    mpf\add.c \
    mpf\add_ui.c \
    mpf\ceilfloor.c \
    mpf\clear.c \
    mpf\cmp.c \
    mpf\cmp_d.c \
    mpf\cmp_si.c \
    mpf\cmp_ui.c \
    mpf\div.c \
    mpf\div_2exp.c \
    mpf\div_ui.c \
    mpf\dump.c \
    mpf\eq.c \
    mpf\fits_sint.c \
    mpf\fits_slong.c \
    mpf\fits_sshort.c \
    mpf\fits_uint.c \
    mpf\fits_ulong.c \
    mpf\fits_ushort.c \
    mpf\get_d.c \
    mpf\get_d_2exp.c \
    mpf\get_dfl_prec.c \
    mpf\get_prc.c \
    mpf\get_si.c \
    mpf\get_str.c \
    mpf\get_ui.c \
    mpf\init.c \
    mpf\init2.c \
    mpf\inp_str.c \
    mpf\int_p.c \
    mpf\iset.c \
    mpf\iset_d.c \
    mpf\iset_si.c \
    mpf\iset_str.c \
    mpf\iset_ui.c \
    mpf\mul.c \
    mpf\mul_2exp.c \
    mpf\mul_ui.c \
    mpf\neg.c \
    mpf\out_str.c \
    mpf\pow_ui.c \
    mpf\random2.c \
    mpf\reldiff.c \
    mpf\set.c \
    mpf\set_d.c \
    mpf\set_dfl_prec.c \
    mpf\set_prc.c \
    mpf\set_prc_raw.c \
    mpf\set_q.c \
    mpf\set_si.c \
    mpf\set_str.c \
    mpf\set_ui.c \
    mpf\set_z.c \
    mpf\size.c \
    mpf\sqrt.c \
    mpf\sqrt_ui.c \
    mpf\sub.c \
    mpf\sub_ui.c \
    mpf\swap.c \
    mpf\trunc.c \
    mpf\ui_div.c \
    mpf\ui_sub.c \
    mpf\urandomb.c \

MPZ_SOURCES = \
    mpz\abs.c \
    mpz\add.c \
    mpz\add_ui.c \
    mpz\and.c \
    mpz\aorsmul.c \
    mpz\aorsmul_i.c \
    mpz\array_init.c \
    mpz\bin_ui.c \
    mpz\bin_uiui.c \
    mpz\cdiv_q.c \
    mpz\cdiv_q_ui.c \
    mpz\cdiv_qr.c \
    mpz\cdiv_qr_ui.c \
    mpz\cdiv_r.c \
    mpz\cdiv_r_ui.c \
    mpz\cdiv_ui.c \
    mpz\cfdiv_q_2exp.c \
    mpz\cfdiv_r_2exp.c \
    mpz\clear.c \
    mpz\clrbit.c \
    mpz\cmp.c \
    mpz\cmp_d.c \
    mpz\cmp_si.c \
    mpz\cmp_ui.c \
    mpz\cmpabs.c \
    mpz\cmpabs_d.c \
    mpz\cmpabs_ui.c \
    mpz\com.c \
    mpz\combit.c \
    mpz\cong.c \
    mpz\cong_2exp.c \
    mpz\cong_ui.c \
    mpz\dive_ui.c \
    mpz\divegcd.c \
    mpz\divexact.c \
    mpz\divis.c \
    mpz\divis_2exp.c \
    mpz\divis_ui.c \
    mpz\dump.c \
    mpz\export.c \
    mpz\fac_ui.c \
    mpz\fdiv_q.c \
    mpz\fdiv_q_ui.c \
    mpz\fdiv_qr.c \
    mpz\fdiv_qr_ui.c \
    mpz\fdiv_r.c \
    mpz\fdiv_r_ui.c \
    mpz\fdiv_ui.c \
    mpz\fib2_ui.c \
    mpz\fib_ui.c \
    mpz\fits_sint.c \
    mpz\fits_slong.c \
    mpz\fits_sshort.c \
    mpz\fits_uint.c \
    mpz\fits_ulong.c \
    mpz\fits_ushort.c \
    mpz\gcd.c \
    mpz\gcd_ui.c \
    mpz\gcdext.c \
    mpz\get_d.c \
    mpz\get_d_2exp.c \
    mpz\get_si.c \
    mpz\get_str.c \
    mpz\get_ui.c \
    mpz\getlimbn.c \
    mpz\hamdist.c \
    mpz\import.c \
    mpz\init.c \
    mpz\init2.c \
    mpz\inp_raw.c \
    mpz\inp_str.c \
    mpz\invert.c \
    mpz\ior.c \
    mpz\iset.c \
    mpz\iset_d.c \
    mpz\iset_si.c \
    mpz\iset_str.c \
    mpz\iset_ui.c \
    mpz\lcm.c \
    mpz\lcm_ui.c \
    mpz\lucnum2_ui.c \
    mpz\lucnum_ui.c \
    mpz\millerrabin.c \
    mpz\mod.c \
    mpz\mul.c \
    mpz\mul_2exp.c \
    mpz\mul_si.c \
    mpz\mul_ui.c \
    mpz\n_pow_ui.c \
    mpz\neg.c \
    mpz\nextprime.c \
    mpz\out_raw.c \
    mpz\out_str.c \
    mpz\perfpow.c \
    mpz\perfsqr.c \
    mpz\popcount.c \
    mpz\pow_ui.c \
    mpz\powm.c \
    mpz\powm_ui.c \
    mpz\pprime_p.c \
    mpz\random.c \
    mpz\random2.c \
    mpz\realloc.c \
    mpz\realloc2.c \
    mpz\remove.c \
    mpz\root.c \
    mpz\rootrem.c \
    mpz\rrandomb.c \
    mpz\scan0.c \
    mpz\scan1.c \
    mpz\set.c \
    .\mpz\set_d.c \
    mpz\set_f.c \
    mpz\set_q.c \
    mpz\set_si.c \
    mpz\set_str.c \
    mpz\set_ui.c \
    mpz\setbit.c \
    mpz\size.c \
    mpz\sizeinbase.c \
    mpz\sqrt.c \
    mpz\sqrtrem.c \
    mpz\sub.c \
    mpz\sub_ui.c \
    mpz\swap.c \
    mpz\tdiv_q.c \
    mpz\tdiv_q_2exp.c \
    mpz\tdiv_q_ui.c \
    mpz\tdiv_qr.c \
    mpz\tdiv_qr_ui.c \
    mpz\tdiv_r.c \
    mpz\tdiv_r_2exp.c \
    mpz\tdiv_r_ui.c \
    mpz\tdiv_ui.c \
    mpz\tstbit.c \
    mpz\ui_pow_ui.c \
    mpz\ui_sub.c \
    mpz\urandomb.c \
    mpz\urandomm.c \
    mpz\xor.c

GMP_SOURCES = \
    assert.c \
    compat.c \
    errno.c \
    extract-dbl.c \
    invalid.c \
    memory.c \
    mp_bpl.c \
    mp_clz_tab.c \
    mp_dv_tab.c \
    mp_get_fns.c \
    mp_minv_tab.c \
    mp_set_fns.c \
    rand.c \
    randbui.c \
    randclr.c \
    randdef.c \
    randiset.c \
    randlc2s.c \
    randlc2x.c \
    randmt.c \
    randmts.c \
    randmui.c \
    rands.c \
    randsd.c \
    randsdui.c \
    tal-reent.c \
    version.c \
    mp_bases.c \
    fib_table.c

MPN_AMD64_ASM_SOURCES = \
    mpn\amd64i\aors_n.asm \
    mpn\amd64i\aorsmul_1.asm \
    mpn\amd64i\copyd.asm \
    mpn\amd64i\copyi.asm \
    mpn\amd64i\dive_1.asm \
    mpn\amd64i\lshift.asm \
    mpn\amd64i\mode1o.asm \
    mpn\amd64i\mul_1.asm \
    mpn\amd64i\mul_basecase.asm \
    mpn\amd64i\rshift.asm \
    mpn\amd64i\sqr_basecase.asm

MPN_P4_ASM_SOURCES = \
    mpn\x86i\aors_n.asm \
    mpn\x86i\aorsmul_1.asm \
    mpn\x86i\pentium4\copyd.asm \
    mpn\x86i\pentium4\copyi.asm \
    mpn\x86i\dive_1.asm \
    mpn\x86i\diveby3.asm \
    mpn\x86i\divrem_1.asm \
    mpn\x86i\lshift.asm \
    mpn\x86i\mod_1.asm \
    mpn\x86i\mod_34lsub1.asm \
    mpn\x86i\mul_1.asm \
    mpn\x86i\mul_basecase.asm \
    mpn\x86i\rshift.asm \
    mpn\x86i\udiv.asm \
    mpn\x86i\umul.asm \
    mpn\x86i\pentium4\mmx\lshift.asm \
    mpn\x86i\pentium4\mmx\popham.asm \
    mpn\x86i\pentium4\mmx\rshift.asm \
    mpn\x86i\pentium4\sse2\add_n.asm \
    mpn\x86i\pentium4\sse2\addmul_1.asm \
    mpn\x86i\pentium4\sse2\dive_1.asm \
    mpn\x86i\pentium4\sse2\diveby3.asm \
    mpn\x86i\pentium4\sse2\mod_34lsub1.asm \
    mpn\x86i\pentium4\sse2\mode1o.asm \
    mpn\x86i\pentium4\sse2\mul_1.asm \
    mpn\x86i\pentium4\sse2\mul_basecase.asm \
    mpn\x86i\pentium4\sse2\sqr_basecase.asm \
    mpn\x86i\pentium4\sse2\sub_n.asm \
    mpn\x86i\pentium4\sse2\submul_1.asm

MPN_P3_ASM_SOURCES = \
    mpn\x86i\aors_n.asm \
    mpn\x86i\p6\aorsmul_1.asm \
    mpn\x86i\p6\copyd.asm \
    mpn\x86i\copyi.asm \
    mpn\x86i\p6\dive_1.asm \
    mpn\x86i\p6\diveby3.asm \
    mpn\x86i\divrem_1.asm \
    mpn\x86i\lshift.asm \
    mpn\x86i\p6\mod_1.asm \
    mpn\x86i\mod_34lsub1.asm \
    mpn\x86i\p6\mode1o.asm \
    mpn\x86i\mul_1.asm \
    mpn\x86i\mul_basecase.asm \
    mpn\x86i\rshift.asm \
    mpn\x86i\p6\sqr_basecase.asm \
    mpn\x86i\udiv.asm \
    mpn\x86i\umul.asm \
    mpn\x86i\p6\mmx\divrem_1.asm \
    mpn\x86i\p6\mmx\lshift.asm \
    mpn\x86i\p6\mmx\popham.asm \
    mpn\x86i\p6\p3mmx\popham.asm \
    mpn\x86i\p6\mmx\rshift.asm

MPN_GC_OBJS = $(MPN_GC_SOURCES:.c=.obj)
#MPN_P0_OBJS = $(MPN_P0_ASM_SOURCES:.asm=.obj) $(MPN_P0_C_SOURCES:.c=.obj)
MPN_P3_OBJS = $(MPN_P3_ASM_SOURCES:.asm=.obj) $(MPN_P3_C_SOURCES:.c=.obj)
MPN_P4_OBJS = $(MPN_P4_ASM_SOURCES:.asm=.obj) $(MPN_P4_C_SOURCES:.c=.obj)
MPN_AMD64_OBJS = $(MPN_AMD64_ASM_SOURCES:.asm=.obj) $(MPN_AMD64_C_SOURCES:.c=.obj) #
MPZ_OBJS = $(MPZ_SOURCES:.c=.obj)
GMP_OBJS = $(GMP_SOURCES:.c=.obj)

GMP_ALL_OBJS = $(MPZ_OBJS) $(GMP_OBJS)

AUTO_FILES = mp_bases.h mp_bases.c fac_ui.h fib_table.h fib_table.c perfsqr.h

all: $(AUTO_FILES) gmp.lib

mpn_gc.lib: msvc-build $(MPN_GC_OBJS)
	link -lib /NOLOGO /OUT:$@ $(MPN_GC_OBJS)

mpn_p0.lib: msvc-build $(MPN_P0_OBJS)
	link -lib /NOLOGO /OUT:$@ $(MPN_P0_OBJS)

mpn_p3.lib: msvc-build $(MPN_P3_OBJS)
	link -lib /NOLOGO /OUT:$@ $(MPN_P3_OBJS)

mpn_p4.lib: msvc-build $(MPN_P4_OBJS)
	link -lib /NOLOGO /OUT:$@ $(MPN_P4_OBJS)

mpn_amd64.lib: msvc-build $(MPN_AMD64_OBJS)
	link -lib /NOLOGO /OUT:$@ $(MPN_AMD64_OBJS)

gmp.lib: mpn_$(MPN_TYPE).lib $(GMP_ALL_OBJS)
	link -lib /NOLOGO /OUT:$@ *.obj mpz\*.obj mpn_$(MPN_TYPE).lib

#CFLAGS = $(CFLAGS_CONFIG) /nologo /W3 /EHsc /I "." /I $(srcdir) /I "$(srcdir)\mpn\generic" /D "WIN32" /D "_LIB" /D "_WIN32" /D "_MBCS"
CFLAGS = $(CFLAGS_CONFIG) /nologo /W3 /EHsc /I "." /I $(srcdir) /I "$(srcdir)\mpn\generic" /D "_LIB" /D "_MBCS" $(D_WIN64)

{.\mpn\generic}.c{.\mpn\generic}.obj:
	cl -c $(CFLAGS) /Fo$@ $?
{.\mpz}.c{.\mpz}.obj:
	cl -c $(CFLAGS) /Fo$@ $?
{$(srcdir)\mpn\generic}.c{mpn\generic}.obj:
	cl -c $(CFLAGS) /Fo$@ $?
{$(srcdir)\mpn}.c{mpn}.obj:
	cl -c $(CFLAGS) /Fo$@ $?
{$(srcdir)\mpz}.c{mpz}.obj:
	cl -c $(CFLAGS) /Fo$@ $?
{$(srcdir)}.c{}.obj:
	cl -c $(CFLAGS) /Fo$@ $?

.asm.obj:
#	nasm -O9 -Xvc -f win32 -i $(@D)\ -o $@ $?
	$(YASM) -O9 $(YASM_FLAGS) -Xvc -f $(YASM_TARGET) -i $(@D)\ -o $@ $?

gen-bases.exe: build.vc8\gen-bases\gen-bases.c
	cl $(CFLAGS) /Fe$@ $?
	erase gen-bases.obj
mp_bases.h: gen-bases.exe
	gen-bases.exe header $(BITS) 0 > $@
mp_bases.c: gen-bases.exe
	gen-bases.exe table $(BITS) 0 > $@

gen-fac_ui.exe: build.vc8\gen-fac_ui\gen-fac_ui.c
	cl $(CFLAGS) /Fe$@ $?
	erase gen-fac_ui.obj
fac_ui.h: gen-fac_ui.exe
	gen-fac_ui.exe $(BITS) 0 > $@

gen-fib.exe: build.vc8\gen-fib\gen-fib.c
	cl $(CFLAGS) /Fe$@ $?
	erase gen-fib.obj
fib_table.h: gen-fib.exe
	gen-fib.exe header $(BITS) 0 > $@
fib_table.c: gen-fib.exe
	gen-fib.exe table $(BITS) 0 > $@

gen-psqr.exe: build.vc8\gen-psqr\gen-psqr.c
	cl $(CFLAGS) /Fe$@ $?
	erase gen-psqr.obj
perfsqr.h: gen-psqr.exe
	gen-psqr.exe $(BITS) 0 > $@


msvc-build: build.vc8\config-vc8.$(MPN_TYPE) build.vc8\gmp.h
	copy build.vc8\config-vc8.$(MPN_TYPE) config.h
	copy build.vc8\gmp.h gmp.h
	echo > $@

clean:
	-erase mpn\generic\*.obj
	-erase mpn\*.obj
	-erase mpz\*.obj
	-erase *.obj
	-erase mpn_*.lib
	-erase gmp.lib
	-for %o in ($(MPN_P0_ASM_SOURCES:.asm=.obj)) do erase %o
	-for %o in ($(MPN_P3_ASM_SOURCES:.asm=.obj)) do erase %o
	-for %o in ($(MPN_P4_ASM_SOURCES:.asm=.obj)) do erase %o
	-for %o in ($(AUTO_FILES)) do erase %o
	-erase config.h
	-erase gmp.h
	-erase msvc-build
	-erase *.exe *.manifest
