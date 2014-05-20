dnl
dnl $HEADER
dnl

dnl This file is automatically created by autogen.sh; it should not
dnl be edited by hand!!

m4_define([mca_backtrace_no_config_component_list], [])
m4_define([mca_backtrace_m4_config_component_list], [execinfo, printstack, darwin, none])
m4_define([mca_carto_no_config_component_list], [auto_detect, file])
m4_define([mca_carto_m4_config_component_list], [])
m4_define([mca_crs_no_config_component_list], [none])
m4_define([mca_crs_m4_config_component_list], [blcr, self])
m4_define([mca_hwloc_no_config_component_list], [])
m4_define([mca_hwloc_m4_config_component_list], [hwloc132, external])
m4_define([mca_installdirs_no_config_component_list], [])
m4_define([mca_installdirs_m4_config_component_list], [env, config, windows])
m4_define([mca_maffinity_no_config_component_list], [first_use])
m4_define([mca_maffinity_m4_config_component_list], [hwloc])
m4_define([mca_memchecker_no_config_component_list], [])
m4_define([mca_memchecker_m4_config_component_list], [valgrind])
m4_define([mca_memcpy_no_config_component_list], [])
m4_define([mca_memcpy_m4_config_component_list], [])
m4_define([mca_memory_no_config_component_list], [])
m4_define([mca_memory_m4_config_component_list], [linux, malloc_solaris])
m4_define([mca_paffinity_no_config_component_list], [])
m4_define([mca_paffinity_m4_config_component_list], [hwloc, test])
m4_define([mca_pstat_no_config_component_list], [])
m4_define([mca_pstat_m4_config_component_list], [linux, darwin])
m4_define([mca_shmem_no_config_component_list], [])
m4_define([mca_shmem_m4_config_component_list], [mmap, posix, sysv, windows])
m4_define([mca_sysinfo_no_config_component_list], [])
m4_define([mca_sysinfo_m4_config_component_list], [linux, solaris, darwin])
m4_define([mca_timer_no_config_component_list], [])
m4_define([mca_timer_m4_config_component_list], [altix, catamount, aix, darwin, solaris, windows, linux])
m4_define([mca_opal_framework_list], [backtrace, carto, crs, hwloc, installdirs, maffinity, memchecker, memcpy, memory, paffinity, pstat, shmem, sysinfo, timer])
m4_define([mca_errmgr_no_config_component_list], [default])
m4_define([mca_errmgr_m4_config_component_list], [])
m4_define([mca_ess_no_config_component_list], [])
m4_define([mca_ess_m4_config_component_list], [portals_utcp, cnos, alps, env, hnp, lsf, pmi, singleton, slave, slurm, slurmd, tm, tool])
m4_define([mca_filem_no_config_component_list], [rsh])
m4_define([mca_filem_m4_config_component_list], [])
m4_define([mca_grpcomm_no_config_component_list], [])
m4_define([mca_grpcomm_m4_config_component_list], [cnos, bad, basic, hier, pmi])
m4_define([mca_iof_no_config_component_list], [hnp, orted, tool])
m4_define([mca_iof_m4_config_component_list], [])
m4_define([mca_notifier_no_config_component_list], [])
m4_define([mca_notifier_m4_config_component_list], [command, ftb, smtp, syslog])
m4_define([mca_odls_no_config_component_list], [])
m4_define([mca_odls_m4_config_component_list], [default, process])
m4_define([mca_oob_no_config_component_list], [])
m4_define([mca_oob_m4_config_component_list], [tcp])
m4_define([mca_plm_no_config_component_list], [])
m4_define([mca_plm_m4_config_component_list], [alps, ccp, lsf, process, rsh, slurm, tm])
m4_define([mca_ras_no_config_component_list], [cm])
m4_define([mca_ras_m4_config_component_list], [alps, ccp, gridengine, loadleveler, lsf, slurm, tm])
m4_define([mca_rmaps_no_config_component_list], [load_balance, rank_file, resilient, round_robin, seq, topo])
m4_define([mca_rmaps_m4_config_component_list], [])
m4_define([mca_rml_no_config_component_list], [oob])
m4_define([mca_rml_m4_config_component_list], [ftrm])
m4_define([mca_routed_no_config_component_list], [binomial, cm, direct, linear, radix, slave])
m4_define([mca_routed_m4_config_component_list], [])
m4_define([mca_snapc_no_config_component_list], [])
m4_define([mca_snapc_m4_config_component_list], [full])
m4_define([mca_orte_framework_list], [errmgr, ess, filem, grpcomm, iof, notifier, odls, oob, plm, ras, rmaps, rml, routed, snapc])
m4_define([mca_allocator_no_config_component_list], [basic, bucket])
m4_define([mca_allocator_m4_config_component_list], [])
m4_define([mca_bml_no_config_component_list], [r2])
m4_define([mca_bml_m4_config_component_list], [])
m4_define([mca_btl_no_config_component_list], [self])
m4_define([mca_btl_m4_config_component_list], [elan, mx, ofud, openib, portals, sctp, sm, tcp, udapl])
m4_define([mca_coll_no_config_component_list], [basic, hierarch, inter, self, sm, sync, tuned])
m4_define([mca_coll_m4_config_component_list], [fca])
m4_define([mca_common_no_config_component_list], [sm])
m4_define([mca_common_m4_config_component_list], [mx, portals])
m4_define([mca_crcp_no_config_component_list], [])
m4_define([mca_crcp_m4_config_component_list], [bkmrk])
m4_define([mca_dpm_no_config_component_list], [orte])
m4_define([mca_dpm_m4_config_component_list], [])
m4_define([mca_io_no_config_component_list], [])
m4_define([mca_io_m4_config_component_list], [romio])
m4_define([mca_mpool_no_config_component_list], [fake, rdma, sm])
m4_define([mca_mpool_m4_config_component_list], [])
m4_define([mca_mtl_no_config_component_list], [])
m4_define([mca_mtl_m4_config_component_list], [mx, mxm, portals, psm])
m4_define([mca_op_no_config_component_list], [])
m4_define([mca_op_m4_config_component_list], [])
m4_define([mca_osc_no_config_component_list], [pt2pt, rdma])
m4_define([mca_osc_m4_config_component_list], [])
m4_define([mca_vprotocol_no_config_component_list], [pessimist])
m4_define([mca_vprotocol_m4_config_component_list], [])
m4_define([mca_pml_no_config_component_list], [bfo, cm, csum, ob1])
m4_define([mca_pml_m4_config_component_list], [crcpw, v])
m4_define([mca_pubsub_no_config_component_list], [orte])
m4_define([mca_pubsub_m4_config_component_list], [pmi])
m4_define([mca_rcache_no_config_component_list], [vma])
m4_define([mca_rcache_m4_config_component_list], [])
m4_define([mca_topo_no_config_component_list], [unity])
m4_define([mca_topo_m4_config_component_list], [])
m4_define([mca_ompi_framework_list], [allocator, bml, btl, coll, common, crcp, dpm, io, mpool, mtl, op, osc, pml, pubsub, rcache, topo])
m4_define([mca_project_list], [opal, orte, ompi])

dnl List all the no-configure components that we found, and AC_DEFINE
dnl their versions

AC_DEFUN([MCA_NO_CONFIG_CONFIG_FILES],[

AC_CONFIG_FILES(opal/mca/backtrace/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/backtrace/darwin

AC_CONFIG_FILES([opal/mca/backtrace/darwin/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/backtrace/execinfo

AC_CONFIG_FILES([opal/mca/backtrace/execinfo/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/backtrace/none

AC_CONFIG_FILES([opal/mca/backtrace/none/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/backtrace/printstack

AC_CONFIG_FILES([opal/mca/backtrace/printstack/Makefile])
AC_CONFIG_FILES(opal/mca/carto/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    opal/mca/carto/auto_detect

AC_CONFIG_FILES([opal/mca/carto/auto_detect/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    opal/mca/carto/file

AC_CONFIG_FILES([opal/mca/carto/file/Makefile])
AC_CONFIG_FILES(opal/mca/crs/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/crs/blcr

AC_CONFIG_FILES([opal/mca/crs/blcr/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    opal/mca/crs/none

AC_CONFIG_FILES([opal/mca/crs/none/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/crs/self

AC_CONFIG_FILES([opal/mca/crs/self/Makefile])
AC_CONFIG_FILES(opal/mca/hwloc/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/hwloc/external

AC_CONFIG_FILES([opal/mca/hwloc/external/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/hwloc/hwloc132

AC_CONFIG_FILES([opal/mca/hwloc/hwloc132/Makefile])
AC_CONFIG_FILES(opal/mca/installdirs/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/installdirs/config

AC_CONFIG_FILES([opal/mca/installdirs/config/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/installdirs/env

AC_CONFIG_FILES([opal/mca/installdirs/env/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/installdirs/windows

AC_CONFIG_FILES([opal/mca/installdirs/windows/Makefile])
AC_CONFIG_FILES(opal/mca/maffinity/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    opal/mca/maffinity/first_use

AC_CONFIG_FILES([opal/mca/maffinity/first_use/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/maffinity/hwloc

AC_CONFIG_FILES([opal/mca/maffinity/hwloc/Makefile])
AC_CONFIG_FILES(opal/mca/memchecker/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/memchecker/valgrind

AC_CONFIG_FILES([opal/mca/memchecker/valgrind/Makefile])
AC_CONFIG_FILES(opal/mca/memcpy/Makefile)
AC_CONFIG_FILES(opal/mca/memory/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/memory/linux

AC_CONFIG_FILES([opal/mca/memory/linux/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/memory/malloc_solaris

AC_CONFIG_FILES([opal/mca/memory/malloc_solaris/Makefile])
AC_CONFIG_FILES(opal/mca/paffinity/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/paffinity/hwloc

AC_CONFIG_FILES([opal/mca/paffinity/hwloc/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/paffinity/test

AC_CONFIG_FILES([opal/mca/paffinity/test/Makefile])
AC_CONFIG_FILES(opal/mca/pstat/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/pstat/darwin

AC_CONFIG_FILES([opal/mca/pstat/darwin/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/pstat/linux

AC_CONFIG_FILES([opal/mca/pstat/linux/Makefile])
AC_CONFIG_FILES(opal/mca/shmem/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/shmem/mmap

AC_CONFIG_FILES([opal/mca/shmem/mmap/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/shmem/posix

AC_CONFIG_FILES([opal/mca/shmem/posix/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/shmem/sysv

AC_CONFIG_FILES([opal/mca/shmem/sysv/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/shmem/windows

AC_CONFIG_FILES([opal/mca/shmem/windows/Makefile])
AC_CONFIG_FILES(opal/mca/sysinfo/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/sysinfo/darwin

AC_CONFIG_FILES([opal/mca/sysinfo/darwin/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/sysinfo/linux

AC_CONFIG_FILES([opal/mca/sysinfo/linux/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/sysinfo/solaris

AC_CONFIG_FILES([opal/mca/sysinfo/solaris/Makefile])
AC_CONFIG_FILES(opal/mca/timer/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/aix

AC_CONFIG_FILES([opal/mca/timer/aix/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/altix

AC_CONFIG_FILES([opal/mca/timer/altix/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/catamount

AC_CONFIG_FILES([opal/mca/timer/catamount/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/darwin

AC_CONFIG_FILES([opal/mca/timer/darwin/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/linux

AC_CONFIG_FILES([opal/mca/timer/linux/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/solaris

AC_CONFIG_FILES([opal/mca/timer/solaris/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    opal/mca/timer/windows

AC_CONFIG_FILES([opal/mca/timer/windows/Makefile])
AC_CONFIG_FILES(orte/mca/errmgr/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/errmgr/default

AC_CONFIG_FILES([orte/mca/errmgr/default/Makefile])
AC_CONFIG_FILES(orte/mca/ess/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/alps

AC_CONFIG_FILES([orte/mca/ess/alps/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/cnos

AC_CONFIG_FILES([orte/mca/ess/cnos/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/env

AC_CONFIG_FILES([orte/mca/ess/env/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/hnp

AC_CONFIG_FILES([orte/mca/ess/hnp/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/lsf

AC_CONFIG_FILES([orte/mca/ess/lsf/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/pmi

AC_CONFIG_FILES([orte/mca/ess/pmi/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/portals_utcp

AC_CONFIG_FILES([orte/mca/ess/portals_utcp/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/singleton

AC_CONFIG_FILES([orte/mca/ess/singleton/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/slave

AC_CONFIG_FILES([orte/mca/ess/slave/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/slurm

AC_CONFIG_FILES([orte/mca/ess/slurm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/slurmd

AC_CONFIG_FILES([orte/mca/ess/slurmd/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/tm

AC_CONFIG_FILES([orte/mca/ess/tm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ess/tool

AC_CONFIG_FILES([orte/mca/ess/tool/Makefile])
AC_CONFIG_FILES(orte/mca/filem/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/filem/rsh

AC_CONFIG_FILES([orte/mca/filem/rsh/Makefile])
AC_CONFIG_FILES(orte/mca/grpcomm/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/grpcomm/bad

AC_CONFIG_FILES([orte/mca/grpcomm/bad/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/grpcomm/basic

AC_CONFIG_FILES([orte/mca/grpcomm/basic/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/grpcomm/cnos

AC_CONFIG_FILES([orte/mca/grpcomm/cnos/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/grpcomm/hier

AC_CONFIG_FILES([orte/mca/grpcomm/hier/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/grpcomm/pmi

AC_CONFIG_FILES([orte/mca/grpcomm/pmi/Makefile])
AC_CONFIG_FILES(orte/mca/iof/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/iof/hnp

AC_CONFIG_FILES([orte/mca/iof/hnp/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/iof/orted

AC_CONFIG_FILES([orte/mca/iof/orted/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/iof/tool

AC_CONFIG_FILES([orte/mca/iof/tool/Makefile])
AC_CONFIG_FILES(orte/mca/notifier/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/notifier/command

AC_CONFIG_FILES([orte/mca/notifier/command/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/notifier/ftb

AC_CONFIG_FILES([orte/mca/notifier/ftb/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/notifier/smtp

AC_CONFIG_FILES([orte/mca/notifier/smtp/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/notifier/syslog

AC_CONFIG_FILES([orte/mca/notifier/syslog/Makefile])
AC_CONFIG_FILES(orte/mca/odls/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/odls/default

AC_CONFIG_FILES([orte/mca/odls/default/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/odls/process

AC_CONFIG_FILES([orte/mca/odls/process/Makefile])
AC_CONFIG_FILES(orte/mca/oob/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/oob/tcp

AC_CONFIG_FILES([orte/mca/oob/tcp/Makefile])
AC_CONFIG_FILES(orte/mca/plm/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/alps

AC_CONFIG_FILES([orte/mca/plm/alps/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/ccp

AC_CONFIG_FILES([orte/mca/plm/ccp/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/lsf

AC_CONFIG_FILES([orte/mca/plm/lsf/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/process

AC_CONFIG_FILES([orte/mca/plm/process/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/rsh

AC_CONFIG_FILES([orte/mca/plm/rsh/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/slurm

AC_CONFIG_FILES([orte/mca/plm/slurm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/plm/tm

AC_CONFIG_FILES([orte/mca/plm/tm/Makefile])
AC_CONFIG_FILES(orte/mca/ras/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/alps

AC_CONFIG_FILES([orte/mca/ras/alps/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/ccp

AC_CONFIG_FILES([orte/mca/ras/ccp/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/ras/cm

AC_CONFIG_FILES([orte/mca/ras/cm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/gridengine

AC_CONFIG_FILES([orte/mca/ras/gridengine/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/loadleveler

AC_CONFIG_FILES([orte/mca/ras/loadleveler/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/lsf

AC_CONFIG_FILES([orte/mca/ras/lsf/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/slurm

AC_CONFIG_FILES([orte/mca/ras/slurm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/ras/tm

AC_CONFIG_FILES([orte/mca/ras/tm/Makefile])
AC_CONFIG_FILES(orte/mca/rmaps/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/load_balance

AC_CONFIG_FILES([orte/mca/rmaps/load_balance/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/rank_file

AC_CONFIG_FILES([orte/mca/rmaps/rank_file/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/resilient

AC_CONFIG_FILES([orte/mca/rmaps/resilient/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/round_robin

AC_CONFIG_FILES([orte/mca/rmaps/round_robin/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/seq

AC_CONFIG_FILES([orte/mca/rmaps/seq/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rmaps/topo

AC_CONFIG_FILES([orte/mca/rmaps/topo/Makefile])
AC_CONFIG_FILES(orte/mca/rml/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/rml/ftrm

AC_CONFIG_FILES([orte/mca/rml/ftrm/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/rml/oob

AC_CONFIG_FILES([orte/mca/rml/oob/Makefile])
AC_CONFIG_FILES(orte/mca/routed/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/binomial

AC_CONFIG_FILES([orte/mca/routed/binomial/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/cm

AC_CONFIG_FILES([orte/mca/routed/cm/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/direct

AC_CONFIG_FILES([orte/mca/routed/direct/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/linear

AC_CONFIG_FILES([orte/mca/routed/linear/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/radix

AC_CONFIG_FILES([orte/mca/routed/radix/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    orte/mca/routed/slave

AC_CONFIG_FILES([orte/mca/routed/slave/Makefile])
AC_CONFIG_FILES(orte/mca/snapc/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    orte/mca/snapc/full

AC_CONFIG_FILES([orte/mca/snapc/full/Makefile])
AC_CONFIG_FILES(ompi/mca/allocator/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/allocator/basic

AC_CONFIG_FILES([ompi/mca/allocator/basic/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/allocator/bucket

AC_CONFIG_FILES([ompi/mca/allocator/bucket/Makefile])
AC_CONFIG_FILES(ompi/mca/bml/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/bml/r2

AC_CONFIG_FILES([ompi/mca/bml/r2/Makefile])
AC_CONFIG_FILES(ompi/mca/btl/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/elan

AC_CONFIG_FILES([ompi/mca/btl/elan/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/mx

AC_CONFIG_FILES([ompi/mca/btl/mx/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/ofud

AC_CONFIG_FILES([ompi/mca/btl/ofud/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/openib

AC_CONFIG_FILES([ompi/mca/btl/openib/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/portals

AC_CONFIG_FILES([ompi/mca/btl/portals/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/sctp

AC_CONFIG_FILES([ompi/mca/btl/sctp/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/btl/self

AC_CONFIG_FILES([ompi/mca/btl/self/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/sm

AC_CONFIG_FILES([ompi/mca/btl/sm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/tcp

AC_CONFIG_FILES([ompi/mca/btl/tcp/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/btl/udapl

AC_CONFIG_FILES([ompi/mca/btl/udapl/Makefile])
AC_CONFIG_FILES(ompi/mca/coll/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/basic

AC_CONFIG_FILES([ompi/mca/coll/basic/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/coll/fca

AC_CONFIG_FILES([ompi/mca/coll/fca/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/hierarch

AC_CONFIG_FILES([ompi/mca/coll/hierarch/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/inter

AC_CONFIG_FILES([ompi/mca/coll/inter/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/self

AC_CONFIG_FILES([ompi/mca/coll/self/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/sm

AC_CONFIG_FILES([ompi/mca/coll/sm/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/sync

AC_CONFIG_FILES([ompi/mca/coll/sync/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/coll/tuned

AC_CONFIG_FILES([ompi/mca/coll/tuned/Makefile])
AC_CONFIG_FILES(ompi/mca/common/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/common/mx

AC_CONFIG_FILES([ompi/mca/common/mx/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/common/portals

AC_CONFIG_FILES([ompi/mca/common/portals/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/common/sm

AC_CONFIG_FILES([ompi/mca/common/sm/Makefile])
AC_CONFIG_FILES(ompi/mca/crcp/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/crcp/bkmrk

AC_CONFIG_FILES([ompi/mca/crcp/bkmrk/Makefile])
AC_CONFIG_FILES(ompi/mca/dpm/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/dpm/orte

AC_CONFIG_FILES([ompi/mca/dpm/orte/Makefile])
AC_CONFIG_FILES(ompi/mca/io/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/io/romio

AC_CONFIG_FILES([ompi/mca/io/romio/Makefile])
AC_CONFIG_FILES(ompi/mca/mpool/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/mpool/fake

AC_CONFIG_FILES([ompi/mca/mpool/fake/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/mpool/rdma

AC_CONFIG_FILES([ompi/mca/mpool/rdma/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/mpool/sm

AC_CONFIG_FILES([ompi/mca/mpool/sm/Makefile])
AC_CONFIG_FILES(ompi/mca/mtl/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/mtl/mx

AC_CONFIG_FILES([ompi/mca/mtl/mx/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/mtl/mxm

AC_CONFIG_FILES([ompi/mca/mtl/mxm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/mtl/portals

AC_CONFIG_FILES([ompi/mca/mtl/portals/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/mtl/psm

AC_CONFIG_FILES([ompi/mca/mtl/psm/Makefile])
AC_CONFIG_FILES(ompi/mca/op/Makefile)
AC_CONFIG_FILES(ompi/mca/osc/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/osc/pt2pt

AC_CONFIG_FILES([ompi/mca/osc/pt2pt/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/osc/rdma

AC_CONFIG_FILES([ompi/mca/osc/rdma/Makefile])
AC_CONFIG_FILES(ompi/mca/pml/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pml/bfo

AC_CONFIG_FILES([ompi/mca/pml/bfo/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pml/cm

AC_CONFIG_FILES([ompi/mca/pml/cm/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/pml/crcpw

AC_CONFIG_FILES([ompi/mca/pml/crcpw/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pml/csum

AC_CONFIG_FILES([ompi/mca/pml/csum/Makefile])
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pml/ob1

AC_CONFIG_FILES([ompi/mca/pml/ob1/Makefile])
AC_CONFIG_FILES(ompi/mca/pml/v/mca/vprotocol/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pml/v/mca/vprotocol/pessimist

AC_CONFIG_FILES([ompi/mca/pml/v/mca/vprotocol/pessimist/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/pml/v

AC_CONFIG_FILES([ompi/mca/pml/v/Makefile])
AC_CONFIG_FILES(ompi/mca/pubsub/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/pubsub/orte

AC_CONFIG_FILES([ompi/mca/pubsub/orte/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mca/pubsub/pmi

AC_CONFIG_FILES([ompi/mca/pubsub/pmi/Makefile])
AC_CONFIG_FILES(ompi/mca/rcache/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/rcache/vma

AC_CONFIG_FILES([ompi/mca/rcache/vma/Makefile])
AC_CONFIG_FILES(ompi/mca/topo/Makefile)
dnl ----------------------------------------------------------------

dnl No-configure component: 
dnl    ompi/mca/topo/unity

AC_CONFIG_FILES([ompi/mca/topo/unity/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/contrib/libompitrace

AC_CONFIG_FILES([ompi/contrib/libompitrace/Makefile])
])dnl
