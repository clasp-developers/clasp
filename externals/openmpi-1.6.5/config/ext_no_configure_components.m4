dnl
dnl $HEADER
dnl

dnl This file is automatically created by autogen.sh; it should not
dnl be edited by hand!!

m4_define([ext_mpiext_no_config_component_list], [])
m4_define([ext_mpiext_m4_config_component_list], [affinity, example])
m4_define([ext_ompi_framework_list], [mpiext])
m4_define([ext_project_list], [ompi])

dnl List all the no-configure components that we found, and AC_DEFINE
dnl their versions

AC_DEFUN([EXT_NO_CONFIG_CONFIG_FILES],[

AC_CONFIG_FILES(ompi/mpiext/Makefile)
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mpiext/affinity

AC_CONFIG_FILES([ompi/mpiext/affinity/Makefile])
dnl ----------------------------------------------------------------

dnl m4-configure component: 
dnl    ompi/mpiext/example

AC_CONFIG_FILES([ompi/mpiext/example/Makefile])
])dnl
