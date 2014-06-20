#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_pml_v_CONFIG],[
    # We are going to make recursive call in shell, nothing is impossible
    # Still, we need to be extra careful
    (
        vprotocol_parent_component=$component
        # Move the root of our project to the current component
        srcdir=`cd $srcdir && pwd`/$project/mca/$framework
        cd "$project/mca/$framework"
        
        MCA_CONFIGURE_FRAMEWORK($vprotocol_parent_component, vprotocol, $should_build)

        # Save results
        cat >$vprotocol_parent_component/mca_vprotocol_config_output <<EOF
            #
            # /!\ This is Automatically generated file. Do not edit. 
            #

            # Apply the modifications to current shell context
            # and remove leading mca/vprotocol from subdirs

            MCA_vprotocol_ALL_COMPONENTS="$MCA_vprotocol_ALL_COMPONENTS"
            MCA_vprotocol_STATIC_COMPONENTS="$MCA_vprotocol_STATIC_COMPONENTS"
            MCA_vprotocol_DSO_COMPONENTS="$MCA_vprotocol_DSO_COMPONENTS"
            MCA_vprotocol_STATIC_LTLIBS="$MCA_vprotocol_STATIC_LTLIBS"
            
            MCA_vprotocol_ALL_SUBDIRS="$MCA_vprotocol_ALL_SUBDIRS"
            MCA_vprotocol_STATIC_SUBDIRS="$MCA_vprotocol_STATIC_SUBDIRS"
            MCA_vprotocol_DSO_SUBDIRS="$MCA_vprotocol_DSO_SUBDIRS"

            m4_foreach(mca_component, [mca_vprotocol_no_config_component_list],
                [m4_ifval(mca_component, [
                    [BUILD_vprotocol_]mca_component[_DSO]="$[BUILD_vprotocol_]mca_component[_DSO]"
                    AM_CONDITIONAL([OMPI_BUILD_vprotocol_]mca_component[_DSO], test "\$[BUILD_vprotocol_]mca_component[_DSO]" = "1")])])

            m4_foreach(mca_component, [mca_vprotocol_m4_config_component_list],
                [m4_ifval(mca_component, [
                    [BUILD_vprotocol_]mca_component[_DSO]= $[BUILD_vprotocol_]mca_component[_DSO]
                    AM_CONDITIONAL([OMPI_BUILD_vprotocol_]mca_component[_DSO], test "\$[BUILD_vprotocol_]mca_component[_DSO]" = "1")])])

EOF
    )
    
    # Reload the output from vprotocol framework's config
    . $project/mca/$framework/$component/mca_vprotocol_config_output
    rm  $project/mca/$framework/$component/mca_vprotocol_config_output

    AC_SUBST(MCA_vprotocol_ALL_COMPONENTS)
    AC_SUBST(MCA_vprotocol_STATIC_COMPONENTS)
    AC_SUBST(MCA_vprotocol_DSO_COMPONENTS)
    AC_SUBST(MCA_vprotocol_STATIC_LTLIBS)
    
    AC_SUBST(MCA_vprotocol_ALL_SUBDIRS)
    AC_SUBST(MCA_vprotocol_STATIC_SUBDIRS)
    AC_SUBST(MCA_vprotocol_DSO_SUBDIRS)
    
    # This unmatched if is intended to match the fi of the if we disabled
    AS_IF( [test "$should_build" = "1"],
           [$1],
           [$2])
])dnl
