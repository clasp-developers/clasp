#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.options

# Conditionals whether to build each subdir or not

if BUILD_BGL
BGL_DIR = ad_bgl
BGL_LIB = ad_bgl/libadio_bgl.la
else
BGL_DIR =
BGL_LIB =
endif

if BUILD_BGLOCKLESS
BGLOCKLESS_DIR = ad_bglockless
BGLOCKLESS_LIB = ad_bglockless/libadio_bglockless.la
else
BGLOCKLESS_DIR =
BGLOCKLESS_LIB =
endif

if BUILD_GRIDFTP
GRIDFTP_DIR = ad_gridftp
GRIDFTP_LIB = ad_gridftp/libadio_gridftp.la
else
GRIDFTP_DIR =
GRIDFTP_LIB =
endif

# 8 July 2008: romio-maint@mcs.anl.gov says that this is deprecated
#if BUILD_HFS
#HFS_DIR = ad_hfs
#HFS_LIB = ad_hfs/libadio_hfs.la
#else
#HFS_DIR =
#HFS_LIB =
#endif

if BUILD_LUSTRE
LUSTRE_DIR = ad_lustre
LUSTRE_LIB = ad_lustre/libadio_lustre.la
else
LUSTRE_DIR =
LUSTRE_LIB =
endif

if BUILD_NFS
NFS_DIR = ad_nfs
NFS_LIB = ad_nfs/libadio_nfs.la
else
NFS_DIR =
NFS_LIB =
endif

# Currently disabled
#if BUILD_NTFS
#NTFS_DIR = ad_ntfs
#NTFS_LIB = ad_ntfs/libadio_ntfs.la
#else
#NTFS_DIR =
#NTFS_LIB =
#endif

if BUILD_PANFS
PANFS_DIR = ad_panfs
PANFS_LIB = ad_panfs/libadio_panfs.la
else
PANFS_DIR =
PANFS_LIB =
endif

if BUILD_PFS
PFS_DIR = ad_pfs
PFS_LIB = ad_pfs/libadio_pfs.la
else
PFS_DIR =
PFS_LIB =
endif

# 8 July 2008: romio-maint@mcs.anl.gov says that this is deprecated
#if BUILD_PIOFS
#PIOFS_DIR = ad_piofs
#PIOFS_LIB = ad_piofs/libadio_piofs.la
#else
#PIOFS_DIR =
#PIOFS_LIB =
#endif

if BUILD_PVFS
PVFS_DIR = ad_pvfs
PVFS_LIB = ad_pvfs/libadio_pvfs.la
else
PVFS_DIR =
PVFS_LIB =
endif

if BUILD_PVFS2
PVFS2_DIR = ad_pvfs2
PVFS2_LIB = ad_pvfs2/libadio_pvfs2.la
else
PVFS2_DIR =
PVFS2_LIB =
endif

if BUILD_SFS
SFS_DIR = ad_sfs
SFS_LIB = ad_sfs/libadio_sfs.la
else
SFS_DIR =
SFS_LIB =
endif

if BUILD_TESTFS
TESTFS_DIR = ad_testfs
TESTFS_LIB = ad_testfs/libadio_testfs.la
else
TESTFS_DIR =
TESTFS_LIB =
endif

if BUILD_UFS
UFS_DIR = ad_ufs
UFS_LIB = ad_ufs/libadio_ufs.la
else
UFS_DIR =
UFS_LIB =
endif

if BUILD_XFS
XFS_DIR = ad_xfs
XFS_LIB = ad_xfs/libadio_xfs.la
else
XFS_DIR =
XFS_LIB =
endif

if BUILD_ZOIDFS
ZOID_DIR = ad_zoidfs
ZOID_LIB = ad_zoidfs/libadio_zoidfs.la
else
ZOID_DIR =
ZOID_LIB =
endif

SUBDIRS = common include \
	$(BG_DIR) $(BGLOCKLESS_DIR) \
	$(GRIDFTP_DIR) $(LUSTRE_DIR) $(NFS_DIR) $(NTFS_DIR) $(PANFS_DIR) \
	$(PFS_DIR) $(PVFS_DIR) $(PVFS2_DIR) $(SFS_DIR) \
	$(TESTFS_DIR) $(UFS_DIR) $(XFS_DIR) $(ZOID_DIR)
DIST_SUBDIRS = common include \
        ad_bgl ad_bglockless ad_gridftp ad_lustre ad_nfs ad_ntfs \
        ad_panfs ad_pfs ad_pvfs	ad_pvfs2 ad_sfs ad_testfs ad_ufs \
        ad_xfs ad_zoidfs

# Library
noinst_LTLIBRARIES = libadio.la
libadio_la_SOURCES =
libadio_la_LIBADD = \
        common/libadio_common.la \
	$(BG_LIB) $(BGLOCKLESS_LIB) \
        $(GRIDFTP_LIB) $(LUSTRE_LIB) $(NFS_LIB) $(NTFS_LIB) $(PANFS_LIB) \
        $(PFS_LIB) $(PVFS_LIB) $(PVFS2_LIB) $(SFS_LIB) \
        $(TESTFS_LIB) $(UFS_LIB) $(XFS_LIB) $(ZOID_LIB)
