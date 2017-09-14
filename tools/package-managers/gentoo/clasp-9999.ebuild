# Copyright 1999-2017 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Id$

EAPI=6

PYTHON_COMPAT=( python3_3 python3_4 python3_5 )
PYTHON_REQ_USE="+threads"

inherit flag-o-matic git-r3 llvm python-r1 waf-utils

LLVM_MAX_SLOT="5"

DESCRIPTION="Common LISP implementation targetting LLVM 5"
HOMEPAGE="https://github.com/drmeister/clasp/wiki"
EGIT_REPO_URI="git://github.com/drmeister/clasp
			   https://github.com/drmeister/clasp"
EGIT_BRANCH="dev"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64 ~x86 ~arm ~ppc"
IUSE=""

DEPEND="sys-devel/llvm:5
		sys-devel/clang:5
		sys-devel/binutils
		dev-libs/boehm-gc
		dev-libs/gmp:0
		app-arch/bzip2
		sys-libs/zlib
		dev-libs/expat
		dev-libs/boost
		sys-devel/flex
		net-libs/zeromq"
RDEPEND="${DEPEND}"

CC="clang"
CXX="clang++"

pkg_setup() {
	llvm_pkg_setup
	python_setup
	einfo "Trimming C(XX)FLAGS known not to work with Clang(++)..."
	filter-flags "-mfxsr" "-mlwp" "-msahf" "-mxsave"
}

src_prepare() {
	cd "${S}"
	einfo "Copying wscript.config from wscript.config.template..."
	cp wscript.config.template wscript.config
	einfo "Munging wscript.config to point LLVM_CONFIG_BINARY to:\n$(get_llvm_prefix 5)/bin/llvm-config ..."
	sed -i -e "s:\\(LLVM_CONFIG_BINARY = '\\).*\\('\\):\\1$(get_llvm_prefix 5)/bin/llvm-config\\2:" wscript.config
	einfo "Running Clasp-specific automated build prep..."
	"${S}/waf" update_submodules # Submodules have already been fetched
								 # and checked out, this just concatenates
								 # some stuff for the build
	eapply_user
}

src_compile() {
	"${S}/waf" build_cboehm
}

src_install() {
	into /usr
	newbin ${S}/build/boehm/iclasp-boehm clasp
}

