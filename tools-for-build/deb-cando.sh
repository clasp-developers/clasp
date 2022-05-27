#!/usr/bin/env bash
################################################################################
# This script was derived from llvm.sh of the LLVM Project and therefore is
# under the Apache License v2.0 with LLVM Exceptions.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
################################################################################
#
# This script will install the clasp or cando on the different Debian and Ubuntu
# versions

set -eux

if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root!"
   exit 1
fi

apt-get install -y lsb-release curl gpg software-properties-common

DISTRO=$(lsb_release -is)
VERSION=$(lsb_release -sr)
DIST_VERSION="${DISTRO}_${VERSION}"

# find the right repository name for the distro and version
case "$DIST_VERSION" in
    Debian_testing )  REPO_NAME="deb https://www.thirdlaw.tech/pkg/bookworm/ ./" ;;
    Ubuntu_22.04 )    REPO_NAME="deb https://www.thirdlaw.tech/pkg/jammy/ ./" ;;
    * )
    echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
esac

# install everything
curl -fsSL https://www.thirdlaw.tech/pkg/key.gpg | gpg --yes --dearmor --output /etc/apt/trusted.gpg.d/thirdlaw.gpg
add-apt-repository "${REPO_NAME}"
apt-get update
apt-get install -y cando