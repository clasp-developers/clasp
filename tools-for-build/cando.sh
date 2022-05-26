#!/usr/bin/env bash

set -eux

if [[ $EUID -ne 0 ]]; then
  echo "This script must be run as root!"
  exit 1
fi

if [ -f /etc/os-release ]; then
  . /etc/os-release
else
  echo "Unable to find os-release!"
  exit 1
fi

function arch {
  curl -fsSL https://www.thirdlaw.tech/pkg/key.gpg | pacman-key --add -
  pacman-key --lsign-key 009C1F20737D81230A0928CBC812E10EC39DF202
  echo -e "\n[thirdlaw]\nServer = https://www.thirdlaw.tech/pkg/arch\n" >>/etc/pacman.conf
  pacman -Sy --noconfirm cando-git
}

function debian_like {
  apt-get install -y curl gpg software-properties-common
  curl -fsSL https://www.thirdlaw.tech/pkg/key.gpg | gpg --yes --dearmor --output /etc/apt/trusted.gpg.d/thirdlaw.gpg
  add-apt-repository "${REPO_NAME}"
  apt-get update
  apt-get install -y cando
}

function debian {
  if [ -z "${VERSION_ID-}" ]; then
    REPO_NAME="deb https://www.thirdlaw.tech/pkg/bookworm/ ./"
  else
    echo "Debian '${VERSION_ID-}' is not supported by this script."
    exit 2
  fi
  debian_like
}

function ubuntu {
  case "${UBUNTU_CODENAME-}" in
    jammy )
      REPO_NAME="deb https://www.thirdlaw.tech/pkg/$UBUNTU_CODENAME/ ./" ;;
    * )
      echo "Ubuntu ${UBUNTU_CODENAME-} not supported by this script."
      exit 2
  esac
  debian_like
}

case "${ID-}" in
  arch )
    arch ;;
  ubuntu )
    ubuntu ;;
  debian )
    debian ;;
  * )
    echo "Distribution '${ID-}' is not supported by this script."
    exit 2
esac
