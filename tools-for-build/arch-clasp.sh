#!/usr/bin/env bash

curl -fsSL https://www.thirdlaw.tech/pkg/key.gpg | pacman-key --add -
pacman-key --lsign-key 009C1F20737D81230A0928CBC812E10EC39DF202
echo -e "\n[thirdlaw]\nServer = https://www.thirdlaw.tech/pkg/arch\n" >>/etc/pacman.conf
pacman -Sy --noconfirm clasp-cl-git