#! /bin/bash
nm -nmap ~/Development/cando/brcl/build/cando.app/Contents/MacOS/brcl_d >brcl_d_symbols.txt
nm -nmap $1 >$1.txt
