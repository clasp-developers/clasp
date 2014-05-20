#!/bin/bash

#export N=0 What does N do?
echo Compiling kernel-wrapper.o
llc -march=x86-64 -relocation-model=pic -filetype=obj ${1}-wrapper.bc -o=${1}-wrapper.o
echo About to compile files in: ${1}-wrapper.lnk
objfiles=
while read LINE; do
#	N=$((N+1))
#       echo $LINE
	echo "compiling ${LINE%.*}.o"
	objfiles="${objfiles} ${LINE%.*}.o"
	llc -march=x86-64 -relocation-model=pic -filetype=obj $LINE -o=${LINE%.*}.o
done < "${1}-wrapper.lnk"
clang++ "${1}-wrapper.o" ${objfiles} -shared -Wl,-soname -Wl,kernel.bundle -o "${1}.bundle"