#!/bin/bash

export N=0
echo Compiling _image-wrapper.o
llc -filetype=obj _image-wrapper.bc -o=_image-wrapper.o
echo About to compile files in: $1
for LINE `cat $1`; do
	N=$((N+1))
	echo "compiling ${LINE%.*}.o"
#	llc -filetype=obj $LINE -o=${LINE%.*}.o
	oneObj=${LINE%.*}.o
	echo "   appending " $oneObj
	xxx+=($oneObj)
done
echo objects: ${xxx[@]}
