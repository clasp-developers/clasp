#! /bin/bash
for n in $*
do
        dot -Tsvg -o ${n%.*}.svg ${n%.*}.dot
done
