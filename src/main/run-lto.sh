time source llvm-link-clasp.sh
time opt -o=clasp-lto.bc -O3 -std-link-opts clasp.bc
time llc -o=clasp-lto.o -filetype=obj clasp-lto.bc
time source link-clasp-lto.sh
