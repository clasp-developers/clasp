


void include_snapshot() {

__asm__("\n\
    .pushsection .clasp_snapshot, \"a\", @progbits\n\
    .align 8\n\
    .global start_of_snapshot\n\
start_of_snapshot:\n\
    .incbin \"/home/meister/Development/clasp-future/build/boehmprecise/generated/cclasp-boehmprecise-snapshot.snapshot\"\n\
    .global end_of_snapshot\n\
end_of_snapshot:\n\
    .popsection\n");

}
