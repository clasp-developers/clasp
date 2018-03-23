#!/usr/sbin/dtrace -s
# Usage:  sudo dtrace -x ustackframes=1000 -c ./cclasp-boehm -s onefunc.d -o /tmp/out.user_stacks
pid$target::*throw*:entry
/ pid == $target /
{
    @[ustack()] = count();
}
