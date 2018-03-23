#!/usr/sbin/dtrace -s
# Usage:  sudo dtrace -x ustackframes=1000 -c ./cclasp-boehm -s onefunc.d -o /tmp/out.user_stacks
pid$target::*dynamic_cast*:entry
{
        interested = 1;
        printf("Entered init-translators interested = %d\n", interested);
}

profile-2
/ pid == $target && interested /
{
    @[ustack()] = count();
}
