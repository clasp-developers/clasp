#!/usr/sbin/dtrace -s
/* Usage:  sudo dtrace -x ustackframes=4000 -p $1 -o /tmp/out.user_stacks */

pid$target::*start_dtrace*:entry
{
        interested = 1;
}

pid$target::*stop_dtrace*:entry
{
        interested = 0;
        exit(0);
}

profile-97
/ pid == $target && interested /
{
    @[ustack()] = count();
}
