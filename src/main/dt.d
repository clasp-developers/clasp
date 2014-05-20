#!/usr/sbin/dtrace -s
pid$1:::entry
{
    @count_table[probefunc] = count() ;
}

##  --------  output when executed
fry:main$ sudo dtrace -s dt.d -p 34398
Password:
dtrace: failed to compile script dt.d: line 2: invalid probe description "pid$1:::entry": Undefined macro variable in probe description
fry:main$ 


