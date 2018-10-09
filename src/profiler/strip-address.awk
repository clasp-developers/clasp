#!/usr/bin/awk -f
/\+0x[0-9A-F]*$/ { printf("%s\n",substr($0,0,index($0,"+0x")-1)); }
// {print $0};
