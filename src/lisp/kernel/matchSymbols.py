#! /bin/env python

import sys, os

executableFilename = sys.argv[1]
bundleFilename = sys.argv[2]

executableMap = {}
fex = open(executableFilename,"r")
for l in fex.readlines():
    line = l.rstrip();
    symbol = line[19:]
    executableMap[symbol] = line

bundleCount = 0
foundCount = 0
fmis = open(bundleFilename,"r")
for l in fmis.readlines():
    line = l.rstrip()
    bundleCount = bundleCount + 1
    symbol = line[19:]
    if ( symbol not in executableMap ):
        print("Missing symbol: %s"% symbol)
    else:
        hit = executableMap[symbol]
        exportStatusFromExecutable = hit[17:18]
        if ( exportStatusFromExecutable == "t" ):
            print("Export symbol export-status[%s]: %s" % (exportStatusFromExecutable, hit ))
        foundCount += 1

print("Search count: %d\n" % bundleCount)
print("Found count:  %d\n" % foundCount)
