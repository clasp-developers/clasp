import os
import sys
import re
import json

compileCommand = sys.argv[1]
inputFilename = sys.argv[2]
outputFilename = sys.argv[3]

f = open(inputFilename)
lines = f.readlines()
f.close()


jsonData = []
print("Searching for %s" % compileCommand)
print("Parsing: %s" % inputFilename)
csearch = re.compile("^.*"+compileCommand+".*\s-c\s")
for l in lines:
    if (csearch.match(l)):
        line = l.rstrip()
        compileCommand = line.replace(" -c ", " -v -c ")
        parts = line.split(" ")
        sourcePath = parts[-1]
        sourcePathParts = sourcePath.split("/")
        sourceFileName = sourcePathParts[-1].rstrip('"').lstrip('"')
        directory = sourcePath[0:len(sourcePath)-len(sourcePathParts[-1])].lstrip('"')
        absPath = os.path.abspath(directory)
        jsonData.append({"directory" : absPath, "command" : compileCommand, "file" : sourceFileName })
#        print compileCommand
#        print("."),
print("")
print("Writing JSON database to: %s"%outputFilename)
fout = open(outputFilename,"w")
fout.write(json.dumps(jsonData,indent=4))
fout.close()
print("Generated %d entries in JSON database" % len(jsonData))

    
