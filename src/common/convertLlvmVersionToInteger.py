import sys

versionString = sys.argv[1]
parts = versionString.split(".")
versionInt = int(parts[0])*100+int(parts[1])*10+int(parts[2])
print("%d" % versionInt),
