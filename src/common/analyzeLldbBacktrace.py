filename = "stack.log"
prevsp = None
with open(filename) as f:
    total = 0
    for line in f:
    	parts = line.lstrip(" ").split(" ");
        try:
            sp = int(parts[0],16)
            fp = int(parts[1],16)
        except:
            print "Skipping"
        frameSize = fp - sp
        total += frameSize
        print("%5d %20s %20s %5s %s" %( frameSize, parts[0], parts[1], parts[2], parts[3] ))
print("Total size = %d"%total)
