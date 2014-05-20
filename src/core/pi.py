#/*--- pi.c       PROGRAM RANPI
# *
# *   Program to compute PI by probability.
# *   By Mark Riordan  24-DEC-1986; 
# *   Original version apparently by Don Shull.
# *   To be used as a CPU benchmark.
# *  
# *  Translated to C from FORTRAN 20 Nov 1993
# */

def myadd( sum, addend ):
    return sum + addend


print("Starting PI...\n")
ztot = 0.0
low = 1
ixran = 1907
yran = 5813.0
ymult = 1307.0
ymod = 5471.0
itot = 120000 # itot = 1200000
for j in range(1,itot+1):
    iprod = 27611 * ixran
    ixran = iprod - 74383*int(iprod/74383)
    x = float(ixran) / 74383.0
    prod = ymult * yran
    yran = (prod - ymod*int(prod/ymod) )
    y = yran / ymod
    z = x*x + y*y
    ztot = myadd(ztot,z)
    if ( z <= 1.0 ):
        low = low + 1;
print("Number of steps = %d" % itot )
print(" x=%8.5f y=%8.5f low=%7d j=%7d\n"%(x,y,low,j))
pi = 4.0 * float(low)/float(itot)
print("Pi = %9.6f ztot=%12.2f itot=%8d\n"%(pi,ztot,itot))

