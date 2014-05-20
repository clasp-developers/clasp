#-- pi.c       PROGRAM RANPI
# *
#  Program to compute PI by probability.
#  By Mark Riordan  24-DEC-1986; 
#  Original version apparently by Don Shull.
#  To be used as a CPU benchmark.
# 
# Translated to C from FORTRAN 20 Nov 1993
#

#  Simple adding subroutine thrown in to allow subroutine
#  calls/returns to be factored in as part of the benchmark.

defFunction myadd ( sum addend )
      return [ sum + addend ]

#print [ "Press enter to start: " ]
#[ s = (getline) ]
println [ "Starting PI..." ]
[ ztot = 0.0 ]
[ low = 1 ]
[ ixran = 1907 ]
[yran = 5813.0 ]
[ymult = 1307.0 ]
[ymod = 5471.0 ]
#[itot = 1200000 ]
[itot = 120000 ]

# X and Y are two uniform random numbers between 0 and 1.
# They are computed using two linear congruential generators.
# A mix of integer and real arithmetic is used to simulate a
# real program.  Magnitudes are kept small to prevent 32-bit
# integer overflow and to allow full precision even with a 23-bit
# mantissa.
foreach j ( Range 1 itot )
    [ iprod = [ 27611 * ixran ] ]
    [ ixran = [ iprod - [ 74383 * (int [ iprod / 74383 ] ) ] ] ]
    [ x = [ ixran / 74383.0 ] ]
    [ prod = [ ymult * yran ] ]
    [ yran = [ prod - [ ymod * ( int [ prod / ymod ] ) ] ] ]
    [ y = [ yran / ymod ] ]
    [ z = [ [ x * x ] + [y * y ] ] ]
    [ ztot = ( myadd ztot z) ]
    ifTrue [ z <= 1.0 ]
        [ low = [ low + 1 ] ]

println [ "Number of steps = %d" % itot ]
println [" x=%8.5f y=%8.5f low=%7d j=%7d\n" % x y low j ]
[ pi = [ 4.0 * [ [1.0 * low ] / itot ] ] ]
println [ "Pi = %9.6f ztot=%12.2f low=%8d itot=%8d\n" % pi ztot low itot ]

