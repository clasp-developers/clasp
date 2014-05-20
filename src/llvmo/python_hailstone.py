length = 0
longest = 0
n_longest = 0
for sn in range(1,500000):
    n = sn
    while n != 1:
        if n % 2 == 0:
           n = n / 2
           length = length + 1
        else:
            n = (n * 3) + 1
            length = length + 1
        if n == 1:
            length = length + 1
    if ( length > longest ):
        n_longest = sn
        longest = length
    length = 0
print "The longest sequence contains", longest, "numbers start=", n_longest
