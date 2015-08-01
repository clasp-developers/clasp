import time

def fibn(reps,num):
    for r in range(0,reps):
        p1 = 1
        p2 = 1
        rnum = num - 2
        for i in range(0,rnum):
            z = p1 + p2
            p2 = p1
            p1 = z
    return z

start = time.time()
res = fibn(10000000, 78)
end = time.time()
print( "Result = %f\n", res)
print( "elapsed time: %f seconds\n" % (end-start))

#python fib.py
#('Result = %f\n', 8944394323791464)
#elapsed time: 8.034955 seconds
