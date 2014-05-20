class A:
    def __init__(self):
        print "A init"

class B(A):
    def __init__(self):
        print "B init"

a = B()
