include Makefile.config

TARGETS = $(HELLO_TGT1) $(HELLO_TGT2) $(HELLO_TGT3) $(HELLO_TGT4) \
          $(PI_TGT1) $(PI_TGT2) $(PI_TGT3) $(PI_TGT4) \
          $(RING_TGT1) $(RING_TGT2) $(RING_TGT3) $(RING_TGT4)

.PHONY: all
all: $(TARGETS)


### hello-man ###
hello-man: hello.F
	$(MPIFC) -vt:inst manual $(MPIFFLAGS) $(FDOPT)MANUAL $^ -o $@ $(FDOPT)VTRACE $(MPILIB)
### hello-comp ###
hello-comp: hello.F
	$(MPIFC) $(MPIFFLAGS) $^ -o $@ $(MPILIB)
### hello-dyn ###
hello-dyn: hello.F
	$(MPIFC) -vt:inst dyninst $(MPIFFLAGS) $^ -o $@ $(MPILIB)
### hello-tau ###
hello-tau: hello.F
	$(MPIFC) -vt:inst tauinst $(MPIFFLAGS) $^ -o $@ $(MPILIB)


### pi-man ###
pi-man: pi.F
	$(MPIFC) -vt:inst manual $(MPIFFLAGS) $(FDOPT)MANUAL $^ -o $@ -lm $(FDOPT)VTRACE $(MPILIB)
### pi-comp ###
pi-comp: pi.F
	$(MPIFC) $(MPIFFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-dyn ###
pi-dyn: pi.F
	$(MPIFC) -vt:inst dyninst $(MPIFFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-tau ###
pi-tau: pi.F
	$(MPIFC) -vt:inst tauinst $(MPIFFLAGS) $^ -o $@ -lm $(MPILIB)


### ring-man ###
ring-man: ring.F
	$(MPIFC) -vt:inst manual $(MPIFFLAGS) $(FDOPT)MANUAL $^ -o $@ $(FDOPT)VTRACE $(MPILIB)
### ring-comp ###
ring-comp: ring.F
	$(MPIFC) $(MPIFFLAGS) $^ -o $@ $(MPILIB)
### ring-dyn ###
ring-dyn: ring.F
	$(MPIFC) -vt:inst dyninst $(MPIFFLAGS) $^ -o $@ $(MPILIB)
### ring-tau ###
ring-tau: ring.F
	$(MPIFC) -vt:inst tauinst $(MPIFFLAGS) $^ -o $@ $(MPILIB)


.PHONY: clean
clean:
	-rm -f $(TARGETS) *~ *.o
	-rm -f *.uctl *.otf *.z

