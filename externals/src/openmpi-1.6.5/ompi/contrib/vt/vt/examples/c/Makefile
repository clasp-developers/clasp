include Makefile.config

TARGETS = $(HELLO_TGT1) $(HELLO_TGT2) $(HELLO_TGT3) $(HELLO_TGT4) \
          $(PI_TGT1) $(PI_TGT2) $(PI_TGT3) $(PI_TGT4) \
          $(RING_TGT1) $(RING_TGT2) $(RING_TGT3) $(RING_TGT4) \
          $(MDLBROT_TGT1) $(MDLBROT_TGT2) $(MDLBROT_TGT3) $(MDLBROT_TGT4) $(MDLBROT_TGT5) $(MDLBROT_TGT6)

.PHONY: all
all: $(TARGETS)


### hello-man ###
hello-man: hello.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### hello-comp ###
hello-comp: hello.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ $(MPILIB)
### hello-dyn ###
hello-dyn: hello.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ $(MPILIB)
### hello-tau ###
hello-tau: hello.c
	$(MPICC) -vt:inst tauinst $(MPICFLAGS) $^ -o $@ $(MPILIB)


### pi-man ###
pi-man: pi.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -lm -DVTRACE $(MPILIB)
### pi-comp ###
pi-comp: pi.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-dyn ###
pi-dyn: pi.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ -lm $(MPILIB)
### pi-tau ###
pi-tau: pi.c
	$(MPICC) -vt:inst tauinst $(MPICFLAGS) $^ -o $@ -lm $(MPILIB)


### ring-man ###
ring-man: ring.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB)
### ring-comp ###
ring-comp: ring.c
	$(MPICC) $(MPICFLAGS) $^ -o $@ $(MPILIB)
### ring-dyn ###
ring-dyn: ring.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $^ -o $@ $(MPILIB)
ring-tau: ring.c
	$(MPICC) -vt:inst tauinst $(MPICFLAGS) $^ -o $@ $(MPILIB)


### mdlbrot-man ###
mdlbrot-man: mdlbrot.c
	$(MPICC) -vt:inst manual $(MPICFLAGS) $(SDLINCDIR) -DMANUAL $^ -o $@ -DVTRACE $(MPILIB) $(SDLLIB)
### mdlbrot-comp ###
mdlbrot-comp: mdlbrot.c
	$(MPICC) $(MPICFLAGS) $(SDLINCDIR) $^ -o $@ $(MPILIB) $(SDLLIB)
### mdlbrot-dyn ###
mdlbrot-dyn: mdlbrot.c
	$(MPICC) -vt:inst dyninst $(MPICFLAGS) $(SDLINCDIR) $^ -o $@ $(MPILIB) $(SDLLIB)
### mdlbrot-tau ###
mdlbrot-tau: mdlbrot.c
	$(MPICC) -vt:inst tauinst $(MPICFLAGS) $(SDLINCDIR) $^ -o $@ $(MPILIB) $(SDLLIB)
### libSDLwrap.<a|so> ###
libSDLwrap.a: SDLwrap.c
	vtlibwrapgen --build -o libSDLwrap --static $^
libSDLwrap.so: SDLwrap.c
	vtlibwrapgen --build -o libSDLwrap --shared $^
SDLwrap.c:
	vtlibwrapgen -g SDL -o $@ $(SDLHEADERS)


.PHONY: clean
clean:
	-rm -f $(TARGETS) SDLwrap.c *~ *.o
	-rm -f *.uctl *.otf *.z

