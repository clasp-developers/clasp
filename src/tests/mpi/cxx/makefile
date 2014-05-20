
all:
	$(BJAM) -a --toolset=$(BOOST_TOOLSET) link=static debug


alln:
	bjam -a -n --toolset=$(BOOST_TOOLSET) link=static release


DEST=/lustre/scratch/cschafme/tests/mpi 

install:
	install -d $(DEST)
	install -c bin/gcc-4.5.3//debug//link-static//bmain $(DEST)
	install -c run.pbs $(DEST)
clean:
	bjam --clean
