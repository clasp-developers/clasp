#include <stdio.h>
#include <boost/mpi.hpp>

int main(int argc, char** argv){
	
	boost::mpi::environment env(argc, argv);
	boost::mpi::communicator world;

	if(world.rank() == 0){
	    printf("Hello, world! I'm rank %d\n", world.rank());
	}
	else{
	    printf("Hmm - I'm rank %d...\n", world.rank());
	}
	
}
