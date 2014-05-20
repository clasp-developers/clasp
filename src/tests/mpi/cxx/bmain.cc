#include <boost/mpi.hpp>
#include <stdio.h>
#include <iostream>
#include <string>
#include <boost/serialization/string.hpp>
namespace mpi = boost::mpi;

using namespace std;

int main(int argc, char* argv[]) 
{
    cout << "Starting bmain" << endl;
    cout << "Creating environment" << endl;
    mpi::environment env(argc, argv);
    cout << "Creating communicator" << endl;
    mpi::communicator world;
    cout << "Starting program rank=" << world.rank() << "  size = " << world.size() << endl;

    if (world.rank() == 0)
    {
	int procs = world.size()-1;
	std::string msg;
	while ( procs )
	{
	    world.recv(mpi::any_source,mpi::any_tag,msg);
	    std::cout << "Manager received: " << msg << endl;
	    procs--;
	}
	std::cout << "All messages received, shutting down" << endl;
    } else {
	stringstream msg;
	msg << "Worker#" << world.rank() << "responding";
	cout << "Sending message from worker: " << world.rank() << endl;
	world.send(0, 1, msg.str() );
	while (1) {} // infinite loop
    }

    return 0;
}
