/*
    File: bmain.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
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
