//
// Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
//
// Simple ring test program
//

#include "mpi.h"
#include <iostream>

int main(int argc, char *argv[])
{
    int rank, size, next, prev, message, tag = 201;

    // Start up MPI

    MPI::Init();
    rank = MPI::COMM_WORLD.Get_rank();
    size = MPI::COMM_WORLD.Get_size();
 
    // Calculate the rank of the next process in the ring.  Use the
    // modulus operator so that the last process "wraps around" to
    // rank zero.

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;

    // If we are the "master" process (i.e., MPI_COMM_WORLD rank 0),
    // put the number of times to go around the ring in the message.

    if (0 == rank) {
        message = 10;

        std::cout << "Process 0 sending " << message << " to " << next
                  << ", tag " << tag << " (" << size << " processes in ring)"
                  << std::endl;
        MPI::COMM_WORLD.Send(&message, 1, MPI::INT, next, tag);
        std::cout << "Process 0 sent to " << next << std::endl;
    }

    // Pass the message around the ring.  The exit mechanism works as
    // follows: the message (a positive integer) is passed around the
    // ring.  Each time it passes rank 0, it is decremented.  When
    // each processes receives a message containing a 0 value, it
    // passes the message on to the next process and then quits.  By
    // passing the 0 message first, every process gets the 0 message
    // and can quit normally.

    while (1) {
        MPI::COMM_WORLD.Recv(&message, 1, MPI::INT, prev, tag);

        if (0 == rank) {
            --message;
            std::cout << "Process 0 decremented value: " << message 
                      << std::endl;
        }

        MPI::COMM_WORLD.Send(&message, 1, MPI::INT, next, tag);
        if (0 == message) {
            std::cout << "Process " << rank << " exiting" << std::endl;
            break;
        }
    }

    // The last process does one extra send to process 0, which needs
    // to be received before the program can exit */

    if (0 == rank) {
        MPI::COMM_WORLD.Recv(&message, 1, MPI::INT, prev, tag);
    }
    
    // All done

    MPI::Finalize();
    return 0;
}
