//
// Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
//
// Sample MPI "hello world" application in C++
//

#include "mpi.h"
#include <iostream>

int main(int argc, char **argv)
{
    int rank, size;

    MPI::Init();
    rank = MPI::COMM_WORLD.Get_rank();
    size = MPI::COMM_WORLD.Get_size();
    std::cout << "Hello, world!  I am " << rank << " of " << size << std::endl;
    MPI::Finalize();

    return 0;
}
