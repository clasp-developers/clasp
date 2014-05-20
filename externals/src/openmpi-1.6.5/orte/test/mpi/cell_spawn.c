#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg, rc;
    MPI_Comm child;
    MPI_Info info;
    int rank, size;
    pid_t pid;
    char *cellhost, *ptr, *app;
    char hostname[128];

    pid = getpid();
    printf("Cell_spawn [pid %ld] starting up!\n", (long)pid);
    
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    /* get our hostname */
    gethostname(hostname, 128);

    /* construct the name of our cell co-processor - for now, pick the "b" side */
    if (NULL != (ptr = strchr(hostname, '.'))) {
        /* truncate the name */
        *ptr = '\0';
    }
    cellhost = strdup(hostname);
    cellhost[strlen(cellhost)-1] = 'b';
    
    printf("Master on host %s is spawning cell slave on host %s\n", hostname, cellhost);
    
    MPI_Info_create(&info);
    MPI_Info_set(info, "host", cellhost);
    
    /* setup the prefix to point to the special packages area */
    MPI_Info_set(info, "ompi_prefix", "/usr/projects/packages/openmpi/rhc/opt/slave");
    
    /* declare this to be a local slave launch */
    MPI_Info_set(info, "ompi_local_slave", "true");
    
    /* at this time, we don't need to preload binaries as we cannot
     * cross-compile OMPI programs. However, if you want to compile
     * your program on a Cell, and then move it to the NFS server
     * for later use, then turn the following lines on and indicate
     * where you want the binary placed
     */
#if 0
    MPI_Info_set(info, "ompi_preload_binary", "true");
    MPI_Info_set(info, "ompi_preload_files_dest_dir", rdir);
    MPI_Info_set(info, "ompi_preload_files", list-of-files);
    MPI_Info_set(info, "ompi_preload_files_src_dir", getcwd(cwd, 256));
#endif
    
    app = strdup("/usr/projects/packages/openmpi/rhc/cell_slave");
    
    pid = getpid();
    printf("Cell_spawn [pid %ld] about to spawn!\n", (long)pid);
    if (MPI_SUCCESS != (rc = MPI_Comm_spawn(app, MPI_ARGV_NULL, 1, info, 
                                            0, MPI_COMM_SELF, &child, MPI_ERRCODES_IGNORE))) {
        printf("Cell slave failed to spawn\n");
        return rc;
    }
    printf("Cell_spawn done with spawn\n");
    msg = 38;
    printf("Cell_spawn sending message to child\n");
    MPI_Send(&msg, 1, MPI_INT, 0, 1, child);
    MPI_Comm_disconnect(&child);
    printf("Cell_spawn disconnected\n");
    
    MPI_Info_free(&info);
    MPI_Finalize();
    fprintf(stderr, "%d: exiting\n", pid);
    return 0;
}
