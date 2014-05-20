#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg, rc;
    MPI_Comm child;
    MPI_Info info;
    int rank, size;
    pid_t pid;
    char *host, *app, *rdir=NULL, *prefix;
    char cwd[256];
    
    if (argc < 3) {
        printf("Usage: slave_spawn host prefix-for-host <remote-tmp-dir> <files-to-move>\n");
        return 1;
    }

    host = argv[1];
    prefix = argv[2];
    app = "slave";
    
    if (5 == argc) {
        rdir = argv[4];
    }
    
    pid = getpid();
    printf("Slave_spawn [pid %ld] starting up!\n", (long)pid);
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    MPI_Info_create(&info);
    MPI_Info_set(info, "host", host);
    MPI_Info_set(info, "ompi_prefix", prefix);
    MPI_Info_set(info, "ompi_local_slave", "true");
    
    if (NULL != rdir) {
        MPI_Info_set(info, "ompi_preload_binary", "true");
        MPI_Info_set(info, "ompi_preload_files_dest_dir", rdir);
    }
    
    if (argc == 6) {
        /* files were specified */
        MPI_Info_set(info, "ompi_preload_files", argv[5]);
        MPI_Info_set(info, "ompi_preload_files_src_dir", getcwd(cwd, 256));
    }
    
    pid = getpid();
    printf("Slave_spawn [pid %ld] about to spawn!\n", (long)pid);
    if (MPI_SUCCESS != (rc = MPI_Comm_spawn(app, MPI_ARGV_NULL, 1, info, 
                                            0, MPI_COMM_SELF, &child, MPI_ERRCODES_IGNORE))) {
        printf("Slave failed to spawn\n");
        return rc;
    }
    printf("Slave_spawn done with spawn\n");
    msg = 38;
    printf("Slave_spawn sending message to child\n");
    MPI_Send(&msg, 1, MPI_INT, 0, 1, child);
    MPI_Comm_disconnect(&child);
    printf("Slave_spawn disconnected\n");
    
    MPI_Info_free(&info);
    MPI_Finalize();
    fprintf(stderr, "%d: exiting\n", pid);
    return 0;
}
