/*
  cc -o crisscross crisscross.c -lmpi
*/

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include <unistd.h>
#include <string.h>
#include <time.h>

#define MAX_RR_NAME 7

int main(int argc, char *argv[])
{ 
 MPI_Status     status;               /* MPI status                          */
 int            mpierr;               /* MPI function return code            */
 int            rank;                 /* Process rank within MPI_COMM_WORLD  */
 int            nproc;                /* Total number of MPI processes       */
 int            tag0=41;              /* MPI message tag                     */
 int            tag1=42;              /* MPI message tag                     */
 int            tag2=43;              /* MPI message tag                     */
 int            warmup=1;             /* MPI warmup loops                    */

 char           process_name[MPI_MAX_PROCESSOR_NAME + 1];
 char           partner_name[MPI_MAX_PROCESSOR_NAME + 1];

 char           rr_blank[] = {"       "};
 char           rr_empty[] = {"???????"};

 int            n_bytes=128*1024*1024; 
 int            n_loops=2; 
 unsigned char* send_buff;
 unsigned char* recv_buff;

 int            i,j,k,m,count,mismatch;

 double         et1,et2,mbs;
 double         avg_mbs=0, sum_avg_mbs=0;
 int            xfers=0, sum_xfers=0;
 double         max_mbs=-1.0,min_mbs=999999.9;
 double         r_max_mbs,r_min_mbs;

 time_t         curtime;
 struct tm     *loctime;

 if ( argc > 2 )
   {
    n_loops = atoi(argv[2]);
    n_loops = n_loops < 1 ? 10 : n_loops;
   }
 if ( argc > 1 )
   {
    n_bytes = atoi(argv[1]);
    n_bytes = n_bytes < 1 ? 32768 : n_bytes;
   }

 send_buff = (unsigned char *) valloc(n_bytes);
 recv_buff = (unsigned char *) valloc(n_bytes);

 for ( i=0; i<n_bytes; i++ )
   {
    send_buff[i] = i%128;
   }

 mpierr = MPI_Init(&argc, &argv);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr, "MPI Error %d (MPI_Init)\n",mpierr);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

 mpierr = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
 if (mpierr != MPI_SUCCESS || rank < 0)
   {
    fprintf(stderr, "MPI Error %d (MPI_Comm_rank)\n",mpierr);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 if ( rank == 0 )
   {
    curtime = time (NULL);
    loctime = localtime (&curtime);
    printf("\n   %s\n",asctime (loctime));
   }

 mpierr = MPI_Comm_size(MPI_COMM_WORLD, &nproc);
 if (mpierr != MPI_SUCCESS || nproc < 1 || nproc <= rank)
   {
    fprintf(stderr, "MPI Error %d (MPI_Comm_size) [%d]\n",mpierr, rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 mpierr = MPI_Get_processor_name(process_name, &count);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Get_processor_name) [%d]\n", mpierr, rank);
    sprintf(process_name, rr_empty);
   }
 else
   {
    if (count < MAX_RR_NAME) strncat(&process_name[count],rr_blank,MAX_RR_NAME-count);
    process_name[MAX_RR_NAME] = '\0';
   }

 for ( i=0; i<nproc; i++ )
   {
    mpierr = MPI_Barrier(MPI_COMM_WORLD);
    if (mpierr != MPI_SUCCESS)
      {
       fprintf(stderr, "MPI Error %d (MPI_Barrier) [%d]\n", mpierr, rank);
       fflush(stderr);
       MPI_Abort(MPI_COMM_WORLD, -1);
      }
    for ( j=0; j<nproc; j++ )
      {
       if ( i != j )
         {
          if (rank == j)
            {
             mpierr = MPI_Sendrecv(process_name, MPI_MAX_PROCESSOR_NAME + 1, MPI_CHAR, i, tag0,
                                   partner_name, MPI_MAX_PROCESSOR_NAME + 1, MPI_CHAR, i, tag0, MPI_COMM_WORLD, &status);
             if (mpierr != MPI_SUCCESS)
               {
                fprintf(stderr,"MPI Error %d (MPI_Sendrecv) %s [%d,%d]\n",mpierr,process_name,rank,i);
                fflush(stderr);
                MPI_Abort(MPI_COMM_WORLD, -1);
               }
             for ( k=0; k<n_bytes; k++ )
               {
                recv_buff[k] = 0x80;
               }
            }
          if ( rank == i )
            {
             mpierr = MPI_Sendrecv(process_name, MPI_MAX_PROCESSOR_NAME + 1, MPI_CHAR, j, tag0,
                                   partner_name, MPI_MAX_PROCESSOR_NAME + 1, MPI_CHAR, j, tag0, MPI_COMM_WORLD, &status);
             if (mpierr != MPI_SUCCESS)
               {
                fprintf(stderr,"MPI Error %d (MPI_Sendrecv) %s [%d,%d]\n",mpierr,process_name,i,j);
                fflush(stderr);
                MPI_Abort(MPI_COMM_WORLD, -1);
               }
            }
          for ( k=0; k<n_loops+warmup; k++ )
            {
             if ( rank == i )
               {
                if (k == warmup) et1 = MPI_Wtime();
                mpierr = MPI_Send(send_buff, n_bytes, MPI_BYTE, j, tag1, MPI_COMM_WORLD);
                if (mpierr != MPI_SUCCESS)
                  {
                   fprintf(stderr,"MPI Error %d (MPI_Send) %s [4%d] --> %s [4%d]\n",mpierr,process_name,i,partner_name,j);
                   fflush(stderr);
                   MPI_Abort(MPI_COMM_WORLD, -1);
                  }
               }
             if ( rank == j )
               {
                mpierr = MPI_Recv(recv_buff, n_bytes, MPI_BYTE, i, tag1, MPI_COMM_WORLD, &status);
                if (mpierr != MPI_SUCCESS)
                  {
                   fprintf(stderr,"MPI Error %d (MPI_Recv) %s [4%d] <-- %s [4%d]\n",mpierr,process_name,j,partner_name,i);
                   fflush(stderr);
                   MPI_Abort(MPI_COMM_WORLD, -1);
                  }
                if (k == n_loops+warmup-1)  et2 = MPI_Wtime();
               }
            }
          if ( rank == i )
            {
             mpierr = MPI_Send(&et1, 1, MPI_DOUBLE, j, tag1, MPI_COMM_WORLD);
             if (mpierr != MPI_SUCCESS)
               {
                fprintf(stderr,"MPI Error %d (MPI_Send) %s [4%d] --> %s [4%d]\n",mpierr,process_name,i,partner_name,j);
                fflush(stderr);
                MPI_Abort(MPI_COMM_WORLD, -1);
               }
            }
          if ( rank == j )
            {
             mpierr = MPI_Recv(&et1, 1, MPI_DOUBLE, i, tag1, MPI_COMM_WORLD, &status);
             if (mpierr != MPI_SUCCESS)
               {
                fprintf(stderr,"MPI Error %d (MPI_Recv) %s [4%d] <-- %s [4%d]\n",mpierr,process_name,j,partner_name,i);
                fflush(stderr);
                MPI_Abort(MPI_COMM_WORLD, -1);
               }
             mbs = ((double)n_loops*n_bytes)/(1000000.0*(et2-et1));
             if (mbs < 50.0)
               {
                printf("   %s [%4d]   =====>>   %s [%4d]  %9.1f mbs     SLOW!\n",partner_name,i,process_name,j,mbs); 
               }
             else
               {
                printf("   %s [%4d]   =====>>   %s [%4d]  %9.1f mbs\n",partner_name,i,process_name,j,mbs); 
               }

             min_mbs = (mbs < min_mbs) ? mbs:min_mbs;
             max_mbs = (mbs > max_mbs) ? mbs:max_mbs;
           
             avg_mbs += mbs;
             xfers++;
             mismatch = 0;
             for ( k=0; k<n_bytes; k++ )
               {
                if ( recv_buff[k] != k%128 ) mismatch++;
               }
             if ( mismatch ) printf("                                                                  WARNING! %d data mismatches\n",mismatch);
             fflush(stdout);
            }
         }
      }
   }

 mpierr = MPI_Reduce(&xfers, &sum_xfers, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Reduce) %s [%d]\n",mpierr,process_name,rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 mpierr = MPI_Reduce(&avg_mbs, &sum_avg_mbs, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Reduce) %s [%d]\n",mpierr,process_name,rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 mpierr = MPI_Reduce(&min_mbs, &r_min_mbs, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Reduce) %s [%d]\n",mpierr,process_name,rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 mpierr = MPI_Reduce(&max_mbs, &r_max_mbs, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Reduce) %s [%d]\n",mpierr,process_name,rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 mpierr = MPI_Finalize();
 if (mpierr != MPI_SUCCESS)
   {
    fprintf(stderr,"MPI Error %d (MPI_Finalize) %s [%d]\n",mpierr,process_name,rank);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD, -1);
   }

 fflush(stdout);
 
 if ( rank == 0 )
   { 
    mbs = sum_avg_mbs/sum_xfers;
    printf("\n     average tranfer rate for %d transfers: %9.1f mbs\n",sum_xfers, mbs);
    printf("     minimum tranfer rate for %d transfers: %9.1f mbs\n",sum_xfers, r_min_mbs);
    printf("     maximum tranfer rate for %d transfers: %9.1f mbs\n",sum_xfers, r_max_mbs);
    fflush(stdout);
   } 

 return 0;
}
