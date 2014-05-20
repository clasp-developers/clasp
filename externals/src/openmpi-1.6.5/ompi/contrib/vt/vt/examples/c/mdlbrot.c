#include <stdio.h>
#include <stdlib.h>

#include <SDL/SDL.h>

#include <mpi.h>

#ifdef MANUAL
#include "vt_user.h"
#endif

#define MAX_X 640
#define MAX_Y 480

static void draw_pixel(SDL_Surface* pic, Uint32 x, Uint32 y, Uint32 color)
{
  Uint32* pixel;

#ifdef MANUAL
  VT_USER_START("draw_pixel");
#endif

  pixel = (Uint32*)pic->pixels + y * MAX_X + x;
  *pixel = color;

#ifdef MANUAL
  VT_USER_END("draw_pixel");
#endif
}

static void draw(SDL_Surface* pic, Uint32* field)
{
  Uint32 i, j;

#ifdef MANUAL
  VT_USER_START("draw");
#endif

  for(i = 0; i < MAX_X; i++)
  {
    for(j = 0; j < MAX_Y; j++)
    {
      draw_pixel(pic, i, j, field[i * MAX_Y + j]);
    }
  }

#ifdef MANUAL
  VT_USER_END("draw");
#endif
}

static Uint32 mandelbrot_point(double cx, double cy, double max_value_sq,
                               Uint32 max_iter)
{
  double value_sq = 0;
  double x = 0, xt;
  double y = 0, yt;
  Uint32 iter = 0;

#ifdef MANUAL
  VT_USER_START("mandelbrot_point");
#endif
  
  while((value_sq <= max_value_sq) && (iter < max_iter))
  {
    xt = (x * x) - (y * y) + cx;
    yt = 2 * x * y + cy;
    x = xt;
    y = yt;
    iter++;
    value_sq = x * x + y * y;
  }

#ifdef MANUAL
  VT_USER_END("mandelbrot_point");
#endif

  return iter;
}

static void calc_lines(Uint32 start, Uint32 end, Uint32* lines,
                       double max_values_sq, Uint32 max_iter)
{
  Uint32 i, iter_wert, icolor;
  double cx, cy;

  double pd_x = 3.0 / (double)MAX_X;
  double pd_y = 2.0 / (double)MAX_Y;

#ifdef MANUAL
  VT_USER_START("calc_lines");
#endif

  for(i = start; i < end; i++)
  {
    cx = -2.0 + (i / MAX_Y) * pd_x;
    cy = -1.0 + (i % MAX_Y) * pd_y;

    iter_wert = mandelbrot_point(cx, cy, max_values_sq, max_iter);

    icolor = (double)iter_wert / (double)max_iter * (1u << 24);
    lines[i-start] = icolor;
  }

#ifdef MANUAL
  VT_USER_END("calc_lines");
#endif
}

int main(int argc, char* argv[])
{
  int numprocs, rank, edge, pixel_count, start, end;
  double max_values_sq;
  Uint32 max_iter;

#ifdef MANUAL
  VT_USER_START("main");
#endif
  
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if(numprocs <= 1)
  {
    fprintf(stderr, "%s: error: requires at least two MPI processes",
            argv[0]);
#ifdef MANUAL
  VT_USER_END("main");
#endif
    return 1;
  }
  
  max_values_sq = 4.0;
  max_iter = 5000;

  edge = (MAX_X * MAX_Y) / (numprocs - 1);

  if(rank > 0)
  {
    int i = rank - 1;

    Uint32* pixels;

    start = i * edge;
    end = (i == numprocs - 2) ? MAX_X * MAX_Y : (i + 1) * edge;
    pixel_count = end - start;

    pixels = malloc(pixel_count * sizeof(Uint32));
    calc_lines(start, end, pixels, max_values_sq, max_iter);

    MPI_Send((void*)pixels, pixel_count, MPI_INT, 0, 0, MPI_COMM_WORLD);
    free(pixels);
  }
  else /* rank == 0 */
  {
    int i, recv_count = (edge + 1);

    Uint32* field = malloc(MAX_X * MAX_Y * sizeof(Uint32));
    Uint32* fieldpos;

    SDL_Surface* pic;
    SDL_Event event;
        
    MPI_Status status;

    for(i = 1; i < numprocs; i++)
    {
      start = (i - 1) * edge;
      end = (i == numprocs - 1) ? MAX_X * MAX_Y : i * edge;

      pixel_count = end - start;
      recv_count = pixel_count;

      fieldpos = field+start;

      MPI_Recv(fieldpos, recv_count, MPI_INT, i, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
    }
    
    SDL_Init(SDL_INIT_EVERYTHING);

    pic = SDL_SetVideoMode(MAX_X, MAX_Y, 32, SDL_HWSURFACE | SDL_DOUBLEBUF);
    SDL_WM_SetCaption("Mandelbrot", "Mandelbrot");

    draw(pic, field);

    SDL_Flip(pic);
   
    do
    {
      SDL_Delay(50);
      SDL_PollEvent(&event);
    } while( event.type != SDL_QUIT && event.type != SDL_KEYDOWN );
        
    SDL_FreeSurface(pic);
    SDL_Quit();

    free(field);
  }

  MPI_Finalize();

#ifdef MANUAL
  VT_USER_END("main");
#endif

  return 0;
}
