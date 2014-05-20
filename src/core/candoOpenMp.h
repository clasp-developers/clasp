#ifndef	CANDO_OPENMP
#define CANDO_OPENMP

	// Set OPENMP_ON if openMP is available
	//
#define	OPENMP_ON 0

#ifdef _OPENMP
#include <omp.h>
#undef OPENMP_ON
#define	OPENMP_ON 1
#endif

namespace core
{

extern bool	cando_omp_enabled();
extern int	cando_omp_get_thread_num();
extern int	cando_omp_get_num_threads();

};



#endif // CANDO_OPENMP
