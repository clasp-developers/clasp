
#include "foundation.h"
#include "candoOpenMp.h"


namespace core
{

bool	cando_omp_enabled()
{
#if _OPENMP
    return true;
#else
    return false;
#endif
}

int	cando_omp_get_thread_num()
{
#if OPENMP_ON
    return omp_get_thread_num();
#else
    return 0;
#endif
}

int	cando_omp_get_num_threads()
{
#if OPENMP_ON
    return omp_get_num_threads();
#else
    return 0;
#endif
}



};
