#include "adio.h"

int ADIOI_GEN_Feature(ADIO_File fd, int flag)
{
	switch(flag) {
		case ADIO_LOCKS:
		case ADIO_SHARED_FP:
		case ADIO_ATOMIC_MODE:
		case ADIO_DATA_SIEVING_WRITES:
			return 1;
			break;
		case ADIO_SCALABLE_OPEN:
		default:
			return 0;
			break;
	}
}
