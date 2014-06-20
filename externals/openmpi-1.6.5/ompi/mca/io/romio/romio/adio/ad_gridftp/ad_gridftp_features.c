int ADIOI_GRIDFTP_Feature (ADIO_File fd, int flag)
{
	switch(flag) {
		case ADIO_SCALABLE_OPEN:
		case ADIO_SHARED_FP:
		case ADIO_LOCKS:
		case ADIO_SEQUENTIAL:
		case ADIO_DATA_SIEVING_WRITES:
		default:
			return 0;
	}
}
