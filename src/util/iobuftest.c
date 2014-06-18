#include <stdio.h>
#include "../machine.h"

/*
 * On some machines (Solaris 2), the FILE structure stores the
 * buffer passed by setbuf() with an offset
 */

char buf[BUFSIZ];

main()
{
  setbuf(stdin, buf);
  printf("%d\n", (int)buf - (int)stdin->_IO_buf_base);
}
