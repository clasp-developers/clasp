#include <stdio.h>
#include <string.h>

void
sed_emulator(int narg, char **patterns)
{
  char buffer[1024];
  char buffer2[1024];
  size_t i, lengths[1024];
  char *b1 = buffer, *b2 = buffer2;

  for (i = 0; i < narg; i++)
    lengths[i] = strlen(patterns[i]);

  while(1) {
    if (gets(b1) == 0)
      exit(0);
    for (i = 0; i < narg; i+=2) {
      char *b3, *b4;
      while ((b3 = strstr(b1, patterns[i]))) {
	if (strcmp(patterns[i+1], "/DELETE/") == 0)
	  goto GO_ON;
	b3[0] = 0;
	strcpy(b2, b1);
	strcat(b2, patterns[i+1]);
	strcat(b2, b3 + lengths[i]);
	b4 = b2; b2 = b1; b1 = b4;
      }
    }
    puts(b1);
  GO_ON:;
  }
}

int
main(int narg, char **argv) {
  int i, j;
  char buffer[1024];

  narg--;
  argv++;

  /* To make canonical paths in Windows */
  for (i = 1; i < narg; i+=2) {
    char *new = strdup(argv[i]);
    for (j = 0; new[j]; j++) {
      if (new[j] == '\\') {
	new[j] = '/';
      }
    }
    argv[i] = new;
  }  

  if (narg >= 2)
    sed_emulator(narg, argv);

  while(1) {
    if (gets(buffer) == 0) {
      exit(0);
    }
    if (narg == 0) {
      /* This is used to remove part of config.h */
      if (strstr(buffer, "-CUT-")) {
	exit(0);
      }
    }
    puts(buffer);
  }
}
