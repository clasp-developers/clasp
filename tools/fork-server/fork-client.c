#include <stdlib.h>
#include<stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
int main(int argc, const char* argv[])
{
  int sid;
  size_t port = strtol(argv[2],NULL,10);
  struct sockaddr_in ssock,csock;
  sid=socket(AF_INET,SOCK_STREAM,0);
  ssock.sin_family=AF_INET;
  ssock.sin_addr.s_addr=inet_addr("127.0.0.1");
  ssock.sin_port=htons(port);
//  fcntl(sid, F_SETFL, O_NONBLOCK);
  int err = connect(sid,(struct sockaddr *)&ssock,sizeof(ssock));
  if (err<0) {
    perror("connect");
  }
  const char* s = argv[1];
  printf("Sending: |%s|\n", s);
  write(sid,(void*)s,strlen(s));
  write(sid,(void*)"\n",strlen("\n"));
  while(1)
  {
    char* line = readline("fork-server client> ");
    if (line) {
      printf("Read: %s\n", line);
      free(line);
    } else {
      printf("Nothing read\n");
    }
    printf("Sleep(1)\n");
    sleep(1);
//    read(sid,(void*)s1,sizeof(s1));
//    printf("\n The received string is:%s\n",s1);
  }
  close(sid);
}
