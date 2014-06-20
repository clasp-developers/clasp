#ifndef PROCLIST_H
#define PROCLIST_H
#include <map>
#include <string>

using namespace std;

/*--------------------------------------*/
/*Stores every single process that	*/
/*will be handled.			*/
/*--------------------------------------*/
map<int,string> procList;

int procList_append	(map<int,string>& procList, int id, int parent_id, char* name);
int procList_print	(map<int, string>& procList);



int procList_append (map<int,string>& procList, int id, int parent_id, char* name) {
  /*--------------------------------------*/
  /*Insert values to the datatype.	*/
  /*--------------------------------------*/
  procList[id]=name;
  return 0;
}

int procList_print (map<int,string>& procList) {
  /*--------------------------------------*/
  /*Displays the whole datatype and group */
  /*them together.			*/
  /*--------------------------------------*/
  map<int,string>::iterator iter=procList.begin();
  map<int,string>::iterator iterEnd=procList.end();
  
  int fid=iter->first;
  int lid=iter->first;
  int counter=0;
  string str_name="\""+iter->second+"\"";
  iter++;
  int groupC=0;
  
  while (iter!=iterEnd) {
    //printf("iter->first: %d	iter->second: %s\n",iter->first,iter->second.c_str());
    if((iter->first) == lid+1) { 
      if( 1 > counter ) {
	if( 0 == counter ) {
	  str_name+=",";
	}
	str_name+="\""+iter->second+"\"";
      }
      counter++;
    }
    else {
      if(1 < counter) {
        str_name+=",...";
      }
      groupC++;
      printf("%-6d    %-30s",groupC,str_name.c_str());
      if(fid==lid) {
	printf("    %d\n",fid);
      } else {
 	printf("    %d - %d\n", fid,lid);
      }
      fid=iter->first;
      counter=0;
      str_name.clear();
      str_name+="\""+iter->second+"\"";
    }
    lid=iter->first;
    iter++;
    if(iter==iterEnd) {
      groupC++;
      if(1 < counter) {
        str_name+=",...";
      }
      printf("%-6d    %-30s",groupC,str_name.c_str());
      if(fid==lid) {
	printf("    %d\n",fid);
      } else {
 	printf("    %d - %d\n", fid,lid);
      }
    }
  }
  return 0;
}

#endif /* PROCLIST_H */
