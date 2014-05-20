/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#include "handler.h"
#include "otfinfo_error.h"

#include "otf.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#define PROGRESSBARLEN 20

static void set_handles_level_1( OTF_HandlerArray *handles,
                                 definitionInfoT *info );
static void set_handles_level_2( OTF_HandlerArray *handles,
                                 definitionInfoT *info );
static void set_handles_level_3( OTF_HandlerArray *handles,
                                 definitionInfoT *info );
static void set_handles_level_4( OTF_HandlerArray *handles,
                                 definitionInfoT *info );

static void show_info_level_1( definitionInfoT *info );
static void show_info_level_2( definitionInfoT *info );
static void show_info_level_3( definitionInfoT *info );
static void show_info_level_4( definitionInfoT *info );

static void free_data_level_1( definitionInfoT *info );
static void free_data_level_2( definitionInfoT *info );
static void free_data_level_3( definitionInfoT *info );
static void free_data_level_4( definitionInfoT *info );

int main(int argc, char **argv)
{
  definitionInfoT info;              /*structur where the trace information are stored*/
  int               i              = 0;
  uint32_t          numFileHandles = 1;    /*number of filehandles */
  uint64_t          checkVal       = 0;    /*value used for asserts*/
  uint64_t          size           = 0;    /*size of the event files*/
  uint64_t          minRead        = 0;    /*variable for progress*/
  uint64_t          currRead       = 0;    /*count of current read bytes*/
  uint32_t          numStreams     = 0;
  char*             parameter      = NULL; /*reference to the parameters*/
  char*             fileLocation   = NULL; /*path to the tracefiles*/
  OTF_FileManager  *manager        = NULL;
  OTF_Reader       *reader         = NULL;
  OTF_MasterControl* master        = NULL;
  OTF_HandlerArray *handles        = NULL;
  int showProgress                 = 0;
  int infoLevel                    = 1;    /*level for the output of information (local)*/

  static const char* Helptext[] = {
    "                                                                  \n",
    " otfinfo - Program to get basic information of an OTF trace.      \n",
    "                                                                  \n",
    " Syntax: otfinfo [options] <input file name>                      \n",
    "                                                                  \n",
    "   options:                                                       \n",
    "      -h, --help    show this help message                        \n",
    "      -V            show OTF version                              \n",
    "      -f <n>        set max number of filehandles available       \n",
    "      -l <ilevel>   set the information level for the output      \n",
    "                    (0 - 4, default: 1)                           \n",
    "      -a            set the information level to 4                \n",
    "      -p            show progress bar for reading event files     \n",
    "                                                                  \n",
    NULL };

  info.infoLevel = 1; /*level of information for the handles*/

  /*if no parameter was given printing the helptext*/
  if( 1 >= argc )
  {
    int l = 0;
    while( Helptext[l] )
    {
      printf( "%s",Helptext[l++] );
    }
    return 0;
  }

  /*checking for additional parameters*/
  for( i = 1 ; i < argc; i++ )
  {
    parameter = argv[i];
    if( (strcmp(parameter,"-f") == 0) && (i+1 < argc) )
    {
      i++;
      numFileHandles = atoi(argv[i]);
    }
    else if( (strcmp(parameter,"-l") == 0) && (i+1 < argc) )
    {
      i++;
      infoLevel = atoi(argv[i]);
    }
    else if( strcmp(parameter,"-a") == 0 )
    {
      infoLevel = 4;
    }
    else if( strcmp(parameter,"-p") == 0 )
    {
      showProgress = 1;
    }
    else if( strcmp(parameter,"-V") == 0 )
    {
      printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
                                   OTF_VERSION_SUB, OTF_VERSION_STRING );
      return 0;
    }
    else if( (strcmp(parameter,"-h") == 0) ||
             (strcmp(parameter,"--help") == 0) )
    {
      int l=0;
      while(Helptext[l])
      {
        printf( "%s",Helptext[l++] );
      }
      return 0;
    }
    else if( '-' != parameter[0] )
    {
      char* p;
      fileLocation = parameter;
      p = strrchr( parameter, '.' );
      if( p && strlen(p) >= 4 && strcmp(p, ".otf") == 0 )
        *p = '\0';
    }
    else
    {
      fprintf(stderr,"ERROR: Unknown option: '%s'\n",argv[i]);
      exit(1);
    }
  }
  if( numFileHandles < 1 )
  {
    fprintf( stderr, "ERROR: less than 1 filehandle is not permitted\n" );
    exit(1);
  }
  if( NULL == fileLocation )
  {
    fprintf( stderr, "ERROR: no trace file was specified\n" );
    exit(1);
  }

  info.filePrefix = OTF_getFilename( fileLocation, 0, OTF_FILETYPE_MASTER,
                                     0, NULL );

  /*preparing the reader*/
  manager = OTF_FileManager_open( numFileHandles );
  otfinfo_assert( manager );
  reader = OTF_Reader_open( fileLocation, manager );
  otfinfo_assert( reader );
  handles = OTF_HandlerArray_open();
  otfinfo_assert( handles );
  if( 0 < infoLevel )
  {
    info.infoLevel = 1;
    set_handles_level_1( handles, &info );
    if( 1 < infoLevel )
    {
      info.infoLevel = 2;
      set_handles_level_2( handles,&info );
    }
  }

  /*read definition file*/
  checkVal = OTF_Reader_readDefinitions( reader, handles );
  otfinfo_assert( checkVal != OTF_READ_ERROR );
  checkVal = OTF_Reader_readMarkers( reader, handles );
  otfinfo_assert( checkVal != OTF_READ_ERROR );

  /*getting the size of the event files*/
  master = OTF_Reader_getMasterControl( reader );
  otfinfo_assert( master );

  numStreams = OTF_MasterControl_getCount( master );
  otfinfo_assert( numStreams > 0 );

  info.traceFileSize = 0;
  for( i = 0; i < (int)numStreams; i++ )
  {
    static char filename[1024];
    static uint32_t filecomp = OTF_FILECOMPRESSION_COMPRESSED;
    static struct stat filestat;
    OTF_MapEntry* entry = OTF_MasterControl_getEntryByIndex( master, i );

    /*get event file name of stream*/
    OTF_getFilename( fileLocation, entry->argument, OTF_FILETYPE_EVENT | filecomp,
                     sizeof(filename), filename );

    /*if stat succeeds, compute total file size*/
    if( stat( filename, &filestat ) == 0 )
    {
      info.traceFileSize += (uint64_t)filestat.st_size;
    }
    /*otherwise, re-try with uncompressed file*/
    else
    {
      if( filecomp == OTF_FILECOMPRESSION_COMPRESSED )
      {
        filecomp = OTF_FILECOMPRESSION_UNCOMPRESSED;
        i--; continue;
      }
    }

    filecomp = OTF_FILECOMPRESSION_COMPRESSED;
  }

  /*printing the results and cleanup*/
  if( 0 < infoLevel )
  {
    show_info_level_1( &info );
    free_data_level_1( &info );
    if( 1 < infoLevel )
    {
      show_info_level_2( &info );
      free_data_level_2( &info );
    }
  }
  checkVal = OTF_HandlerArray_close( handles );
  otfinfo_assert( checkVal );
  checkVal = OTF_Reader_close( reader );
  otfinfo_assert( checkVal );
  OTF_FileManager_close( manager );

  /*if the info level is higher than two, we have to read again the files*/
  if( 2 < infoLevel )
  {
    /*prepare for reading*/
    manager = OTF_FileManager_open( numFileHandles );
    otfinfo_assert( manager );
    reader = OTF_Reader_open( fileLocation, manager );
    otfinfo_assert( reader );
    handles = OTF_HandlerArray_open();
    otfinfo_assert( handles );

    info.infoLevel = 3;
    set_handles_level_3( handles, &info );
    if( 3 < infoLevel )
    {
      info.infoLevel = 4;
      set_handles_level_4( handles, &info );
    }

    /*read definitions*/
    checkVal = OTF_Reader_readDefinitions( reader, handles );
    otfinfo_assert( checkVal != OTF_READ_ERROR );

    if( info.counterMarkerDefinition != 0 )
    {
      /*read markers*/
      checkVal = OTF_Reader_readMarkers( reader, handles );
      otfinfo_assert( checkVal != OTF_READ_ERROR );
    }

    if( 1 == showProgress )
    {
      /*printing a kind of progress bar*/
      OTF_Reader_setRecordLimit( reader, 10000 );
      size = 1;
      currRead = 0;
      printf( "\n\n" );
      printf( "reading events:\n" );
      for( i = 0; i < PROGRESSBARLEN + 3; i++ )
        printf( " " );
      printf( "|100%%\r" );
      printf( "0%%|" ); fflush( stdout );
      i = 0;
      while( (checkVal = OTF_Reader_readEvents( reader, handles )) == 10000 )
      {
        int current;
        otfinfo_assert( checkVal != OTF_READ_ERROR )
        OTF_Reader_eventBytesProgress( reader, &minRead, &currRead, &size );
        current = ( PROGRESSBARLEN * currRead ) / size;
        for( ; i < current; i++ )
          printf( "#" );
        fflush( stdout );
      }
      otfinfo_assert( checkVal != OTF_READ_ERROR )
      for( ; i < PROGRESSBARLEN; i++ )
        printf( "#" );
      printf( "\n\n" );
    }
    else
    {
      checkVal = OTF_Reader_readEvents( reader, handles );
      otfinfo_assert( checkVal != OTF_READ_ERROR );
    }
    /*printing the results and cleanup*/
    show_info_level_3( &info );
    free_data_level_3( &info );
    if( 3 < infoLevel )
    {
      show_info_level_4( &info );
      free_data_level_4( &info );
    }
    checkVal = OTF_HandlerArray_close( handles );
    otfinfo_assert( checkVal );
    checkVal = OTF_Reader_close( reader );
    otfinfo_assert( checkVal );
    OTF_FileManager_close( manager );
  }

  free( info.filePrefix );

  return 0;
}

static void set_handles_level_1( OTF_HandlerArray *handles,
                                 definitionInfoT *info )
{
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleUnknownRecord,
                               OTF_UNKNOWN_RECORD );

  /*handler and inits for getting the creator name*/
  info->creatorName = NULL;
  OTF_HandlerArray_setHandler( handles, (OTF_FunctionPointer*)handleDefCreator,
                               OTF_DEFCREATOR_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCREATOR_RECORD );

  /*handler and inits for getting trace's unique id*/
  info->traceUniqueId = 0;
  OTF_HandlerArray_setHandler( handles, (OTF_FunctionPointer*)handleDefUniqueId,
                               OTF_DEFUNIQUEID_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFUNIQUEID_RECORD );

  /*handler and inits for getting the otf version*/
  info->otfVersionString = NULL;
  info->otfVersionMajor = 0;
  info->otfVersionMinor = 0;
  info->otfVersionSub = 0;
  OTF_HandlerArray_setHandler( handles, (OTF_FunctionPointer*)handleDefVersion,
                               OTF_DEFVERSION_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFVERSION_RECORD );

  /*handler and inits for getting the count of process definitions*/
  info->counterProcessDefinition = 0;
  OTF_HandlerArray_setHandler( handles, (OTF_FunctionPointer*)handleDefProcess,
                               OTF_DEFPROCESS_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFPROCESS_RECORD );

  /*handler and inits for getting the timer resolution*/
  info->timerResolution = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefTimerResolution,
                               OTF_DEFTIMERRESOLUTION_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFTIMERRESOLUTION_RECORD );

  /*handler and inits for getting comments*/
  info->definitionComments = NULL;
  info->counterDefinitionComment = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefinitionComment,
                               OTF_DEFINITIONCOMMENT_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFINITIONCOMMENT_RECORD );
}

static void show_info_level_1( definitionInfoT *info )
{
  uint32_t i = 0, index = 0;
  double resolution = info->timerResolution;
  double fileSize = info->traceFileSize;
  char* unitTimer;
  char* unitFileSize;

  /*formating the timerresolution*/
  while( (resolution / 1000 >= 1.0) && (i < 4) )
  {
    resolution /= 1000;
    i++;
  }
  switch( i )
  {
    case 0: unitTimer = "Hz";break;
    case 1: unitTimer = "KHz";break;
    case 2: unitTimer = "MHz";break;
    default: unitTimer = "GHz";break;
  }

  /*formating the size of the event files*/
  i = 0;
  while( (fileSize / 1024 >= 1.0) && (i < 5) )
  {
    fileSize /= 1024;
    i++;
  }
  switch( i )
  {
    case 0: unitFileSize = "Bytes"; break;
    case 1: unitFileSize = "KB"; break;
    case 2: unitFileSize = "MB"; break;
    case 3: unitFileSize = "GB"; break;
    default: unitFileSize = "TB";break;
  }

  printf( "\n##############" );
  printf( "\n#info level 1#" );
  printf( "\n##############\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| general information \n" );
  printf( "+----------------------+--------------------------------------------------------\n" );
  printf( "| tracefile name       | %s\n", info->filePrefix );
  printf( "| creator of the trace | %s\n", info->creatorName );
  printf( "| trace unique id      | %lli\n", (unsigned long long int)info->traceUniqueId );
  printf( "| used OTF version     | %i.%i.%i %s\n", info->otfVersionMajor,
          info->otfVersionMinor, info->otfVersionSub, info->otfVersionString );
  printf( "| event files size     | %.2f %s\n", fileSize,unitFileSize );
  printf( "| process definitions  | %llu\n",
          (unsigned long long)info->counterProcessDefinition );
  printf( "| timer resolution     | %.2f %s\n",resolution,unitTimer );
  printf( "+----------------------+--------------------------------------------------------\n" );
  index = info->counterDefinitionComment;
  printf( "| definition comments\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < index; i++ )
  {
    printf( "| %s\n", info->definitionComments[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
}

static void free_data_level_1( definitionInfoT *info )
{
  uint32_t i, index;
  free( info->creatorName );

  index = info->counterDefinitionComment;
  for( i = 0; i < index; i++ )
  {
    free( info->definitionComments[i] );
  }
  free( info->definitionComments );
  free( info->otfVersionString );
}

static void set_handles_level_2( OTF_HandlerArray *handles, definitionInfoT *info )
{
  /*handler and inits for getting the count of function definitions*/
  info->counterFunctionDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefFunction,
                               OTF_DEFFUNCTION_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFFUNCTION_RECORD );

  /*handler and inits for getting the count of collop. definitions*/
  info->counterCollectiveOperationDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCollectiveOperation,
                               OTF_DEFCOLLOP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOLLOP_RECORD );

  /*handler and inits for getting the count of counter definitions*/
  info->counterCounterDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCounter,
                               OTF_DEFCOUNTER_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOUNTER_RECORD );

  /*handler and inits for getting the count of process group definitions*/
  info->counterProcessGroupDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefProcessGroup,
                               OTF_DEFPROCESSGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFPROCESSGROUP_RECORD );

  /*handler and inits for getting the count of function group definitions*/
  info->counterFunctionGroupDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefFunctionGroup,
                               OTF_DEFFUNCTIONGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFFUNCTIONGROUP_RECORD );

  /*handler and inits for getting the count of counter group definitions*/
  info->counterCounterGroupDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCounterGroup,
                               OTF_DEFCOUNTERGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOUNTERGROUP_RECORD );

  /*handler and inits for getting the source files*/
  info->sourceFileNames = NULL;
  info->counterSourceFileName = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefSclFile,
                               OTF_DEFSCLFILE_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFSCLFILE_RECORD );

  /*handler and inits for getting the count of marker definitions*/
  info->counterMarkerDefinition = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefMarker,
                               OTF_DEFMARKER_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFMARKER_RECORD  );
}

static void show_info_level_2( definitionInfoT *info )
{
  int index,i;

  printf( "\n##############" );
  printf( "\n#info level 2#" );
  printf( "\n##############\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| trace content\n" );
  printf( "+----------------------------+--------------------------------------------------\n" );
  printf( "| function definitions       | %llu\n",
          (unsigned long long)info->counterFunctionDefinition );
  printf( "| counter definitions        | %llu\n",
          (unsigned long long)info->counterCounterDefinition );
  printf( "| collective op. definitions | %llu\n",
          (unsigned long long)info->counterCollectiveOperationDefinition );
  printf( "|                            |\n" );
  printf( "| process group definitions  | %llu\n",
          (unsigned long long)info->counterProcessGroupDefinition );
  printf( "| function group definitions | %llu\n",
          (unsigned long long)info->counterFunctionGroupDefinition );
  printf( "| counter group definitions  | %llu\n",
          (unsigned long long)info->counterCounterGroupDefinition );
  printf( "|                            |\n" );
  printf( "| marker definitions         | %llu\n",
          (unsigned long long)info->counterMarkerDefinition );
  printf( "+----------------------------+--------------------------------------------------\n" );

  index = info->counterSourceFileName;
  printf( "| source file names\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < index; i++)
  {
    printf( "| %s\n", info->sourceFileNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
}

static void free_data_level_2( definitionInfoT *info )
{
  int i, index;

  index = info->counterSourceFileName;

  for( i = 0; i < index; i++ )
  {
    free( (info->sourceFileNames)[i] );
  }
  free( info->sourceFileNames );
}

static void set_handles_level_3( OTF_HandlerArray *handles,
                                 definitionInfoT *info )
{
  uint64_t i = 0;

  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleUnknownRecord,
                               OTF_UNKNOWN_RECORD );

  /*handler and inits for getting the count of enters*/
  info->counterEnter = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleEnter,
                               OTF_ENTER_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_ENTER_RECORD );

  /*handler and inits for getting the count of leaves*/
  info->counterLeave = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleLeave,
                               OTF_LEAVE_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_LEAVE_RECORD );

  /*handler and inits for getting the count of sends*/
  info->counterSend = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleSendMsg,
                               OTF_SEND_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_SEND_RECORD );

  /*handler and inits for getting the count of receives*/
  info->counterReceive = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleRecvMsg,
                               OTF_RECEIVE_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_RECEIVE_RECORD );

  /*handler and inits for getting the count of RMA puts*/
  info->counterRMAPut = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleRMAPut,
                               OTF_RMAPUT_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_RMAPUT_RECORD  );

  /*handler and inits for getting the count RAM put remote end*/
  info->counterRMAPutRemoteEnd = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleRMAPutRemoteEnd,
                               OTF_RMAPUTRE_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_RMAPUTRE_RECORD  );

   /*handler and inits for getting the count of RMA get*/
  info->counterRMAGet = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleRMAGet,
                               OTF_RMAGET_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_RMAGET_RECORD  );

  /*handler and inits for getting the count of RMA end*/
  info->counterRMAEnd = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleRMAEnd,
                               OTF_RMAEND_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_RMAEND_RECORD  );

  /*handler and inits for getting the count of collective operations*/
  info->counterCollectiveOperation = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleCollectiveOperation,
                               OTF_COLLOP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_COLLOP_RECORD  );

  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleEndCollectiveOperation,
                               OTF_ENDCOLLOP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_ENDCOLLOP_RECORD  );

  /*handler and inits for getting the count of file operations*/
  info->counterFileOperation = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleFileOperation,
                               OTF_FILEOPERATION_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_FILEOPERATION_RECORD  );

  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleEndFileOperation,
                               OTF_ENDFILEOP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_ENDFILEOP_RECORD  );

  /*handler and inits for getting the count of markers*/
  info->counterMarker = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleMarker,
                               OTF_MARKER_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_MARKER_RECORD  );

  /*handler and inits for getting the count of snapshots*/
  info->counterSnapshot = 0;
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleEnterSnapshot,
                               OTF_ENTERSNAPSHOT_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_ENTERSNAPSHOT_RECORD  );

  (info->counters) = (counterT*)malloc( info->counterCounterDefinition *
                                        sizeof(counterT) );

  for( i = 0; i < info->counterCounterDefinition; i++ )
  {
    (info->counters)[i].name = NULL;
    (info->counters)[i].id = 0;
    (info->counters)[i].properties = 3;
    (info->counters)[i].processMap = NULL;
  }
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCounter,
                               OTF_DEFCOUNTER_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOUNTER_RECORD );

  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleCounter,
                               OTF_COUNTER_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_COUNTER_RECORD );

}

static void show_info_level_3( definitionInfoT *info )
{
  uint64_t i;

  printf( "\n##############" );
  printf( "\n#info level 3#" );
  printf( "\n##############\n" );
  printf( "+-----------------------+-------------------------------------------------------\n" );
  printf( "| enters                | %llu\n",
          (unsigned long long)info->counterEnter );
  printf( "| leaves                | %llu\n",
          (unsigned long long)info->counterLeave );
  printf( "| sends                 | %llu\n",
          (unsigned long long)info->counterSend );
  printf( "| receives              | %llu\n",
          (unsigned long long)info->counterReceive );
  printf( "| RMA Put               | %llu\n",
          (unsigned long long)info->counterRMAPut );
  printf( "| RMA Put remote end    | %llu\n",
          (unsigned long long)info->counterRMAPutRemoteEnd );
  printf( "| RMA Get               | %llu\n",
          (unsigned long long)info->counterRMAGet );
  printf( "| RMA End               | %llu\n",
          (unsigned long long)info->counterRMAEnd );
  printf( "| collective operations | %llu\n",
          (unsigned long long)info->counterCollectiveOperation );
  printf( "| file operations       | %llu\n",
          (unsigned long long)info->counterFileOperation );
  printf( "| markers               | %llu\n",
          (unsigned long long)info->counterMarker );
  printf( "| snapshots             | %llu\n",
          (unsigned long long)info->counterSnapshot );
  printf( "+-----------------------+-------------------------------------------------------\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| counters[%llu]\n",
          (unsigned long long)info->counterCounterDefinition );
  printf( "+---------------+---------------------------------------------------------------\n" );
  for( i = 0; i < info->counterCounterDefinition; i++ )
  {
     printf( "| name          | %s\n", info->counters[i].name );
     if( ((info->counters[i].properties) & OTF_COUNTER_TYPE_BITS) ==
         OTF_COUNTER_TYPE_ACC )
     {
       uint64_t lastValue =
                  process_get_sum_value( info->counters[i].processMap );
       uint64_t lastTime =
                  process_get_sum_time( info->counters[i].processMap );
       double average =
                  ((double)(lastValue)) * ((double)(info->timerResolution)) /
                  ((double)(lastTime));
       double highestRate =
                  process_get_highest_rate(info->counters[i].processMap);

       printf( "| last value    | %llu\n", (unsigned long long)lastValue );
       printf( "| average rate  | %7.4E per sec\n", average );
       printf( "| highest rate  | %7.4E per sec\n", highestRate );
     }
     printf( "|               |\n" );
  }
  printf( "+---------------+---------------------------------------------------------------\n" );
  printf( "\n" );
}

static void free_data_level_3( definitionInfoT *info )
{
  uint64_t i;

  for( i = 0; i < info->counterCounterDefinition; i++ )
  {
    free( (info->counters)[i].name );
    if( NULL != (info->counters)[i].processMap )
      hash_delete( (info->counters)[i].processMap );
  }
  free( info->counters );
}

static void set_handles_level_4( OTF_HandlerArray *handles,
                                 definitionInfoT *info )
{
  /*handler and inits for getting the names of processes*/
  info->processNames = NULL;
  info->processNames = (char**)calloc( info->counterProcessDefinition,
                                       sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefProcess,
                               OTF_DEFPROCESS_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFPROCESS_RECORD );

  /*handler and inits for getting the names of functions*/
  info->functionNames = NULL;
  info->functionNames = (char**)calloc( info->counterFunctionDefinition,
                                        sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefFunction,
                               OTF_DEFFUNCTION_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFFUNCTION_RECORD );

  /*handler and inits for getting the names of collective operations*/
  info->collectiveOperationNames = NULL;
  info->collectiveOperationNames = (char**)calloc( info->counterCollectiveOperationDefinition,
                                                   sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCollectiveOperation,
                               OTF_DEFCOLLOP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOLLOP_RECORD );

  /*handler and inits for getting the names of processe groups*/
  info->processGroupNames = NULL;
  info->processGroupNames = (char**)calloc( info->counterProcessGroupDefinition,
                                            sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefProcessGroup,
                               OTF_DEFPROCESSGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFPROCESSGROUP_RECORD );

  /*handler and inits for getting the names of function groups*/
  info->functionGroupNames = NULL;
  info->functionGroupNames = (char**)calloc( info->counterFunctionGroupDefinition,
                                             sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefFunctionGroup,
                               OTF_DEFFUNCTIONGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFFUNCTIONGROUP_RECORD );

  /*handler and inits for getting the names of counter groups*/
  info->counterGroupNames = NULL;
  info->counterGroupNames = (char**)calloc( info->counterCounterGroupDefinition,
                                            sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefCounterGroup,
                               OTF_DEFCOUNTERGROUP_RECORD);
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFCOUNTERGROUP_RECORD );

  /*handler and inits for getting the names of markers*/
  info->markerNames = NULL;
  info->markerNames = (char**)calloc( info->counterMarkerDefinition,
                                      sizeof(char*) );
  OTF_HandlerArray_setHandler( handles,
                               (OTF_FunctionPointer*)handleDefMarker,
                               OTF_DEFMARKER_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handles, info, OTF_DEFMARKER_RECORD  );
}

static void show_info_level_4( definitionInfoT *info )
{
  uint64_t i;

  printf( "\n##############" );
  printf( "\n#info level 4#" );
  printf( "\n##############\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| process definitions[%llu]\n",
          (unsigned long long)info->counterProcessDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterProcessDefinition; i++ )
  {
    printf( "| %s\n", info->processNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| function definitions[%llu]\n",
          (unsigned long long)info->counterFunctionDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for(i = 0; i < info->counterFunctionDefinition; i++ )
  {
    printf( "| %s\n",info->functionNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| marker definitions[%llu]\n",
          (unsigned long long)info->counterMarkerDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterMarkerDefinition; i++ )
  {
     printf( "| %s\n", info->markerNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| collective operation definitions[%llu]\n",
          (unsigned long long)info->counterCollectiveOperationDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterCollectiveOperationDefinition; i++ )
  {
    printf( "| %s\n", info->collectiveOperationNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| process group definitions[%llu]\n",
          (unsigned long long)info->counterProcessGroupDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterProcessGroupDefinition; i++ )
  {
    printf( "| %s\n", info->processGroupNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| function group definitions[%llu]\n",
          (unsigned long long)info->counterFunctionGroupDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterFunctionGroupDefinition; i++ )
  {
    printf( "| %s\n", info->functionGroupNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "\n" );
  printf( "+-------------------------------------------------------------------------------\n" );
  printf( "| counter group definitions[%llu]\n",
          (unsigned long long)info->counterCounterGroupDefinition );
  printf( "+-------------------------------------------------------------------------------\n" );
  for( i = 0; i < info->counterCounterGroupDefinition; i++ )
  {
     printf( "| %s\n", info->counterGroupNames[i] );
  }
  printf( "+-------------------------------------------------------------------------------\n" );
}

static void free_data_level_4( definitionInfoT *info )
{
  uint64_t i;

  if( info->processNames != NULL )
  {
    for( i = 0; i < info->counterProcessDefinition; i++ )
    {
      free( (info->processNames)[i] );
    }
    free( info->processNames );
  }

  if( info->processGroupNames != NULL )
  {
    for( i = 0; i < info->counterProcessGroupDefinition; i++ )
    {
      free( (info->processGroupNames)[i] );
    }
    free( info->processGroupNames );
  }

  if( info->functionNames != NULL )
  {
    for( i = 0; i < info->counterFunctionDefinition; i++ )
    {
      free( (info->functionNames)[i] );
    }
    free( info->functionNames );
  }

  if( info->functionGroupNames != NULL )
  {
    for( i = 0; i < info->counterFunctionGroupDefinition; i++ )
    {
      free( (info->functionGroupNames)[i] );
    }
    free( info->functionGroupNames );
  }

  for(i = 0; i < info->counterCollectiveOperationDefinition; i++ )
  {
    free( (info->collectiveOperationNames)[i] );
  }
  free( info->collectiveOperationNames );

  for(i = 0; i < info->counterCounterGroupDefinition; i++ )
  {
    free( (info->counterGroupNames)[i] );
  }
  free( info->counterGroupNames );

  for( i = 0; i < info->counterMarkerDefinition; i++ )
  {
    free( (info->markerNames)[i] );
  }
  free( info->markerNames );
}
