/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_java.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include "util/installdirs.h"

#define DEFAULT_FILTER_FILE "${sysconfdir}/vt-java-default-filter.spec"
#define MAX_FILTER_LINE_LEN 131072
#define MAX_METHOD_NAME_LEN 300
 /* size of the method ID hash table (must be a power of two!) */
#define MAX_METHOD_ID_HKEY  10024

/* Enum for filter class types */
typedef enum
{
  JAVA_FILTER_CLASS_THREAD,
  JAVA_FILTER_CLASS_METHOD
} FilterClassT;

/* Enum for filter action types */
typedef enum
{
  JAVA_FILTER_ACTION_INCLUDE,
  JAVA_FILTER_ACTION_EXCLUDE
} FilterActionT;

/* Data structure for filter entry */
typedef struct FilterEntryS
{
  FilterActionT action;  /* filter action (INCLUDE/EXCLUDE) */
  char*         pattern; /* filter pattern */
} FilterEntryT;

/* Data structure for holding all filter entries for threads/methods */
typedef struct FilterS
{
  FilterEntryT* thrdv; /* vector of thread filter entries */
  FilterEntryT* metdv; /* vector of method filter entries */
  uint32_t      thrdn; /* number of thread filter entries */
  uint32_t      metdn; /* number of method filter entries */
} FilterT;

/* Data structure for method information */
typedef struct MethodInfoS
{
  char     long_name[MAX_METHOD_NAME_LEN]; /* method name (incl. class name) */
  char*    class_name;                     /* "demangled" class name */
  char*    source_filename;                /* source file */
  uint32_t line_number;                    /* start line number */
  uint8_t  is_native;                      /* native? */
  uint8_t  is_synthetic;                   /* synthetic? (gen. by compiler) */
} MethodInfoT;

/* Data structure for hash table to map jmethodID to region identifier */
typedef struct MethodIDMapS
{
  jmethodID            mid;  /* method identifier (jmethodID) */
  uint32_t             rid;  /* associated region identifier */
  struct MethodIDMapS* next; /* pointer to next hash node */
} MethodIDMapT;

/* Local variables */

static FilterT          filter;

static MethodIDMapT**   methodid_map = NULL;

/* Global variables */

VTJVMAgent*             vt_jvmti_agent = NULL;

static void read_filter(void)
{
  char*       filenamev[2];
  char*       line;
  int         i;

  /* set filter file names for reading in this order:
     1. user's filter specifications (VT_JAVA_FILTER_SPEC)
     2. VampirTrace's default filter specifications */
  filenamev[0] = vt_env_java_filter_spec();
  filenamev[1] = vt_installdirs_expand(DEFAULT_FILTER_FILE);
  if ( filenamev[1] == NULL ) vt_error();

  /* allocate memory for filter lines */
  line = (char*)malloc(MAX_FILTER_LINE_LEN * sizeof(char));
  if ( line == NULL ) vt_error();

  /* read all filter files */

  for ( i = 0; i < 2; i++ )
  {
    FilterClassT  fclass;
    FilterActionT faction;
    FILE*         file;
    char*         token;
    char*         ptr;
    uint32_t      lineno = 0;
    uint8_t       parse_error = 0;

    /* skip filter file, if it's not specified */
    if ( filenamev[i] == NULL ) continue;

    /* open filter file for reading */
    if ( (file = fopen(filenamev[i], "r")) == NULL )
      vt_error_msg("Cannot open file %s: %s", filenamev[i],
                   strerror(errno));

    /* read line by line */

    while ( fgets(line, MAX_FILTER_LINE_LEN - 1, file ) )
    {
      /* increment line number */
      lineno++;

      /* remove newline, if necessary */
      if ( strlen(line) > 0 && line[strlen(line)-1] == '\n' )
        line[strlen(line)-1] = '\0';

      /* strip whitespace from line */
      vt_strtrim(line);

      /* skip line, if it's empty or a comment */
      if ( strlen(line) == 0 ) continue;
      if ( line[0] == '#' ) continue;

      /* get filter class type */

      if ( (token = strtok(line, " ")) == NULL)
      {
        parse_error = 1;
        break;
      }

      /* convert token to lower case */
      ptr = token;
      while ( *ptr ) { *ptr = tolower(*ptr); ptr++; }

      if ( strcmp(token, "thread") == 0 )
      {
        fclass = JAVA_FILTER_CLASS_THREAD;
      }
      else if ( strcmp(token, "method") == 0 )
      {
        fclass = JAVA_FILTER_CLASS_METHOD;
      }
      else
      {
        parse_error = 1;
        break;
      }

      /* get filter action type */

      if ( (token = strtok(NULL, " ")) == NULL)
      {
        parse_error = 1;
        break;
      }

      /* convert token to lower case */
      ptr = token;
      while ( *ptr ) { *ptr = tolower(*ptr); ptr++; }

      if ( strcmp(token, "include") == 0 )
      {
        faction = JAVA_FILTER_ACTION_INCLUDE;
      }
      else if ( strcmp(token, "exclude") == 0 )
      {
        faction = JAVA_FILTER_ACTION_EXCLUDE;
      }
      else
      {
        parse_error = 1;
        break;
      }

      /* get filter pattern */

      if ( (token = strtok(NULL, ";")) != NULL)
      {
        do
        {
          FilterEntryT* new_filter_entry;

          /* create new filter entry ... */

          /* ... for a thread */
          if ( fclass == JAVA_FILTER_CLASS_THREAD )
          {
            filter.thrdv =
              (FilterEntryT*)realloc(filter.thrdv,
                (filter.thrdn + 1) * sizeof(FilterEntryT));
            if ( filter.thrdv == NULL ) vt_error();
            new_filter_entry = &(filter.thrdv[filter.thrdn++]);
          }
          /* ... for a method */
          else /* JAVA_FILTER_CLASS_METHOD */
          {
            filter.metdv =
                (FilterEntryT*)realloc(filter.metdv,
                  (filter.metdn + 1) * sizeof(FilterEntryT));
            if ( filter.metdv == NULL ) vt_error();
            new_filter_entry = &(filter.metdv[filter.metdn++]);
          }

          new_filter_entry->action  = faction;
          new_filter_entry->pattern = strdup(token);

        } while( (token = strtok(NULL, ";")) != NULL );
      }
      else
      {
        parse_error = 1;
        break;
      }
    }

    /* close filter file */
    fclose(file);

    if ( parse_error )
      vt_error_msg("%s:%u: Could not be parsed", filenamev[i], lineno);
  }

  /* free memory for default filter file name */
  free(filenamev[1]);

  /* free memory for filter lines */
  free(line);
}

static uint8_t check_filter(FilterClassT fclass, const char* name)
{
  FilterEntryT* entryv;
  uint32_t      entryn;
  uint32_t      i;
  uint8_t       res;

  if ( fclass == JAVA_FILTER_CLASS_THREAD )
  {
    entryv = filter.thrdv;
    entryn = filter.thrdn;
  }
  else /* JAVA_FILTER_CLASS_METHOD */
  {
    entryv = filter.metdv;
    entryn = filter.metdn;
  }

  if ( strcmp(name, "Unknown") == 0 )
  {
    res = 1;
  }
  else
  {
    res = 0;
    for ( i = 0; i < entryn; i++ )
    {
      if ( fnmatch(entryv[i].pattern, name, 0) == 0 )
      {
        if ( entryv[i].action == JAVA_FILTER_ACTION_INCLUDE )
        {
          break;
        }
        else /* JAVA_FILTER_ACTION_EXCLUDE */
        {
          res = 1;
          break;
        }
      }
    }
  }

  return res;
}

static MethodIDMapT* get_methodid_map(jmethodID mid)
{
  size_t        hkey = (size_t)mid & (MAX_METHOD_ID_HKEY - 1);
  MethodIDMapT* curr = methodid_map[hkey];

  while ( curr )
  {
    if ( curr->mid == mid )
      return curr;
    curr = curr->next;
  }

  return NULL;
}

static MethodIDMapT* add_methodid_map(jmethodID mid, uint32_t rid)
{
  size_t        hkey = (size_t)mid & (MAX_METHOD_ID_HKEY - 1);
  MethodIDMapT* add = (MethodIDMapT*)malloc(sizeof(MethodIDMapT));
  if ( add == NULL ) vt_error();

  add->mid  = mid;
  add->rid  = rid;
  add->next = methodid_map[hkey];
  methodid_map[hkey] = add;

  return add;
}

static void get_method_info(jvmtiEnv* jvmti, jmethodID method,
                            MethodInfoT* methodInfo)
{
  jclass                class;
  jvmtiLineNumberEntry* lines = NULL;
  jint                  line_count;
  jboolean              is_native;
  jboolean              is_synthetic;
  jvmtiError            error;
  char*                 method_name = NULL;
  char*                 class_signature = NULL;
  char*                 source_filename = NULL;

  /* initialize long method name */
  strcpy(methodInfo->long_name, "Unknown");

  /* check whether method is native */
  error = (*jvmti)->IsMethodNative(jvmti, method, &is_native);
  VT_JAVA_CHECK_ERROR(jvmti, error, "IsMethodNative");
  methodInfo->is_native = (is_native) ? 1 : 0;

  /* check whether method is synthetic */
  error = (*jvmti)->IsMethodSynthetic(jvmti, method, &is_synthetic);
  VT_JAVA_CHECK_ERROR(jvmti, error, "IsMethodSynthetic");
  methodInfo->is_synthetic = (is_synthetic) ? 1 : 0;

  /* get method name */
  error = (*jvmti)->GetMethodName(jvmti, method, &method_name, NULL, NULL);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetMethodName");

  /* get class object that declaring method */
  error = (*jvmti)->GetMethodDeclaringClass(jvmti, method, &class);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetMethodDeclaringClass");

  /* get class signature */
  error = (*jvmti)->GetClassSignature(jvmti, class, &class_signature, NULL);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetClassSignature");

  /* "demangle" class signature, if available */

  methodInfo->class_name = NULL;
  if ( class_signature != NULL )
  {
    char* ptr = class_signature;
    while ( *ptr != '\0' )
    {
      switch ( *ptr )
      {
        case '/':
          case ';': *ptr = (*(ptr+1) == '\0') ? '\0' : '.'; break;
          default : break;
      }
      ptr++;
    }

    /* store "demangled" class signature */
    methodInfo->class_name = strdup(class_signature+1);
  }

  /* copy class name and method name into long_name, if possible */

  if ( method_name != NULL )
  {
    int len = 0;

    if ( methodInfo->class_name != NULL)
      len = (int)strlen(methodInfo->class_name)+1;
    len += (int)strlen(method_name)+1;

    if ( len < MAX_METHOD_NAME_LEN )
    {
      /* include class name, if available */
      if ( methodInfo->class_name != NULL )
      {
        snprintf(methodInfo->long_name, len, "%s.%s",
                 methodInfo->class_name, method_name);
      }
      /* otherwise copy method name only */
      else
      {
        strncpy(methodInfo->long_name, method_name, len);
        methodInfo->long_name[len] = '\0';
      }
    }
  }

  /* get source code location, if possible */

  methodInfo->source_filename = NULL;
  methodInfo->line_number = 0;
  if ( !methodInfo->is_native )
  {
    (*jvmti)->GetSourceFileName(jvmti, class, &source_filename);
    (*jvmti)->GetLineNumberTable(jvmti, method, &line_count, &lines);
    if ( source_filename && line_count > 0 )
    {
      methodInfo->source_filename = strdup(source_filename);
      methodInfo->line_number = lines[0].line_number;
    }
  }

  /* free strings/arrays allocated by JVMTI, if necessary */

  if ( method_name != NULL )
    (*jvmti)->Deallocate(jvmti, (void*)method_name);
  if ( class_signature != NULL )
    (*jvmti)->Deallocate(jvmti, (void*)class_signature);
  if ( source_filename != NULL )
    (*jvmti)->Deallocate(jvmti, (void*)source_filename);
  if ( lines )
    (*jvmti)->Deallocate(jvmti, (void*)lines);
}

static MethodIDMapT* register_method(uint32_t threadid, jvmtiEnv* jvmti,
                                     jmethodID method)
{
  MethodIDMapT*    methodid_map;
  MethodInfoT      method_info;
  uint32_t         rid = VT_NO_ID;
  uint32_t         fid = VT_NO_ID;
  uint32_t         lno = VT_NO_LNO;

  /* get method information */
  memset(&method_info, 0, sizeof(MethodInfoT));
  get_method_info(jvmti, method, &method_info);

  /* check whether method shall be excluded */
  if ( !( (method_info.is_native && !vt_env_java_native())
           || (method_info.is_synthetic && !vt_env_java_synthetic())
           || check_filter(JAVA_FILTER_CLASS_METHOD, method_info.long_name) ) )
  {
    /* register source file, if available */

    if ( method_info.source_filename != NULL && method_info.line_number != 0 )
    {
      fid = vt_def_scl_file(threadid, method_info.source_filename);
      lno = method_info.line_number;
    }

    /* register method and store region identifier */
    rid = vt_def_region(threadid,
                        method_info.long_name,
                        fid, lno, VT_NO_LNO,
                        (vt_env_java_group_classes()) ? method_info.class_name : NULL,
                        VT_FUNCTION);

    /* free allocated strings, if necessary */

    if ( method_info.class_name ) free(method_info.class_name);
    if ( method_info.source_filename ) free(method_info.source_filename);
  }

  /* create hash map entry */
  methodid_map = add_methodid_map(method, rid);

  return methodid_map;
}

static void lock_agent(jvmtiEnv* jvmti)
{
  jvmtiError error;

  error = (*jvmti)->RawMonitorEnter(jvmti, vt_jvmti_agent->lock);
  VT_JAVA_CHECK_ERROR(jvmti, error, "RawMonitorEnter");
}

static void unlock_agent(jvmtiEnv *jvmti)
{
  jvmtiError error;

  error = (*jvmti)->RawMonitorExit(jvmti, vt_jvmti_agent->lock);
  VT_JAVA_CHECK_ERROR(jvmti, error, "RawMonitorExit");
}

/* Callback for JVMTI_EVENT_VM_START */
static void JNICALL cbVMStart(jvmtiEnv* jvmti, JNIEnv* env)
{
  /* not used */
  (void)env;

  lock_agent(jvmti);
  {
    /* indicate VM has started */
    vt_jvmti_agent->vm_is_started = JNI_TRUE;

    vt_cntl_msg(2, "JVMTI: VM started");
  }
  unlock_agent(jvmti);
}

/* Callback for JVMTI_EVENT_VM_INIT */
static void JNICALL cbVMInit(jvmtiEnv* jvmti, JNIEnv* env, jthread thread)
{
  /* not used */
  (void)env; (void)thread;

  lock_agent(jvmti);
  {
    jvmtiError error;

    /* set remaining event notification modes */

    error =
      (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                         JVMTI_EVENT_THREAD_START, NULL);
    VT_JAVA_CHECK_ERROR(jvmti, error,
                        "SetEventNotificationMode[JVMTI_EVENT_THREAD_START]");
    error =
      (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                         JVMTI_EVENT_THREAD_END, NULL);
    VT_JAVA_CHECK_ERROR(jvmti, error,
                        "SetEventNotificationMode[JVMTI_EVENT_THREAD_END]");
    error =
      (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                         JVMTI_EVENT_METHOD_ENTRY, NULL);
    VT_JAVA_CHECK_ERROR(jvmti, error,
                        "SetEventNotificationMode[JVMTI_EVENT_METHOD_ENTRY]");
    error =
      (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                         JVMTI_EVENT_METHOD_EXIT, NULL);
    VT_JAVA_CHECK_ERROR(jvmti, error,
                        "SetEventNotificationMode[JVMTI_EVENT_METHOD_EXIT]");

    /* read filter specifications for threads and methods */
    read_filter();

    /* allocate memory for method ID hash table */
    methodid_map = (MethodIDMapT**)calloc(MAX_METHOD_ID_HKEY, sizeof(MethodIDMapT*));
    if ( methodid_map == NULL ) vt_error();

    /* initialize VampirTrace */
    vt_open();

    /* indicate VM has initalized */
    vt_jvmti_agent->vm_is_initialized = JNI_TRUE;

    vt_cntl_msg(2, "JVMTI: VM initialized");
  }
  unlock_agent(jvmti);
}

/* Callback for JVMTI_EVENT_VM_DEATH */
static void JNICALL cbVMDeath(jvmtiEnv* jvmti, JNIEnv* env)
{
  uint32_t i;

  /* not used */
  (void)env;

  lock_agent(jvmti);
  {
    /* indicate VM is dead */
    vt_jvmti_agent->vm_is_dead = JNI_TRUE;

    /* finalize VampirTrace */
    vt_close();

    /* free vector of thread filter entries */

    if ( filter.thrdn > 0 )
    {
      for ( i = 0; i < filter.thrdn; i++ )
        free(filter.thrdv[i].pattern);
      free(filter.thrdv);
    }

    /* free vector of method filter entries */

    if ( filter.metdn > 0 )
    {
      for ( i = 0; i < filter.metdn; i++ )
        free(filter.metdv[i].pattern);
      free(filter.metdv);
    }

    /* free method ID hash table */

    if ( methodid_map != NULL )
    {
      MethodIDMapT* tmp;

      for ( i = 0; i < MAX_METHOD_ID_HKEY; i++ )
      {
        while ( methodid_map[i] )
        {
          tmp = methodid_map[i]->next;
          free(methodid_map[i]);
          methodid_map[i] = tmp;
        }
      }
      free(methodid_map);
    }

    vt_cntl_msg(2, "JVMTI: VM terminated");
  }
  unlock_agent(jvmti);
}

/* Callback for JVMTI_EVENT_THREAD_START */
static void JNICALL cbThreadStart(jvmtiEnv* jvmti, JNIEnv* env, jthread thread)
{
  /* not used */
  (void)env;

  lock_agent(jvmti);
  {
    if ( !vt_jvmti_agent->vm_is_dead )
    {
      char thread_name[VT_MAX_THREAD_NAME_LEN];

      /* get thread name */
      vt_java_get_thread_name(jvmti, thread, thread_name, sizeof(thread_name));

      /* register thread, if it's not excluded */
      if ( !check_filter(JAVA_FILTER_CLASS_THREAD, thread_name) )
        VTThrd_registerThread(thread, thread_name);
    }
  }
  unlock_agent(jvmti);
}

/* Callback for JVMTI_EVENT_THREAD_END */
static void JNICALL cbThreadEnd(jvmtiEnv* jvmti, JNIEnv* env, jthread thread)
{
  /* not used */
  (void)jvmti; (void)env; (void)thread;
}

/* Callback for JVMTI_EVENT_METHOD_ENTRY */
static void JNICALL cbMethodEntry(jvmtiEnv* jvmti, JNIEnv* env,
                                  jthread thread, jmethodID method)
{
  jvmtiError    error;
  MethodIDMapT* methodid_map;
  uint32_t*     threadid;
  uint64_t      time;

  /* not used */
  (void)env;

  if ( vt_jvmti_agent->vm_is_dead ) return;

  time = vt_pform_wtime();

  /* get thread identifier */
  error = (*jvmti)->GetThreadLocalStorage(jvmti, thread, (void**)&threadid);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetThreadLocalStorage");

  /* return immediately, if thread is excluded (no thread ID assigned) */
  if ( threadid == NULL ) return;

  /* get map entry for method identifier */
  if ( (methodid_map = get_methodid_map(method)) == NULL )
  {
    /* method entered the first time, register method */

    VTTHRD_LOCK_IDS();
    if ( (methodid_map = get_methodid_map(method)) == NULL )
      methodid_map = register_method(*threadid, jvmti, method);
    VTTHRD_UNLOCK_IDS();
  }

  /* write enter record, if method isn't excluded */
  if ( methodid_map->rid != VT_NO_ID )
    vt_enter(*threadid, &time, methodid_map->rid);
}

/* Callback for JVMTI_EVENT_METHOD_EXIT */
static void JNICALL cbMethodExit(jvmtiEnv* jvmti, JNIEnv* env,
                                 jthread thread, jmethodID method,
                                 jboolean was_popped_by_exception,
                                 jvalue return_value)
{
  jvmtiError    error;
  MethodIDMapT* methodid_map;
  uint32_t*     threadid;
  uint64_t      time;

  if(vt_jvmti_agent->vm_is_dead) return;

  time = vt_pform_wtime();

  /* get thread identifier */
  error = (*jvmti)->GetThreadLocalStorage(jvmti, thread, (void**)&threadid);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetThreadLocalStorage");

  /* return immediately, if thread is excluded (no thread ID assigned) */
  if ( threadid == NULL ) return;

  /* get map entry for method identifier */
  methodid_map = get_methodid_map(method);

  /* write exit record, if method was entered and isn't excluded */
  if ( methodid_map != NULL && methodid_map->rid != VT_NO_ID )
    vt_exit(*threadid, &time);
}

/* Load the VM Agent */
JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *vm, char *options, void *reserved)
{
  static VTJVMAgent   agent;
  jvmtiEnv*           jvmti;
  jvmtiError          error;
  jint                res;
  jint                version;
  jvmtiCapabilities   capabilities;
  jvmtiEventCallbacks callbacks;

  memset((void*)&agent, 0, sizeof(agent));
  vt_jvmti_agent = &agent;

  /* get JVMTI's environment */

  res = (*vm)->GetEnv(vm, (void**)&jvmti, JVMTI_VERSION_1);
  if ( res != JNI_OK )
  {
    vt_error_msg("Unable to access JVMTI Version 1 (0x%x), "
                 "is your JDK a 5.0 or newer version? "
                 "JNIEnv's GetEnv() returned %d", JVMTI_VERSION_1, res);
  }
  agent.jvmti = jvmti;

  /* get JVMTI's version number */
  error = (*jvmti)->GetVersionNumber(jvmti, &version);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetVersionNumber");
  agent.jvmti_version = version;

  /* set capabilities */

  memset(&capabilities, 0, sizeof(jvmtiCapabilities));
  capabilities.can_generate_method_entry_events = 1;
  capabilities.can_generate_method_exit_events  = 1;
  capabilities.can_get_line_numbers             = 1;
  capabilities.can_get_source_file_name         = 1;
  capabilities.can_get_synthetic_attribute      = 1;

  error = (*jvmti)->AddCapabilities(jvmti, &capabilities);
  VT_JAVA_CHECK_ERROR(jvmti, error, "AddCapabilities");

  /* set event callbacks */

  memset(&callbacks, 0, sizeof(jvmtiCapabilities));
  callbacks.VMStart     = cbVMStart;
  callbacks.VMInit      = cbVMInit;
  callbacks.VMDeath     = cbVMDeath;
  callbacks.ThreadStart = cbThreadStart;
  callbacks.ThreadEnd   = cbThreadEnd;
  callbacks.MethodEntry = cbMethodEntry;
  callbacks.MethodExit  = cbMethodExit;

  error = (*jvmti)->SetEventCallbacks(jvmti, &callbacks,
          (jint)sizeof(callbacks));
  VT_JAVA_CHECK_ERROR(jvmti, error, "SetEventCallbacks");

  /* set event notification modes */

  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                             JVMTI_EVENT_VM_START, NULL);
  VT_JAVA_CHECK_ERROR(jvmti, error,
                      "SetEventNotificationMode[JVMTI_EVENT_VM_START]");
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                             JVMTI_EVENT_VM_INIT, NULL);
  VT_JAVA_CHECK_ERROR(jvmti, error,
                      "SetEventNotificationMode[JVMTI_EVENT_VM_INIT]");
  error = (*jvmti)->SetEventNotificationMode(jvmti, JVMTI_ENABLE,
                                             JVMTI_EVENT_VM_DEATH, NULL);
  VT_JAVA_CHECK_ERROR(jvmti, error,
                      "SetEventNotificationMode[JVMTI_EVENT_VM_DEATH]");

  /* create raw monitor for this agent to protect critical sections of code */
  error = (*jvmti)->CreateRawMonitor(jvmti, "agent", &(vt_jvmti_agent->lock));
  VT_JAVA_CHECK_ERROR(jvmti, error, "CreateRawMonitor[agent]");

  vt_cntl_msg(2, "JVMTI: VM agent loaded");

  return JNI_OK;
}

/* Unload the VM Agent */
JNIEXPORT void JNICALL Agent_OnUnload(JavaVM *vm)
{
  vt_cntl_msg(2, "JVMTI: VM agent unloaded");
}

void vt_java_get_thread_name(jvmtiEnv* jvmti, jthread thread,
                             char* tname, int maxlen)
{
  jvmtiThreadInfo thread_info;
  jvmtiError      error;

  if ( jvmti == NULL ) jvmti = vt_jvmti_agent->jvmti;
  vt_libassert(jvmti != NULL);

  /* initialize thread name */
  strcpy(tname, "Unknown");

  /* due to a bug in JVMTI version 1.0.x we must return immediately,
     if given thread isn't specified */
  if ( thread == NULL && vt_jvmti_agent->jvmti_version < 0x30010100 )
    return;

  /* get thread information */
  memset(&thread_info,0, sizeof(thread_info));
  error = (*jvmti)->GetThreadInfo(jvmti, thread, &thread_info);
  VT_JAVA_CHECK_ERROR(jvmti, error, "GetThreadInfo");

  /* copy thread name into tname, if possible */
  if ( thread_info.name != NULL ) {
    int len = (int)strlen(thread_info.name);
    if ( len < maxlen ) strcpy(tname, thread_info.name);

    /* free string allocated by JVMTI */
    (*jvmti)->Deallocate(jvmti, (void*)thread_info.name);
  }
}

void vt_java_error(jvmtiEnv* jvmti, jvmtiError error, const char* prefix)
{
  char* error_str = NULL;

  if ( jvmti == NULL )
    jvmti = vt_jvmti_agent->jvmti;

  vt_libassert(jvmti != NULL);
  vt_libassert(error != JVMTI_ERROR_NONE);

  (*jvmti)->GetErrorName(jvmti, error, &error_str);
  vt_error_msg("JVMTI: %s%s%d(%s)",
               (prefix == NULL ? "" : prefix),
               (prefix == NULL ? " " : ": "),
               error,
               (error_str == NULL ? "Unknown" : error_str));
}
