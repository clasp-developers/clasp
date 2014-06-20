/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2010 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */


#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifdef _MSC_VER
#include <winsock2.h>
#include <comutil.h>
#include <Wbemidl.h>
#include <wincred.h>

#pragma comment(lib, "wbemuuid.lib")
#pragma comment(lib, "comsuppw.lib")
#pragma comment(lib, "Credui.lib")
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "orte/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/util/name_fns.h"

#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/process/plm_process.h"


#define rindex(a,b) strrchr((a),(b))

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
static int orte_plm_process_launch_threaded(orte_jobid_t jobid);
#endif

/*
 * Interface
 */
static int orte_plm_process_init(void);
static int orte_plm_process_launch(orte_job_t*);
static int orte_plm_process_terminate_orteds(void);
static int orte_plm_process_signal_job(orte_jobid_t, int32_t);

orte_plm_base_module_t orte_plm_process_module = {
    orte_plm_process_init,
    orte_plm_base_set_hnp_name,
#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
    orte_plm_process_launch_threaded,
#else
    orte_plm_process_launch,
#endif
    NULL,
    orte_plm_base_orted_terminate_job,
    orte_plm_process_terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    orte_plm_process_signal_job,
    orte_plm_process_finalize
};

static void set_handler_default(int sig);

enum {
    ORTE_PLM_RSH_SHELL_BASH = 0,
    ORTE_PLM_RSH_SHELL_ZSH,
    ORTE_PLM_RSH_SHELL_TCSH,
    ORTE_PLM_RSH_SHELL_CSH,
    ORTE_PLM_RSH_SHELL_KSH,
    ORTE_PLM_RSH_SHELL_SH,
    ORTE_PLM_RSH_SHELL_UNKNOWN
};

typedef int orte_plm_process_shell;

static const char * orte_plm_process_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
}; 

/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* global storage of active jobid being launched */
static orte_jobid_t active_job = ORTE_JOBID_INVALID;

#ifdef _MSC_VER
/*
 * local functions
 */
static char *generate_commandline(char *prefix, int argc, char **argv);
static int wmi_launch_child(char *prefix, char *remote_node, int argc, char **argv);
static int get_credential(char *node_name);
static char *read_remote_registry(uint32_t root, char *sub_key, char *key, char *remote_node, char *ntlm_auth);


/* local global storage of user credential */
static char user_name[CREDUI_MAX_USERNAME_LENGTH+1];
static char user_password[CREDUI_MAX_PASSWORD_LENGTH+1];

/* WMI service locator */
IWbemLocator *pLoc = NULL;
/* namespace for \hostname\root\default */
IWbemServices *pSvc_registry = NULL;
/* namespace for \hostname\root\cimv2 */
IWbemServices *pSvc_cimv2 = NULL;



static char *generate_commandline(char *prefix, int argc, char **argv)
{
    int i, len = 0;
    char *commandline;

    /* Generate remote launch command line. */
    for( i = 0; i < argc; i++ ) {
        if(argv[i] != NULL) {
            len += strlen(argv[i]) + 1;
        }
    }

    commandline = (char*)malloc( len + strlen(prefix) + 13);
    memset(commandline, 0, len + strlen(prefix) + 13);

    strcat(commandline, "\"");
    strcat(commandline, prefix);
    strcat(commandline, "\\bin\\orted\" ");

    for(i=1;i<argc;i++) {
       if(argv[i]!= NULL) {                        
           /* Append command args, and separate them with spaces. */
           strcat(commandline, argv[i]);
           commandline[strlen(commandline)]=' ';
       }
    }

    return commandline;
}


static int get_credential(char *node_name)
{
    if(mca_plm_process_component.use_gui_prompt) {
        
        CREDUI_INFO cui;
        BOOL fSave = true;
        DWORD dwErr;

        char gui_message[255];
        cui.cbSize = sizeof(CREDUI_INFO);
        cui.hwndParent = NULL;
        cui.pszMessageText = TEXT(gui_message);
        cui.pszCaptionText = TEXT("CredUITest");
        cui.hbmBanner = NULL;

        /* message to show on the prompt GUI. */
        SecureZeroMemory(gui_message, sizeof(gui_message));
        strcat(gui_message, "account for ");
        strcat(gui_message, node_name);

        /* uncheck the checkbox by default. */
        fSave = false;

        dwErr = CredUIPromptForCredentials(&cui,                              /* CREDUI_INFO structure */
                                           node_name,                         /* Target for credentials */
                                           NULL,                              /* Reserved */
                                           0,                                 /* Reason */
                                           user_name,                         /* User name */
                                           CREDUI_MAX_USERNAME_LENGTH+1,      /* Max number of char for user name */
                                           user_password,                     /* Password */
                                           CREDUI_MAX_PASSWORD_LENGTH+1,      /* Max number of char for password */
                                           &fSave,                            /* State of save check box */
                                           CREDUI_FLAGS_GENERIC_CREDENTIALS | /* flags */
                                           CREDUI_FLAGS_DO_NOT_PERSIST);

        if(!dwErr) {
            return ORTE_SUCCESS;
        } else {
            opal_output(0, "%s plm:process: failed to get user credential for node %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node_name);
            return ORTE_ERROR;
        }

    } else {
        char ch;
        int pos = 0;

        /* Get the user name for computer */
        printf("connecting to %s\n", node_name);
        printf("username:");
        scanf("%s",user_name);

        /* Get the password for the remote computer,
           and display '*'s on the screen. */
        printf("password:");
        while((ch=_getch())!='\r') {
            if ( ch == '\03') {
                /* user could abort with ctrl-c here. */
                opal_output(0, "Remote job aborted.");
                return ORTE_ERROR;
            }

            if(ch =='\b') {
                pos--;
                if(pos<0) {
                    pos=0;
                    continue;
                }
                printf("\b \b");
            }
            else {
                if(pos<CREDUI_MAX_PASSWORD_LENGTH+1) {
                     user_password[pos] = ch;
                    printf("*");
                    pos++;
                } else {
                    opal_output(0, "plm:process: password length exceeds limit(%d) for account %s",
                                CREDUI_MAX_PASSWORD_LENGTH+1, user_name);
                    return ORTE_ERROR;
                }
            }
        }
        user_password[pos]='\0';
        printf("\nSave Credential?(Y/N) ");

        while(ch = _getch()) {
            if ( ch == '\03') {
                /* user could abort with ctrl-c here. */
                opal_output(0, "Remote job aborted.");
                return ORTE_ERROR;
            }

            if(ch == 'y' || ch == 'Y') {
                /* something more to be done here. */
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                    "%s plm:process: credential saved for %s on %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), user_name, node_name));
                printf("%c\n", ch);
                opal_output(0, "This feature hasn't been implemented yet.");
                break;
            } else if(ch == 'n' || ch == 'N') {
                    printf("%c\n", ch);
                    break;
            }
        }
        return ORTE_SUCCESS;
    }
}

static char *read_remote_registry(uint32_t root, char *sub_key, char *key, char *remote_node, char *ntlm_auth)
{
    HRESULT hres;
    char namespace_default[100];
    char *rt_namespace_default = "\\root\\default";

    IWbemClassObject* pClass_registry = NULL;
    IWbemClassObject* pInParamsDefinition_registry = NULL;
    IWbemClassObject* pClassInstance_registry = NULL;
    IWbemClassObject* pOutParams_registry = NULL;
    VARIANT varsValue_registry;
    VariantInit(&varsValue_registry);

    BSTR ClassName_registry = SysAllocString(L"StdRegProv");
    BSTR MethodName_registry = SysAllocString(L"GetStringValue");
    
    strcpy(namespace_default, "\\\\");  
    strcat(namespace_default, remote_node );  
    strcat(namespace_default, rt_namespace_default); 

    if( NULL == pSvc_registry) {
        /* connect to default namespace */ 
        hres = pLoc->ConnectServer(_com_util::ConvertStringToBSTR(namespace_default),      /* namespace */
                                   _com_util::ConvertStringToBSTR(user_name),              /* User name */ 
                                   _com_util::ConvertStringToBSTR(user_password),          /* User password */ 
                                   (L"MS_409"),                                            /* Locale */              
                                   NULL,                                                   /* Security flags */ 
                                   _com_util::ConvertStringToBSTR(ntlm_auth),            /* Authority */         
                                   0,                                                      /* Context object */  
                                   &pSvc_registry                                          /* IWbemServices proxy */ 
                                  );

        if (FAILED(hres)) {
            opal_output(0,"Could not connect to namespace DEFAULT on node %s. Error code = %d \n",
                        remote_node, hres);
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:process: Connected to \\\\%s\\\\ROOT\\\\DEFAULT",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             remote_node));
    }

    hres = pSvc_registry->GetObject(ClassName_registry, 0, NULL, &pClass_registry, NULL);
    if (FAILED(hres)) {
        opal_output(0,"Could not get Wbem class object. Error code = %d \n", hres);
        goto cleanup;
    }

    hres = pClass_registry->GetMethod(MethodName_registry, 0, 
                                      &pInParamsDefinition_registry, NULL);

    hres = pInParamsDefinition_registry->SpawnInstance(0, &pClassInstance_registry);

    VARIANT hkey_root;
    hkey_root.vt = VT_I4;
    hkey_root.intVal = root;

    hres = pClassInstance_registry->Put(L"hDefKey", 0, &hkey_root, 0);

    VARIANT varSubKeyName;
    varSubKeyName.vt = VT_BSTR;
    varSubKeyName.bstrVal = _com_util::ConvertStringToBSTR(sub_key);

    hres = pClassInstance_registry->Put(L"sSubKeyName", 0, &varSubKeyName, 0);
    
    if(FAILED(hres)) {
        opal_output(0,"Could not Store the value for the in parameters. Error code = %d \n",hres);
        goto cleanup;
    }

    VARIANT varsValueName;
    varsValueName.vt = VT_BSTR;
    varsValueName.bstrVal = _com_util::ConvertStringToBSTR(key);

    hres = pClassInstance_registry->Put(L"sValueName", 0, &varsValueName, 0);
    if(FAILED(hres)) {
        opal_output(0,"Could not Store the value for the in parameters. Error code = %d \n",hres);
        goto cleanup;
    }
    
    /* Execute Method to read OPAL_PREFIX in the registry */
    hres = pSvc_registry->ExecMethod(ClassName_registry, MethodName_registry, 0,
                                     NULL, pClassInstance_registry, &pOutParams_registry, NULL);

    if (FAILED(hres)) {
        opal_output(0,"Could not execute method. Error code = %d \n",hres);
        goto cleanup;
    }

    /* To see what the method returned */
    /* The return value will be in &varReturnValue */
    VARIANT varReturnValue_registry;
    hres = pOutParams_registry->Get(L"ReturnValue", 0, 
                                    &varReturnValue_registry, NULL, 0);
    
    hres = pOutParams_registry->Get(L"sValue", 0, 
                                    &varsValue_registry, NULL, 0);

cleanup:
    if(NULL!=pClass_registry){
        pClass_registry->Release();
    }

    if(NULL!=pOutParams_registry){
        pOutParams_registry->Release();
    }

    if(NULL!=pInParamsDefinition_registry){
        pInParamsDefinition_registry->Release();
    }

    if(NULL!=pClassInstance_registry) {
        pClassInstance_registry->Release();
    }

    if(NULL!=ClassName_registry){
        SysFreeString(ClassName_registry);
    }

    if(NULL!=MethodName_registry){
        SysFreeString(MethodName_registry);
    }

    if( VT_NULL != varsValue_registry.vt && VT_EMPTY != varsValue_registry.vt) {
        char *value = strdup(_com_util::ConvertBSTRToString(varsValue_registry.bstrVal));
        return value;
    } else {
        return NULL;
    }

}


/**
 * Remote spawn process using WMI.
 */
static int wmi_launch_child(char *prefix, char *remote_node, int argc, char **argv)
{
    char *command_line = NULL;
    int len = 0, pid = -1;
    
    HRESULT hres;
    IWbemClassObject* pClass_cimv2 = NULL;
    IWbemClassObject* pInParamsDefinition_cimv2 = NULL;
    IWbemClassObject* pClassInstance_cimv2 = NULL;
    IWbemClassObject* pOutParams_cimv2 = NULL;
    VARIANT varCommand;
    VARIANT varProcessId;
    VariantInit(&varCommand);
    VariantInit(&varProcessId);
    BSTR MethodName_cimv2 = SysAllocString(L"Create");
    BSTR ClassName_cimv2 = SysAllocString(L"Win32_Process");


    /*Connect to WMI through the IWbemLocator::ConnectServer method*/
    char namespace_cimv2[100];
    
    char *domain_name = getenv("USERDOMAIN");
    char *ntlm_auth = (char *) malloc(sizeof(char)*(strlen("ntlmdomain:")+strlen(domain_name)+1));
    memset(ntlm_auth, 0, sizeof(char)*(strlen("ntlmdomain:")+strlen(domain_name)+1));
    strcat(ntlm_auth, "ntlmdomain:");
    strcat(ntlm_auth, domain_name);

    if( 0 != get_credential(remote_node)) {
        goto cleanup;
    }

    /* set up remote namespace path */
    char *rt_namespace_cimv2 = "\\root\\cimv2";
    strcpy(namespace_cimv2, "\\\\");  
    strcat(namespace_cimv2, remote_node );  
    strcat(namespace_cimv2, rt_namespace_cimv2);

    /* connect to cimv2 namespace */  
    hres = pLoc->ConnectServer(_com_util::ConvertStringToBSTR(namespace_cimv2),      /* namespace */ 
                               _com_util::ConvertStringToBSTR(user_name),            /*  User name */
                               _com_util::ConvertStringToBSTR(user_password),        /* User password */ 
                               (L"MS_409"),                                          /* Locale */              
                               NULL,                                                 /* Security flags */ 
                               _com_util::ConvertStringToBSTR(ntlm_auth),            /* Authority */         
                               0,                                                    /* Context object */  
                               &pSvc_cimv2                                           /* IWbemServices proxy */ 
                              );

    if (FAILED(hres)) {
        opal_output(0,"Could not connect to namespace cimv2 on node %s. Error code =%d \n",
                     remote_node, hres);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:process: Connected to \\\\%s\\\\ROOT\\\\CIMV2",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         remote_node));

    /* if there isn't a prefix ( e.g., '--noprefix' specfied,
       or Open MPI was configured without ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT), 
       let's first check the OPENMPI_HOME in user environment variables,
       and then the OPAL_PREFIX registry value. */
    if( NULL == prefix ) {
        if( mca_plm_process_component.remote_env_prefix ) {
            char *path = "Environment";
            char *key = "OPENMPI_HOME";
            /* read registry at HKEY_CURRENT_USER
               please note: this MUST be the same user as for WMI authorization. */
            char *reg_prefix = read_remote_registry(0x80000001, path, key, remote_node, ntlm_auth);
            if( NULL != reg_prefix) {
                command_line = generate_commandline(reg_prefix, argc, argv);
            }
        }
        if( NULL == command_line && mca_plm_process_component.remote_reg_prefix ) {
            char *path = "Software\\Open MPI\\";
            char *key = "OPAL_PREFIX";
            char *reg_prefix = read_remote_registry(0x80000002, path, key, remote_node, ntlm_auth);
            if( NULL != reg_prefix) {
                command_line = generate_commandline(reg_prefix, argc, argv);
            }
        }
    } else {
        /* use user specified/default prefix */
        command_line = generate_commandline(prefix, argc, argv);
    }

    if ( NULL == command_line ) {
        /* we couldn't find the execute path, abort. */
        opal_output(0, "couldn't find executable path for orted on node %s. aborted.", remote_node);
        goto cleanup;
    }

    /* Use the IWbemServices pointer to make requests of WMI */
    /* set up to call the Win32_Process::Create method */
    hres = pSvc_cimv2->GetObject(ClassName_cimv2, 0, NULL, &pClass_cimv2, NULL);

    if (FAILED(hres)) {
        opal_output(0,"Could not get Wbem class object. Error code = %d \n", hres);
        goto cleanup;
    }

    hres = pClass_cimv2->GetMethod(MethodName_cimv2, 0, 
                                   &pInParamsDefinition_cimv2, NULL);

    hres = pInParamsDefinition_cimv2->SpawnInstance(0, &pClassInstance_cimv2);

    /* Create the values for the in parameters */
    varCommand.vt = VT_BSTR;
    varCommand.bstrVal = _com_util::ConvertStringToBSTR(command_line);

    /* Store the value for the in parameters */
    hres = pClassInstance_cimv2->Put(L"CommandLine", 0,
                                     &varCommand, 0);

    /* Execute Method to launch orted on remote node*/
    hres = pSvc_cimv2->ExecMethod(ClassName_cimv2, MethodName_cimv2, 0,
                                  NULL, pClassInstance_cimv2, &pOutParams_cimv2, NULL);

    if (FAILED(hres)) {
        opal_output(0,"Could not execute method. Error code = %d \n",hres);
        goto cleanup;
    }

    /* get remote process ID */
    hres = pOutParams_cimv2->Get((L"ProcessId"), 0, 
                                 &varProcessId, NULL, 0);

    pid = varProcessId.intVal;

cleanup:

    if(NULL!=pClass_cimv2) {
        pClass_cimv2->Release();
    }

    if(NULL!=pInParamsDefinition_cimv2) {
        pInParamsDefinition_cimv2->Release();
    }

    if(NULL!=pOutParams_cimv2){
        pOutParams_cimv2->Release();
    }

    if(NULL!=ClassName_cimv2){
        SysFreeString(ClassName_cimv2);
    }

    if(NULL!=MethodName_cimv2){
        SysFreeString(MethodName_cimv2);
    }

    if(VT_NULL!=varCommand.vt) {
        VariantClear(&varCommand);
    }

    if(VT_NULL!=varProcessId.vt){
        VariantClear(&varProcessId);
    }

    return pid;
}

#endif /*_MSC_VER*/


/**
* Init the module
 */
int orte_plm_process_init(void)
{
    int rc;
    HRESULT hres;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }

#ifdef _MSC_VER
    /* Initialize COM for WMI */
    hres =  CoInitializeEx(0, COINIT_APARTMENTTHREADED); 
    if (FAILED(hres)) {
        opal_output(0, "Failed to initialize COM library. Error code = %d \n", hres);
        return ORTE_ERROR;
    }

    /* Set general COM security levels. */
    hres =  CoInitializeSecurity(NULL, 
                                 -1,                          /* COM authentication */ 
                                 NULL,                        /* Authentication services */ 
                                 NULL,                        /* Reserved */ 
                                 RPC_C_AUTHN_LEVEL_CONNECT,   /* Default authentication */  
                                 RPC_C_IMP_LEVEL_IMPERSONATE, /* Default Impersonation */  
                                 NULL,                        /* Authentication info */ 
                                 EOAC_NONE,                   /* Additional capabilities */  
                                 NULL                         /* Reserved */ 
                                );

    if (FAILED(hres)) {     
        opal_output(0, "Failed to initialize security. Error code = %d \n",hres);
        CoUninitialize();
        return ORTE_ERROR;
    }

    /* Obtain the initial locator to WMI. */ 
    hres = CoCreateInstance(CLSID_WbemLocator,
                            0,
                            CLSCTX_INPROC_SERVER, 
                            IID_IWbemLocator, (LPVOID *) &pLoc);
 
    if (FAILED(hres)) {   
        opal_output(0,"Failed to create IWbemLocator object. Err code = %d \n", hres);
        CoUninitialize();
        return ORTE_ERROR;
    }
    
    SecureZeroMemory(user_name, sizeof(user_name));
    SecureZeroMemory(user_password, sizeof(user_password));
    
#endif /*_MSC_VER*/
    
    return rc;
}


/**
 * Check the Shell variable on the specified node
 */

static int orte_plm_process_probe(orte_node_t * node, orte_plm_process_shell * shell)
{
    char ** argv;
    int rc, nfds;
    int fd[2];
    pid_t pid;

/*    HANDLE myPipeFd[2]; 
    SECURITY_ATTRIBUTES securityAttr;
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
*/    
    fd_set readset;
    fd_set errset;
    char outbuf[4096];

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:process: going to check SHELL variable on node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         node->name));

    *shell = ORTE_PLM_RSH_SHELL_UNKNOWN;
    /*
     * Build argv array
     */
     
    pid = _spawnve( _P_DETACH, argv[0], argv, NULL);
     
#if 0
    securityAttr.nLength = sizeof(SECURITY_ATTRIBUTES);     // Size of struct
    securityAttr.lpSecurityDescriptor = NULL;               // Default descriptor
    securityAttr.bInheritHandle = TRUE;                     // Inheritable

    // Create the pipe
    if (CreatePipe(&myPipeFd[0], &myPipeFd[1], &securityAttr, 0)) {

        // Create duplicate of write end so that original 
        if (!DuplicateHandle(
            GetCurrentProcess(),
            myPipeFd[0],           // Original handle
            GetCurrentProcess(),
            NULL,                    // don't create new handle
            0,
            FALSE,                   // Not inheritable
            DUPLICATE_SAME_ACCESS)
        ) {
            CloseHandle(myPipeFd[0]);
            CloseHandle(myPipeFd[1]);
            opal_output(0, "plm:process: DuplicateHandle failed with errno=%d\n", errno);
            return ORTE_ERR_IN_ERRNO;
        }  
        
        ZeroMemory( &startupInfo, sizeof(startupInfo) );
        startupInfo.cb = sizeof(startupInfo);
        ZeroMemory( &processInfo, sizeof(processInfo) );

        // Now populate startup info for CreateProcess
        GetStartupInfo(&startupInfo);
        startupInfo.dwFlags    = STARTF_USESTDHANDLES;
        startupInfo.hStdInput  = myPipeFd[0];
        startupInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
        startupInfo.hStdError  = GetStdHandle(STD_ERROR_HANDLE);
        

        // Start the child process. 
        if( !CreateProcess( argv[0],    //module name   NULL,
            NULL, //(LPSTR)(const char *) argv,
            NULL,           // Process handle not inheritable
            NULL,           // Thread handle not inheritable
            TRUE,           // Set handle inheritance to TRUE; 
            // each inheritable handle in the calling process is inherited by the new process
            0,              // No creation flags
            NULL,           // Use parent's environment block
            NULL,           // Use parent's starting directory 
            &startupInfo,   // Pointer to STARTUPINFO structure
            &processInfo )  // Pointer to PROCESS_INFORMATION structure
        ) 
        {
            CloseHandle(myPipeFd[1]);
            opal_output(0, "plm:process: CreateProcess failed with errno=%d\n", errno); //, GetLastError() ); 
            return ORTE_ERR_IN_ERRNO;
        }
    }
#endif    
    /*
    if ((pid = fork()) < 0) {
        opal_output(0, "plm:process: fork failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          // child      //processInfo.hProcess

        if (dup2(fd[1], 1) < 0) {
            opal_output(0, "plm:process: dup2 failed with errno=%d\n", errno);
            return ORTE_ERR_IN_ERRNO;
        }
        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        opal_output(0, "plm:process: close failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    */
    
    
    /* Monitor stdout */
    FD_ZERO(&readset);
    nfds = fd[0]+1;

    memset (outbuf, 0, sizeof (outbuf));
    rc = ORTE_SUCCESS;;
    while (ORTE_SUCCESS == rc) {
        int err;
        FD_SET (fd[0], &readset);
        errset = readset;
        err = select(nfds, &readset, NULL, &errset, NULL);
        if (err == -1) {
            if (errno == EINTR)
                continue;
            else {
                rc = ORTE_ERR_IN_ERRNO;
                break;
            }
        }
        if (FD_ISSET(fd[0], &errset) != 0)
            rc = ORTE_ERR_FATAL;
        /* In case we have something valid to read on stdin */
        if (FD_ISSET(fd[0], &readset) != 0) {
            ssize_t ret = 1;
            char temp[4096];
            char * ptr = outbuf;
            ssize_t outbufsize = sizeof(outbuf);

            memset (temp, 0, sizeof(temp));

            while (ret != 0) {
                ret = read (fd[0], temp, 256);
                if (ret < 0) {
                    if (errno == EINTR)
                        continue;
                    else {
                        rc = ORTE_ERR_IN_ERRNO;
                        break;
                    }
                }
                else {
                    if (outbufsize > 0) {
                        memcpy (ptr, temp, (ret > outbufsize) ? outbufsize : ret);
                        outbufsize -= ret;
                        ptr += ret;
                        if (outbufsize > 0)
                            *ptr = '\0';
                    }
                }
            }
            /* After reading complete string (aka read returns 0), we just break */
            break;
        }
    }

    /* Search for the substring of known shell-names */
/*    for (i = 0; i < (int)(sizeof (orte_plm_process_shell_name)/
                          sizeof(orte_plm_process_shell_name[0])); i++) {
        char *sh_name = NULL;

        sh_name = rindex(outbuf, '/');
        if ( sh_name != NULL ) {
            sh_name++; /* skip '/' */
            
            /* We cannot use "echo -n $SHELL" because -n is not portable. Therefore
             * we have to remove the "\n" */
/*            if ( sh_name[strlen(sh_name)-1] == '\n' ) {
                sh_name[strlen(sh_name)-1] = '\0';
            }
            if ( 0 == strcmp(sh_name, orte_plm_process_shell_name[i]) ) {
                *shell = i;
                break;
            }
        }
    }
*/
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:process: node:%s has SHELL: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         node->name, orte_plm_process_shell_name[*shell]));

    return rc;
}

/**
 * Fill the exec_path variable with the directory to the orted
 */

static int orte_plm_process_fill_exec_path( char ** exec_path )
{
    struct stat buf;

    asprintf(exec_path, "%s/orted", opal_install_dirs.bindir);
    if (0 != stat(*exec_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = "PATH is empty!";
        }
        orte_show_help("help-plm-process.txt", "no-local-orted",
                        true, path, opal_install_dirs.bindir);
        return ORTE_ERR_NOT_FOUND;
    }
   return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */

static void orte_plm_process_wait_daemon(pid_t pid, int status, void* cbdata)
{
    unsigned long deltat;

    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* tell the user something went wrong */
        opal_output(0, "ERROR: A daemon failed to start as expected.");
        opal_output(0, "ERROR: There may be more information available from");
        opal_output(0, "ERROR: the remote shell (see above).");

        if (WIFEXITED(status)) {
            opal_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                   WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                opal_output(0, "The daemon received a signal %d (with core).",
                            WTERMSIG(status));
            } else {
                opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
            opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            opal_output(0, "No extra status information is available: %d.", status);
        }
        /* report that the daemon has failed so we break out of the daemon
         * callback receive and can exit
         */
        orte_plm_base_launch_failed(active_job, pid, status, ORTE_JOB_STATE_FAILED_TO_START);
    } /* if abnormal exit */

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_plm_process_component.lock);

    if (mca_plm_process_component.num_children-- >=
        mca_plm_process_component.num_concurrent ||
        mca_plm_process_component.num_children == 0) {
        opal_condition_signal(&mca_plm_process_component.cond);
    }

    if (mca_plm_process_component.timing && mca_plm_process_component.num_children == 0) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "plm_process: could not obtain job launch stop time");
        } else {
            deltat = (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 +
                     (joblaunchstop.tv_usec - joblaunchstart.tv_usec);
            opal_output(0, "plm_process: total time to launch job is %lu usec", deltat);
        }
    }
    
    OPAL_THREAD_UNLOCK(&mca_plm_process_component.lock);

}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int orte_plm_process_launch(orte_job_t *jdata)
{
    orte_job_map_t *map = NULL;
    int proc_vpid_index;
    int local_exec_index;
    char *vpid_string = NULL;
    char *param;
    char **argv = NULL;
    char *prefix_dir;
    int argc = 0;
    int rc;
    char *lib_base = NULL, *bin_base = NULL;
    bool failed_launch = true;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_std_cntr_t nnode;
    orte_job_state_t job_state = ORTE_JOB_NEVER_LAUNCHED;

    if (orte_timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "plm_process: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:process: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* set the active jobid */
    active_job = jdata->jobid;

    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(active_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)map->nodes->addr;
    
     if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:process: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }

    if (orte_debug_daemons_flag &&
        mca_plm_process_component.num_concurrent < map->num_new_daemons) {
        /**
         * If we are in '--debug-daemons' we keep the ssh connection 
         * alive for the span of the run. If we use this option 
         * AND we launch on more than "num_concurrent" machines
         * then we will deadlock. No connections are terminated 
         * until the job is complete, no job is started
         * since all the orteds are waiting for all the others
         * to come online, and the others ore not launched because
         * we are waiting on those that have started to terminate
         * their ssh tunnels. :(
         * As we cannot run in this situation, pretty print the error
         * and return an error code.
         */
        orte_show_help("help-plm-process.txt", "deadlock-params",
                       true, mca_plm_process_component.num_concurrent, map->num_new_daemons);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }

    /*
     * After a discussion between Ralph & Jeff, we concluded that we
     * really are handling the prefix dir option incorrectly. It currently
     * is associated with an app_context, yet it really refers to the
     * location where OpenRTE/Open MPI is installed on a NODE. Fixing
     * this right now would involve significant change to orterun as well
     * as elsewhere, so we will intentionally leave this incorrect at this
     * point. The error, however, is identical to that seen in all prior
     * releases of OpenRTE/Open MPI, so our behavior is no worse than before.
     *
     * A note to fix this, along with ideas on how to do so, has been filed
     * on the project's Trac system under "feature enhancement".
     *
     * For now, default to the prefix_dir provided in the first app_context.
     * Since there always MUST be at least one app_context, we are safe in
     * doing this.
     */
    prefix_dir = apps[0]->prefix_dir;
    
    /*
     * Build argv array
     */
    opal_argv_append(&argc, &argv, "<template>");

    /* add the daemon command (as specified by user) */
    local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_plm_process_component.orted);

    /*
     * Add the basic arguments to the orted command line, including
     * all debug options
     */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "env",
                                          &proc_vpid_index,
                                          false, NULL);
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:process: final template argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
    }
    
    /* Figure out the basenames for the libdir and bindir.  This
       requires some explanation:

       - Use OPAL_LIBDIR and OPAL_BINDIR instead of -D'ing some macros
       in this directory's Makefile.am because it makes all the
       dependencies work out correctly.  These are defined in
       opal/install_dirs.h.

       - After a discussion on the devel-core mailing list, the
       developers decided that we should use the local directory
       basenames as the basis for the prefix on the remote note.
       This does not handle a few notable cases (e.g., f the
       libdir/bindir is not simply a subdir under the prefix, if the
       libdir/bindir basename is not the same on the remote node as
       it is here in the local node, etc.), but we decided that
       --prefix was meant to handle "the common case".  If you need
       something more complex than this, a) edit your shell startup
       files to set PATH/LD_LIBRARY_PATH properly on the remove
       node, or b) use some new/to-be-defined options that
       explicitly allow setting the bindir/libdir on the remote
       node.  We decided to implement these options (e.g.,
       --remote-bindir and --remote-libdir) to orterun when it
       actually becomes a problem for someone (vs. a hypothetical
       situation).

       Hence, for now, we simply take the basename of this install's
       libdir and bindir and use it to append this install's prefix
       and use that on the remote node.
    */

    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /* set the job state to indicate we attempted to launch */
    job_state = ORTE_JOB_STATE_FAILED_TO_START;
    
    /*
     * Iterate through each of the nodes
     */
    
    for(nnode=0; nnode < map->num_nodes; nnode++) {
        pid_t pid = -1;
        char *exec_path = NULL;
        char **exec_argv;
        
        /* if this daemon already exists, don't launch it! */ 
        if (nodes[nnode]->daemon_launched) { 
            continue; 
        }

        /* if the node's daemon has not been defined, then we
         * have an error!
         */
        if (NULL == nodes[nnode]->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:process:launch daemon failed to be defined on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[nnode]->name));
            return ORTE_ERR_FATAL;
        }
        
        if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
            param = opal_argv_join(argv, ' ');
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:process: start daemon as:\n\t%s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == param) ? "NULL" : param));
            if (NULL != param) free(param);
        }
        {
            char** env;

            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:process: launching on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[nnode]->name));

            exec_argv = &argv[local_exec_index];
            /* If the user provide a prefix then first try to find the application there */
            if( NULL != prefix_dir ) {
                char* full_path[3];
                
                full_path[0] = opal_os_path( false, prefix_dir, NULL );
                full_path[1] = opal_os_path( false, prefix_dir, bin_base, NULL );
                full_path[2] = NULL;
                exec_path = opal_path_find(exec_argv[0], full_path, F_OK, NULL);
                free(full_path[0]); free(full_path[1]);
            }
            if( NULL == exec_path ) {
                /* find the application in the default PATH */
                exec_path = opal_path_findv(exec_argv[0], F_OK, environ, NULL);
                if( NULL == exec_path ) {
                    char* full_path[2];
                    
                    full_path[0] = opal_os_path( false, opal_install_dirs.bindir, NULL );
                    full_path[1] = NULL;
                    exec_path = opal_path_find(exec_argv[0], full_path, F_OK, NULL);
                    free(full_path[0]);
                    
                    if( NULL == exec_path ) {
                        rc = orte_plm_process_fill_exec_path (&exec_path);
                        if (ORTE_SUCCESS != rc) {
                            return rc;
                        }
                    }
                }
            }
            
            /* If we have a prefix, then modify the PATH and
               LD_LIBRARY_PATH environment variables.  We're
               already in the child process, so it's ok to modify
               environ. */
            if (NULL != prefix_dir) {
                char *oldenv, *newenv;
                
                /* Reset PATH */
                newenv = opal_dirname(exec_path);
                /*newenv = opal_os_path( false, prefix_dir, bin_base, NULL );*/
                oldenv = getenv("PATH");
                if (NULL != oldenv) {
                    char *temp;
                    asprintf(&temp, "%s;%s", newenv, oldenv );
                    free( newenv );
                    newenv = temp;
                }
                opal_setenv("PATH", newenv, true, &environ);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:process: reset PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
                free(newenv);
                
                /* Reset LD_LIBRARY_PATH */
                newenv = opal_os_path( false, prefix_dir, lib_base, NULL );
                oldenv = getenv("LD_LIBRARY_PATH");
                if (NULL != oldenv) {
                    char* temp;
                    asprintf(&temp, "%s;%s", newenv, oldenv);
                    free(newenv);
                    newenv = temp;
                }
                opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:process: reset LD_LIBRARY_PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
               free(newenv);
            }
            
            /* tell the daemon to setup its own process session/group */
            opal_argv_append(&argc, &argv, "--set-sid");
            exec_argv = &argv[local_exec_index];

#if 0
            /* Finally, chdir($HOME) because we're making the
               assumption that this is what will happen on
               remote nodes (via submit).  This allows a user
               to specify a path that is relative to $HOME for
               both the cwd and argv[0] and it will work on
               all nodes -- including the local host.
               Otherwise, it would work on remote nodes and
               not the local node.  If the user does not start
               in $HOME on the remote nodes... well... let's
               hope they start in $HOME.  :-) */
            var = opal_home_directory();
            if (NULL != var) {
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:process: changing to directory %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     var));
                /* Ignore errors -- what are we going to do?
                   (and we ignore errors on the remote nodes
                   in the fork plm, so this is consistent) */
                chdir(var);
            }
#endif                
        
            /* pass the vpid */
            rc = orte_util_convert_vpid_to_string(&vpid_string, nodes[nnode]->daemon->name.vpid);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "plm:process: unable to get daemon vpid as string");
                goto cleanup;
            }
            free(argv[proc_vpid_index]);
            argv[proc_vpid_index] = strdup(vpid_string);
            free(vpid_string);
            
            /* Set signal handlers back to the default.  Do this close
               to the execve() because the event library may (and likely
               will) reset them.  If we don't do this, the event
               library may have left some set that, at least on some
               OS's, don't get reset via fork() or exec().  Hence, the
               orted could be unkillable (for example). */
            
            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
            set_handler_default(SIGCHLD);
            
            /* setup environment */
            env = opal_argv_copy(orte_launch_environ);
            
            /* exec the daemon */
            if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
                param = opal_argv_join(exec_argv, ' ');
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:process: executing:\n\t%s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     (NULL == param) ? "NULL" : param));
                if (NULL != param) free(param);
            }
            
#ifdef _MSC_VER
            /* launch remote process */
            pid = wmi_launch_child(prefix_dir, nodes[nnode]->name, argc, exec_argv);
#else
            pid = _spawnve( _P_NOWAIT, exec_path, exec_argv, env);
#endif

            if (pid < 0) {
                failed_launch = true;
                rc = ORTE_ERROR;
                goto cleanup;
            }
            /* indicate this daemon has been launched in case anyone is sitting on that trigger */
            nodes[nnode]->daemon->state = ORTE_PROC_STATE_LAUNCHED;
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:process: daemon launched (pid %d on %s)\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 pid, nodes[nnode]->name));
        
            OPAL_THREAD_LOCK(&mca_plm_process_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_plm_process_component.num_children++ >=
                mca_plm_process_component.num_concurrent) {
                opal_condition_wait(&mca_plm_process_component.cond, &mca_plm_process_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_plm_process_component.lock);

            /* if required - add delay to avoid problems w/ X11 authentication */
            if (0 < opal_output_get_verbosity(orte_plm_globals.output)
                && mca_plm_process_component.delay) {
                sleep(mca_plm_process_component.delay);
            }
        }
    }

    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:process: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
launch_apps:
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:process: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
    /* get here if launch went okay */
    failed_launch = false;
    
 cleanup:
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    if (NULL != argv) {
        opal_argv_free(argv);
    }

    /* check for failed launch - if so, force terminate */
    if( failed_launch ) {
        orte_plm_base_launch_failed(jdata->jobid, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, job_state);
    }

    return rc;
}


/**
* Terminate the orteds for a given job
 */
static int orte_plm_process_terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_EXIT_CMD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

static int orte_plm_process_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_plm_process_finalize(void)
{
    int rc;

#ifdef _MSC_VER
    /* release the locator and service objects*/
    if(NULL!=pLoc) {
        pLoc->Release();
    }

    if(NULL!=pSvc_cimv2){
        pSvc_cimv2->Release();
    }
    
    if( NULL!=pSvc_registry ) {
        pSvc_registry->Release();
    }

    CoUninitialize();
#endif
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS

struct orte_plm_process_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_plm_process_stack_t orte_plm_process_stack_t;

static void orte_plm_process_stack_construct(orte_plm_process_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_plm_process_stack_destruct(orte_plm_process_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_plm_process_stack_t,
    opal_object_t,
    orte_plm_process_stack_construct,
    orte_plm_process_stack_destruct);

static void orte_plm_process_launch_cb(int fd, short event, void* args)
{
    orte_plm_process_stack_t *stack = (orte_plm_process_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_plm_process_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_plm_process_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_plm_process_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_plm_process_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_plm_process_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_plm_process_launch_cb, &stack);
        opal_evtimer_add(&event, &tv);

        OPAL_THREAD_LOCK(&stack.mutex);
        while (stack.complete == false) {
            opal_condition_wait(&stack.cond, &stack.mutex);
        }
        OPAL_THREAD_UNLOCK(&stack.mutex);
    }
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif


static void set_handler_default(int sig)
{
    OPAL_TRACE(1);
/*
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
*/
}
