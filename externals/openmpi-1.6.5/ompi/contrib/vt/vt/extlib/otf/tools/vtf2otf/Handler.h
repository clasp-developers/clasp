/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef VTF3TOOTF_HANDLER_H
#define VTF3TOOTF_HANDLER_H

int handleClstrregval( void *fcbout, double time, int clstrtoken,
	int clstrregarraydim, const int *clstrregtokenarray,
	const int *clstrregvaluetypearray,
	const void *clstrregvaluearray );

int handleComment( void *fcbout, double time, const char *comment );

int handleCpuregval( void *fcbout, double time, unsigned int cpuid,
	int cpuregarraydim, const int *cpuregtokenarray,
	const int *cpuregvaluetypearray,
	const void *cpuregvaluearray );

int handleDefact( void *fcbout, int activitytoken,
	const char *activityname );

int handleDefact_obsol( void *fcbout, int activitytoken,
	const char *activityname );

int handleDefclkperiod( void *fcbout, double clkperiod );

int handleDefclstr( void *fcbout, int clstrtoken, const char *clstrname,
	int cpuidarraydim, const unsigned int *cpuidarray );

int handleDefclstrreg( void *fcbout, int clstrregtoken,
	int clstrregclasstoken, int valuetype,
	const void *valuebounds, const char *clstrregname,
	const char *clstrregunit );

int handleDefclstrregclass( void *fcbout, int clstrregclasstoken,
	const char *clstrregclassname );

int handleDefcommunicator( void *fcbout, int communicator,
	int communicatorsize, int tripletarraydim,
	const unsigned int *tripletarray );

int handleDefcpugrp( void *fcbout, unsigned int cpugrpid,
	int cpuorcpugrpidarraydim,
	const unsigned int *cpuorcpugrpidarray,
	const char *cpugrpname );

int handleDefcpuname( void *fcbout, unsigned int cpuid,
	const char *cpuname );

int handleDefcpureg( void *fcbout, int cpuregtoken, int cpuregclasstoken,
	int valuetype, const void *valuebounds,
	const char *cpuregname, const char *cpuregunit );

int handleDefcpuregclass( void *fcbout, int cpuregclasstoken,
	const char *cpuregclassname );

/*
int handleDefcreator( void *fcbout, const char *creator );
*/

int handleDefglobalop( void *fcbout, int globaloptoken,
	const char *globalopname );

int handleDefiofile( void *fcbout, int iofiletoken, int communicator,
	const char *iofilename );

int handleDefkparreg( void *fcbout, int kparregtoken,
	const char *kparregname );

int handleDefmsgname( void *fcbout, int msgtype, int communicator,
	const char *msgname );

int handleDefopenmpname( void *fcbout, unsigned int nametoken,
	const char *openmpname );

int handleDefopenmptype( void *fcbout, unsigned int constructtypetoken,
	const char *constructtypename );

int handleDefpattern( void *fcbout, int activitytoken, int pattoken,
	int patshptoken, double radius, int ratio,
	int timesteparraydim, const double *timesteparray );

int handleDefpatternshape( void *fcbout, int activitytoken, int patshptoken,
	int patterntype, int patshptokenbref1, int patshptokenbref2 );

int handleDefredfunc_obsol( void *fcbout, int redfunctoken,
	const char *redfuncname );

int handleDefsamp( void *fcbout, int sampletoken, int sampleclasstoken,
	int iscpugrpsamp, unsigned int cpuorcpugrpid, int valuetype,
	const void *valuebounds, int dodifferentiation,
	int datarephint, const char *samplename,
	const char *sampleunit );

int handleDefsampclass( void *fcbout, int sampleclasstoken,
	const char *sampleclassname );

int handleDefscl( void *fcbout, int scltoken, int sclarraydim,
	const int *sclfiletokenarray,
	const int *scllinepositionarray );
 
int handleDefsclfile( void *fcbout, int sclfiletoken,
	const char *sclfilename );
 
int handleDefstate( void *fcbout, int activitytoken, int statetoken,
	const char *statename, int scltoken );
 
int handleDefstate_obsol( void *fcbout, int activitytoken,
	const char *activityname, unsigned int activityvalidity,
	int statetoken, const char *statename );
 
int handleDefsyscpunames( void *fcbout, int systemcpunamearraydim,
	char * const *systemcpunamearray );
 
int handleDefsyscpunums( void *fcbout, int systemcpunumberarraydim,
	const int *systemcpunumberarray );
 
int handleDefthreadnums( void *fcbout, int threadnumarraydim,
	const int *threadnumarray );
 
int handleDeftimeoffset( void *fcbout, double timeoffset );
 
int handleDefunmerged( void *fcbout );

/* doesn´t exist in otf
int handleDefversion( void *fcbout, int versionnumber );
*/
int handleDownto( void *fcbout, double time, int statetoken,
	unsigned int cpuid, int scltoken );
 
int handleExchange( void *fcbout, double time, unsigned int cpuid,
	int exchangetype, int statetoken, int job, int scltoken );
 
int handleExchange_obsol( void *fcbout, double time, unsigned int cpuid,
	int exchangetype, int statetoken, int activitytoken,
	const char *activityname, unsigned int activityvalidity,
	int job );
 
int handleFileiobegin( void *fcbout, double time, unsigned int cpuid,
	int fileiotype, int iofiletoken, int bytescopied,
	int scltoken );
 
int handleFileioend( void *fcbout, double time, unsigned int cpuid,
	int fileiotype, int iofiletoken, int bytescopied,
	int scltoken );
 
int handleGlobalop( void *fcbout, double time, int globaloptoken,
	unsigned int cpuid, int communicator,
	unsigned int rootcpuid, int bytessent, int bytesreceived,
	double durationtimesteps, int scltoken );
 
int handleKparregbarsum( void *fcbout, double time, unsigned int cpuid,
	int kparregtoken, int seqn, int opasize,
	const void *opastream, int scltoken );
 
int handleKparregbegin( void *fcbout, double time, unsigned int cpuid,
	int kparregtoken, int seqn, int numthreads, int scltoken );
 
int handleKparregend( void *fcbout, double time, unsigned int cpuid,
	int kparregtoken, int seqn, int opasize,
	const void *opastream, int scltoken );
 
int handleMutexacquire( void *fcbout, double time, unsigned int cpuid,
	int enterstatetoken, int leavestatetoken, 
	int leavestatetokenisupfrom, double durationtimesteps,
	int mutexsize, const void *mutex, int scltoken );
 
int handleMutexrelease( void *fcbout, double time, unsigned int cpuid,
	int enterstatetoken, int leavestatetoken,
	int leavestatetokenisupfrom, double durationtimesteps, 
	int mutexsize, const void *mutex, int scltoken );

int handleOpenmpenter( void *fcbout, double time, unsigned int cpuid,
	unsigned int constructtypetoken, unsigned int nametoken, int scltoken );

int handleOpenmpleave( void *fcbout, double time, unsigned int cpuid,
	int scltoken );

int handlePattern( void *fcbout, double time, unsigned int cpuid,
	int patorpatshptoken, double durationtimesteps,
	int timesteparraydim, const double *timesteparray,
	const int *patchindexarray );
 
int handleRecvmsg( void *fcbout, double time, unsigned int receiver,
	unsigned int sender, int communicator, int msgtype,
	int msglength, int scltoken );
 
int handleSamp( void *fcbout, double time, unsigned int cpuorcpugrpid,
	int samplearraydim, const int *sampletokenarray,
	const int *samplevaluetypearray,
	const void *samplevaluearray );
 
int handleSendmsg( void *fcbout, double time, unsigned int sender,
	unsigned int receiver, int communicator, int msgtype,
	int msglength, int scltoken );
 
int handleSrcinfo_obsol( void *fcbout, double time, int activitytoken,
	int statetoken, int scllineposition );
 
int handleUnrecognizable( void *fcbout, double lastvalidtime,
	int numberofunrecognizablechars,
	int typeofunrecognizablerecord,
	const char *unrecognizablerecord );
 
int handleUpfrom( void *fcbout, double time, int statetoken,
	unsigned int cpuid, int scltoken );

int handleUpto( void *fcbout, double time, int statetoken,
	unsigned int cpuid, int scltoken ) ;


#endif /* VTF3TOOTF_HANDLER_H */
