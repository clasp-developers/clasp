#!/bin/sh
# The purpose of this shell script is to extract the ALPS resId for the
# current batch job (under which we are running).  Ideally, we would not
# relegate such a function to a script; rather, we would extract this
# value directly from the ALPS "reservations" file.  However, we do not
# know the file structure used for the "reservations" file and this
# structure is not locally available.  Then, in addition, we have the
# complication that as of the instantiation date of this script, the
# user command interface to ALPS is somewhat unreliable.  So, to keep
# the load module fairly simple, we use this script.

  APSTAT=/usr/bin/apstat
  GREP=/usr/bin/grep
  AWK=/usr/bin/awk
  SLEEP=/bin/sleep
  ECHO=/bin/echo

# If the old variable still is being set, use it.
  if [ "${BATCH_PARTITION_ID}" != "" ]
  then
	${ECHO} ${BATCH_PARTITION_ID}
	exit 0
  fi

# Extract the batch job ID directly from the environment, if available.
  jid=${BATCH_JOBID:--1}
  if [ $jid -eq -1 ]
  then

#	Otherwise, parse it from the PBS/Torque environmental variable.
	jid=${PBS_JOBID:--1}
	if [ $jid = "-1" ]
	then
		${ECHO} -1
		exit 0
	fi
	jid=`${ECHO} $jid | ${AWK} -F\. '{print $1}'`
  fi

# Next, let the ALPS user command interface read the "reservations"
# file; but let's not be too hasty about reporting failure.
  resId=""
  count=0
  while [ "$resId" = "" ]
  do

#	We're in a while loop, so skip the delay on the first trip.
	if [ $count -gt 0 ]
	then
		${SLEEP} 1
	fi

#	Try to get the ALPS resId.
	resId=`${APSTAT} -r | ${GREP} $jid | ${AWK} '{print $1}'`

#	Give up after 10 tries.
	count=`expr $count + 1 `
	if [ $count -ge 10 ]
	then
		break
	fi
  done

# If we still don't have it after 10 attempts, then, I reckon that it
# just wasn't meant to be.
  if [ "$resId" = "" ]
  then
	${ECHO} 2
	exit 0
  fi

  ${ECHO} $resId
  exit 0
