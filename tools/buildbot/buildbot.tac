
import os
import boto3

import requests

from buildbot_worker.bot import Worker
from twisted.application import service

def get_instance_name(fid):
    # When given an instance ID as str e.g. 'i-1234567', return the instance 'Name' from the name tag.
    ec2 = boto3.resource('ec2',region_name="us-east-2")
    ec2instance = ec2.Instance(fid)
    instancename = ''
    for tags in ec2instance.tags:
        if tags["Key"] == 'Name':
            instancename = tags["Value"]
    return instancename


basedir = '/home/admin/worker'
rotateLength = 10000000
maxRotatedFiles = 10

# if this is a relocatable tac file, get the directory containing the TAC
if basedir == '.':
    import os.path
    basedir = os.path.abspath(os.path.dirname(__file__))

# note: this line is matched against to check that this is a worker
# directory; do not edit it.
application = service.Application('buildbot-worker')

from twisted.python import log
from twisted.python.logfile import LogFile
from twisted.python.log import ILogObserver, FileLogObserver
logfile = LogFile.fromFullPath(
    os.path.join(basedir, "twistd.log"), rotateLength=rotateLength,
    maxRotatedFiles=maxRotatedFiles)
application.setComponent(ILogObserver, FileLogObserver(logfile).emit)

buildmaster_host = 'clasp-buildbot.ddns.net'
port = 9989
#
# The following will make this worker get its name from the EC2 Name tag
#
req = requests.get("http://169.254.169.254/latest/meta-data/instance-id")
ec2_instance_id = req.text
log.msg("buildbot-worker instance-id: %s" % ec2_instance_id)
workername = get_instance_name(ec2_instance_id)
log.msg("buildbot-worker name: %s" % workername)
passwd = 'claspsekrit'
keepalive = 600
umask = 0o022
maxdelay = 300
numcpus = None
allow_shutdown = True
maxretries = 10

s = Worker(buildmaster_host, port, workername, passwd, basedir,
           keepalive, umask=umask, maxdelay=maxdelay,
           numcpus=numcpus, allow_shutdown=allow_shutdown,
           maxRetries=maxretries)
s.setServiceParent(application)

