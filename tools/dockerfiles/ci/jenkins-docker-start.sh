#!/usr/bin/env bash
# ensures that Jenkins has access to /var/run/docker.sock
# Then starts the Jenkins docker image as usual

echo "Checking /var/run/docker.sock..."
SOCKET_GID=$(stat -c '%g' /var/run/docker.sock)
echo "Writing GID $SOCKET_GID into /etc/group for group docker..."
sed -rie "s/docker:x:999:jenkins/docker:x:${SOCKET_GID}:jenkins/" /etc/group
grep docker /etc/group
echo "Starting Jenkins with /usr/local/bin/jenkins.sh..."
exec sudo -Eu jenkins /bin/tini -- /usr/local/bin/jenkins.sh
