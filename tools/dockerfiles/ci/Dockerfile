FROM jenkins:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

USER root

# install prerequisites for all further steps - hopefully
RUN apt-get update && apt-get -y install python-pip sudo

# AWS command-line utilities
RUN pip install awscli

# install docker client
RUN wget -qO- https://get.docker.com/gpg | apt-key add -
RUN wget -qO- https://get.docker.com/    | sh
RUN usermod -aG docker jenkins && usermod -aG users jenkins && usermod -aG staff jenkins

# install docker-compose
RUN curl -L https://github.com/docker/compose/releases/download/1.12.0/docker-compose-`uname -s`-`uname -m` \
  > /usr/local/bin/docker-compose
RUN chmod 0755 /usr/local/bin/docker-compose

# install Jenkins plugins
ADD plugin_list.txt /usr/share/jenkins/plugin_list.txt
ADD batch-install-jenkins-plugins.sh /usr/local/sbin/
RUN mkdir /usr/share/jenkins/ref/plugins /usr/share/jenkins/ref/ssh &&\
  /usr/local/sbin/batch-install-jenkins-plugins.sh \
  --plugins /usr/share/jenkins/plugin_list.txt \
  --plugindir /usr/share/jenkins/ref/plugins

# install Jenkins config and startup file
ADD jenkins-config.xml /usr/share/jenkins/ref/config.xml
ADD jobs /usr/share/jenkins/ref/jobs
ADD jenkins-docker-start.sh /usr/local/bin/
RUN chmod 0755 /usr/local/bin/jenkins-docker-start.sh

ENV HOME /var/jenkins_home
ENV PATH $PATH:/var/jenkins_home/.rbenv/bin:$HOME/bin
WORKDIR $HOME
ENTRYPOINT [ "/usr/local/bin/jenkins-docker-start.sh" ]
