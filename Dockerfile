FROM ubuntu:14.04
MAINTAINER Arjun Guha <arjun@cs.umass.edu>

WORKDIR /root
USER root

RUN apt-get install -y wget

#
# Install Racket
#

RUN wget http://mirror.racket-lang.org/installers/6.0.1/racket-6.0.1-x86_64-linux-ubuntu-precise.sh

RUN chmod a+x racket-6.0.1-x86_64-linux-ubuntu-precise.sh

# Seems to skip the interactive prompts while building.
RUN ./racket-6.0.1-x86_64-linux-ubuntu-precise.sh

RUN ln -s /usr/racket/bin/racket /usr/local/bin/racket

RUN adduser --disabled-password --gecos "" admiraledu

#
# Install MySQL
#

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get install -y mysql-server

#
# Configure MySQL for AdmiralEdu
#

RUN service mysql start && \
  mysql -e "CREATE USER 'captain_teach'@'localhost' IDENTIFIED BY 'captain_teach'" && \
  mysql -e "CREATE DATABASE captain_teach;" && \
  mysql -e 'GRANT ALL PRIVILEGES ON captain_teach.* to captain_teach@localhost;'

#
# Copy AdmiralEdu to container
#

ADD server /home/admiraledu/server

#
# Run AdmiralEdu
#

WORKDIR /home/admiraledu
EXPOSE 8080
CMD service mysql start; su admiraledu -c 'racket server/captain-teach.rkt'


