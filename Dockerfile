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
# Install Apache
# 

RUN apt-get install -y apache2

#
# Install mod_auth_openidc
#

# Dependencies
RUN apt-get install -y libcurl3

RUN wget https://github.com/pingidentity/mod_auth_openidc/releases/download/v1.3/libapache2-mod-auth-openidc_1.3_amd64.deb 
RUN dpkg -i libapache2-mod-auth-openidc_1.3_amd64.deb

#
# Configure Apache
#

RUN a2enmod auth_openidc
RUN a2enmod proxy
RUN a2enmod proxy_http
RUN a2enmod ssl
RUN a2ensite default-ssl

# Note: You need to modify this file
ADD docker/captain-teach.conf /etc/apache2/conf-available/captain-teach.conf

RUN a2enconf captain-teach

#
# Copy AdmiralEdu to container
#


ADD server /home/admiraledu/server

#
# Run AdmiralEdu
#

#WORKDIR /home/admiraledu
CMD service apache2 start; service mysql start; su admiraledu -c 'racket ~/server/captain-teach.rkt'
