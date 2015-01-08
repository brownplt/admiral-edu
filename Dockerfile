FROM ubuntu:14.04
MAINTAINER Joseph Collard <josephmcollard@gmail.com>

WORKDIR /root
USER root

RUN apt-get update

RUN apt-get install -y wget


ENV DEBIAN_FRONTEND noninteractive

#
# Install Apache
#

RUN apt-get install -y apache2

#
# Install mod_auth_openidc
#

# Dependencies
RUN apt-get install -y libcurl3 libjansson4

#RUN wget https://github.com/pingidentity/mod_auth_openidc/releases/download/v1.3/libapache2-mod-auth-openidc_1.3_amd64.deb
RUN wget https://github.com/pingidentity/mod_auth_openidc/releases/download/v1.5/libapache2-mod-auth-openidc_1.5_amd64.deb
#RUN wget https://github.com/pingidentity/mod_auth_openidc/releases/download/v1.5.3/libapache2-mod-auth-openidc_1.5.3-2_amd64.deb
RUN dpkg -i libapache2-mod-auth-openidc_1.5_amd64.deb

#
# Configure Apache
#

RUN a2enmod auth_openidc
RUN a2enmod proxy
RUN a2enmod proxy_http
RUN a2enmod ssl

#
# Install Racket
#

RUN wget http://mirror.racket-lang.org/installers/6.1/racket-6.1-x86_64-linux-ubuntu-precise.sh

RUN chmod a+x racket-6.1-x86_64-linux-ubuntu-precise.sh

RUN ./racket-6.1-x86_64-linux-ubuntu-precise.sh

RUN ln -s /usr/racket/bin/racket /usr/local/bin/racket
RUN ln -s /usr/racket/bin/raco /usr/local/bin/raco


# Setup Captain Teach Server

# Create User
RUN adduser --disabled-password --gecos "" admiraledu

# Install Captain Teach Dependencies
RUN raco pkg install --auto aws
RUN raco pkg install --auto yaml

#
# Install supervisord
#
RUN apt-get install -y supervisor

#
# Install zip / unzip utilities
#
RUN apt-get install -y zip unzip

#######################################################################
# Add captain-teach apache configuration file
# This file specifies how the user is authenticated
# Note: You need to modify this file
#######################################################################
ADD docker/captain-teach.conf /etc/apache2/conf-available/captain-teach.conf

RUN a2enconf captain-teach

RUN mkdir -p /home/admiraledu/files
RUN chown admiraledu /home/admiraledu/files
RUN chgrp admiraledu /home/admiraledu/files

#
# Copy Captain Teach Scripts, Images, CSS
#
ADD html/bin /var/www/html/bin
ADD html/css /var/www/html/css
ADD html/imgs /var/www/html/imgs
ADD code-mirror/mode /var/www/html/mode
ADD code-mirror/lib /var/www/html/lib
ADD html/logout.html /var/www/html/logout.html

#
# Configure Supervisor
#
ADD docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf

#
# Apache fails to start on install since it has unbound variables. That puts
# it into an inconsistent state. The line below cleans up.
#
RUN service apache2 start; service apache2 stop

#
# Add Debug Script
#
ADD docker/debug.sh /root/debug.sh
RUN chmod +x /root/debug.sh

#
# Add some default rubrics for testing purposes
#
ADD rubrics/implementation-rubric.json /home/admiraledu/reviews/cmpsci220/clock/implementation/rubric.json
ADD rubrics/tests-rubric.json /home/admiraledu/reviews/cmpsci220/clock/tests/rubric.json

#
# Copy AdmiralEdu to container
#
ADD server /home/admiraledu/server

ADD html/index.html /var/www/html/index.html

#
# Run AdmiralEdu
#

CMD supervisord
