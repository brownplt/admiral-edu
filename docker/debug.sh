#!/bin/bash
source /etc/apache2/envvars && apache2 -DFOREGROUND & service mysql start ; cd /home/admiraledu/ ; racket server/captain-teach.rkt
