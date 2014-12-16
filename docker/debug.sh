#!/bin/bash
source /etc/apache2/envvars && apache2 -DFOREGROUND & cd /home/admiraledu/; racket server/captain-teach.rkt
