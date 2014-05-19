# Configuration

I am using Ubunto 12.04

- Racket 6.0.1
- Apache 2.2.22
- MySQL 5

## MySQL

The default configuration expects MySQL to have a user `captain_teach` with the 
password `captain_teach` and that they have all priveleges on a database called
`captain_teach`. From the mysql prompt, you can do this by running:

    CREATE DATABASE IF NOT EXISTS captain_teach;
    CREATE USER 'captain_teach'@'localhost' IDENTIFIED BY 'captain_teach';
    GRANT ALL ON captain_teach.* TO 'captain_teach'@'localhost';

These values are specified in `server/database/mysql.rkt` and should be updated
to reflect the username, password, and databse you would like to use.

# Launching the Service

To run type `racket server/captain-teach.rkt`. This starts a service on port 8080.

In your apache configuration file, you will want to setup a proxy to forward to the service. For example:

    ProxyPass / http://localhost:8080/
    ProxyPassReverse / http://localhost:8080/
