# Configuration

I am using Ubuntu 14.04 x86_64

- Racket 6.0.1
- Apache 2.4.7
  * [mod_auth_openidc v1.3](https://github.com/pingidentity/mod_auth_openidc/releases)
- MySQL 5.5.37

## Racket 6.0.1
On Ubuntu 14.04 x86_64 this can be installed by running:

    wget http://mirror.racket-lang.org/installers/6.0.1/racket-6.0.1-x86_64-linux-ubuntu-precise.sh
    ./racket-6.0.1-x86_64-linux-ubuntu-precise.sh

## Apache 2.4.7
On Ubuntu 14.04 x86_64 this can be installed by running:

    apt-get update
    apt-get install apache2
    
### mod_auth_openidc v1.3
This module allows the Apache Web Server to authenticate using [OpenID Connect](http://openid.net/connect/).

On Ubuntu 14.04 x86_64 mod_auth_openidc v1.3 can be installed by running:

    wget https://github.com/pingidentity/mod_auth_openidc/releases/download/v1.3/libapache2-mod-auth-openidc_1.3_amd64.deb 
    dpkg -i libapache2-mod-auth-openidc_1.3_amd64.deb

Once this is installed, you will need to enable it:

    a2enmod auth_openidc

Now that it is enabled, you can configure how openidc will work with your service. For a protected service that is
authenticated using a [Google Account](https://developers.google.com/accounts/cookbook/technologies/OpenID-Connect)
you might have something like the following:

    OIDCRedirectURI https://www.example-site.com/service/redirect
    OIDCCryptoPassphrase aBetterPassword
    OIDCScope "email"
    OIDCClientID FoundOnDeveloperConsole
    OIDCClientSecret FoundOnGoogleDeveloperConsole
    OIDCProviderIssuer accounts.google.com
    OIDCProviderAuthorizationEndpoint https://accounts.google.com/o/oauth2/auth?approval_prompt=force&[hd=https://www.example-site.com/service]
    OIDCProviderJwksUri https://www.googleapis.com/oauth2/v2/certs
    OIDCProviderTokenEndpoint https://accounts.google.com/o/oauth2/token
    OIDCProviderTokenEndpointAuth client_secret_post
    OIDCProviderUserInfoEndpoint https://www.googleapis.com/plus/v1/people/me/openIdConnect
    OIDCSessionInactivityTimeout 3600

    <Location /service/ >
      Authtype openid-connect
      require valid-user
    </Location>

### Proxy to Captain Teach Service
Since we are leveraging Apache to do authentication for us, we will want to use use a proxy to forward to the captain teach service. To
do this, you will first need to enable the proxy module.

    a2enmod proxy
    a2enmod proxy_http

Once this is enabled, you will want to add the following to your Apache configuration file:

    # The default setting is to run captain-teach on port 8080
    ProxyPass / http://localhost:8080/
    ProxyPassReverse / http://localhost:8080/

You will now need to restart apache.

    service apache2 restart


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
