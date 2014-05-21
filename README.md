# Launching Docker Instance

### Configuring OpenID Connect with Google

1. Create a project on the [Google Developer Console](https://console.developers.google.com/)

2. Once you are on the project page, in the side menu select APIs & auth > Credentials.
  * Click Create a New Client ID 
  * Add https://localhost/redirect to the AUTHORIZED REDIRECT URI section
  * Click Create Client ID

3. Copy the Client ID field into docker/captain-teach.conf to the OIDCClientID field.

4. Copy the Client secret field into docker/captain-teach.conf to the OIDCClientSecret field.

### Launch Docker

You should now be able to run the service in a docker container:

    make run

