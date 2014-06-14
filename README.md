# Launching Docker Instance

### Configuring OpenID Connect with Google

1. Create a project on the [Google Developer Console](https://console.developers.google.com/)

2. Once you are on the project page, in the side menu select APIs & auth > Credentials.
  * Click Create a New Client ID
  * Add https://localhost/redirect to the AUTHORIZED REDIRECT URI section
  * Click Create Client ID

3. Create a `config` file based on `config.sample`:

     $ cp config.sample config

4. Copy the Client ID, Client Secret, and RedirectUri values from the
   Google Developers Console into the `config` file you created above.

   Edit other settings in `config`, such as the hostname. These are obvious.


### Configuring Service

You will need to modify the `initialize` function in `server/config.rkt` to construct your initial class.

### Launch Docker

You should now be able to run the service in a docker container:

    make run

