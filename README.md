# Launching Docker Instance

### Configuring OpenID Connect with Google

1. Create a project on the [Google Developer Console](https://console.developers.google.com/)

2. Once you are on the project page, in the side menu select APIs & auth > Credentials.
  * Click Create a New Client ID
  * Add https://localhost/redirect to the AUTHORIZED REDIRECT URI section
  * Click Create Client ID

3. Create a `config` file based on `config.sample`:

     $ cp config.sample config

4. Copy the Client ID, Client Secret, values from the
   Google Developers Console into the `config` file you created above.

   Edit other settings in `config`, such as the hostname. These are obvious.

5. In the Google Developers Console, you must have a `Redirect URIs` to
   `BaseUri`/ct/redirect. For example, if `BaseUri = https://localhost`
   I would add `https://localhost/ct/redirect`.
   


### Configuring Service

You will need to modify the `initialize` function in `server/config.rkt` to construct your initial class.

### Launch Docker

You should now be able to run the service in a docker container:

    make run

