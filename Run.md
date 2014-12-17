# Running Captain Teach

## Assumptions

Throughout these instructions, it will be assumed that you are setting up an instance of Captain Teach to run at `https://www.yoursite.com` and that your class is called `test-class`. You should replace each of these through out with your domain and class name. For example, if you want to run locally, you could use `https://localhost` and `local-class`.

## Google Authentication Setup
Captain Teach utilizes OAuth 2.0 via Google for authenticating users. This section describes how to set up a new project and generate the necessary credentials.

  1. Access Google's Developers Console: https://console.developers.google.com
  2. Click the `Create Project` button
    * Enter a `PROJECT NAME`, `PROJECT ID`, and click `Create`
  3. On the side bar select `API & auth` and then `Credentials`
  4. Under the `OAuth` section, click the `Create new Client ID`
    * Select `Web Application` and click `Configure consent screen`
    * Enter an `EMAIL ADDRESS`, `PRODUCT NAME`, and click `Save`
   * Remove anything in the `AUTHORIZED JAVASCRIPT ORIGINS` box
   * In the `AUTHORIZED REDIRECT URIS` box enter `https://www.yoursite.com/authentication/redirect`
   * Click `Create Client ID`

After completing the steps above, the `Credentials` page from step 3 should now contain a `Client ID for web application` area with the fields `CLIENT ID` and `CLIENT SECRET`. You will need these for your Captain Teach configuration.

## Configuration Files
Create a directory containing the following files:
  * apache-vars : Variables used by Apache
  * captain-teach.config : Variables used by captain-teach
  * cert.crt : SSL Certificate
  * cert.key : SSL Key
  * cert.pem : SSL Chain File

#### Sample apache-vars file
```
# Get these from the Google Developers Console
Define ClientID long-value.apps.googleusercontent.com
Define ClientSecret short-value

# This the URL where the site is hosted (no trailing slash)
Define BaseUrl https://www.yoursite.com

# The class name. This will cause your class to be at https://www.yoursite.com/test-class/
# This should match class-name in captain-teach.config
Define ClassName test-class

# Used internally by authentication module
Define CryptoPassphrase aBetterPassword

# The email you would like to appear on error screens
Define AdminEmail yourname@domain.com
```

#### Sample captain-teach.config file
```
# The address to the MySQL database you are using
db-address=localhost
db-name=captain_teach
db-password=captain_teach
db-user-name=captain_teach

# The first time Captain Teach loads, it will add this user as an instructor
master-user=youremail@domain.com

# These are used to generate complete links when sending email to users
server-name=yoursite.com
sub-domain=www.

# Mail Server information for sending email
mail-server=smtp.sendgrid.net
mail-port=2525
mail-username=username
mail-password=password

# The class-name, this should match ClassName in apache-vars
class-name=test-class

# Select the port that apache will redirect incomming traffic
# This allows multiple instances of CT to run on the same machine
# You probably don't need to change this
ct-port=8080

# Select the Storage Mode for Captain Teach
# Either `cloud-storage` or `local`
storage-mode=cloud-storage

# If using cloud-storage:
# The name of the bucket to use for file storage
bucket=some-bucket-name/

# If using cloud-storage:
# The Cloud Storage REST API host
cloud-host=storage.googleapis.com

# If using cloud-storage:
# Authentication information for cloud storage
cloud-access-key-id=YOUR-ACCESS-ID
cloud-secret-key=YOUR-SECRET-KEY
```

Sample files are available in the conf/ directory of this repository for configuring apache-vars and captain-teach.config

Snake Oil Certificates are also available in the conf/ directory

## Launching Captain Teach with MySQL

The following script will install a mysql docker image and the captain-teach docker image and launch both.

If you are using an external MySQL database, there is no need to run the MySQL portion here.

```bash

# Install the MySQL Image
docker pull mysql

# Start MySQL Container 
# Replace MYSQL_PASSWORD, MYSQL_USER, and MYSQL_DATABASE
# with the same values that you chose in captain-teach.config
# Replace MYSQL_ROOT_PASSWORD to anything
docker run -e MYSQL_USER=captain_teach \
           -e MYSQL_DATABASE=captain_teach \
           -e MYSQL_PASSWORD=captain_teach \
           -e MYSQL_ROOT_PASSWORD=some_password \
           --net=host \
           -d mysql

# Install the Captain Teach images
docker pull jcollard/captain-teach

# Start Captain Teach
# Replace /path/to/local/conf with the full path to your /conf directory
docker run -v /path/to/local/conf:/conf \
           --net=host \
           -d jcollard/captain-teach
```

After running the commands above, you should now be able to access your site and see the Captain Teach landing page.
To access your class visit https://www.yoursite.com/test-class/.

**Note**: You may initially get a 503 Service Unavailable error if CT is still loading.
