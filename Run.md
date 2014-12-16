# Running Captain Teach

## Google Authentication Setup
You will need to create OAuth 2.0 credentials allow for redirect to https://www.yoursite.com/authentication/redirect.

## Configuration Files
Create a directory containing the following files:
  * apache-vars : Variables used by Apache
  * captain-teach.config : Variables used by captain-teach
  * cert.crt : SSL Certificate
  * cert.key : SSL Key
  * cert.pem : SSL Chain File

Sample files are available in the conf/ directory of this repository for configuring apache-vars and captain-teach.config
Snake Oil Certificates are also available in the conf/ directory

The BaseURL value in conf/apache-vars should match the url your console. 

For example: https://www.yoursite.com

**Note**: There is no trailing slash

The ClientID and ClientSecret should be the values associated with the credentials created in the Google Authentication Setup

# Install the mysql docker images
docker pull mysql


## Launching Captain Teach with MySQL

The following script will install a mysql docker image and the captain-teach docker image and launch both.

If you are using an external MySQL database, there is no need to run the MySQL portion here.

```bash

# Install the MySQL Image
docker pull mysql

# Start MySQL Container 
# Replace MYSQL_ROOT_PASSWORD
# Replace MYSQL_PASSWORD with the same value that is in captain-teach.config
docker run -e MYSQL_USER=captain_teach \
           -e MYSQL_DATABASE=captain_teach \
           -e MYSQL_ROOT_PASSWORD=some_password \
           -e MYSQL_PASSWORD=some_password \
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
To access your class visit https://www.yoursite.com/class-name/.

**Note**: You may initially get a 503 Service Unavailable error if CT is still loading.
