# Running Captain Teach Locally

## Google Authentication Setup
You will need to create OAuth 2.0 credentials allow for redirect to https://localhost/authentication/redirect.

## Configuration Files
Create a directory containing the following files:
  * apache-vars : Variables used by Apache
  * captain-teach.config : Variables used by captain-teach
  * cert.crt : SSL Certificate
  * cert.key : SSL Key
  * cert.pem : SSL Chain File

Sample files are available in the conf/ directory of this repository for configuring apache-vars and captain-teach.config
Snake Oil Certificates are also available in the conf/ directory

The BaseURL value in conf/apache-vars should be set to https://localhost

The ClientID and ClientSecret should be the values associated with the credentials created in the Google Authentication Setup

## Launch Script
```bash
# Install the mysql docker images
docker pull mysql

# Install the Captain Teach images
docker pull jcollard/captain-teach

# Start MySQL Container 
# Replace MYSQL_ROOT_PASSWORD
# Replace MYSQL_PASSWORD with the same value that is in captain-teach.config
docker run -e MYSQL_USER=captain_teach -e MYSQL_ROOT_PASSWORD=some_password -e MYSQL_PASSWORD=your_ct_password -e MYSQL_DATABASE=captain_teach --net=host --rm -d mysql


# Start Captain Teach
docker run --net=host -v /path/to/local/conf:/conf -p 443:443 -d jcollard/captain-teach
```

After running the commands above, you should now be able to access https://localhost/ and see the Captain Teach landing page.
To access your class visit https://localhost/class-name/.

*Note:* You may initially get a 503 Service Unavailable error if CT is still loading.

