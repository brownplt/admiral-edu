include config

ENV = -e ClientID=$(ClientID) \
	-e ClientSecret=$(ClientSecret) \
	-e AdminEmail=$(AdminEmail) \
	-e RedirectUri=$(RedirectUri) \
	-e BaseUrl=$(BaseUrl) \
	-e CryptoPassphrase=$(CryptoPassphrase) \
	-e AdminEmail=$(AdminEmail)

all:
	docker build -t admiral-edu .

run: all
	docker run -i -t --rm -p 443:443 $(ENV) admiral-edu

bash: all
	docker run -i -t --rm -p 443:443 $(ENV) admiral-edu /bin/bash

debug: all
	docker run -i -t --rm -p 443:443 $(ENV) admiral-edu ./debug.sh
