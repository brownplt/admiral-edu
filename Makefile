all:
	docker build -t admiral-edu .

run: all
	docker run -i -t --rm -p 443:443 admiral-edu 

bash: all
	docker run -i -t --rm -p 443:443 admiral-edu /bin/bash
