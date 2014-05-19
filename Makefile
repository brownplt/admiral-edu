all:
	docker build -t admiral-edu .

run: all
	docker run -i -t --rm -p 8080:8080 admiral-edu 
