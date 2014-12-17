
ConfPath = $(shell pwd)/conf

TAG = captain-teach:latest

RUN = docker run --name captain_teach -i -t -v $(ConfPath):/conf --net=host --rm $(TAG)

all:
	docker build -t $(TAG) .

run: all
	$(RUN)

bash: all
	$(RUN) /bin/bash

debug: all
	$(RUN) ./debug.sh
