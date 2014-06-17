CC=tsc
src=ts
bin=bin

all: $(bin)/captain-teach.js $(bin)/review.js $(bin)/rubric.js

$(bin)/captain-teach.js:$(src)/CodeMirrorBuilder.ts
	$(CC) --out $(bin)/captain-teach.js $(src)/CodeMirrorBuilder.ts

$(bin)/review.js: $(src)/review.ts $(bin)/captain-teach.js
	$(CC) --out $(bin)/review.js $(src)/review.ts

$(bin)/rubric.js: $(src)/rubric.ts
	$(CC) --out $(bin)/rubric.js $(src)/rubric.ts

clean:
	find . -name "*~" -type f -delete
	find . -name "[#]*" -type f -delete
	rm $(bin)/* -rf
