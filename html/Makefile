CC=tsc
src=ts
bin=bin

all: $(bin)/captain-teach.js $(bin)/review.js $(bin)/rubric.js $(bin)/review-disabled.js $(bin)/rubric-disabled.js $(bin)/author.js

$(bin)/author.js:$(src)/author.ts
	$(CC) --out $(bin)/author.js $(src)/author.ts

$(bin)/captain-teach.js:$(src)/CodeMirrorBuilder.ts
	$(CC) --out $(bin)/captain-teach.js $(src)/CodeMirrorBuilder.ts

$(bin)/review.js: $(src)/review.ts $(bin)/captain-teach.js
	$(CC) --out $(bin)/review.js $(src)/review.ts

$(bin)/rubric.js: $(src)/rubric.ts
	$(CC) --out $(bin)/rubric.js $(src)/rubric.ts

$(bin)/review-disabled.js: $(src)/review-disabled.ts $(bin)/captain-teach.js
	$(CC) --out $(bin)/review-disabled.js $(src)/review-disabled.ts

$(bin)/rubric-disabled.js: $(src)/rubric-disabled.ts
	$(CC) --out $(bin)/rubric-disabled.js $(src)/rubric-disabled.ts

clean:
	find . -name "*~" -type f -delete
	find . -name "[#]*" -type f -delete
	rm $(bin)/* -rf
