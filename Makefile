
all: src test
	echo "Finished!"

.PHONY: src
src: 
	(cd src; erlc -o ../ebin *.erl)

.PHONY: test
test: 
	(cd test; erlc -o ../ebin *.erl)

clean:
	rm -rf ./ebin/*.beam
