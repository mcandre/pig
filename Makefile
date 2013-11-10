all: test

test: pig
	./pig

pig: pig.hs
	ghc --make -o pig pig.hs -package random-extras

hlint:
	-hlint .

lacheck:
	-for f in *.tex; do lacheck $$f; done

style-check:
	-style-check.rb *.tex

lint: hlint lacheck style-check

clean:
	-rm *.exe
	-rm pig
	-rm pig.o
	-rm pig.hi
