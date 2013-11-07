all: test

test: pig
	./pig

pig: pig.hs
	ghc --make -o pig pig.hs -package random-extras

lint:
	-hlint .
	-for f in *.tex; do lacheck $$f; done	

clean:
	-rm *.exe
	-rm pig
	-rm pig.o
	-rm pig.hi
