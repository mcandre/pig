all: test

test: pig
	./pig

pig: pig.hs
	ghc --make -o pig pig.hs -package random-extras

lint:
	hlint .

clean:
	-rm *.exe
	-rm pig
	-rm pig.o
	-rm pig.hi
