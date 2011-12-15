all: pig

pig: pig.hs
	ghc --make -o pig pig.hs -package random-extras

clean:
	-rm pig.hi
	-rm pig