all: test

BIN=bin/pig

FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp -o $(BIN) -main-is Pig

test: $(BIN)
	$(BIN)

$(BIN): Pig.hs
	mkdir -p bin/
	ghc $(FLAGS) Pig.hs -package random-extras

hlint:
	-hlint .

lacheck:
	-for f in *.tex; do lacheck $$f; done

style-check:
	-style-check.rb *.tex

lint: hlint lacheck style-check

clean:
	-rm -rf bin/
	-rm *.o
	-rm *.hi
