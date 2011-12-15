pig - dice game instructions + analysis + monte carlo simulation 

HOMEPAGE

[Wikipedia: Pig](http://en.wikipedia.org/wiki/Pig_%28dice%29)

REQUIREMENTS

 * [Haskell Platform](http://hackage.haskell.org/platform/)
 * [random-extras](http://hackage.haskell.org/packages/archive/random-extras/latest/doc/html/Data-Random-Extras.html)

CONTENTS

 * pig.txt - Instructions for playing the game
 * analysis.txt - Probabilistic game deconstruction
 * pig.hs - Monte Carlo simulation in Haskell

EXAMPLE

	$ make
	$ ./pig
	Running 10000 games...
	Totaling wins...

	Winners:

	Roll Six       32%
	Roll Five      28%
	Roll K Times   27%
	Roll Until 100 8%
	Roll Bad K     3%
	Never Roll     0%
	Always Roll    0%
	Roll Once      0%