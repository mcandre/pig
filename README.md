# pig - dice game instructions + analysis + monte carlo simulation 

## HOMEPAGE

[Wikipedia: Pig](http://en.wikipedia.org/wiki/Pig_%28dice%29)

## REQUIREMENTS

 * [Haskell Platform](http://hackage.haskell.org/platform/)
 * [random-extras](http://hackage.haskell.org/packages/archive/random-extras/latest/doc/html/Data-Random-Extras.html)

## CONTENTS

 * analysis.pdf - Statistical analysis
 * pig.hs - Monte Carlo simulation in Haskell

## EXAMPLE

    $ make
    $ ./pig
    Running 10000 games...
    Totaling wins...

    Roll Six       32%
    Roll Five      28%
    Roll K Times   27%
    100 or Bust    8%
    Roll Bad K     3%
    Always Hold    0%
    Always Roll    0%
    Roll Once      0%
