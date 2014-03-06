# pig - dice game instructions + analysis + monte carlo simulation 

# HOMEPAGE

[Wikipedia: Pig](http://en.wikipedia.org/wiki/Pig_%28dice%29)

# CONTENTS

 * analysis.pdf - Statistical analysis
 * pig.hs - Monte Carlo simulation in Haskell

# EXAMPLE

    $ git clone https://github.com/mcandre/pig.git
    $ cd pig
    $ cabal install random-fu
    $ make
    ./pig
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

# REQUIREMENTS

* [Haskell](http://www.haskell.org/)
* [random-extras](http://hackage.haskell.org/package/random-extras)

## Optional

* [LaTeX](http://www.latex-project.org/)
* [Ruby](https://www.ruby-lang.org/) 1.9+
* [Guard](http://guardgem.org/) 1.8.2+
* [aspelllint](https://github.com/mcandre/aspelllint)

Install Guard and aspelllint:

    $ bundle

# DEVELOPMENT

## Lint

Keep the code tidy with HLint:

    $ cabal install hlint
    $ make lint

## Spell Check

    $ aspelllint
    ...

## Local CI

Start Guard in a shell, and it will automatically run unit tests when the source code changes:

    $ guard
    ...
