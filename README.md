# pig - dice game instructions + analysis + monte carlo simulation

# EXAMPLE

```console
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
```

[Wikipedia: Pig](http://en.wikipedia.org/wiki/Pig_%28dice%29)

# RUNTIME REQUIREMENTS

(None)

# BUILDTIME REQUIREMENTS

* [GHC Haskell](http://www.haskell.org/) 8+

## Recommended

* [shake](https://shakebuild.com/) (e.g., `cabal install shake`)
* [hlint](https://hackage.haskell.org/package/hlint) (e.g., `cabal install happy; cabal install hlint`)
* [LaTeX](https://www.latex-project.org/)

# BUILD

```console
$ cabal install --only-dependencies --enable-documentation
$ cabal install --only-dependencies --enable-tests
$ shake
```

# LINT

```console
$ shake lint
```

# TEST

```console
$ shake test
```

# PUBLISH

```console
$ shake publish
```
