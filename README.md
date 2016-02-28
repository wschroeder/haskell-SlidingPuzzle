# Sliding Puzzle

This is a console version of the [Sliding Puzzle game](https://en.wikipedia.org/wiki/Sliding_puzzle).
Choose any width and height for a board.


## Installation

Use [Haskell stack](http://haskellstack.org/) to build, run, and install the
game.  In addition to the installation options listed on the website, you can
also run `stack` with the [official docker container for Haskell](https://hub.docker.com/_/haskell/).

    $ stack build
    $ stack exec SlidingPuzzle


## Motivation

This is an experiment to test my current abilities with Haskell.  I was
motivated by @dummey to try this out while he explored the same project in
Elixir, one of my current favorite languages.  I am hoping to compare notes
and experiences with regard to code maintenance.

I lament that I absolutely dropped the ball on testing.  I actually had a

    stack ghci


terminal open the entire time so that I could continuously reload specific
modules and hand-test outputs.

Nonetheless, it would still be a useful exercise for me to experiment with
QuickCheck and HSpec when I get a chance.

