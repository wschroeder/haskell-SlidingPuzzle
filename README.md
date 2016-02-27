# Sliding Puzzle

This is a console version of the [Sliding Puzzle game](https://en.wikipedia.org/wiki/Sliding_puzzle).
Choose any width and height for a board.


## Installation

Use [Haskell stack](http://haskellstack.org/) to build, run, and install the
game.  In addition to the installation options listed on the website, you can
also run `stack` with the [official docker container for Haskell](https://hub.docker.com/_/haskell/).

    $ stack build
    $ stack exec SlidingPuzzle


## Motivation and Experience

This is an experiment to test my current abilities with Haskell.  I was
motivated by @dummey to try this out while he explored the same project in
Elixir, one of my current favorite languages.  I am hoping to compare notes
and experiences with regard to code maintenance.


### Testing, or Lack Thereof

I lament that I absolutely dropped the ball on testing.  I actually had a

    stack ghci


terminal open the entire time so that I could continuously reload specific
modules and hand-test outputs.

Nonetheless, it would still be a useful exercise for me to experiment with
QuickCheck and HSpec when I get a chance.


### Maintaining

As I added features to the game, the compiler successfully caught my many
mistakes and helped me resolve them.  I learned quickly that providing type
annotations to my functions helped the compiler to help me narrow down
unexpected problems.  I made heavy use of `where` and `let` clauses, which
caused me to spend a lot of time trying to understand the real problem behind
what the compiler was telling me about its deductions: annotate types for
great good!


### Learning

I probably spent, tops, five hours on this project.  In that process, here is
what I learned:

- [System.Random](http://hackage.haskell.org/package/random-1.0.0.3/docs/System-Random.html)
  is an external package I must install!
- [System.Console.Ansi](http://hackage.haskell.org/package/ansi-terminal-0.6.2.3/docs/System-Console-ANSI.html)
  is so much easier to use than ncurses-style approaches, and I regret
  discovering it at the end of my work on this project.  It reminds me of
  [conio](https://en.wikipedia.org/wiki/Conio.h) from my Turbo Pascal days.
  Still, more serious games probably ought to at least use something like
  [SDL](https://www.libsdl.org/) and not the console.
- The Haskell ecosystem does not appear to have a function for shuffling
  arrays!?  Later, I realized that was the wrong solution for my problem, but
  I ended up implementing a kind of randomized reverse quicksort with
  `splitAt` to solve the problem.
- I have previously had too much stress over trying to reduce IO.
- `(..)` is a valuable friend.
- I had a frustrating time trying to figure out the best way to parse a string
  into an integer.  I was aware of `read`, but I thought it made huge
  assumptions, and I was trying to find something similar to Elixir's
  `Integer.parse/1`, which returns a tuple with whatever integer it could find
  and the remaining string.  I ended up wading through parsers and whitepapers
  about parsers, along with a growing feeling of disappointment.  I just want
  to parse out an `Int`, not dabble in parser or category theory to get there.
  The next morning, I realized I could just validate that every character in
  my string was a digit and use `read`.  I probably missed out on a valuable
  opportunity somewhere in there, but at the moment, it is more interesting to
  solve the problem of the game logic, not the problem of safely parsing
  integers.
- Generally, I found what I needed and was sometimes surprised at what did not
  previously exist, at least under terminology I knew to search for as per
  experiences from other languages and paradigms.
  [Hoogle's](https://www.haskell.org/hoogle/) type searcher is a *big deal*.
- Initially, I felt that monad markers would guarantee that I would know
  exactly what a function was going to do, ultimately, as a side-effect.
  However, both `stdout` and random number generation involve the IO monad,
  which means I only know in a general sense.  That is still *much* better
  than the situation in every other language I have explored, including
  [Rust](https://www.rust-lang.org/) and Elixir/Erlang with Dialyzer, where
  side-effects may happily occur.
- I wish ghci had an interface like `iex`, `pry`, or `sbcl`, where I could
  search for functions related to my current data.  Knowing how much effort
  has been put into Haskell by the community, I bet it exists somewhere as an
  add-on or replacement.
- When learning a language, it helps to have a mentor/reviewer!  It makes the
  difference between "book smart" and "street smart" programming.

