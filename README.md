SmoothLife in Haskell
=====================

This is a Haskell port of [Mikola Lysenko](http://0fps.wordpress.com/)'s
[simplified JavaScript implementation](http://0fps.wordpress.com/2012/11/19/conways-game-of-life-for-curved-surfaces-part-1/)
of Stephan Rafler's [generalization of Conway's Game of Life](http://arxiv.org/abs/1111.1567) to a continuous domain.

![Example output](https://raw.github.com/travisbrown/smoothlife/master/samples/output.png)

It exists because it's Saturday morning on a holiday weekend and I wanted to
write some Haskell. And also because I think these patterns are beautiful,
like a weird cross between a slice of a brain and the ornamentation in a
William Morris woodcut.

Warnings!
---------

The code is essentially undocumented, there's no error handling to speak of,
and this is the first time I've used [Repa](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial),
so I'm sure what I'm doing with it is horribly inefficient.

A few more concrete things:

 * The input can be any image format supported by [DevIL](http://hackage.haskell.org/package/repa-devil-0.3.2),
   but it must be a square whose sides are a power of two, and it must be
   grayscale.
 * The program won't overwrite existing files, so you'll have to clear the
   output directory manually between runs (or specify a new one).

Examples
--------

![Example output](https://raw.github.com/travisbrown/smoothlife/master/samples/output.gif)

Usage
-----

The following should work:

    cabal build
    mkdir output
    dist/build/smoothlife/smoothlife \
      -c=samples/config.txt -i=samples/squiggles.png -o=output

This will save an image to the `output` after every time step.

You can easily change the rules of the game by editing the config file, or
providing your own. The current defaults looks like this:

    alphas = 0.127 0.030
    radii  = 6     6.6
    birth  = 0.258 0.310
    death  = 0.010 0.740

These values come from [a comment](http://0fps.wordpress.com/2012/11/19/conways-game-of-life-for-curved-surfaces-part-1/#comment-699)
on Lysenko's blog post.

