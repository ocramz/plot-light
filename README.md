# plot-light

A lightweight plotting library, exporting to SVG

[![Build Status](https://travis-ci.org/ocramz/plot-light.png)](https://travis-ci.org/ocramz/plot-light)
[![Hackage](https://img.shields.io/hackage/v/plot-light.svg)](https://hackage.haskell.org/package/plot-light)
[![plot-light](http://stackage.org/package/plot-light/badge/lts)](http://stackage.org/lts/package/plot-light)
[![plot-light](http://stackage.org/package/plot-light/badge/nightly)](http://stackage.org/nightly/package/plot-light)




`plot-light` provides functionality for rendering vector graphics as SVG.
It is geared in particular towards scientific plotting, and it is termed "light" because it only requires a few common Haskell dependencies and no external libraries.
It builds upon `blaze-svg` by adding type-safe combinators, geometry primitives and functionality.

## Usage

To use this project you just need to import this module qualified (to avoid name clashes with any other modules you might have loaded on the side), for example as follows :

`import Graphics.Rendering.Plot.Light as P`