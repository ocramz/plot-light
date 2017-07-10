# plot-light

[![Build Status](https://travis-ci.org/ocramz/plot-light.png)](https://travis-ci.org/ocramz/plot-light)

A lightweight plotting library, exporting to SVG


## Usage

Import the user-facing module: `Graphics.Rendering.Plot.Light.Internal`

`plot-light` provides functionality for rendering vector graphics as SVG.
It is geared in particular towards scientific plotting, and it is termed "light" because it only requires native Haskell dependencies.

It builds upon `blaze-svg` by adding type-safe combinators, geometry primitives and functionality

To use this project you just need to import this module qualified (to avoid name clashes with any other modules you might have loaded on the side), for example as follows :

`import Graphics.Rendering.Plot.Light as P`