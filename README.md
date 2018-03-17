# plot-light

A lightweight plotting library

[![Build Status](https://travis-ci.org/ocramz/plot-light.png)](https://travis-ci.org/ocramz/plot-light)
[![Hackage](https://img.shields.io/hackage/v/plot-light.svg)](https://hackage.haskell.org/package/plot-light)
[![plot-light](http://stackage.org/package/plot-light/badge/lts)](http://stackage.org/lts/package/plot-light)
[![plot-light](http://stackage.org/package/plot-light/badge/nightly)](http://stackage.org/nightly/package/plot-light)




`plot-light` provides functionality for rendering 2D graphics. It is geared in particular towards scientific plotting, and it is called "light" because it only requires a few common Haskell dependencies.
It builds upon `blaze-svg` by adding type-safe combinators, geometry primitives and high-level graphing functionality.

## Usage

To use this project you just need `import Graphics.Rendering.Plot.Light`. If GHC complains of name clashes you can import the module in "qualified" form.


## Documentation

Available on Hackage : https://hackage.haskell.org/package/plot-light in the `Graphics.Rendering.Plot.Light` module

## Contributing

You can use `plot-light` in your own projects (either personal, academic or commercial). All feedback such as comments, bug reports, new documentation, feature requests and patches is welcome.


## License

BSD-3, see LICENSE file