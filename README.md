[![Build Status](https://travis-ci.org/adinapoli/snaplet-purescript.svg?branch=master)](https://travis-ci.org/adinapoli/snaplet-purescript)

# Snaplet for automatic recompilation of your Purescript project

More doc to come, but this is being used in production so it's likely
it will stay maintained for a while.

# Running the example

* Install it via `cabal install`
* Run it with `example`: it will create the snaplet configuration for you,
  but it will then fail as it failed to load the local node modules
* Install `grunt` and `grunt-purescript` in the snaplet directory:
    - `cd snaplets/purs`
    - `npm install grunt`
    - `npm install grunt-purescript`
* Run it again

## Automatic recompilation

There are two compilation modes: `CompileOnce` and `CompileAlways`.
As the name implies, the former will run your Grunt psc task just
once, during the first load of your app.
The latter will run it everytime something under `/purescript` is
demanded.

When you run your project using the `devel` env, `CompileAlways`
is assumed. For all the other envs, production included, `CompileOnce`
is the default.


# Contributions
This library scratches my own itches, but please fork away!
Pull requests are encouraged to implement the part of the API
you need.
