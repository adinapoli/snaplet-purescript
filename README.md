[![Build Status](https://travis-ci.org/adinapoli/snaplet-purescript.svg?branch=master)](https://travis-ci.org/adinapoli/snaplet-purescript)

# Snaplet for automatic recompilation of your Purescript project

![ps_repl](http://adinapoli.github.io/snaplet-purescript/images/ps_repl.png)

# Table of contents

* Adding `snaplet-purescript` to your snaplets
* Requirements
* Use `snaplet-purescript` on a fresh project
* Use `snaplet-purescript` on an existing project
* Guide to the snaplet configuration
    * Verbosity
    * Automatic recompilation
    * Bundle
    * Pulp Path
    * PureScript Path
    * Psa Options
    * Permissive Init
    * Bundle Name
    * Modules
    * Hooks

## Automatic recompilation

There are two compilation modes: `CompileOnce` and `CompileAlways`.
As the name implies, the former will run your Grunt psc task just
once, during the first load of your app.
The latter will run it everytime something under `/purescript` is
demanded.

When you run your project using the `devel` env, `CompileAlways`
is assumed. For all the other envs, production included, `CompileOnce`
is the default.

You can also control the verbosity of the snaplet choosing between
`Verbose` or `Quiet`.

# Contributions
This library scratches my own itches, but please fork away!
Pull requests are encouraged to implement the part of the API
you need.
