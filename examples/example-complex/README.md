
# Snaplet-purescript example app

This example project is meant to get you up and running with `snaplet-purescript`.

# Installing the project

First of all install the project:

```
git clone git@github.com:adinapoli/snaplet-purescript.git
cd example
cabal install
```

# Running the project

At this point, your `example` directory should **not** contain a `snaplets` directory.
`snaplet-purescripts` will generate the appropriate scaffolding for you if nothing is
there. Let's run the app:

```
☁  example [prettify-errors] ⚡ example
no port specified, defaulting to port 8000
Local grunt not found, installing it for you...
Initializing app @ /
Initializing purs @ /purs
...setting up filesystem

Listening on http://0.0.0.0:8000/
```

As you can see, `snaplet-purescript` detected that your vanilla project didn't have
a local grunt installed and installed it for you, together with `grunt-purescript`.
Let's look at the generated project itself:

```
☁  purs [prettify-errors] ⚡ ls -lh
total 16
-rw-r--r--  1 adinapoli  staff   446B  5 Jul 16:15 Gruntfile.js
-rw-r--r--  1 adinapoli  staff   163B  5 Jul 16:15 devel.cfg
drwxr-xr-x  3 adinapoli  staff   102B  5 Jul 16:15 js
drwxr-xr-x  3 adinapoli  staff   102B  5 Jul 16:15 src
```

`snaplet-purescript` generated for you the scaffolding. You are
free to customise the `Gruntfile.js` at your will. We now have
a default `src/Main.purs` file, which will simply print
`Hello World` to the console. By default `snaplet-purescript` will
compile everything into a fat app under `js/app.js`, so let's try
to call:

```
☁  purs [prettify-errors] ⚡ curl -s http://localhost:8000/purescript/app.js | wc -l
    1322
```

As you can see what we get is the entire PS environment plus our `Main.purs`
function. Note how we prefixed `purescript` to `app.js`, where `purescript`
is what we defined in our `Site.hs` routes:

```
routes :: [(ByteString, AppHandler ())]
routes = [ ("/purescript", with purs pursServe)]
```

# When disaster strikes

Let's try now to edit our `src/Main.purs` like so:

```
module Main where

import Debug.Trace

main = trac "Hello PS world!"
```

Note how we dropped the final `e` from `trace`. Hitting `localhost:8000/purescript/app.js`
will now yield:

```
[4mRunning "psc:all" (psc) task[24m
[31m>> [39mError creating file js/app.js
[31m>> [39m
[33mWarning: Error in module Main:
Error at src/Main.purs line 6, column 10 - line 6, column 15:
Unknown value trac
See https://github.com/purescript/purescript/wiki/Error-Code-UnknownValue for more information, or to contribute content related to this error. Use --force to continue.[39m

[31mAborted due to warnings.[39m
```

After fixing the typo, our js will be re-generated and served again. Success!
