schemeskell
===========

A toy, mostly broken implementation of Scheme in Haskell. It only implements
an undocumented subset of the
[R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/). Still
a WIP.


## Installation

* Clone the repo
* ``` cd schemeskell ```
* ``` cabal install ```

Make sure that the location that Cabal installs packages to is in your
``` $PATH ```. eg. ```~/Library/Haskell/bin``` on OSX.

## Running

From the command line:

```
$ schemeskell "(+ 4 5)"
9
```

```
$ schemeskell "(if (> 2 3) \"no\" \"yes\")"
"yes"
```

REPL to be added soon.


## Credits

Based on the [Write Yourself A Scheme in 48 Hours](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html)
tutorial, by Jonathan Tang.
