`z3.rkt`: Racket bindings for Z3
================================

We aim to provide a reasonably complete and *easy-to-use* implementation of Z3
on Racket. The documentation is rather incomplete right now, but here's what's
working:

* Basic assertions on integers, booleans, arrays and integer lists
* Custom datatypes: only scalar datatypes supported right now
* Extracting values from generated models
* A few examples, including Sudoku, n-queens, and bounded model checking for quicksort

Check out the `tests` and `examples` subdirectories for examples. For a
slightly more involved example, see [numbermind](https://github.com/sid0/numbermind),
a small web app written using this library.

Important things to do:

* Better model navigation
* Assertions on other types of lists and other non-scalar datatypes
* More examples, a more comprehensive test suite
* Better debugging and printing

Pull requests are always welcome!

Installing
----------

`z3.rkt` requires Z3 4.0, which you can download for your platform from [the
Microsoft Research
site](http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html). We
only work on Linux for now, though we would like to support all the platforms Z3
and Racket support. For now, you'll need to copy or create a symlink to `bin/libz3.so`
in this directory as `libz3.so`.

License
-------

Licensed under the Simplified BSD License. See the LICENSE file for more
details.

Please note that this license does not apply to Z3, only to these bindings.
This license also does not apply to the `doc` directory, which we reserve all
rights to.
