`z3.rkt`: Z3 bindings for Racket
--------------------------------

We aim to provide a reasonably complete and *easy-to-use* implementation of Z3
on Racket. The documentation is rather incomplete right now, but here's what's
working:

* Basic assertions on integers, booleans and arrays
* Custom datatypes: only scalar datatypes supported right now
* Extracting values from generated models
* A few examples using the assertions, including Sudoku and n-queens

Check out the `tests` and `examples` subdirectories for examples.

Important things to do:

* Better model navigation
* Assertions on lists and other non-scalar datatypes
* More examples, a more comprehensive test suite
* Better debugging and printing

Pull requests are always welcome!

Installing
==========

`z3.rkt` requires Z3 3.2, which you can download for your platform from [the
Microsoft Research
site](http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html). We
only work on Linux for now, though we would like to support all the platforms Z3
and Racket support. For now, you'll need to extract the Z3 tarball into this
directory, which should create a subdirectory called `z3`.

If you see an error looking for "libgomp" or something similar, make sure you
have this library installed. For Ubuntu running the following command should
work:

    sudo apt-get install libgomp1

License
=======

Licensed under the Simplified BSD License. See the LICENSE file for more
details.

Please note that this license does not apply to Z3, only to these bindings.
