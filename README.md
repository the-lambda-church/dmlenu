A library to easily create dmenu-like applications.

Dependencies
------------

- Oasis
- Batteries
- Yojson (optional)
- Cmdliner (optional, needed only to build the examples)

Building and installing
-----------------------

    $ oasis setup
    $ ./configure [options]
    $ make
    $ make install

Examples
--------

In the `examples/` directory. They can be built using:

    $ ./configure --enable-examples # optionally: --enable-extra
    $ make

Documentation
-------------

Once `oasis setup` has run, just call `make doc`.
Or just go to : http://the-lambda-church.github.io/dmlenu/
