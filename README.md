# Transparent Procedures

[![Build Status][build-badge]][build]
[![Scribble][docs-badge]][docs]

Transparent procedures provide a first-order representation
of a procedure's extensionally behavior.
See [the documentation][docs] for information on how to install and use the library.

## Install

```
$ raco pkg install transparent-procedure
```

## Organization

This repository is organized into two separate packages.

* `transparent-procedure/`
  contains the documentation for how to use the library
  and well as the unit tests for the implementation.
* `transparent-procedure-lib/`
  contains the main code for the library.

[build-badge]: https://github.com/camoy/transparent-procedure/actions/workflows/build.yml/badge.svg
[build]: https://github.com/camoy/transparent-procedure/actions/workflows/build.yml?query=workflow%3Abuild
[docs-badge]: https://img.shields.io/badge/Docs-Scribble-blue.svg
[docs]: https://docs.racket-lang.org/transparent-procedure
