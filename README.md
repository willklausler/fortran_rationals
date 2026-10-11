# fortran_rationals

[![CI](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml/badge.svg)](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml)
[![Docs](https://github.com/willklausler/fortran_rationals/actions/workflows/docs.yml/badge.svg)](https://willklausler.github.io/fortran_rationals/)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23300374.svg)](https://doi.org/10.5281/zenodo.23300374)

Exact rational numbers for modern Fortran: one derived type, `rational`, with
the arithmetic operators and the numeric intrinsics overloaded. Overflow stops
the program instead of returning a wrong answer.

```fortran
use, intrinsic :: iso_fortran_env, only: dp => real64
use rationals
type(rational) :: a, b

a = rational(2, 3)                        ! 2/3
b = rational("3/4") + 1                   ! 7/4
print *, a + b, a*b, a < b                ! 29/12  7/6  T
print *, rational(0.1_dp), real(a)        ! 1/10  0.66666666666666663
print *, sum([a, b, a]), floor(-b)        ! 37/12  -2
```

> Coding agents: the complete API reference is [llms.txt](llms.txt), and
> contributor instructions are in [AGENTS.md](AGENTS.md).

## Features

- Type `rational` with an `int64` numerator and denominator, always in lowest terms.
- Operators `+ - * / ** == /= < > <= >=` between rationals and `int32` integers; exact `==`.
- Checked arithmetic: overflow, division by zero and bad text stop with a message. Addition and multiplication cancel common factors first, and comparison never overflows.
- Conversion from reals to the simplest equal fraction, from text, and back with `real`, `int` and `char`.
- `abs`, `sign`, `min`, `max`, `mod`, `floor`, `ceiling`, `sum`, `product`, `dot_product`, `matmul`, `transpose` and `random_number` work on rationals; also `trunc`, `round`, `compare`, `divmod`, `gcf`, `lcm`.
- Elemental procedures, text I/O with `print *` and `'(dt)'`, no dependencies.

Conventions: the components are private (`get_num`, `get_den`), the denominator is positive, and errors stop the program.

## Installation

With [fpm](https://fpm.fortran-lang.org) 0.13 or later, add to your `fpm.toml`:

```toml
[dependencies]
fortran_rationals = { git = "https://github.com/willklausler/fortran_rationals", tag = "v0.2.0" }
```

With make, which builds `build/make/libfortran_rationals.a` and the module file:

```sh
make FC=gfortran && make test
make install PREFIX=$HOME/.local    # library, .mod file and man page
```

## Documentation

- [User manual and API documentation](https://willklausler.github.io/fortran_rationals/), generated with FORD (`make doc`)
- [llms.txt](llms.txt): the API reference as one plain-text file, also the manual's source
- `man 3 rationals` after `make install` ([man/rationals.3](man/rationals.3))
- Example programs in [example/](example/) (`fpm run --example --all`) and an app in [app/](app/) (`fpm run`) that inverts Hilbert matrices exactly and shows how far double precision drifts

## Compilers

CI tests every push on:

| Compiler | Linux | macOS | Windows |
| --- | --- | --- | --- |
| gfortran 14 | yes | yes | yes |
| gfortran 13 | yes | | |
| ifx 2026 | yes | | yes |
| LLVM flang 21 | yes | | |
| nvfortran 26 | yes | | |

Known compiler limitations, mostly in derived-type input, are listed in [llms.txt](llms.txt#compiler-notes).

## Why another rational package?

Two other Fortran rational-number modules exist on GitHub.
[art-rasa/rational_number](https://github.com/art-rasa/rational_number) has
addition and subtraction only, through function calls, with no overflow
detection and no license.
[wayne-mj/fractions](https://github.com/wayne-mj/fractions) is a fraction
calculator with functions such as `add_fraction` and a decimal-places
approximation for reals, but no operators. This package makes a rational
behave like a numeric type: operators, comparisons, the numeric and array
intrinsics, exact conversion from reals, and overflow checks on every
operation, packaged for fpm and tested with gfortran, ifx, LLVM flang and
nvfortran on Linux, macOS and Windows.

## References

- D. E. Knuth, *The Art of Computer Programming*, Vol. 2: *Seminumerical Algorithms*, 3rd ed., §4.5.1 (fractions) and §4.5.3 (continued fractions), Addison-Wesley, 1997.

## Status and support

Version 0.2.0. The API may still change before 1.0; changes are recorded in
[CHANGELOG.md](CHANGELOG.md). Bug reports, questions and pull requests are
welcome on [GitHub](https://github.com/willklausler/fortran_rationals/issues);
see [CONTRIBUTING.md](CONTRIBUTING.md). To cite this package, use
[CITATION.cff](CITATION.cff) or the DOI [10.5281/zenodo.23300374](https://doi.org/10.5281/zenodo.23300374),
which always points to the latest version.

## License

[MIT](LICENSE)
