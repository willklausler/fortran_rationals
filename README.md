# fortran_rationals

[![CI](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml/badge.svg)](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml)

Rational numbers for exact computation, packaged as one derived type,
`rational`, with the arithmetic operators and the numeric intrinsics
overloaded.

- 64-bit numerator and denominator, always in lowest terms
- Checked arithmetic: overflow stops with an error, never a wrong answer
- Conversion from reals to the simplest equal fraction, and from text
- `sum`, `product`, `dot_product`, `matmul`, `transpose`, `abs`, `min`,
  `max`, `floor`, `ceiling`, `mod` and more work on rationals
- Elemental procedures, no dependencies, Fortran 2018, fpm and make builds

## Installation

Add the package to your `fpm.toml`:

```toml
[dependencies]
fortran_rationals = { git = "https://github.com/willklausler/fortran_rationals" }
```

Without fpm, `make && make install PREFIX=...` builds and installs the library,
the module file and a pkg-config file. Or compile the single source file
`src/rationals.f90` with your project.

## Quick start

```fortran
use rationals, only: rational, operator(+), operator(-), operator(/), operator(<), &
                     assignment(=)

type(rational) :: a, b, c

a = rational(2, 3)       ! 2/3
b = rational("3/4")      ! 3/4
c = rational(0.75d0)     ! 3/4, the simplest fraction equal to the real
a = 2                    ! 2

write(*,*) a%is_integer()                       ! T
if (rational(5,8) < rational(8,13)) then
  write(*,*) rational(8,13) - rational(5,8)
else
  write(*,*) (rational(8,13) + rational(5,8))/21 ! 43/728
end if
```

The runnable example, rational approximations of π and √2, is
[example/rationals_example.f90](example/rationals_example.f90):

```sh
fpm run --example
```

## API

| Entity | Description |
| --- | --- |
| `rational(n, d)` | Constructor from `int32` or `int64` numerator and denominator |
| `rational(n)`, `rational(x)`, `rational(str)` | From an `int32`, a `real64`, or text `"n"`, `"n/d"`, `"0.75"` |
| `r = n`, `r = x` | Assignment from `int32` or `real64` |
| `+ - * / **` | Arithmetic with rationals and `int32`; `**` takes an `int32` exponent |
| `== /= < > <= >=` | Comparison with rationals and `int32` |
| `r%get_num()`, `r%get_den()` | Numerator and denominator, `int64` |
| `r%is_integer()` | `.true.` if the denominator is 1 |
| `r%inverse()` | Reciprocal |
| `char(r)`, `int(r)`, `real(r)` | Text `"n/d"` or `"n"`; `int32` truncated toward zero; `real64` |
| `floor`, `ceiling`, `trunc`, `round` | Integer part, `int64`; `round` ties away from zero |
| `abs`, `sign`, `min`, `max`, `mod` | As the intrinsics |
| `call divmod(a, b, q, r)` | Truncated quotient `q` (`int64`) and remainder `r` |
| `compare(a, b)` | -1, 0 or 1 |
| `gcf`, `lcm` | Greatest common factor and least common multiple of integers; `gcf` of rationals |
| `sum`, `product`, `dot_product`, `matmul`, `transpose` | On rational arrays |
| `random_number(r)` | Random rational in [0, 1] with denominator dividing `set_order(n)`, default 10 |
| `write(*,*) r`, `write(*,"(DT)") r` | Same text as `char(r)` |

Overflow, division by zero and unparsable text stop with `error stop` and a
message. The module does not export kind parameters; take `int32`, `int64`
and `real64` from `iso_fortran_env`.

## Limitations

- **Range.** Numerator and denominator are `int64`. Repeated sums and products
  grow denominators quickly, so long computations, for example Gaussian
  elimination on larger matrices, can exceed this range and stop with
  `rational: integer overflow`. There is no arbitrary-precision fallback.
- **Errors stop the program.** They cannot be caught.
- **Integer kinds.** Mixed rational-integer operators take `int32`. Construct
  `int64` values with `rational(n, d)`.
- **Reals.** `0.1d0` becomes `1/10`, the simplest fraction equal to that real,
  which is not always the value intended.
- **Formatted input.** `read` with `DT` or list-directed formatting works for
  values in the middle of a record, but compilers differ at the end of a
  record: gfortran skips the next record, ifx reports end-of-record for `(DT)`,
  and nvfortran fails. For portable input, read a line into a character
  variable and use `rational(str)`.

## Building, testing and documentation

```sh
fpm test --profile debug     # bounds checks, sanitizers, FP traps
fpm test --profile release
fpm run --example
make test                    # without fpm
ford ford.md                 # API documentation in docs/
```

The unit tests check every constructor, conversion, operator and overloaded
intrinsic against hand-computed results, and regression cases near the
`int64` limit. CI runs gfortran 13–15, Intel ifx and NVIDIA nvfortran on
Linux, gfortran on macOS and Windows, the make build and the documentation
build, and publishes the [documentation](https://willklausler.github.io/fortran_rationals/)
to GitHub Pages from `main`.

## Pedigree and support

Addition and multiplication follow Knuth's algorithms for fractions [1].
Every procedure is verified by the tests described above. The code has not
been independently reviewed. It is maintained by the author on a best-effort
basis.

Please report bugs and request features through
[GitHub issues](https://github.com/willklausler/fortran_rationals/issues).
Pull requests are welcome; please include a test.

## References

1. Knuth, D. E. (1997). *The Art of Computer Programming*, Vol. 2: *Seminumerical Algorithms*, 3rd ed., §4.5.1 and §4.5.3. Addison-Wesley.

## License

[MIT](LICENSE) © 2025-2026 Will Klausler
