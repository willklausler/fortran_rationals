# Fortran rationals

## About

A package that implements rational numbers as a numeric type, for exact computations. Arithmetic procedures are all elemental.

Values are kept in lowest terms with a positive denominator. Arithmetic is checked: an operation whose exact result does not fit in 64 bits stops with an error instead of returning a wrong answer.

## Install

Add the package to the `[dependencies]` of your `fpm.toml`:

```toml
[dependencies]
fortran_rationals = { git = "https://github.com/willklausler/fortran_rationals" }
```

## Build

With [fpm](https://fpm.fortran-lang.org):

```sh
fpm build
fpm test
fpm run --example
```

Without fpm, the library is a single file: `gfortran -c src/rationals.f90` (or any Fortran 2018 compiler).

## Use

Here is an overview; see the test and example code for all available uses.

```fortran
use rationals

! Declare type
type(rational) :: a, b, c

! Set value via constructor
a = rational(2,3)     ! fraction 2/3
b = rational(2)       ! integer 2
c = rational("3/4")   ! fraction 3/4
c = rational(0.75d0)  ! fraction 3/4, the simplest fraction equal to the real

! Set value via assignment
a = 2   ! integer 2

! Write a value
write(*,*) rational(4,5)

! Check if rational is an integer
write(*,"(L1)") a%is_integer()

! Do conditional and arithmetic
if (rational(5,8) < rational(8,13)) then
  write(*,*) rational(8,13) - rational(5,8)
else
  write(*,*) (rational(8,13) + rational(5,8))/21
end if
```

The module does not export kind parameters. Where a kind is needed, use `int32`, `int64` and `real64` from `iso_fortran_env`.

## Limitations

- **Range.** Numerator and denominator are `int64`. Repeated sums and products grow denominators quickly, so long computations (for example Gaussian elimination on larger matrices) can exceed this range. The program then stops with `rational: integer overflow`. There is no arbitrary-precision fallback.
- **Errors stop the program.** Overflow, division by zero, and unparsable text all use `error stop`, so they cannot be caught.
- **Integer kinds.** Mixed rational-integer operators accept `int32` integers. Construct `int64` values with `rational(n, d)`.
- **Reals.** Conversion from `real64` gives the simplest fraction that converts back to exactly the same real. A decimal like `0.1` becomes `1/10`, but this is the value the real represents, not necessarily the one intended.
- **Formatted input.** `read` with `DT` or list-directed formatting works for values in the middle of a record, but compilers differ at the end of a record: gfortran skips the next record, ifx reports end-of-record for `(DT)`, and nvfortran fails. For portable input, read a line into a character variable and use `rational(str)`.

## References

- D. E. Knuth, *The Art of Computer Programming*, Vol. 2: *Seminumerical Algorithms*, 3rd ed., Addison-Wesley, 1997, Section 4.5.1, "Fractions". Addition and multiplication follow its reduce-before-multiply method.

## License

[MIT](LICENSE)
