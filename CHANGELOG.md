# Changelog

All notable changes are listed here. The project follows [Semantic Versioning](https://semver.org).

## [0.2.0] - 2026-10-11

- Fixed: `rational(str)` and derived-type `read` ignored trailing text: `"1/2/3"`, `"3/4 junk"`, `"1 2"` and `"7,5"` gave 1/2, 3/4, 1 and 7. They are now errors.
- Fixed: `**` took time linear in the exponent (`rational(-1)**huge(1)` ran for 45 s) and `abs(-huge(1)-1)` overflowed. It now squares repeatedly, and still stops on overflow.
- Fixed: `int(r)` stopped for r = -2147483648, which fits in `int32`.
- `set_order(n)` stops with `set_order: order must be positive` for n < 1, instead of a misleading division by zero in a later `random_number`.
- Fixed wrong results on nvfortran when an array assignment reads its own elements, such as a row update in Gaussian elimination: rational = rational is now intrinsic assignment.
- List-directed output of a rational starts with a blank, so `print *, a, b` separates the values on ifx and nvfortran too.
- `rational(str)` is elemental instead of impure elemental.
- Error messages start with the public generic name (`gcf:`, `lcm:`, `int:`, `matmul:`, `dot_product:`).
- New layout following the maintainer's package standard: `llms.txt` API reference, man page, `AGENTS.md`, `CITATION.cff`, `CONTRIBUTING.md`, two example programs and an app.
- fpm profiles with per-compiler flags; the make build installs the man page and no longer writes a pkg-config file or has `uninstall` and `example` targets.
- CI adds ifx on Windows and LLVM flang; documentation is deployed by a separate workflow.

## [0.1.0] - 2025-09-04

First public release.

- Type `rational` with `int64` numerator and denominator in lowest terms, and checked arithmetic that stops on overflow.
- Constructors from integers, reals and text; operators with rationals and `int32`; overloaded numeric and array intrinsics; `gcf`, `lcm`, `compare`, `divmod`, `random_number`.
