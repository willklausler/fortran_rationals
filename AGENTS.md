# AGENTS.md

Instructions for coding agents working on this repository. The public API and
its conventions are in `llms.txt`; read it before changing behaviour.

## Layout

| Path | Role |
|---|---|
| `src/rationals.f90` | **Source of truth**: the whole library, module `rationals` |
| `test/test_rationals.f90` | The tests: one program, `check(cond, name)` per assertion |
| `example/*.f90` | Demo programs; each is copied verbatim into `llms.txt` |
| `app/main.f90` | Exact vs. real64 inversion of Hilbert matrices, the fpm app |
| `llms.txt` | API reference for agents, and the online user manual |
| `man/rationals.3`, `README.md`, `ford.md` | Human documentation |
| `tools/check_docs.py` | CI check: every public name and example is in `llms.txt` |

## Commands

```sh
pip install ford                     # once
fpm test --profile debug             # fast check, gfortran
fpm test --profile release
fpm run --example --all
fpm run                              # the app
python3 tools/check_docs.py
./ci.sh                              # gfortran, ifx, nvfortran, flang if installed
make test FC=ifx                     # make build
make doc                             # FORD HTML in build/doc
```

Done means: the tests pass for every compiler you have in both profiles, the
examples and the app run, and `tools/check_docs.py` passes.

## Decided conventions (do not change without the maintainer)

- `num/den` with `int64` parts, always in lowest terms with `den > 0`; zero is 0/1.
- The components are `private`; users read them with `get_num` and `get_den`.
- `==` is exact. Comparison uses continued fractions so it never overflows.
- Mixed operators take `int32` only; `rational(n, d)` takes `int32` or `int64`. Reals convert only through the constructor or assignment.
- Error policy: `error stop` on integer overflow, division by zero, unparsable text and out-of-range reals, with a message that starts with the public generic name (`rational: integer overflow`, `int: result does not fit in int32`). Derived-type `read` returns `iostat > 0` instead. The policy is also stated in `llms.txt` and the man page NOTES.
- `random_number` uses module state, `order`, set by `set_order` (default 10). It is the only module state.
- Module name `rationals`; kinds are not exported (users take them from `iso_fortran_env`). Internally `ir`, `ik`, `rk` alias `int64`, `int32`, `real64`.
- Elemental wherever the interface allows; generic names reuse the intrinsic names.

## Adding or changing a procedure

1. Implement it in `src/rationals.f90` as a private specific (`rat_<verb>` or `rat_<op>_<operand>`) with a `!!` line.
2. Add it to the generic interface (or create one with a `!!` line) and to the `public` statement of its group.
3. Add checks to `test/test_rationals.f90`.
4. Document it in `llms.txt` (the CI check requires the name), the man page and `CHANGELOG.md`.
5. Run the tests in both profiles, the examples and `tools/check_docs.py`.

## Test rules

- The tests must stay deterministic: no check depends on `random_number` values beyond their range and denominators.
- Error stops cannot be tested; test the largest values that still succeed instead (see the cases near `huge(1_int64)`).
- Derived-type reads in tests use `'(dt)'` formats and never end on the last token of a record (see the quirks below).

## Compiler quirks that shaped the code

- nvfortran 26.3 passes an overlapping right-hand side to an elemental defined assignment without a copy (`a(2,:) = a(2,:) - a(2,1)*a(1,:)` gave 0/0): rational = rational is intrinsic assignment, so `assignment(=)` covers only `int32` and `real64`.
- ifx 2026.1 and nvfortran 26.3 write list-directed derived-type items with no separator: `write_formatted` starts list-directed output with a blank.
- nvfortran 26.3: list-directed derived-type `read` fails; the test reads with `'(dt, dt)'`.
- ifx 2026.1 writes no blank between a list-directed rational and a following character item; the test writes `' end'`.
- gfortran 14 consumes the next record when a derived-type read ends at the end of a record, and libgfortran leaks memory on that path, which fails LeakSanitizer in the debug profile. ifx 2026.1 returns an end-of-record iostat there.

## Commits

Commit as the maintainer, Will Klausler, with the GitHub no-reply address
`226649647+willklausler@users.noreply.github.com` (set in the repository's git config).
Do not add `Co-authored-by` trailers: the maintainer is the only contributor.
