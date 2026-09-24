### GitHub Actions status

+ [![CI](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml/badge.svg)](https://github.com/willklausler/fortran_rationals/actions/workflows/ci.yml)
+ [![FORD docs](https://github.com/willklausler/fortran_rationals/actions/workflows/docs.yml/badge.svg)](https://github.com/willklausler/fortran_rationals/actions/workflows/docs.yml)

On each push or pull request, CI runs `fpm test` in the debug and release
profiles with gfortran, ifx, nvfortran, LLVM flang and LFortran on Ubuntu;
gfortran and LFortran on macOS; and gfortran, ifx and LFortran on Windows.
LFortran jobs may fail without failing the build.
