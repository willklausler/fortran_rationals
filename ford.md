---
project: fortran_rationals
summary: Exact rational numbers for modern Fortran: the derived type `rational` with checked 64-bit arithmetic and overloaded numeric intrinsics.
author: Will Klausler
github: https://github.com/willklausler
project_github: https://github.com/willklausler/fortran_rationals
license: mit
src_dir: ./src
output_dir: ./build/doc
page_dir: ./build/pages
preprocess: false
display: public
source: true
search: true
graph: false
---

`use rationals` gives the type `rational`, its arithmetic and comparison
operators, and the numeric intrinsics overloaded for it.  Start with the
[user manual](page/index.html), which is also available as plain text in
[llms.txt](llms.txt).

The specific procedure names, such as `rat_add_rat`, are private; call them
through the generic names.

```fortran
use rationals
type(rational) :: a
a = rational(2, 3) + rational("3/4")
print *, a, real(a)   ! 17/12  1.4166666666666667
```
