# Fortran rationals

## About

A package that implements rationals number as a numeric type, useful for lossless computations. Arithmetic procedures are all pure and elemental.

## Use

Here is an overview, see test and example code for all available uses

```fortran
! Declarae type
type(rational) :: a, b, c

! Set value via constructor
a = rational(2,3) ! fraction 2/3
b = rational(2)   ! integer 2
c = rational("3/4") ! fraction 3/4

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
