program demo_arithmetic
  !! Construction, arithmetic, comparison, conversion and text output
  use, intrinsic :: iso_fortran_env, only: dp => real64
  use rationals
  implicit none
  type(rational) :: a, b, c

  a = rational(2, 3)                         ! 2/3
  b = rational("3/4")                        ! 3/4; also "-2" or "0.75"
  c = rational(0.75_dp)                      ! 3/4: simplest fraction equal to the real
  print '(a)', char(rational(6, -4))         ! -3/2: lowest terms, positive denominator

  print *, a + b, a - b, a*b, a/b            ! 17/12 -1/12 1/2 8/9
  print *, a**(-2), 1 - a, b == c            ! 9/4 1/3 T
  print *, a < b, compare(b, a), max(a, b)   ! T 1 3/4

  print *, real(a), int(rational(-7, 2))     ! 0.66666666666666663 -3
  print *, floor(rational(-7, 2)), round(rational(5, 2))   ! -4 3
  print *, b%get_num(), b%get_den(), a%is_integer()        ! 3 4 F

  print *, sum([a, b, c]), product([a, b, c])              ! 13/6 3/8
  print *, mod(rational(7, 3), rational(1, 2))             ! 1/3
  write(*, '(a, dt)') 'a = ', a              ! a = 2/3
end program demo_arithmetic
