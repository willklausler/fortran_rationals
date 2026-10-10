program demo_approximation
  !! Rational approximations: pi from its decimal digits, sqrt(2) from a continued fraction
  use, intrinsic :: iso_fortran_env, only: dp => real64, int64
  use rationals
  implicit none
  real(dp), parameter :: pi = acos(-1._dp)
  type(rational) :: r
  integer :: i

  r = pi                                     ! simplest fraction that converts back to pi
  print *, r                                 ! 245850922/78256779
  print *, real(r) == pi                     ! T

  do i = 1, 6                                ! truncated decimals, in lowest terms
    print *, rational(int(pi*10**i, int64), 10_int64**i)   ! 31/10 157/50 ... 392699/125000
  end do

  r = 1                                      ! sqrt(2) = 1 + 1/(2 + 1/(2 + ...))
  do i = 1, 6
    r = 1 + 1/(1 + r)                        ! 3/2 7/5 17/12 41/29 99/70 239/169
    print *, r, real(r*r) - 2                ! error shrinks about 6 times per step
  end do
end program demo_approximation
