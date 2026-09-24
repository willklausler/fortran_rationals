program example

  use rationals

  implicit none

  call ex1()

  call ex2()

contains

subroutine ex1()

  integer :: i

  real(rk), parameter :: pi = 4*atan(1.0_rk)

  type(rational) :: rat_pi

  write(*,"(A)") "Example: rational representation of pi"

  rat_pi = pi

  write(*,*) pi
  write(*,*) rat_pi

  do i = 1,8
    write(*,*) rational(int(pi*10**i), 10**i)
  end do ! i
  write(*,*)

end subroutine ex1

!***********************************************************************

subroutine ex2()

  integer :: i

  type(rational) :: roottwo

  write(*,"(A)") "Example: approximation of the square root of two"

  roottwo = 1

  do i = 1,10
    roottwo = 1 + 1/(1 + roottwo)
    write(*,"(I3,')',1x,$)") i
    write(*,*) roottwo, real(roottwo), real(roottwo*roottwo)
  end do ! i

end subroutine ex2

end program example