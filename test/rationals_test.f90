program rationals_test
!! Showcase rationals

  use iso_fortran_env
  use rationals

  implicit none

  integer(ir), parameter :: yoog = huge(1_ir)

  integer(ik) :: ord
  integer :: unit
  integer :: stat

  character(*), parameter :: fmt1 = "(A20,': ',$)"
  character(*), parameter :: fmt2 = "('passed')"
  character(50) :: str

  type(rational) :: a, b

  type(rational) :: u(2), v(2), m(2,2), n(2,2), p(2,2)

  str = "1/2  "

  write(*,"(A)") "--- Testing rationals ---"

  write(*,"(A)") "Basics"

  write(*,fmt1) "Math"
  if (gcf(0,2) /= 0) then
    error stop "gcf failed for 0"
  end if
  if (gcf(2,3) /= 1) then
    error stop "gcf failed for coprime"
  end if
  if (gcf(12,12) /= 12) then
    error stop "gcf failed for equality"
  end if
  if (gcf(3,12) /= 3) then
    error stop "gcf failed for result"
  end if
  if (gcf(-3,12) /= 3) then
    error stop "gcf failed for negative input"
  end if
  if (lcm(0, 1) /= 0) then
    error stop "lcm failed for zero"
  end if
  if (lcm(3, 3) /= 3) then
    error stop "lcm failed for equality"
  end if
  if (lcm(2, 3) /= 6) then
    error stop "lcm failed for product result"
  end if
  if (lcm(4, 6) /= 12) then
    error stop "lcm failed for non-product result"
  end if
  if (lcm(-4, 6) /= 12) then
    error stop "lcm failed for negative"
  end if
  write(*,fmt2)

  write(*,fmt1) "Constructor"
  ! a = rational(1, 1)
  ! if ((a%get_num() /= 1) .or. (a%get_den() /= 1)) then
  !   error stop "Default constructor failed"
  ! end if
  a = rational(3)
  if ((a%get_num() /= 3) .or. (a%get_den() /= 1)) then
    error stop "Integer constructor failed"
  end if
  a = rational(0.5_rk)
  if ((a%get_num() /= 1) .or. (a%get_den() /= 2)) then
    error stop "Real constructor failed"
  end if
  a = rational("1/2")
  if ((a%get_num() /= 1) .or. (a%get_den() /= 2)) then
    error stop "Character constructor failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "I/O"
  read(str,"(A)",iostat=stat) a
  if (stat /= 0) then
    error stop "Formatted read failed"
  end if
  open(newunit=unit,file="scratch",action='write')
  write(unit,*) a
  close(unit,status='delete')
  write(*,fmt2)

  write(*,fmt1) "Assignment"
  a = 2
  if ((a%get_num() /= 2) .or. (a%get_den() /= 1)) then
    error stop "Integer assignment failed"
  end if
  a = 0.5_rk
  if ((a%get_num() /= 1) .or. (a%get_den() /= 2)) then
    error stop "Real assignment failed"
  end if
  ! a = "1/2"   !! Not supported by Fortran
  b = a
  if ((b%get_num() /= a%get_num()) .or. (b%get_den() /= a%get_den())) then
    error stop "Copying failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Casting"
  a = 2
  if (int(a) /= 2) then
    error stop "Casting to integer failed"
  end if
  a = 0.5_rk
  if (real(a) /= 0.5_rk) then
    error stop "Casting to real failed"
  end if
  if (char(a) /= "1/2") then
    error stop "Casting to character failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "Boolean operators"

  write(*,fmt1) "Equality"
  a = 2
  b = 2
  if (.not.(a == b)) then
    error stop "Rational-rational equality failed"
  end if
  if (.not.(a == 2)) then
    error stop "Rational-integer equality failed"
  end if
  if (.not.(2 == a)) then
    error stop "Integer-rational equality failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Inequality"
  a = 2
  b = 3
  if (.not.(a /= b)) then
    error stop "Rational-rational inequality failed"
  end if
  if (.not.(a /= 3)) then
    error stop "Rational-integer inequality failed"
  end if
  if (.not.(3 /= a)) then
    error stop "Integer-rational inequality failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Strict superiority"
  a = rational(3,2)
  b = rational(2,3)
  if (.not.(a > b)) then
    error stop "Rational-rational strict superiority failed"
  end if
  if (.not.(a > 1)) then
    error stop "Rational-integer strict superiority failed"
  end if
  if (.not.(2 > a)) then
    error stop "Integer-rational strict superiority failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Strict inferiority"
  a = rational(2,3)
  b = rational(3,2)
  if (.not.(a < b)) then
    error stop "Rational-rational strict inferiority failed"
  end if
  if (.not.(a < 1)) then
    error stop "Rational-integer strict inferiority failed"
  end if
  if (.not.(0 < a)) then
    error stop "Integer-rational strict inferiority failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Superiority"
  a = rational(3,2)
  b = rational(2,3)
  if (.not.(a >= b)) then
    error stop "Rational-rational superiority failed"
  end if
  if (.not.(a >= 1)) then
    error stop "Rational-integer superiority failed"
  end if
  if (.not.(2 >= a)) then
    error stop "Integer-rational superiority failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Inferiority"
  a = rational(2,3)
  b = rational(3,2)
  if (.not.(a <= b)) then
    error stop "Rational-rational inferiority failed"
  end if
  if (.not.(a <= 1)) then
    error stop "Rational-integer inferiority failed"
  end if
  if (.not.(0 <= a)) then
    error stop "Integer-rational inferiority failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "Arithmetic"

  write(*,fmt1) "Addition"
  a = rational(2,3)
  b = rational(3,2)
  if (a + b /= rational(13, 6)) then
    error stop "Rational-rational addition failed"
  end if
  if (a + 1 /= rational(5, 3)) then
    error stop "Rational-integer addition failed"
  end if
  if (1 + a /= rational(5, 3)) then
    error stop "Integer-rational addition failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Subtraction"
  a = rational(2,3)
  b = rational(3,2)
  if (a - b /= rational(-5, 6)) then
    error stop "Rational-rational subtraction failed"
  end if
  if (a - 1 /= rational(-1, 3)) then
    error stop "Rational-integer subtraction failed"
  end if
  if (1 - a /= rational(1, 3)) then
    error stop "Integer-rational subtraction failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Multiplication"
  a = rational(2,3)
  b = rational(3,2)
  if (a*b /= rational(1, 1)) then
    error stop "Rational-rational multiplication failed"
  end if
  if (a*2 /= rational(4, 3)) then
    error stop "Rational-integer multiplication failed"
  end if
  if (2*a /= rational(4, 3)) then
    error stop "Integer-rational multiplication failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Division"
  a = rational(2,3)
  b = rational(3,2)
  if (a/b /= rational(4, 9)) then
    error stop "Rational-rational division failed"
  end if
  if (a/2 /= rational(1, 3)) then
    error stop "Rational-integer division failed"
  end if
  if (2/a /= rational(3, 1)) then
    error stop "Integer-rational division failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Reciprocal"
  a = rational(2,3)
  if (a%inverse() /= 1/a) then
    error stop "Reciprocation failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Exponentiation"
  a = rational(2,3)
  if (a**2 /= rational(4, 9)) then
    error stop "Rational-integer exponentiation failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "Edge cases"
  a = rational(yoog, 2_ir)
  b = rational(2_ir, yoog)
  if (a*b /= 1) then
    error stop "Multiplication overflow failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "Functions"

  write(*,fmt1) "abs"
  a = rational(-1, 2)
  if (abs(a) /= rational(1, 2)) then
    error stop "Absolute value failed"
  end if
  a = rational(1, 2)
  if (abs(a) /= rational(1, 2)) then
    error stop "Absolute value failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "min"
  a = rational(2, 3)
  b = rational(3, 2)
  if (min(a,b) /= a) then
    error stop "Rational-rational minimum failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "max"
  a = rational(2, 3)
  b = rational(3, 2)
  if (max(a,b) /= b) then
    error stop "Rational-rational maximum failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "dot_product"
  u = [rational(1, 2), rational(2, 3)]
  v = [rational(3, 4), rational(4, 5)]
  if (dot_product(u, v) /= rational(109, 120)) then
    error stop "dot_product failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "matmul"
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  u = [rational(1, 4), rational(3, 4)]
  v = [rational(7, 8), rational(7, 12)]
  if (.not.all(matmul(m, u) == v)) then
    error stop "matmul for matrix-vector failed"
  end if
  n(:,1) = [rational(1, 4), rational(3, 4)]
  n(:,2) = [rational(1, 2), rational(1, 1)]
  p(:,1) = [rational(7, 8), rational(7, 12)]
  p(:,2) = [rational(5, 4), rational(5, 6)]
  if (.not.all(matmul(m, n) == p)) then
    error stop "matmul for matrix-matrix failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "transpose"
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  n(:,1) = [rational(1, 2), rational(1, 1)]
  n(:,2) = [rational(1, 3), rational(2, 3)]
  if (.not.all(transpose(m) == n)) then
    error stop "transpose failed"
  end if
  write(*,fmt2)

  write(*,fmt1) "random_number"
  ord = 10
  call set_order(ord)
  call random_number(m)
  if (any(m < 0) .or. any(m > ord)) then
    error stop "random_number failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "All tests passed"

end program rationals_test
