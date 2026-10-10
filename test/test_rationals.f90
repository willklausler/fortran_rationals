! SPDX-License-Identifier: MIT
program test_rationals
  !! Unit tests for every public procedure of module rationals, with
  !! hand-computed results and regression cases near the int64 limit.
  use, intrinsic :: iso_fortran_env, only: ir => int64, ik => int32, rk => real64
  use rationals
  implicit none

  integer(ir), parameter :: yoog = huge(1_ir)
  integer :: ncheck = 0, nfail = 0
  integer :: u, ios
  integer(ir) :: q
  character(80) :: line
  type(rational) :: a, b, c, r
  type(rational) :: u2(2), v2(2), m(2,2), n(2,2), p(2,2), r23(2,3), v3(3), w(2), empty(0)

  ! gcf and lcm --------------------------------------------------------
  call check(gcf(0, 2) == 2, 'gcf(0, 2) = 2')
  call check(gcf(2, 3) == 1, 'gcf(2, 3) = 1')
  call check(gcf(12, 12) == 12, 'gcf(12, 12) = 12')
  call check(gcf(3, 12) == 3, 'gcf(3, 12) = 3')
  call check(gcf(-3, 12) == 3, 'gcf(-3, 12) = 3')
  call check(gcf(12_ir, 18_ir) == 6_ir, 'gcf(12, 18) = 6 for int64')
  call check(gcf(rational(1, 2), rational(3, 4)) == rational(1, 4), 'gcf(1/2, 3/4) = 1/4')
  call check(gcf(rational(0), rational(3, 4)) == 0, 'gcf(0, 3/4) = 0')
  call check(lcm(0, 1) == 0, 'lcm(0, 1) = 0')
  call check(lcm(3, 3) == 3, 'lcm(3, 3) = 3')
  call check(lcm(2, 3) == 6, 'lcm(2, 3) = 6')
  call check(lcm(4, 6) == 12, 'lcm(4, 6) = 12')
  call check(lcm(-4, 6) == 12, 'lcm(-4, 6) = 12')
  call check(lcm(4_ir, 6_ir) == 12_ir, 'lcm(4, 6) = 12 for int64')

  ! Constructors and accessors -----------------------------------------
  a = rational(1, 1)
  call check(a%get_num() == 1 .and. a%get_den() == 1, 'rational(1, 1) = 1/1')
  a = rational(3)
  call check(a%get_num() == 3 .and. a%get_den() == 1, 'rational(3) = 3/1')
  a = rational(0.5_rk)
  call check(a%get_num() == 1 .and. a%get_den() == 2, 'rational(0.5) = 1/2')
  a = rational('1/2')
  call check(a%get_num() == 1 .and. a%get_den() == 2, 'rational("1/2") = 1/2')
  a = rational(6, -4)
  call check(a%get_num() == -3 .and. a%get_den() == 2, 'rational(6, -4) = -3/2, lowest terms')
  a = rational(4_ir, 6_ir)
  call check(a%get_num() == 2 .and. a%get_den() == 3, 'rational(4, 6) = 2/3 for int64')
  a = rational(0, -5)
  call check(a%get_num() == 0 .and. a%get_den() == 1, 'rational(0, -5) = 0/1')
  call a%simplify()
  call check(a%get_num() == 0 .and. a%get_den() == 1, 'simplify keeps 0/1')
  a = rational(-2, 3)
  call a%simplify()
  call check(a == rational(-2, 3), 'simplify keeps a value in lowest terms')

  ! Text ---------------------------------------------------------------
  a = rational(-7_ik, 12_ik)
  call check(rational(trim(char(a))) == a, 'rational(char(-7/12)) = -7/12')
  call check(char(rational(-3_ik)) == '-3', 'char(-3) = "-3"')
  a = 0.5_rk
  call check(trim(char(a)) == '1/2', 'char(1/2) = "1/2"')
  call check(rational(' -2 ') == -2, 'rational(" -2 ") = -2')
  call check(rational('1e-3') == rational(1, 1000), 'rational("1e-3") = 1/1000')
  open(newunit=u, status='scratch')
  write(u, '(dt, 1x, dt, a)') rational(1, 2), rational(-3, 4), ' end'
  rewind(u)
  read(u, '(a)') line
  close(u)
  call check(line == '1/2 -3/4 end', 'write(dt) gives char() text')
  open(newunit=u, status='scratch')
  write(u, '(a)') '1/2 -3/4 end'
  rewind(u)
  ! nvfortran 26.3 fails list-directed derived-type reads, so read with dt
  read(u, '(dt, dt)') a, b
  close(u)
  call check(a == rational(1, 2) .and. b == rational(-3, 4), 'read(dt) of 1/2 -3/4')
  open(newunit=u, status='scratch')
  write(u, *) rational(5, 6), rational(-7), ' end'   ! ifx writes no blank before 'end'
  rewind(u)
  read(u, '(dt, dt)') a, b
  close(u)
  call check(a == rational(5, 6) .and. b == -7, 'list-directed write separates items')
  open(newunit=u, status='scratch')
  write(u, '(a)') 'x/y end'
  rewind(u)
  read(u, *, iostat=ios) a
  close(u)
  call check(ios > 0, 'read of x/y is an error')

  ! Assignment ---------------------------------------------------------
  a = 2
  call check(a%get_num() == 2 .and. a%get_den() == 1, 'a = 2 gives 2/1')
  a = 0.5_rk
  call check(a%get_num() == 1 .and. a%get_den() == 2, 'a = 0.5 gives 1/2')
  b = a
  call check(b%get_num() == a%get_num() .and. b%get_den() == a%get_den(), 'b = a copies')
  u2 = [rational(1, 2), rational(1, 3)]
  u2 = u2 - u2(1)*u2
  call check(all(u2 == [rational(1, 4), rational(1, 6)]), &
    'array assignment that reads its own elements')

  ! Conversions --------------------------------------------------------
  a = 2
  call check(int(a) == 2, 'int(2) = 2')
  a = 0.5_rk
  call check(real(a) == 0.5_rk, 'real(1/2) = 0.5')
  call check(int(rational(7_ik, 2_ik)) == 3, 'int(7/2) = 3')
  call check(int(rational(-7_ik, 2_ik)) == -3, 'int(-7/2) = -3, truncated')
  call check(rational(1.0_rk/3) == rational(1_ik, 3_ik), 'rational(1._dp/3) = 1/3')
  call check(rational(1.0e5_rk) == 100000, 'rational(1e5) = 100000')
  call check(rational(-2.5_rk) == rational(-5_ik, 2_ik), 'rational(-2.5) = -5/2')
  call check(rational('0.75') == rational(3_ik, 4_ik), 'rational("0.75") = 3/4')

  ! Comparison ---------------------------------------------------------
  a = 2
  b = 2
  call check(a == b, 'rational == rational')
  call check(a == 2, 'rational == integer')
  call check(2 == a, 'integer == rational')
  b = 3
  call check(a /= b, 'rational /= rational')
  call check(a /= 3, 'rational /= integer')
  call check(3 /= a, 'integer /= rational')
  a = rational(3, 2)
  b = rational(2, 3)
  call check(a > b, '3/2 > 2/3')
  call check(a > 1, '3/2 > 1')
  call check(2 > a, '2 > 3/2')
  call check(a >= b, '3/2 >= 2/3')
  call check(a >= 1, '3/2 >= 1')
  call check(2 >= a, '2 >= 3/2')
  call check(b < a, '2/3 < 3/2')
  call check(b < 1, '2/3 < 1')
  call check(0 < b, '0 < 2/3')
  call check(b <= a, '2/3 <= 3/2')
  call check(b <= 1, '2/3 <= 1')
  call check(0 <= b, '0 <= 2/3')
  call check(compare(b, a) == -1, 'compare(2/3, 3/2) = -1')
  call check(compare(a, b) == 1, 'compare(3/2, 2/3) = 1')
  call check(compare(b, b) == 0, 'compare(2/3, 2/3) = 0')
  call check(compare(rational(2_ik, 4_ik), rational(1_ik, 2_ik)) == 0, 'compare(2/4, 1/2) = 0')
  a = rational(yoog - 1, yoog)
  b = rational(yoog - 2, yoog - 1)
  call check(a > b, 'comparison near the int64 limit')
  call check(-a < -b, 'negative comparison near the int64 limit')

  ! Arithmetic ---------------------------------------------------------
  a = rational(2, 3)
  b = rational(3, 2)
  call check(+a == a, '+(2/3) = 2/3')
  call check(-a == rational(-2, 3), '-(2/3) = -2/3')
  call check(a + b == rational(13, 6), '2/3 + 3/2 = 13/6')
  call check(a + 1 == rational(5, 3), '2/3 + 1 = 5/3')
  call check(1 + a == rational(5, 3), '1 + 2/3 = 5/3')
  call check(a - b == rational(-5, 6), '2/3 - 3/2 = -5/6')
  call check(a - 1 == rational(-1, 3), '2/3 - 1 = -1/3')
  call check(1 - a == rational(1, 3), '1 - 2/3 = 1/3')
  call check(a*b == rational(1, 1), '2/3 * 3/2 = 1')
  call check(a*2 == rational(4, 3), '2/3 * 2 = 4/3')
  call check(2*a == rational(4, 3), '2 * 2/3 = 4/3')
  call check(a/b == rational(4, 9), '(2/3) / (3/2) = 4/9')
  call check(a/2 == rational(1, 3), '(2/3) / 2 = 1/3')
  call check(2/a == rational(3, 1), '2 / (2/3) = 3')
  call check(a%inverse() == 1/a, 'inverse(2/3) = 1/(2/3)')
  call check(a**2 == rational(4, 9), '(2/3)**2 = 4/9')
  call check(a**0 == rational(1_ik, 1_ik), '(2/3)**0 = 1')
  call check(a**(-2) == rational(9_ik, 4_ik), '(2/3)**(-2) = 9/4')
  call check(rational(1_ik, 1_ik)**7 == rational(1_ik, 1_ik), '1**7 = 1')
  a = rational(-2_ik, 3_ik)
  call check(a%inverse() == rational(-3_ik, 2_ik), 'inverse(-2/3) = -3/2')
  call check(a**(-1) == rational(-3_ik, 2_ik), '(-2/3)**(-1) = -3/2')
  a = rational(yoog, 2_ir)
  b = rational(2_ir, yoog)
  call check(a*b == 1, 'huge/2 * 2/huge = 1 without overflow')
  a = rational(1_ir, 2*10_ir**17)
  b = rational(1_ir, 3*10_ir**17)
  call check(a + b == rational(1_ir, 12*10_ir**16), 'sum with large common denominator factor')

  ! is_integer ---------------------------------------------------------
  a = rational(4, 2)
  call check(a%is_integer(), 'is_integer(4/2)')
  a = rational(1, 3)
  call check(.not. a%is_integer(), '.not. is_integer(1/3)')
  a = 0
  call check(a%is_integer(), 'is_integer(0)')

  ! abs, sign, min, max ------------------------------------------------
  call check(abs(rational(-1, 2)) == rational(1, 2), 'abs(-1/2) = 1/2')
  call check(abs(rational(1, 2)) == rational(1, 2), 'abs(1/2) = 1/2')
  a = rational(3_ik, 4_ik)
  b = rational(-1_ik, 5_ik)
  call check(sign(a, b) == rational(-3_ik, 4_ik), 'sign(3/4, -1/5) = -3/4')
  call check(sign(a, a) == a, 'sign(3/4, 3/4) = 3/4')
  call check(sign(b, a) == rational(1_ik, 5_ik), 'sign(-1/5, 3/4) = 1/5')
  call check(sign(a, rational(0)) == a, 'sign(3/4, 0) = 3/4, like the intrinsic')
  a = rational(2, 3)
  b = rational(3, 2)
  call check(min(a, b) == a, 'min(2/3, 3/2) = 2/3')
  call check(max(a, b) == b, 'max(2/3, 3/2) = 3/2')

  ! Rounding -----------------------------------------------------------
  call check(floor(rational(4_ik, 2_ik)) == 2_ir, 'floor(4/2) = 2')
  call check(floor(rational(7_ik, 3_ik)) == 2_ir, 'floor(7/3) = 2')
  call check(floor(rational(-7_ik, 3_ik)) == -3_ir, 'floor(-7/3) = -3')
  call check(floor(rational(-6_ik, 3_ik)) == -2_ir, 'floor(-6/3) = -2')
  call check(ceiling(rational(7_ik, 3_ik)) == 3_ir, 'ceiling(7/3) = 3')
  call check(ceiling(rational(-7_ik, 3_ik)) == -2_ir, 'ceiling(-7/3) = -2')
  call check(ceiling(rational(4_ik, 2_ik)) == 2_ir, 'ceiling(4/2) = 2')
  call check(ceiling(rational(-6_ik, 3_ik)) == -2_ir, 'ceiling(-6/3) = -2')
  call check(trunc(rational(7_ik, 3_ik)) == 2_ir, 'trunc(7/3) = 2')
  call check(trunc(rational(-7_ik, 3_ik)) == -2_ir, 'trunc(-7/3) = -2')
  call check(trunc(rational(6_ik, 3_ik)) == 2_ir, 'trunc(6/3) = 2')
  call check(floor(rational(-7_ik, 3_ik)) /= trunc(rational(-7_ik, 3_ik)), &
    'floor(-7/3) /= trunc(-7/3)')
  call check(round(rational(7_ik, 4_ik)) == 2_ir, 'round(7/4) = 2')
  call check(round(rational(5_ik, 4_ik)) == 1_ir, 'round(5/4) = 1')
  call check(round(rational(1_ik, 2_ik)) == 1_ir, 'round(1/2) = 1, ties away from zero')
  call check(round(rational(-1_ik, 2_ik)) == -1_ir, 'round(-1/2) = -1, ties away from zero')
  call check(round(rational(4_ik, 2_ik)) == 2_ir, 'round(4/2) = 2')

  ! mod and divmod -----------------------------------------------------
  call check(mod(rational(7, 3), rational(1, 2)) == rational(1, 3), 'mod(7/3, 1/2) = 1/3')
  call check(mod(rational(3, 2), rational(1, 2)) == 0, 'mod(3/2, 1/2) = 0')
  call check(mod(rational(-7, 3), rational(1, 2)) == rational(-1, 3), &
    'mod(-7/3, 1/2) = -1/3, sign of a')
  a = rational(7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  call divmod(a, b, q, r)
  call check(q == 4_ir, 'divmod(7/3, 1/2) quotient = 4')
  call check(r == rational(1_ik, 3_ik), 'divmod(7/3, 1/2) remainder = 1/3')
  call check(b*rational(q, 1_ir) + r == a, 'divmod: b*q + r = a')
  call divmod(rational(3, 2), rational(1, 2), q, r)
  call check(q == 3_ir .and. r == 0, 'divmod(3/2, 1/2) = 3, 0')

  ! Array intrinsics ---------------------------------------------------
  u2 = [rational(1, 2), rational(2, 3)]
  v2 = [rational(3, 4), rational(4, 5)]
  call check(dot_product(u2, v2) == rational(109, 120), 'dot_product = 109/120')
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  u2 = [rational(1, 4), rational(3, 4)]
  v2 = [rational(7, 8), rational(7, 12)]
  call check(all(matmul(m, u2) == v2), 'matmul, matrix-vector')
  n(:,1) = [rational(1, 4), rational(3, 4)]
  n(:,2) = [rational(1, 2), rational(1, 1)]
  p(:,1) = [rational(7, 8), rational(7, 12)]
  p(:,2) = [rational(5, 4), rational(5, 6)]
  call check(all(matmul(m, n) == p), 'matmul, matrix-matrix')
  r23 = rational(1_ik, 2_ik)
  v3 = rational(2_ik)
  w = matmul(r23, v3)
  call check(size(w) == 2 .and. all(w == 3), 'matmul, non-square matrix-vector')
  n(:,1) = [rational(1, 2), rational(1, 1)]
  n(:,2) = [rational(1, 3), rational(2, 3)]
  call check(all(transpose(m) == n), 'transpose')
  call check(sum([rational(1, 2), rational(2, 3), rational(1, 6)]) == rational(4, 3), &
    'sum([1/2, 2/3, 1/6]) = 4/3')
  call check(sum([rational(3, 5)]) == rational(3, 5), 'sum([3/5]) = 3/5')
  call check(sum(empty) == 0, 'sum of empty array = 0')
  call check(product([rational(1, 2), rational(2, 3), rational(3, 4)]) == rational(1, 4), &
    'product([1/2, 2/3, 3/4]) = 1/4')
  call check(product([rational(3, 5)]) == rational(3, 5), 'product([3/5]) = 3/5')
  call check(product([rational(1, 2), rational(0), rational(3, 4)]) == 0, &
    'product with a zero element = 0')
  call check(product(empty) == 1, 'product of empty array = 1')

  ! random_number and set_order ----------------------------------------
  call set_order(10)
  call random_number(m)
  call check(all(m >= 0) .and. all(m <= 1), 'random_number in [0, 1]')
  call check(all(mod(10_ir, m%get_den()) == 0), 'random_number denominators divide 10')
  call set_order(7)
  call random_number(c)
  call check(c >= 0 .and. c <= 1 .and. mod(7_ir, c%get_den()) == 0, 'set_order(7)')

  print '(i0, a, i0, a)', ncheck - nfail, ' of ', ncheck, ' checks passed'
  if (nfail > 0) error stop 1

contains

subroutine check(cond, name)
  logical, intent(in) :: cond
  character(*), intent(in) :: name
  ncheck = ncheck + 1
  if (.not. cond) then
    nfail = nfail + 1
    print '(a)', 'FAIL: ' // name
  end if
end subroutine check

end program test_rationals
