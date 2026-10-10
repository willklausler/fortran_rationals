program rationals_test
!! Unit tests for [[rationals]].
!!
!! Checks constructors, conversions, operators and the overloaded
!! intrinsics against hand-computed results, and regression cases for
!! overflow near the int64 limit, real conversion and sign handling.

  use rationals
  use iso_fortran_env, only: ir => int64, ik => int32, rk => real64

  implicit none

  integer(ir), parameter :: yoog = huge(1_ir)   !! Largest int64
  integer :: nfail = 0                          !! Number of failed checks

  integer(ik) :: ord
  type(rational) :: a, b
  type(rational) :: u(2), v(2), m(2,2), n(2,2), p(2,2)

  call section("Math")
  call expect(gcf(0,2) == 2, "gcf failed for 0")
  call expect(gcf(2,3) == 1, "gcf failed for coprime")
  call expect(gcf(12,12) == 12, "gcf failed for equality")
  call expect(gcf(3,12) == 3, "gcf failed for result")
  call expect(gcf(-3,12) == 3, "gcf failed for negative input")
  call expect(lcm(0, 1) == 0, "lcm failed for zero")
  call expect(lcm(3, 3) == 3, "lcm failed for equality")
  call expect(lcm(2, 3) == 6, "lcm failed for product result")
  call expect(lcm(4, 6) == 12, "lcm failed for non-product result")
  call expect(lcm(-4, 6) == 12, "lcm failed for negative")

  call section("Constructor")
  a = rational(1, 1)
  call expect(a%get_num() == 1 .and. a%get_den() == 1, "Default constructor failed")
  a = rational(3)
  call expect(a%get_num() == 3 .and. a%get_den() == 1, "Integer constructor failed")
  a = rational(0.5_rk)
  call expect(a%get_num() == 1 .and. a%get_den() == 2, "Real constructor failed")
  a = rational("1/2")
  call expect(a%get_num() == 1 .and. a%get_den() == 2, "Character constructor failed")

  call section("Text round-trip")
  !! write(DT) emits char(); formatted DT input is compiler dependent
  a = rational(-7_ik, 12_ik)
  call expect(rational(trim(char(a))) == a, "Text round-trip: value changed through char/parse")
  call expect(char(rational(-3_ik)) == "-3", "char failed for integer")

  call section("Assignment")
  a = 2
  call expect(a%get_num() == 2 .and. a%get_den() == 1, "Integer assignment failed")
  a = 0.5_rk
  call expect(a%get_num() == 1 .and. a%get_den() == 2, "Real assignment failed")
  ! a = "1/2"   !! Not supported by Fortran
  b = a
  call expect(b%get_num() == a%get_num() .and. b%get_den() == a%get_den(), "Copying failed")

  call section("Casting")
  a = 2
  call expect(int(a) == 2, "Casting to integer failed")
  a = 0.5_rk
  call expect(real(a) == 0.5_rk, "Casting to real failed")
  call expect(trim(char(a)) == "1/2", "Casting to character failed")

  call section("Equality")
  a = 2
  b = 2
  call expect(a == b, "Rational-rational equality failed")
  call expect(a == 2, "Rational-integer equality failed")
  call expect(2 == a, "Integer-rational equality failed")

  call section("Inequality")
  a = 2
  b = 3
  call expect(a /= b, "Rational-rational inequality failed")
  call expect(a /= 3, "Rational-integer inequality failed")
  call expect(3 /= a, "Integer-rational inequality failed")

  call section("Strict superiority")
  a = rational(3,2)
  b = rational(2,3)
  call expect(a > b, "Rational-rational strict superiority failed")
  call expect(a > 1, "Rational-integer strict superiority failed")
  call expect(2 > a, "Integer-rational strict superiority failed")

  call section("Strict inferiority")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a < b, "Rational-rational strict inferiority failed")
  call expect(a < 1, "Rational-integer strict inferiority failed")
  call expect(0 < a, "Integer-rational strict inferiority failed")

  call section("Superiority")
  a = rational(3,2)
  b = rational(2,3)
  call expect(a >= b, "Rational-rational superiority failed")
  call expect(a >= 1, "Rational-integer superiority failed")
  call expect(2 >= a, "Integer-rational superiority failed")

  call section("Inferiority")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a <= b, "Rational-rational inferiority failed")
  call expect(a <= 1, "Rational-integer inferiority failed")
  call expect(0 <= a, "Integer-rational inferiority failed")

  call section("Addition")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a + b == rational(13, 6), "Rational-rational addition failed")
  call expect(a + 1 == rational(5, 3), "Rational-integer addition failed")
  call expect(1 + a == rational(5, 3), "Integer-rational addition failed")

  call section("Subtraction")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a - b == rational(-5, 6), "Rational-rational subtraction failed")
  call expect(a - 1 == rational(-1, 3), "Rational-integer subtraction failed")
  call expect(1 - a == rational(1, 3), "Integer-rational subtraction failed")

  call section("Multiplication")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a*b == rational(1, 1), "Rational-rational multiplication failed")
  call expect(a*2 == rational(4, 3), "Rational-integer multiplication failed")
  call expect(2*a == rational(4, 3), "Integer-rational multiplication failed")

  call section("Division")
  a = rational(2,3)
  b = rational(3,2)
  call expect(a/b == rational(4, 9), "Rational-rational division failed")
  call expect(a/2 == rational(1, 3), "Rational-integer division failed")
  call expect(2/a == rational(3, 1), "Integer-rational division failed")

  call section("Reciprocal")
  a = rational(2,3)
  call expect(a%inverse() == 1/a, "Reciprocation failed")

  call section("Exponentiation")
  a = rational(2,3)
  call expect(a**2 == rational(4, 9), "Rational-integer exponentiation failed")
  a = rational(2_ik, 3_ik)
  call expect(a**0 == rational(1_ik, 1_ik), "Exponentiation failed for b=0")
  call expect(a**(-2) == rational(9_ik, 4_ik), "Exponentiation failed for b=-2")
  call expect(a**2 == rational(4_ik, 9_ik), "Exponentiation regression failed for b=2")
  call expect(rational(1_ik, 1_ik)**7 == rational(1_ik, 1_ik), "Exponentiation failed for base=1")

  call section("Edge cases")
  a = rational(yoog, 2_ir)
  b = rational(2_ir, yoog)
  call expect(a*b == 1, "Multiplication overflow failed")

  call section("is_integer")
  a = rational(4_ik, 2_ik)
  call expect(a%is_integer(), "is_integer failed: 4/2 simplifies to 2, should be true")
  a = rational(1_ik, 3_ik)
  call expect(.not. a%is_integer(), "is_integer failed: 1/3 should be false")
  a = rational(0_ik, 1_ik)
  call expect(a%is_integer(), "is_integer failed: 0 should be true")

  call section("abs")
  a = rational(-1, 2)
  call expect(abs(a) == rational(1, 2), "Absolute value failed")
  a = rational(1, 2)
  call expect(abs(a) == rational(1, 2), "Absolute value failed")

  call section("min")
  a = rational(2, 3)
  b = rational(3, 2)
  call expect(min(a,b) == a, "Rational-rational minimum failed")

  call section("max")
  a = rational(2, 3)
  b = rational(3, 2)
  call expect(max(a,b) == b, "Rational-rational maximum failed")

  call section("dot_product")
  u = [rational(1, 2), rational(2, 3)]
  v = [rational(3, 4), rational(4, 5)]
  call expect(dot_product(u, v) == rational(109, 120), "dot_product failed")

  call section("matmul")
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  u = [rational(1, 4), rational(3, 4)]
  v = [rational(7, 8), rational(7, 12)]
  call expect(all(matmul(m, u) == v), "matmul for matrix-vector failed")
  n(:,1) = [rational(1, 4), rational(3, 4)]
  n(:,2) = [rational(1, 2), rational(1, 1)]
  p(:,1) = [rational(7, 8), rational(7, 12)]
  p(:,2) = [rational(5, 4), rational(5, 6)]
  call expect(all(matmul(m, n) == p), "matmul for matrix-matrix failed")

  call section("transpose")
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  n(:,1) = [rational(1, 2), rational(1, 1)]
  n(:,2) = [rational(1, 3), rational(2, 3)]
  call expect(all(transpose(m) == n), "transpose failed")

  call section("random_number")
  ord = 10
  call set_order(ord)
  call random_number(m)
  call expect(all(m >= 0) .and. all(m <= 1), "random_number failed")

  call section("floor")
  call expect(floor(rational(4_ik, 2_ik)) == 2_ir, "floor failed for exact positive integer")
  call expect(floor(rational(7_ik, 3_ik)) == 2_ir, "floor failed for positive fraction (7/3)")
  call expect(floor(rational(-7_ik, 3_ik)) == -3_ir, "floor failed for negative fraction (-7/3)")
  call expect(floor(rational(-6_ik, 3_ik)) == -2_ir, "floor failed for negative exact (-6/3)")

  call section("ceiling")
  call expect(ceiling(rational(7_ik, 3_ik)) == 3_ir, "ceiling failed for positive fraction (7/3)")
  call expect(ceiling(rational(-7_ik, 3_ik)) == -2_ir, "ceiling failed for negative fraction (-7/3)")
  call expect(ceiling(rational(4_ik, 2_ik)) == 2_ir, "ceiling failed for exact positive integer")
  call expect(ceiling(rational(-6_ik, 3_ik)) == -2_ir, "ceiling failed for negative exact integer")

  call section("trunc")
  call expect(trunc(rational(7_ik, 3_ik)) == 2_ir, "trunc failed for positive fraction (7/3)")
  call expect(trunc(rational(-7_ik, 3_ik)) == -2_ir, "trunc failed for negative fraction (-7/3)")
  call expect(trunc(rational(6_ik, 3_ik)) == 2_ir, "trunc failed for exact positive integer")
  call expect(floor(rational(-7_ik, 3_ik)) /= trunc(rational(-7_ik, 3_ik)), "floor and trunc should differ for negative non-integer")

  call section("round")
  call expect(round(rational(7_ik, 4_ik)) == 2_ir, "round failed for 7/4 (rounds up to 2)")
  call expect(round(rational(5_ik, 4_ik)) == 1_ir, "round failed for 5/4 (rounds down to 1)")
  call expect(round(rational(1_ik, 2_ik)) == 1_ir, "round failed for 1/2 tie (should round to 1)")
  call expect(round(rational(-1_ik, 2_ik)) == -1_ir, "round failed for -1/2 tie (should round to -1)")
  call expect(round(rational(4_ik, 2_ik)) == 2_ir, "round failed for exact integer")

  call section("sign")
  a = rational(3_ik, 4_ik)
  b = rational(-1_ik, 5_ik)
  !! Sign transfer: magnitude of a, sign of b
  call expect(sign(a, b) == rational(-3_ik, 4_ik), "sign failed: positive a, negative b")
  call expect(sign(a, a) == a, "sign failed: positive a, positive b")
  call expect(sign(b, a) == rational(1_ik, 5_ik), "sign failed: negative a, positive b")
  !! Zero as b: treat as non-negative (matches intrinsic convention)
  a = rational(3_ik, 4_ik)
  b = rational(0_ik, 1_ik)
  call expect(sign(a, b) == rational(3_ik, 4_ik), "sign failed: b == 0 should yield non-negative result")

  call section("compare")
  a = rational(2_ik, 3_ik)
  b = rational(3_ik, 2_ik)
  call expect(compare(a, b) == -1, "compare failed: a < b should return -1")
  call expect(compare(b, a) == 1, "compare failed: a > b should return 1")
  call expect(compare(a, a) == 0, "compare failed: a == a should return 0")
  !! Same value, different representation (relies on simplify invariant)
  call expect(compare(rational(2_ik, 4_ik), rational(1_ik, 2_ik)) == 0, "compare failed: 2/4 and 1/2 should be equal")

  call section("mod")
  !! Positive dividend and divisor
  !! 7/3 mod 1/2: trunc(7/3 / (1/2)) = trunc(14/3) = 4; rem = 7/3 - 4*(1/2) = 7/3 - 2 = 1/3
  a = rational(7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  call expect(mod(a, b) == rational(1_ik, 3_ik), "mod failed for positive a, positive b")
  !! Exact multiple: remainder is zero
  a = rational(3_ik, 2_ik)
  b = rational(1_ik, 2_ik)
  call expect(mod(a, b) == rational(0_ik, 1_ik), "mod failed: exact multiple should give zero remainder")
  !! Negative dividend: remainder has same sign as dividend (matches Fortran mod)
  a = rational(-7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  !! trunc(-14/3) = -4; rem = -7/3 - (-4)*(1/2) = -7/3 + 2 = -1/3
  call expect(mod(a, b) == rational(-1_ik, 3_ik), "mod failed for negative dividend")

  call section("divmod")
  a = rational(7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  !! Expected: q = trunc(14/3) = 4, r = 1/3
  block
    integer(ir)    :: q
    type(rational) :: r
    call divmod(a, b, q, r)
    call expect(q == 4_ir, "divmod failed: wrong quotient")
    call expect(r == rational(1_ik, 3_ik), "divmod failed: wrong remainder")
    !! Reconstruction: b*q + r == a
    call expect(b * rational(q, 1_ir) + r == a, "divmod failed: b*q + r /= a")
  end block
  !! Exact division: remainder zero, quotient exact
  a = rational(3_ik, 2_ik)
  b = rational(1_ik, 2_ik)
  block
    integer(ir)    :: q
    type(rational) :: r
    call divmod(a, b, q, r)
    call expect(q == 3_ir, "divmod failed: exact division quotient")
    call expect(r == rational(0_ik, 1_ik), "divmod failed: exact division remainder should be zero")
  end block

  call section("sum")
  !! Known result: 1/2 + 2/3 + 1/6 = 3/6 + 4/6 + 1/6 = 8/6 = 4/3
  u = [rational(1_ik, 2_ik), rational(2_ik, 3_ik)]
  call expect(sum([rational(1_ik, 2_ik), rational(2_ik, 3_ik), rational(1_ik, 6_ik)]) == rational(4_ik, 3_ik), "sum failed for three-element array")
  !! Single element: sum == element
  call expect(sum([rational(3_ik, 5_ik)]) == rational(3_ik, 5_ik), "sum failed for single-element array")
  !! Sum of empty array should be zero (additive identity)
  block
    type(rational) :: empty(0)
    call expect(sum(empty) == rational(0_ik, 1_ik), "sum failed for empty array: expected 0")
  end block

  call section("product")
  !! Known result: 1/2 * 2/3 * 3/4 = 6/24 = 1/4
  call expect(product([rational(1_ik, 2_ik), rational(2_ik, 3_ik), rational(3_ik, 4_ik)]) == rational(1_ik, 4_ik), "product failed for three-element array")
  !! Single element
  call expect(product([rational(3_ik, 5_ik)]) == rational(3_ik, 5_ik), "product failed for single-element array")
  !! Product containing zero: result is zero
  call expect(product([rational(1_ik, 2_ik), rational(0_ik, 1_ik), rational(3_ik, 4_ik)]) == rational(0_ik, 1_ik), "product failed: array containing zero should give zero")
  !! Empty array should be one (multiplicative identity)
  block
    type(rational) :: empty(0)
    call expect(product(empty) == rational(1_ik, 1_ik), "product failed for empty array: expected 1")
  end block

  call section("Regressions")
  !! Real conversion gives the simplest exact fraction
  call expect(rational(1.0_rk/3) == rational(1_ik, 3_ik), "real conversion failed for 1/3")
  call expect(rational(1.0e5_rk) == 100000, "real conversion failed for 1e5")
  call expect(rational(-2.5_rk) == rational(-5_ik, 2_ik), "real conversion failed for -2.5")
  call expect(rational("0.75") == rational(3_ik, 4_ik), "character conversion failed for 0.75")
  !! Reciprocal of a negative keeps the denominator positive
  a = rational(-2_ik, 3_ik)
  call expect(a%inverse() == rational(-3_ik, 2_ik), "inverse failed for negative")
  call expect(a**(-1) == rational(-3_ik, 2_ik), "negative power failed for negative base")
  !! int() truncates like the intrinsic
  call expect(int(rational(7_ik, 2_ik)) == 3, "int failed: should truncate 7/2 to 3")
  call expect(int(rational(-7_ik, 2_ik)) == -3, "int failed: should truncate -7/2 to -3")
  !! Comparison near the int64 limit must not overflow
  a = rational(yoog - 1, yoog)
  b = rational(yoog - 2, yoog - 1)
  call expect(a > b, "comparison failed near int64 limit")
  call expect(-a < -b, "negative comparison failed near int64 limit")
  !! Addition cancels common denominator factors before multiplying
  a = rational(1_ir, 2*10_ir**17)
  b = rational(1_ir, 3*10_ir**17)
  call expect(a + b == rational(1_ir, 12*10_ir**16), "addition failed for large common denominator")
  !! Non-square matrix-vector product
  block
    type(rational) :: r23(2,3), v3(3), w(2)
    r23 = rational(1_ik, 2_ik)
    v3 = rational(2_ik)
    w = matmul(r23, v3)
    call expect(size(w) == 2 .and. all(w == 3), "matmul failed for non-square matrix-vector")
  end block

  write(*,*)
  if (nfail > 0) then
    write(*,"(I0,A)") nfail, " check(s) failed"
    error stop 1
  end if
  write(*,"(A)") "All tests passed"

contains

!***********************************************************************

subroutine section(name)
!! Start a group of checks
  character(*), intent(in) :: name
  write(*,"(A)") name
end subroutine section

!***********************************************************************

subroutine expect(cond, msg)
!! Record a failed check
  logical, intent(in) :: cond
  character(*), intent(in) :: msg
  if (cond) return
  nfail = nfail + 1
  write(*,"(2X,'FAIL: ',A)") msg
end subroutine expect

end program rationals_test
