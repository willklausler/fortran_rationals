program rationals_test
!! Showcase rationals

  use rationals
  use iso_fortran_env, only: ir => int64, ik => int32, rk => real64

  implicit none

  integer(ir), parameter :: yoog = huge(1_ir)

  integer(ik) :: ord

  character(*), parameter :: fmt1 = "(A20,': ')"
  character(*), parameter :: fmt2 = "('passed')"
  character(50) :: str

  type(rational) :: a, b

  type(rational) :: u(2), v(2), m(2,2), n(2,2), p(2,2)

  str = "1/2  "

  write(*,"(A)") "--- Testing rationals ---"

  write(*,"(A)") "Basics"

  write(*,fmt1,advance='no') "Math"
  if (gcf(0,2) /= 2) then
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

  write(*,fmt1,advance='no') "Constructor"
  a = rational(1, 1)
  if ((a%get_num() /= 1) .or. (a%get_den() /= 1)) then
    error stop "Default constructor failed"
  end if
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

  write(*,fmt1,advance='no') "Text round-trip"
  !! write(DT) emits char(); formatted DT input is compiler dependent
  a = rational(-7_ik, 12_ik)
  if (rational(trim(char(a))) /= a) then
    error stop "Text round-trip: value changed through char/parse"
  end if
  if (char(rational(-3_ik)) /= "-3") then
    error stop "char failed for integer"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "Assignment"
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

  write(*,fmt1,advance='no') "Casting"
  a = 2
  if (int(a) /= 2) then
    error stop "Casting to integer failed"
  end if
  a = 0.5_rk
  if (real(a) /= 0.5_rk) then
    error stop "Casting to real failed"
  end if
  if (trim(char(a)) /= "1/2") then
    error stop "Casting to character failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "Boolean operators"

  write(*,fmt1,advance='no') "Equality"
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

  write(*,fmt1,advance='no') "Inequality"
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

  write(*,fmt1,advance='no') "Strict superiority"
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

  write(*,fmt1,advance='no') "Strict inferiority"
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

  write(*,fmt1,advance='no') "Superiority"
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

  write(*,fmt1,advance='no') "Inferiority"
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

  write(*,fmt1,advance='no') "Addition"
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

  write(*,fmt1,advance='no') "Subtraction"
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

  write(*,fmt1,advance='no') "Multiplication"
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

  write(*,fmt1,advance='no') "Division"
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

  write(*,fmt1,advance='no') "Reciprocal"
  a = rational(2,3)
  if (a%inverse() /= 1/a) then
    error stop "Reciprocation failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "Exponentiation"
  a = rational(2,3)
  if (a**2 /= rational(4, 9)) then
    error stop "Rational-integer exponentiation failed"
  end if
  a = rational(2_ik, 3_ik)
  if (a**0 /= rational(1_ik, 1_ik)) then
    error stop "Exponentiation failed for b=0"
  end if
  if (a**(-2) /= rational(9_ik, 4_ik)) then
    error stop "Exponentiation failed for b=-2"
  end if
  if (a**2 /= rational(4_ik, 9_ik)) then
    error stop "Exponentiation regression failed for b=2"
  end if
  if (rational(1_ik, 1_ik)**7 /= rational(1_ik, 1_ik)) then
    error stop "Exponentiation failed for base=1"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "Edge cases"
  a = rational(yoog, 2_ir)
  b = rational(2_ir, yoog)
  if (a*b /= 1) then
    error stop "Multiplication overflow failed"
  end if
  write(*,fmt2)

  write(*,"(A)") "Functions"

  write(*,fmt1,advance='no') "is_integer"
  a = rational(4_ik, 2_ik)
  if (.not. a%is_integer()) then
    error stop "is_integer failed: 4/2 simplifies to 2, should be true"
  end if
  a = rational(1_ik, 3_ik)
  if (a%is_integer()) then
    error stop "is_integer failed: 1/3 should be false"
  end if
  a = rational(0_ik, 1_ik)
  if (.not. a%is_integer()) then
    error stop "is_integer failed: 0 should be true"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "abs"
  a = rational(-1, 2)
  if (abs(a) /= rational(1, 2)) then
    error stop "Absolute value failed"
  end if
  a = rational(1, 2)
  if (abs(a) /= rational(1, 2)) then
    error stop "Absolute value failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "min"
  a = rational(2, 3)
  b = rational(3, 2)
  if (min(a,b) /= a) then
    error stop "Rational-rational minimum failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "max"
  a = rational(2, 3)
  b = rational(3, 2)
  if (max(a,b) /= b) then
    error stop "Rational-rational maximum failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "dot_product"
  u = [rational(1, 2), rational(2, 3)]
  v = [rational(3, 4), rational(4, 5)]
  if (dot_product(u, v) /= rational(109, 120)) then
    error stop "dot_product failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "matmul"
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

  write(*,fmt1,advance='no') "transpose"
  m(:,1) = [rational(1, 2), rational(1, 3)]
  m(:,2) = [rational(1, 1), rational(2, 3)]
  n(:,1) = [rational(1, 2), rational(1, 1)]
  n(:,2) = [rational(1, 3), rational(2, 3)]
  if (.not.all(transpose(m) == n)) then
    error stop "transpose failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "random_number"
  ord = 10
  call set_order(ord)
  call random_number(m)
  if (any(m < 0) .or. any(m > ord)) then
    error stop "random_number failed"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "floor"
  if (floor(rational(4_ik, 2_ik)) /= 2_ir) then
    error stop "floor failed for exact positive integer"
  end if
  if (floor(rational(7_ik, 3_ik)) /= 2_ir) then
    error stop "floor failed for positive fraction (7/3)"
  end if
  if (floor(rational(-7_ik, 3_ik)) /= -3_ir) then
    error stop "floor failed for negative fraction (-7/3)"
  end if
  if (floor(rational(-6_ik, 3_ik)) /= -2_ir) then
    error stop "floor failed for negative exact (-6/3)"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "ceiling"
  if (ceiling(rational(7_ik, 3_ik)) /= 3_ir) then
    error stop "ceiling failed for positive fraction (7/3)"
  end if
  if (ceiling(rational(-7_ik, 3_ik)) /= -2_ir) then
    error stop "ceiling failed for negative fraction (-7/3)"
  end if
  if (ceiling(rational(4_ik, 2_ik)) /= 2_ir) then
    error stop "ceiling failed for exact positive integer"
  end if
  if (ceiling(rational(-6_ik, 3_ik)) /= -2_ir) then
    error stop "ceiling failed for negative exact integer"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "trunc"
  if (trunc(rational(7_ik, 3_ik)) /= 2_ir) then
    error stop "trunc failed for positive fraction (7/3)"
  end if
  if (trunc(rational(-7_ik, 3_ik)) /= -2_ir) then
    error stop "trunc failed for negative fraction (-7/3)"
  end if
  if (trunc(rational(6_ik, 3_ik)) /= 2_ir) then
    error stop "trunc failed for exact positive integer"
  end if
  if (floor(rational(-7_ik, 3_ik)) == trunc(rational(-7_ik, 3_ik))) then
    error stop "floor and trunc should differ for negative non-integer"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "round"
  if (round(rational(7_ik, 4_ik)) /= 2_ir) then
    error stop "round failed for 7/4 (rounds up to 2)"
  end if
  if (round(rational(5_ik, 4_ik)) /= 1_ir) then
    error stop "round failed for 5/4 (rounds down to 1)"
  end if
  if (round(rational(1_ik, 2_ik)) /= 1_ir) then
    error stop "round failed for 1/2 tie (should round to 1)"
  end if
  if (round(rational(-1_ik, 2_ik)) /= -1_ir) then
    error stop "round failed for -1/2 tie (should round to -1)"
  end if
  if (round(rational(4_ik, 2_ik)) /= 2_ir) then
    error stop "round failed for exact integer"
  end if
  write(*,fmt2)

  write(*,fmt1,advance='no') "sign"
  a = rational(3_ik, 4_ik)
  b = rational(-1_ik, 5_ik)
  !! Sign transfer: magnitude of a, sign of b
  if (sign(a, b) /= rational(-3_ik, 4_ik)) then
    error stop "sign failed: positive a, negative b"
  end if
  if (sign(a, a) /= a) then
    error stop "sign failed: positive a, positive b"
  end if
  if (sign(b, a) /= rational(1_ik, 5_ik)) then
    error stop "sign failed: negative a, positive b"
  end if
  !! Zero as b: treat as non-negative (matches intrinsic convention)
  a = rational(3_ik, 4_ik)
  b = rational(0_ik, 1_ik)
  if (sign(a, b) /= rational(3_ik, 4_ik)) then
    error stop "sign failed: b == 0 should yield non-negative result"
  end if
  write(*,fmt2)

 write(*,fmt1,advance='no') "compare"
  a = rational(2_ik, 3_ik)
  b = rational(3_ik, 2_ik)
  if (compare(a, b) /= -1) &
    error stop "compare failed: a < b should return -1"
  if (compare(b, a) /=  1) &
    error stop "compare failed: a > b should return 1"
  if (compare(a, a) /=  0) &
    error stop "compare failed: a == a should return 0"
  !! Same value, different representation (relies on simplify invariant)
  if (compare(rational(2_ik, 4_ik), rational(1_ik, 2_ik)) /= 0) &
    error stop "compare failed: 2/4 and 1/2 should be equal"
  write(*,fmt2)

  !! -----------------------------------------------------------
  write(*,fmt1,advance='no') "mod"
  !! Positive dividend and divisor
  !! 7/3 mod 1/2: trunc(7/3 / (1/2)) = trunc(14/3) = 4; rem = 7/3 - 4*(1/2) = 7/3 - 2 = 1/3
  a = rational(7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  if (mod(a, b) /= rational(1_ik, 3_ik)) &
    error stop "mod failed for positive a, positive b"
  !! Exact multiple: remainder is zero
  a = rational(3_ik, 2_ik)
  b = rational(1_ik, 2_ik)
  if (mod(a, b) /= rational(0_ik, 1_ik)) &
    error stop "mod failed: exact multiple should give zero remainder"
  !! Negative dividend: remainder has same sign as dividend (matches Fortran mod)
  a = rational(-7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  !! trunc(-14/3) = -4; rem = -7/3 - (-4)*(1/2) = -7/3 + 2 = -1/3
  if (mod(a, b) /= rational(-1_ik, 3_ik)) &
    error stop "mod failed for negative dividend"
  write(*,fmt2)

  !! -----------------------------------------------------------
  write(*,fmt1,advance='no') "divmod"
  a = rational(7_ik, 3_ik)
  b = rational(1_ik, 2_ik)
  !! Expected: q = trunc(14/3) = 4, r = 1/3
  block
    integer(ir)    :: q
    type(rational) :: r
    call divmod(a, b, q, r)
    if (q /= 4_ir) &
      error stop "divmod failed: wrong quotient"
    if (r /= rational(1_ik, 3_ik)) &
      error stop "divmod failed: wrong remainder"
    !! Reconstruction: b*q + r == a
    if (b * rational(q, 1_ir) + r /= a) &
      error stop "divmod failed: b*q + r /= a"
  end block
  !! Exact division: remainder zero, quotient exact
  a = rational(3_ik, 2_ik)
  b = rational(1_ik, 2_ik)
  block
    integer(ir)    :: q
    type(rational) :: r
    call divmod(a, b, q, r)
    if (q /= 3_ir) &
      error stop "divmod failed: exact division quotient"
    if (r /= rational(0_ik, 1_ik)) &
      error stop "divmod failed: exact division remainder should be zero"
  end block
  write(*,fmt2)

  !! -----------------------------------------------------------
  write(*,fmt1,advance='no') "sum"
  !! Known result: 1/2 + 2/3 + 1/6 = 3/6 + 4/6 + 1/6 = 8/6 = 4/3
  u = [rational(1_ik, 2_ik), rational(2_ik, 3_ik)]
  if (sum([rational(1_ik, 2_ik), rational(2_ik, 3_ik), rational(1_ik, 6_ik)]) &
      /= rational(4_ik, 3_ik)) &
    error stop "sum failed for three-element array"
  !! Single element: sum == element
  if (sum([rational(3_ik, 5_ik)]) /= rational(3_ik, 5_ik)) &
    error stop "sum failed for single-element array"
  !! Sum of empty array should be zero (additive identity)
  block
    type(rational) :: empty(0)
    if (sum(empty) /= rational(0_ik, 1_ik)) &
      error stop "sum failed for empty array: expected 0"
  end block
  write(*,fmt2)

  !! -----------------------------------------------------------
  write(*,fmt1,advance='no') "product"
  !! Known result: 1/2 * 2/3 * 3/4 = 6/24 = 1/4
  if (product([rational(1_ik, 2_ik), rational(2_ik, 3_ik), rational(3_ik, 4_ik)]) &
      /= rational(1_ik, 4_ik)) &
    error stop "product failed for three-element array"
  !! Single element
  if (product([rational(3_ik, 5_ik)]) /= rational(3_ik, 5_ik)) &
    error stop "product failed for single-element array"
  !! Product containing zero: result is zero
  if (product([rational(1_ik, 2_ik), rational(0_ik, 1_ik), rational(3_ik, 4_ik)]) &
      /= rational(0_ik, 1_ik)) &
    error stop "product failed: array containing zero should give zero"
  !! Empty array should be one (multiplicative identity)
  block
    type(rational) :: empty(0)
    if (product(empty) /= rational(1_ik, 1_ik)) &
      error stop "product failed for empty array: expected 1"
  end block
  write(*,fmt2)

  !! -----------------------------------------------------------
  write(*,fmt1,advance='no') "Regressions"
  !! Real conversion gives the simplest exact fraction
  if (rational(1.0_rk/3) /= rational(1_ik, 3_ik)) &
    error stop "real conversion failed for 1/3"
  if (rational(1.0e5_rk) /= 100000) &
    error stop "real conversion failed for 1e5"
  if (rational(-2.5_rk) /= rational(-5_ik, 2_ik)) &
    error stop "real conversion failed for -2.5"
  if (rational("0.75") /= rational(3_ik, 4_ik)) &
    error stop "character conversion failed for 0.75"
  !! Reciprocal of a negative keeps the denominator positive
  a = rational(-2_ik, 3_ik)
  if (a%inverse() /= rational(-3_ik, 2_ik)) &
    error stop "inverse failed for negative"
  if (a**(-1) /= rational(-3_ik, 2_ik)) &
    error stop "negative power failed for negative base"
  !! int() truncates like the intrinsic
  if (int(rational(7_ik, 2_ik)) /= 3) &
    error stop "int failed: should truncate 7/2 to 3"
  if (int(rational(-7_ik, 2_ik)) /= -3) &
    error stop "int failed: should truncate -7/2 to -3"
  !! Comparison near the int64 limit must not overflow
  a = rational(yoog - 1, yoog)
  b = rational(yoog - 2, yoog - 1)
  if (.not. (a > b)) &
    error stop "comparison failed near int64 limit"
  if (.not. (-a < -b)) &
    error stop "negative comparison failed near int64 limit"
  !! Addition cancels common denominator factors before multiplying
  a = rational(1_ir, 2*10_ir**17)
  b = rational(1_ir, 3*10_ir**17)
  if (a + b /= rational(1_ir, 12*10_ir**16)) &
    error stop "addition failed for large common denominator"
  !! Non-square matrix-vector product
  block
    type(rational) :: r23(2,3), v3(3), w(2)
    r23 = rational(1_ik, 2_ik)
    v3 = rational(2_ik)
    w = matmul(r23, v3)
    if (size(w) /= 2 .or. any(w /= 3)) &
      error stop "matmul failed for non-square matrix-vector"
  end block
  write(*,fmt2)

  write(*,"(A)") "All tests passed"

end program rationals_test
