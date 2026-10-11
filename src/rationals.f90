! SPDX-License-Identifier: MIT
module rationals
!! Rational numbers as a derived type with exact arithmetic.
!!
!! A [[rational]] holds a 64-bit numerator and denominator, always in
!! lowest terms with a positive denominator, so equal values have equal
!! representations.
!!
!! ## Conventions
!!
!! The components are private; read them with `get_num` and `get_den`.
!! `==` is exact: it compares numerator and denominator. Operators mix a
!! rational with `int32` integers; construct `int64` values with
!! `rational(n, d)`. Kinds are not exported.
!!
!! ## Checked arithmetic
!!
!! An operation whose exact result does not fit in 64 bits stops with
!! `error stop` and a message instead of returning a wrong value. Division
!! by zero and unparsable text also stop. Addition and multiplication
!! cancel common factors before multiplying [1], and comparisons use
!! continued fractions, so intermediate results stay as small as possible.
!!
!! ## Conversions
!!
!! A real converts to the simplest fraction that converts back to exactly
!! the same real, found from its continued-fraction convergents [1].
!! Text may be `"n"`, `"n/d"` or a decimal real such as `"0.75"`.
!!
!! ## References
!!
!! 1. Knuth, D. E. (1997). *The Art of Computer Programming*, Vol. 2:
!!    *Seminumerical Algorithms*, 3rd ed., §4.5.1 and §4.5.3.
!!    Addison-Wesley.

  use, intrinsic :: iso_fortran_env, only: ir => int64, ik => int32, rk => real64

  implicit none

  private

  ! Type and constructor
  public :: rational
  ! Operators
  public :: assignment(=)
  public :: operator(+), operator(-), operator(*), operator(/), operator(**)
  public :: operator(==), operator(/=), operator(>), operator(<), operator(>=), operator(<=)
  ! Intrinsic overloads
  public :: char, int, real, abs, sign, min, max, mod, floor, ceiling
  public :: sum, product, dot_product, matmul, transpose, random_number
  ! Package functions
  public :: trunc, round, compare, divmod, gcf, lcm, set_order

  integer(ik), parameter :: rat_len = 41
  !! Maximum length of a rational as text, "-numerator/denominator"

  integer(ik) :: order = 10
  !! Denominator used by random_number; module state, changed only by set_order

  type :: rational
    !! Rational number num/den in lowest terms with den > 0
    private
    integer(ir) :: num = 0   !! Numerator
    integer(ir) :: den = 1   !! Denominator
  contains
    procedure :: simplify
    procedure :: get_num, get_den
    procedure :: is_integer
    procedure :: inverse
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
    procedure :: read_formatted
    generic :: read(formatted) => read_formatted
  end type rational

  ! rational = rational is intrinsic assignment: nvfortran 26.3 passes an
  ! overlapping right-hand side to an elemental defined assignment without a copy
  interface assignment(=)
  !! Assign an int32 or a real64 to a rational
    procedure :: rat_set_int, rat_set_real
  end interface assignment(=)

  interface operator(+)
  !! Addition, and unary plus
    procedure :: rat_add_rat, rat_add_int, int_add_rat, unary_plus
  end interface operator(+)

  interface operator(-)
  !! Subtraction, and negation
    procedure :: rat_sub_rat, rat_sub_int, int_sub_rat, negate
  end interface operator(-)

  interface operator(*)
  !! Multiplication
    procedure :: rat_mul_rat, rat_mul_int, int_mul_rat
  end interface operator(*)

  interface operator(/)
  !! Division; stops on division by zero
    procedure :: rat_div_rat, rat_div_int, int_div_rat
  end interface operator(/)

  interface operator(**)
  !! Integer power
    procedure :: rat_power_int
  end interface operator(**)

  interface operator(==)
  !! Equality
    procedure :: rat_eq_rat, rat_eq_int, int_eq_rat
  end interface operator(==)

  interface operator(/=)
  !! Inequality
    procedure :: rat_ne_rat, rat_ne_int, int_ne_rat
  end interface operator(/=)

  interface operator(>)
  !! Greater than
    procedure :: rat_gt_rat, rat_gt_int, int_gt_rat
  end interface operator(>)

  interface operator(<)
  !! Less than
    procedure :: rat_lt_rat, rat_lt_int, int_lt_rat
  end interface operator(<)

  interface operator(>=)
  !! Greater than or equal
    procedure :: rat_ge_rat, rat_ge_int, int_ge_rat
  end interface operator(>=)

  interface operator(<=)
  !! Less than or equal
    procedure :: rat_le_rat, rat_le_int, int_le_rat
  end interface operator(<=)

  interface rational
  !! Construct from numerator and denominator, an integer, a real,
  !! or text such as "3/4", "-2" or "0.75"
    procedure :: rat_const_real, rat_const_char, rat_const_int
    procedure :: rat_const_ik, rat_const_ir
  end interface rational

  interface char
  !! Text form: "n/d", or "n" for an integer
    procedure :: rat_str
  end interface char

  interface int
  !! Integer part, truncated toward zero like the intrinsic
    procedure :: rat_int
  end interface int

  interface real
  !! Nearest real(real64)
    procedure :: rat_real
  end interface real

  interface gcf
  !! Greatest common factor
    procedure :: gcf_ir, gcf_ik, gcf_rat
  end interface gcf

  interface lcm
  !! Least common multiple
    procedure :: lcm_ir, lcm_ik
  end interface lcm

  interface abs
  !! Absolute value
    procedure :: rat_abs
  end interface abs

  interface min
  !! Smaller of two rationals
    procedure :: rat_min
  end interface min

  interface max
  !! Larger of two rationals
    procedure :: rat_max
  end interface max

  interface dot_product
  !! Dot product of rational vectors
    procedure :: rat_dot
  end interface dot_product

  interface matmul
  !! Matrix-matrix and matrix-vector product
    procedure :: rat_matmul
    procedure :: rat_matvec
  end interface matmul

  interface transpose
  !! Matrix transpose
    procedure :: rat_transpose
  end interface transpose

  interface random_number
  !! Random rational in [0, 1] with denominator dividing the order
  !! set by set_order
    procedure :: rat_rand
  end interface random_number

  interface sum
  !! Sum of a rational vector
    procedure :: rat_sum
  end interface sum

  interface product
  !! Product of a rational vector
    procedure :: rat_product
  end interface product

  interface floor
  !! Greatest integer <= argument
    procedure :: rat_floor
  end interface floor

  interface ceiling
  !! Smallest integer >= argument
    procedure :: rat_ceil
  end interface ceiling

  interface sign
  !! Magnitude of the first argument with the sign of the second
    procedure :: rat_sign
  end interface sign

  interface trunc
  !! Integer part, truncated toward zero
    procedure :: rat_trunc
  end interface trunc

  interface round
  !! Nearest integer, ties away from zero
    procedure :: rat_round
  end interface round

  interface compare
  !! Three-way comparison: -1, 0 or 1
    procedure :: rat_compare
  end interface compare

  interface mod
  !! Remainder a - trunc(a/b)*b
    procedure :: rat_mod
  end interface mod

  interface divmod
  !! Truncated quotient and remainder together
    procedure :: rat_divmod
  end interface divmod

contains

!-----------------------------------------------------------------------
! Type-bound accessors
!-----------------------------------------------------------------------

elemental integer(ir) function get_num(self) result(num)
  !! Get rational numerator
  class(rational), intent(in) :: self
  num = self%num
end function get_num

elemental integer(ir) function get_den(self) result(den)
  !! Get rational denominator
  class(rational), intent(in) :: self
  den = self%den
end function get_den

elemental logical function is_integer(self) result(res)
  !! Check if rational is integer
  class(rational), intent(in) :: self
  res = self%den == 1
end function is_integer

!-----------------------------------------------------------------------
! Text I/O
!-----------------------------------------------------------------------

subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
  !! Write formatted rational, in the same form as char(); list-directed
  !! output starts with a blank

  class(rational), intent(in) :: self
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg

  associate(unused => v_list); end associate

  ! ifx and nvfortran put no separator between list-directed derived-type items
  if (iotype == "LISTDIRECTED") then
    write(unit, "(a)", advance="no", iostat=iostat, iomsg=iomsg) " "//trim(rat_str(self))
  else
    write(unit, "(a)", advance="no", iostat=iostat, iomsg=iomsg) trim(rat_str(self))
  end if

end subroutine write_formatted

subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
  !! Read formatted rational: one token delimited by blanks, a comma,
  !! or the end of the record

  use, intrinsic :: iso_fortran_env, only: iostat_eor, iostat_end
  class(rational), intent(inout) :: self
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg

  character(rat_len) :: token
  character :: ch
  integer :: n

  associate(unused => v_list, unused2 => iotype); end associate

  token = ""
  n = 0
  do
    read(unit, "(a1)", iostat=iostat, iomsg=iomsg) ch
    if (iostat /= 0) exit
    if ((ch == " ") .or. (ch == ",")) then
      if (n > 0) exit
      cycle
    end if
    if (n == rat_len) then
      iostat = 1
      iomsg = "rational: token too long"
      return
    end if
    n = n + 1
    token(n:n) = ch
  end do

  if ((n > 0) .and. ((iostat == iostat_eor) .or. (iostat == iostat_end))) then
    iostat = 0
  end if
  if (iostat /= 0) return

  call rat_parse(token, self, iostat)
  if (iostat /= 0) iomsg = "rational: cannot parse '"//trim(token)//"'"

end subroutine read_formatted

pure subroutine rat_parse(str, r, ios)
  !! Parse "n", "n/d" or a decimal real into r; ios /= 0 on failure

  character(*), intent(in) :: str
  type(rational), intent(out) :: r
  integer, intent(out) :: ios

  integer :: slash, e, p
  real(rk) :: f
  character(len(str)) :: s, m

  ! List-directed read stops quietly at a blank, comma or slash, so the
  ! text is checked first: "1/2/3" or "3/4 junk" must not read as 1/2 or 3/4
  ios = 1
  s = adjustl(str)
  slash = index(s, "/")

  if (slash /= 0) then
    if (.not. is_int_text(trim(adjustl(s(:slash-1))))) return
    if (.not. is_int_text(trim(adjustl(s(slash+1:))))) return
    read(s(:slash-1), *, iostat=ios) r%num
    if (ios == 0) read(s(slash+1:), *, iostat=ios) r%den
    if ((ios == 0) .and. (r%den == 0)) ios = 1
  else if (scan(s, ".eEdD") /= 0) then
    ! [sign] digits [. digits] [exponent letter [sign] digits]
    e = scan(s, "eEdD")
    if (e == 0) e = len_trim(s) + 1
    if (e <= len_trim(s)) then
      if (.not. is_int_text(trim(s(e+1:)))) return
    end if
    m = s(:e-1)
    p = index(m, ".")
    if (p /= 0) then
      if (verify(trim(m(p+1:)), "0123456789") /= 0) return
      m = m(:p-1) // m(p+1:)
    end if
    if (.not. is_int_text(trim(m))) return
    read(s, *, iostat=ios) f
    if (ios == 0) r = rat_const_real(f)
    return
  else
    if (.not. is_int_text(trim(s))) return
    read(s, *, iostat=ios) r%num
    r%den = 1
  end if

  if (ios == 0) call simplify(r)

end subroutine rat_parse

pure logical function is_int_text(s) result(ok)
  !! Whether s is an optional sign followed by one or more digits
  character(*), intent(in) :: s
  integer :: i0
  i0 = 1
  if (len(s) > 0) i0 = 1 + scan(s(1:1), "+-")
  ok = len(s) >= i0 .and. verify(s(i0:), "0123456789") == 0
end function is_int_text

!-----------------------------------------------------------------------
! Constructors
!-----------------------------------------------------------------------

elemental type(rational) function rat_const_ik(n, d) result(rat)
  !! Construct rational from numerator and denominator

  integer(ik), intent(in) :: n   !! Numerator
  integer(ik), intent(in) :: d   !! Denominator

  rat%num = n
  rat%den = d

  call simplify(rat)

end function rat_const_ik

elemental type(rational) function rat_const_ir(n, d) result(rat)
  !! Construct rational from numerator and denominator

  integer(ir), intent(in) :: n   !! Numerator
  integer(ir), intent(in) :: d   !! Denominator

  rat%num = n
  rat%den = d

  call simplify(rat)

end function rat_const_ir

elemental type(rational) function rat_const_int(n) result(rat)
  !! Construct rational from integer
  integer(ik), intent(in) :: n
  rat%num = n
  rat%den = 1
end function rat_const_int

elemental type(rational) function rat_const_real(r) result(rat)
  !! Construct rational from real: the simplest fraction that converts
  !! back to exactly r, or else the closest continued-fraction
  !! convergent that fits in 64 bits

  real(rk), intent(in) :: r

  real(rk) :: x
  integer(ir) :: t, p0, q0, p1, q1, p2, q2

  x = abs(r)
  if (.not. (x < real(huge(1_ir), rk))) then
    error stop "rational: real value out of range"
  end if

  p0 = 0
  q0 = 1
  p1 = 1
  q1 = 0

  do
    t = int(x, ir)
    if (p1 /= 0) then
      if (t > (huge(t) - p0)/p1) exit
    end if
    if (q1 /= 0) then
      if (t > (huge(t) - q0)/q1) exit
    end if
    p2 = t*p1 + p0
    q2 = t*q1 + q0
    p0 = p1
    q0 = q1
    p1 = p2
    q1 = q2
    if (real(p1, rk)/real(q1, rk) == abs(r)) exit
    x = x - t
    if (x == 0) exit
    x = 1/x
    if (.not. (x < real(huge(1_ir), rk))) exit
  end do

  rat%num = merge(-p1, p1, r < 0)
  rat%den = q1

end function rat_const_real

elemental type(rational) function rat_const_char(str) result(r)
  !! Construct rational from character, such as "3/4", "-2" or "0.75"

  character(*), intent(in) :: str

  integer :: ios

  call rat_parse(str, r, ios)
  if (ios /= 0) then
    error stop "rational: cannot parse character input"
  end if

end function rat_const_char

!-----------------------------------------------------------------------
! Conversions
!-----------------------------------------------------------------------

elemental character(rat_len) function rat_str(self) result(str)
  !! Convert rational to character

  class(rational), intent(in) :: self

  character(20) :: n1, n2

  write(n1, "(i20)") self%num

  if (self%den == 1) then
    str = trim(adjustl(n1))
  else
    write(n2, "(i20)") self%den
    str = trim(adjustl(n1))//"/"//trim(adjustl(n2))
  end if

end function rat_str

elemental integer(ik) function rat_int(b) result(i)
  !! Convert rational to integer, truncating toward zero

  type(rational), intent(in) :: b

  integer(ir) :: t

  t = rat_trunc(b)
  if (abs(t) > huge(i)) then
    error stop "int: result does not fit in int32"
  end if
  i = int(t, ik)

end function rat_int

elemental real(rk) function rat_real(b) result(r)
  !! Convert rational to real
  type(rational), intent(in) :: b
  r = real(b%num, rk)/b%den
end function rat_real

!-----------------------------------------------------------------------
! Assignment
!-----------------------------------------------------------------------

elemental subroutine rat_set_int(self, b)
  !! Set rational = integer
  type(rational), intent(out) :: self
  integer(ik), intent(in) :: b
  self%num = b
  self%den = 1
end subroutine rat_set_int

elemental subroutine rat_set_real(self, b)
  !! Set rational = real
  type(rational), intent(out) :: self
  real(rk), intent(in) :: b
  self = rat_const_real(b)
end subroutine rat_set_real

!-----------------------------------------------------------------------
! Checked integer arithmetic
!-----------------------------------------------------------------------

elemental integer(ir) function add_chk(a, b) result(c)
  !! Integer sum a + b; stops if it does not fit in integer(int64)

  integer(ir), intent(in) :: a, b

  if (b > 0) then
    if (a > huge(a) - b) error stop "rational: integer overflow"
  else
    if (a < -huge(a) - b) error stop "rational: integer overflow"
  end if

  c = a + b

end function add_chk

elemental integer(ir) function mul_chk(a, b) result(c)
  !! Integer product a*b; stops if it does not fit in integer(int64)

  integer(ir), intent(in) :: a, b

  if (a /= 0) then
    if (abs(b) > huge(b)/abs(a)) error stop "rational: integer overflow"
  end if

  c = a*b

end function mul_chk

!-----------------------------------------------------------------------
! Arithmetic
!-----------------------------------------------------------------------

elemental type(rational) function rat_add_rat(a, b) result(c)
  !! Add two rationals (Knuth, TAOCP Vol. 2, 4.5.1); the result is
  !! already in lowest terms

  type(rational), intent(in) :: a, b

  integer(ir) :: d1, d2, t

  d1 = gcf_ir(a%den, b%den)
  t = add_chk(mul_chk(a%num, b%den/d1), mul_chk(b%num, a%den/d1))

  if (t == 0) then
    c%num = 0
    c%den = 1
    return
  end if

  d2 = gcf_ir(t, d1)
  c%num = t/d2
  c%den = mul_chk(a%den/d1, b%den/d2)

end function rat_add_rat

elemental type(rational) function rat_add_int(a, b) result(c)
  !! Add rational and integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_add_rat(a, rat_const_int(b))
end function rat_add_int

elemental type(rational) function int_add_rat(a, b) result(c)
  !! Add integer and rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_add_rat(rat_const_int(a), b)
end function int_add_rat

elemental type(rational) function unary_plus(self) result(r)
  !! Unary plus: returns its argument
  type(rational), intent(in) :: self
  r = self
end function unary_plus

elemental type(rational) function rat_sub_rat(a, b) result(c)
  !! Subtract two rationals
  type(rational), intent(in) :: a, b
  c = rat_add_rat(a, negate(b))
end function rat_sub_rat

elemental type(rational) function rat_sub_int(a, b) result(c)
  !! Subtract integer from rational
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_sub_rat(a, rat_const_int(b))
end function rat_sub_int

elemental type(rational) function int_sub_rat(a, b) result(c)
  !! Subtract rational from integer
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_sub_rat(rat_const_int(a), b)
end function int_sub_rat

elemental type(rational) function negate(b) result(n)
  !! Negate rational

  type(rational), intent(in) :: b

  n%num = -b%num
  n%den = b%den

end function negate

elemental type(rational) function rat_mul_rat(a, b) result(c)
  !! Multiply two rationals, cancelling cross factors first
  !! (Knuth, TAOCP Vol. 2, 4.5.1)

  type(rational), intent(in) :: a, b

  integer(ir) :: gcf1, gcf2

  gcf1 = gcf_ir(a%num, b%den)
  gcf2 = gcf_ir(a%den, b%num)

  c%num = mul_chk(a%num/gcf1, b%num/gcf2)
  c%den = mul_chk(a%den/gcf2, b%den/gcf1)

  call c%simplify()

end function rat_mul_rat

elemental type(rational) function rat_mul_int(a, b) result(c)
  !! Multiply rational and integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_mul_rat(a, rat_const_int(b))
end function rat_mul_int

elemental type(rational) function int_mul_rat(a, b) result(c)
  !! Multiply integer and rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_mul_rat(rat_const_int(a), b)
end function int_mul_rat

elemental type(rational) function rat_div_rat(a, b) result(c)
  !! Divide two rationals
  type(rational), intent(in) :: a, b
  c = rat_mul_rat(a, inverse(b))
end function rat_div_rat

elemental type(rational) function rat_div_int(a, b) result(c)
  !! Divide rational by integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_div_rat(a, rat_const_int(b))
end function rat_div_int

elemental type(rational) function int_div_rat(a, b) result(c)
  !! Divide integer by rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_div_rat(rat_const_int(a), b)
end function int_div_rat

elemental type(rational) function rat_power_int(a, b) result(c)
  !! Raise rational to integer power, by repeated squaring

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  type(rational) :: x
  integer(ir) :: n

  ! int64 first: abs(-huge(1)-1) does not fit in int32
  n = abs(int(b, ir))
  if (b < 0) then
    x = inverse(a)
  else
    x = a
  end if

  ! Powers of coprime num and den stay coprime, so no reduction is needed.
  ! x is squared only while a higher bit of n remains, so every product
  ! divides the result and overflows only if the result does.
  c%num = 1
  c%den = 1
  do while (n > 0)
    if (mod(n, 2_ir) == 1) then
      c%num = mul_chk(c%num, x%num)
      c%den = mul_chk(c%den, x%den)
    end if
    n = n/2
    if (n > 0) then
      x%num = mul_chk(x%num, x%num)
      x%den = mul_chk(x%den, x%den)
    end if
  end do

end function rat_power_int

!-----------------------------------------------------------------------
! Comparison
!-----------------------------------------------------------------------

elemental logical function rat_eq_rat(a, b) result(c)
  !! Check rational == rational
  type(rational), intent(in) :: a, b
  c = (a%num == b%num) .and. (a%den == b%den)
end function rat_eq_rat

elemental logical function rat_eq_int(a, b) result(c)
  !! Check rational == integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = (a%den == 1) .and. (a%num == b)
end function rat_eq_int

elemental logical function int_eq_rat(a, b) result(c)
  !! Check integer == rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = (b%num == a) .and. (b%den == 1)
end function int_eq_rat

elemental logical function rat_ne_rat(a, b) result(c)
  !! Check rational /= rational
  type(rational), intent(in) :: a, b
  c = (a%num /= b%num) .or. (a%den /= b%den)
end function rat_ne_rat

elemental logical function rat_ne_int(a, b) result(c)
  !! Check rational /= integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = (a%num /= b) .or. (a%den /= 1)
end function rat_ne_int

elemental logical function int_ne_rat(a, b) result(c)
  !! Check integer /= rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = (b%num /= a) .or. (b%den /= 1)
end function int_ne_rat

elemental logical function rat_gt_rat(a, b) result(c)
  !! Check rational > rational
  type(rational), intent(in) :: a, b
  c = rat_compare(a, b) > 0
end function rat_gt_rat

elemental logical function rat_gt_int(a, b) result(c)
  !! Check rational > integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_compare(a, rat_const_int(b)) > 0
end function rat_gt_int

elemental logical function int_gt_rat(a, b) result(c)
  !! Check integer > rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_compare(rat_const_int(a), b) > 0
end function int_gt_rat

elemental logical function rat_lt_rat(a, b) result(c)
  !! Check rational < rational
  type(rational), intent(in) :: a, b
  c = rat_compare(a, b) < 0
end function rat_lt_rat

elemental logical function rat_lt_int(a, b) result(c)
  !! Check rational < integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_compare(a, rat_const_int(b)) < 0
end function rat_lt_int

elemental logical function int_lt_rat(a, b) result(c)
  !! Check integer < rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_compare(rat_const_int(a), b) < 0
end function int_lt_rat

elemental logical function rat_ge_rat(a, b) result(c)
  !! Check rational >= rational
  type(rational), intent(in) :: a, b
  c = rat_compare(a, b) >= 0
end function rat_ge_rat

elemental logical function rat_ge_int(a, b) result(c)
  !! Check rational >= integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_compare(a, rat_const_int(b)) >= 0
end function rat_ge_int

elemental logical function int_ge_rat(a, b) result(c)
  !! Check integer >= rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_compare(rat_const_int(a), b) >= 0
end function int_ge_rat

elemental logical function rat_le_rat(a, b) result(c)
  !! Check rational <= rational
  type(rational), intent(in) :: a, b
  c = rat_compare(a, b) <= 0
end function rat_le_rat

elemental logical function rat_le_int(a, b) result(c)
  !! Check rational <= integer
  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b
  c = rat_compare(a, rat_const_int(b)) <= 0
end function rat_le_int

elemental logical function int_le_rat(a, b) result(c)
  !! Check integer <= rational
  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b
  c = rat_compare(rat_const_int(a), b) <= 0
end function int_le_rat

!-----------------------------------------------------------------------
! Reduction, reciprocal and random order
!-----------------------------------------------------------------------

elemental subroutine simplify(a)
  !! Reduce to lowest terms with a positive denominator

  class(rational), intent(inout) :: a

  integer(ir) :: fac

  if (a%den == 0) then
    error stop "rational: division by zero"
  end if

  if (min(a%num, a%den) < -huge(a%num)) then
    error stop "rational: integer overflow"
  end if

  if (a%num == 0) then
    a%den = 1
    return
  end if

  if (a%den < 0) then
    a%num = -a%num
    a%den = -a%den
  end if

  fac = gcf_ir(a%num, a%den)
  a%num = a%num/fac
  a%den = a%den/fac

end subroutine simplify

elemental type(rational) function inverse(self) result(b)
  !! Reciprocal of rational; stops if it is zero
  class(rational), intent(in) :: self
  if (self%num == 0) then
    error stop "rational: division by zero"
  end if
  b%num = sign(self%den, self%num)
  b%den = abs(self%num)
end function inverse

subroutine set_order(ord)
  !! Set the denominator used by random_number
  integer(ik), intent(in) :: ord
  order = ord
end subroutine set_order

!-----------------------------------------------------------------------
! Overloaded array and value intrinsics
!-----------------------------------------------------------------------

elemental type(rational) function rat_abs(b) result(a)
  !! Absolute value
  type(rational), intent(in) :: b
  a%num = abs(b%num)
  a%den = b%den
end function rat_abs

elemental type(rational) function rat_min(a, b) result(c)
  !! Minimum rational

  type(rational), intent(in) :: a, b

  if (a <= b) then
    c = a
  else
    c = b
  end if

end function rat_min

elemental type(rational) function rat_max(a, b) result(c)
  !! Maximum rational

  type(rational), intent(in) :: a, b

  if (a >= b) then
    c = a
  else
    c = b
  end if

end function rat_max

pure type(rational) function rat_dot(a, b) result(c)
  !! Dot product of two rational vectors

  type(rational), intent(in) :: a(:), b(:)

  integer(ir) :: i

  if (size(a) /= size(b)) then
    error stop "dot_product: vector sizes differ"
  end if

  c = rational(0, 1)
  do i = 1, size(a)
    c = c + a(i)*b(i)
  end do

end function rat_dot

pure function rat_matmul(a, b) result(c)
  !! Matrix multiply two rational arrays

  type(rational), intent(in) :: a(:, :), b(:, :)
  type(rational) :: c(size(a, 1), size(b, 2))

  integer :: i, j

  if (size(a, 2) /= size(b, 1)) then
    error stop "matmul: matrix sizes differ"
  end if

  do concurrent (j=1:size(b, 2), i=1:size(a, 1))
    c(i, j) = rat_dot(a(i, :), b(:, j))
  end do

end function rat_matmul

pure function rat_matvec(a, b) result(c)
  !! Matrix multiply a rational matrix with a rational vector

  type(rational), intent(in) :: a(:, :), b(:)
  type(rational) :: c(size(a, 1))

  integer :: i

  if (size(a, 2) /= size(b)) then
    error stop "matmul: matrix and vector sizes differ"
  end if

  do i = 1, size(a, 1)
    c(i) = rat_dot(a(i, :), b)
  end do

end function rat_matvec

pure function rat_transpose(a) result(b)
  !! Transpose rational matrix

  type(rational), intent(in) :: a(:, :)
  type(rational) :: b(size(a, 2), size(a, 1))

  integer :: i, j

  do i = 1, size(a, 1)
    do j = 1, size(a, 2)
      b(j, i) = a(i, j)
    end do
  end do

end function rat_transpose

!-----------------------------------------------------------------------
! Integer functions, rounding and division
!-----------------------------------------------------------------------

elemental integer(ir) function lcm_ir(a, b) result(c)
  !! Calculate the least common multiple of two integers

  integer(ir), intent(in) :: a, b

  integer(ir) :: g

  if ((a == 0) .or. (b == 0)) then
    c = 0
    return
  end if

  g = gcf_ir(a, b)

  ! Divide before multiplying to avoid intermediate overflow
  c = mul_chk(abs(a) / g, abs(b))

end function lcm_ir

elemental integer(ik) function lcm_ik(a, b) result(c)
  !! Calculate the least common multiple of two int32 integers
  integer(ik), intent(in) :: a, b
  integer(ir) :: hold
  hold = lcm_ir(int(a, ir), int(b, ir))
  if (hold > huge(1_ik)) then
    error stop "lcm: result does not fit in int32"
  else
    c = int(hold, ik)
  end if
end function lcm_ik

elemental integer(ir) function gcf_ir(a, b) result(c)
  !! Calculate the greatest common factor of two integers

  integer(ir), intent(in) :: a, b
  integer(ir) :: x, y, t

  x = abs(a)
  y = abs(b)

  if (x == 0) then
    c = y
    return
  end if
  if (y == 0) then
    c = x
    return
  end if

  do while (y /= 0)
    t = mod(x, y)
    x = y
    y = t
  end do
  c = x

end function gcf_ir

elemental integer(ik) function gcf_ik(a, b) result(c)
  !! Calculate the greatest common factor of two int32 integers
  integer(ik), intent(in) :: a, b
  integer(ir) :: hold
  hold = gcf_ir(int(a, ir), int(b, ir))
  if (hold > huge(1_ik)) then
    error stop "gcf: result does not fit in int32"
  else
    c = int(hold, ik)
  end if
end function gcf_ik

elemental type(rational) function gcf_rat(a, b) result(c)
  !! Calculate the greatest common factor of two rationals

  type(rational), intent(in) :: a, b

  if ((a%num == 0) .or. (b%num == 0)) then
    c = rational(0, 1)
    return
  end if

  c%num = gcf(a%num, b%num)
  c%den = lcm_ir(a%den, b%den)

  call c%simplify()

end function gcf_rat

impure elemental subroutine rat_rand(a)
  !! Random rational in [0, 1] with denominator dividing order

  type(rational), intent(out) :: a
  real(rk) :: ra

  call random_number(ra)

  a%num = nint(ra*order, ik)
  a%den = order

  call a%simplify()

end subroutine rat_rand

elemental type(rational) function rat_sign(a, b) result(c)
  !! Sign transfer: magnitude of a with sign of b (mirrors intrinsic sign())

  type(rational), intent(in) :: a, b

  c%den = a%den
  if (b%num >= 0) then
    c%num = abs(a%num)
  else
    c%num = -abs(a%num)
  end if

end function rat_sign

elemental integer(ir) function rat_floor(b) result(n)
  !! Greatest integer <= b
  type(rational), intent(in) :: b
  n = b%num / b%den
  if ((b%num < 0) .and. (mod(b%num, b%den) /= 0)) n = n - 1_ir
end function rat_floor

elemental integer(ir) function rat_ceil(b) result(n)
  !! Smallest integer >= b
  type(rational), intent(in) :: b
  n = b%num / b%den
  if ((b%num > 0) .and. (mod(b%num, b%den) /= 0)) then
    n = n + 1_ir
  end if
end function rat_ceil

elemental integer(ir) function rat_trunc(b) result(n)
  !! Truncation toward zero (same as int() for positive, ceiling for negative)
  type(rational), intent(in) :: b
  ! Fortran integer division truncates toward zero
  n = b%num / b%den
end function rat_trunc

elemental integer(ir) function rat_round(b) result(n)
  !! Round to nearest integer; ties round away from zero

  type(rational), intent(in) :: b

  integer(ir) :: r

  n = b%num / b%den
  r = abs(mod(b%num, b%den))
  ! r >= den - r is 2r >= den without overflow
  if (r >= b%den - r) n = n + sign(1_ir, b%num)

end function rat_round

elemental integer function rat_compare(a, b) result(c)
  !! Three-way comparison: -1 if a<b, 0 if a==b, 1 if a>b
  !!
  !! Compares continued-fraction expansions, so it never overflows.

  type(rational), intent(in) :: a, b

  integer(ir) :: n1, d1, n2, d2, q1, q2, t

  if (rat_eq_rat(a, b)) then
    c = 0
    return
  end if

  if ((a%num <= 0) .and. (b%num >= 0)) then
    c = -1
    return
  end if
  if ((a%num >= 0) .and. (b%num <= 0)) then
    c = 1
    return
  end if

  ! Same strict sign: c is the result if |a| > |b|
  c = merge(1, -1, a%num > 0)
  n1 = abs(a%num)
  d1 = a%den
  n2 = abs(b%num)
  d2 = b%den

  do
    q1 = n1/d1
    q2 = n2/d2
    if (q1 /= q2) then
      if (q1 < q2) c = -c
      return
    end if
    n1 = mod(n1, d1)
    n2 = mod(n2, d2)
    if (n1 == 0) then
      c = -c
      return
    end if
    if (n2 == 0) return
    ! n1/d1 > n2/d2 exactly when d1/n1 < d2/n2
    t = n1
    n1 = d1
    d1 = t
    t = n2
    n2 = d2
    d2 = t
    c = -c
  end do

end function rat_compare

elemental type(rational) function rat_mod(a, b) result(c)
  !! Rational modulo: a - trunc(a/b)*b
  type(rational), intent(in) :: a, b
  type(rational) :: q
  q = a/b
  c = a - rational(rat_trunc(q), 1_ir)*b
end function rat_mod

elemental subroutine rat_divmod(a, b, q, r)
  !! Compute integer quotient and rational remainder of a/b simultaneously
  type(rational), intent(in) :: a, b
  integer(ir), intent(out) :: q   !! truncated quotient
  type(rational), intent(out) :: r   !! remainder: a - q*b
  q = rat_trunc(a / b)
  r = a - rational(q, 1_ir) * b
end subroutine rat_divmod

pure type(rational) function rat_sum(a) result(s)
  !! Sum of a rational array
  type(rational), intent(in) :: a(:)
  integer :: i

  s = rational(0_ik, 1_ik)
  do i = 1, size(a)
    s = s + a(i)
  end do

end function rat_sum

pure type(rational) function rat_product(a) result(p)
  !! Product of a rational array
  type(rational), intent(in) :: a(:)
  integer :: i

  p = rational(1_ik, 1_ik)
  do i = 1, size(a)
    p = p*a(i)
  end do

end function rat_product

end module rationals
