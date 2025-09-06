module rationals
!! rational numbers

!! @todo
!! mod, divmod, floor, ceil, trunc, round, sign
!! is_integer, compare
!! inverse

  use iso_fortran_env, only: ir => int64, ik => int32, rk => real64

  implicit none

  private

  public :: ir, ik, rk

  integer(ik), parameter :: primes(*) = &
    [  2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,  47,  53, &
      59,  61,  67,  71,  73,  79,  83,  89,  97, 101, 103, 107, 109, 113, 127, 131, &
     137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, &
     227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, &
     313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, &
     419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, &
     509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, &
     617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, &
     727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, &
     829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, &
     947, 953, 967, 971, 977, 983, 991, 997]

  integer(ik) :: order = 10

  type :: rational
    private
    integer(ir) :: num = 0   !! Numerator
    integer(ir) :: den = 1   !! Denominator
    logical :: o             !! Dummy to offset default constructor interface
  contains
    procedure :: simplify
    procedure :: get_num, get_den
    procedure :: is_integer
    procedure :: inverse
    procedure :: write_formatted
    generic   :: write(formatted) => write_formatted
    procedure :: read_formatted
    generic   :: read(formatted) => read_formatted
  end type rational

  ! Assignment
  interface assignment(=)
    procedure :: rat_set_rat, rat_set_int, rat_set_real
  end interface assignment(=)

  ! Addition
  interface operator(+)
    procedure :: rat_add_rat, rat_add_int, int_add_rat, unary_plus
  end interface operator(+)

  ! Subtraction
  interface operator(-)
    procedure :: rat_sub_rat, rat_sub_int, int_sub_rat, negate
  end interface operator(-)

  ! Multiplication
  interface operator(*)
    procedure :: rat_mul_rat, rat_mul_int, int_mul_rat
  end interface operator(*)

  ! Division
  interface operator(/)
    procedure :: rat_div_rat, rat_div_int, int_div_rat
  end interface operator(/)

  ! Exponentiation
  interface operator(**)
    procedure :: rat_power_int
  end interface operator(**)

  ! Check equality
  interface operator(==)
    procedure :: rat_eq_rat, rat_eq_int, int_eq_rat
  end interface operator(==)

  ! Check inequaly
  interface operator(/=)
    procedure :: rat_ne_rat, rat_ne_int, int_ne_rat
  end interface operator(/=)

  ! Check greater than
  interface operator(>)
    procedure :: rat_gt_rat, rat_gt_int, int_gt_rat
  end interface operator(>)

  ! Check less than
  interface operator(<)
    procedure :: rat_lt_rat, rat_lt_int, int_lt_rat
  end interface operator(<)

  ! Check greater than or equal
  interface operator(>=)
    procedure :: rat_ge_rat, rat_ge_int, int_ge_rat
  end interface operator(>=)

  ! Check less than or equal
  interface operator(<=)
    procedure :: rat_le_rat, rat_le_int, int_le_rat
  end interface operator(<=)

  interface rational
    module procedure :: rat_real_const, rat_char_const, rat_int_const
    module procedure :: rat_const_ik, rat_const_ir
  end interface rational

  interface char
    procedure :: rat_str
  end interface char

  interface int
    procedure :: rat_int
  end interface int

  interface real
    procedure :: rat_real
  end interface real

  interface gcf
    module procedure :: gcf_ir, gcf_ik, gcf_rat
  end interface gcf

  interface lcm
    module procedure :: lcm_ir, lcm_ik
  end interface lcm

  interface abs
    module procedure :: rat_abs
  end interface abs

  interface min
    module procedure :: rat_min
  end interface min

  interface max
    module procedure :: rat_max
  end interface max

  interface dot_product
    module procedure :: rat_dot
  end interface dot_product

  interface matmul
    module procedure :: rat_matmul
    module procedure :: rat_matvec
  end interface matmul

  interface transpose
    module procedure :: rat_transpose
  end interface transpose

  interface random_number
    module procedure :: rat_rand
  end interface random_number

  public :: rational
  public :: assignment(=)
  public :: operator(+), operator(-), operator(*), operator(/), operator(**)
  public :: operator(==), operator(/=)
  public :: operator(>), operator(<), operator(>=), operator(<=)
  public :: char, int, real
  public :: set_order, lcm, gcf
  public :: random_number, abs, min, max, dot_product, matmul, transpose

contains

!***********************************************************************

pure elemental integer(ir) function get_num(self) result(num)
!! Get rational numerator
  class(rational), intent(in) :: self
  num = self%num
end function get_num

!***********************************************************************

pure elemental integer(ir) function get_den(self) result(den)
!! Get rational denominator
  class(rational), intent(in) :: self
  den = self%den
end function get_den

!***********************************************************************

pure elemental logical function is_integer(self) result(res)
!! Check if rational is integer

  class(rational), intent(in) :: self

  res = self%den == 1

end function is_integer

!***********************************************************************
!******************* I / O *********************************************
!***********************************************************************

subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
!! Write formatted rational

  class(rational), intent(in) :: self
  integer, intent(in)         :: unit
  character(*), intent(in)    :: iotype
  integer, intent(in)         :: v_list(:)
  integer, intent(out)        :: iostat
  character(*), intent(inout) :: iomsg

  if ((len_trim(iotype) >= 0) .and. (size(v_list) >= 0)) continue

  write(unit,"(I0)",advance='no', iostat=iostat, iomsg=iomsg) self%num
  write(unit,"('/')",advance='no', iostat=iostat, iomsg=iomsg)
  write(unit,"(I0)", iostat=iostat, iomsg=iomsg) self%den

end subroutine write_formatted

!***********************************************************************

subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
!! Read formatted rational

  class(rational), intent(inout) :: self
  integer, intent(in)          :: unit
  character(*), intent(in)     :: iotype
  integer, intent(in)          :: v_list(:)
  integer, intent(out)         :: iostat
  character(*), intent(inout)  :: iomsg

  character(50) :: line

  if ((len_trim(iotype) >= 0) .and. (size(v_list) >= 0)) continue

  read(unit,"(A50)", iostat=iostat, iomsg=iomsg) line

  self = rat_char_const(line)

end subroutine read_formatted

!***********************************************************************
!******************* C O N S T R U C T O R S ***************************
!***********************************************************************

pure elemental type(rational) function rat_const_ik(n,d) result(rat)
!! Construct rational from numerator and denominator

  integer(ik), intent(in) :: n    !! Numerator
  integer(ik), intent(in) :: d    !! Denominator

  rat%num = n
  rat%den = d

  call simplify(rat)

end function rat_const_ik

!***********************************************************************

pure elemental type(rational) function rat_const_ir(n,d) result(rat)
!! Construct rational from numerator and denominator

  integer(ir), intent(in) :: n    !! Numerator
  integer(ir), intent(in) :: d    !! Denominator

  rat%num = n
  rat%den = d

  call simplify(rat)

end function rat_const_ir

!***********************************************************************

pure elemental type(rational) function rat_int_const(n) result(rat)
!! Construct rational from integer

  integer(ik), intent(in) :: n

  rat%num = n
  rat%den = 1

end function rat_int_const

!***********************************************************************

pure elemental type(rational) function rat_real_const(r) result(rat)
!! Construct rational from real

  real(rk), intent(in) :: r

  rat%num = nint(r*(10_ir**16), ir)
  rat%den = 10_ir**16

  call simplify(rat)

end function rat_real_const

!***********************************************************************

pure elemental type(rational) function rat_char_const(str) result(r)
!! Construct rational from character

  character(*), intent(in) :: str
  integer(ik) :: i
  integer(ik) :: slash

  real(rk) :: f

  slash = index(str, "/")

  if (slash == 0) then
    if (index(str, ".") /= 0) then
      read(str,*) f
      r = rat_real_const(f)
    else
      read(str,*) i
      r = rat_int_const(i)
    end if
  else
    read(str(1:slash-1), *) r%num
    read(str(slash+1:), *) r%den
  end if

  call simplify(r)

end function rat_char_const

!***********************************************************************
!******************* C A S T I N G *************************************
!***********************************************************************

pure elemental character(41) function rat_str(self) result(str)
!! Convert rational to character

  class(rational), intent(in) :: self

  character(20) :: n1,n2

  write(n1,"(I20)") self%num

  if (self%den == 1) then
    str = trim(adjustl(n1))
  else if (self%den == 0) then
    if (self%num > 0) then
      str = "Inf"
    else if (self%num < 0) then
      str = "-Inf"
    else
      str = "NaN"
    end if
  else
    write(n2,"(I20)") self%den
    str = trim(adjustl(n1))//"/"//trim(adjustl(n2))
  end if

end function rat_str

!***********************************************************************

pure elemental integer(ik) function rat_int(b) result(i)
!! Convert rational to integer

  type(rational), intent(in) :: b

  i = nint((1.0_rk*b%num)/b%den, ik)

end function rat_int

!***********************************************************************

pure elemental real(rk) function rat_real(b) result(r)
!! Convert rational to real

  type(rational), intent(in) :: b

  r = real(b%num, rk)/b%den

end function rat_real

!***********************************************************************
!******************* A S S I G N M E N T *******************************
!***********************************************************************

pure elemental subroutine rat_set_rat(self,b)
!! Set rational = rational

  type(rational), intent(out) :: self
  type(rational), intent(in) :: b

  self%num = b%num
  self%den = b%den

end subroutine rat_set_rat

!***********************************************************************

pure elemental subroutine rat_set_int(self,b)
!! Set rational = Int

  type(rational), intent(out) :: self
  integer(ik), intent(in) :: b

  self%num = b
  self%den = 1

end subroutine rat_set_int

!***********************************************************************

pure elemental subroutine rat_set_real(self,b)
!! Set rational = Real

  type(rational), intent(out) :: self
  real(rk), intent(in) :: b

  self%num = nint((10_ir**16)*b, ir)
  self%den = 10_ir**16

  call self%simplify()

end subroutine rat_set_real

!***********************************************************************
!******************* A D D I T I O N ***********************************
!***********************************************************************

pure elemental type(rational) function rat_add_rat(a,b) result(c)
!! Add two rationals

  type(rational), intent(in) :: a,b

  c%num = (a%num)*(b%den) + (b%num)*(a%den)
  c%den = (a%den)*(b%den)

  call c%simplify()

end function rat_add_rat

!***********************************************************************

pure elemental type(rational) function rat_add_int(a,b) result(c)
!! Add rational and integer

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c%num = a%num + b*a%den
  c%den = a%den

  call c%simplify()

end function rat_add_int

!***********************************************************************

pure elemental type(rational) function int_add_rat(a,b) result(c)
!! Add integer and rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c%num = b%num + a*b%den
  c%den = b%den

  call c%simplify()

end function int_add_rat

!***********************************************************************

pure elemental type(Rational) function unary_plus(self) result(r)
  type(Rational), intent(in) :: self
  r = self
end function unary_plus

!***********************************************************************
!******************* S U B T R A C T I O N *****************************
!***********************************************************************

pure elemental type(rational) function rat_sub_rat(a,b) result(c)
!! Subtract two rationals

  type(rational), intent(in) :: a,b

  c%den = (a%den)*(b%den)
  c%num = (a%num)*(b%den) - (b%num)*(a%den)

  call c%simplify()

end function rat_sub_rat

!***********************************************************************

pure elemental type(rational) function rat_sub_int(a,b) result(c)
!! Subtract integer from rational

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c%num = a%num - b*a%den
  c%den = a%den

  call c%simplify()

end function rat_sub_int

!***********************************************************************

pure elemental type(rational) function int_sub_rat(a,b) result(c)
!! Subtract rational from integer

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c%num = a*b%den - b%num
  c%den = b%den

  call c%simplify()

end function int_sub_rat

!***********************************************************************

pure elemental type(rational) function negate(b) result(n)
!! Negate rational

  type(rational), intent(in) :: b

  n%num = -b%num
  n%den =  b%den

end function negate

!***********************************************************************
!******************* M U L T I P L I C A T I O N ***********************
!***********************************************************************

pure elemental type(rational) function rat_mul_rat(a,b) result(c)
!! Multiply two rationals safely

  type(rational), intent(in) :: a, b

  integer(ir) :: gcf1, gcf2
  integer(ir) :: hold1, hold2

  gcf1 = gcf_ir(a%num,b%den)
  gcf2 = gcf_ir(a%den,b%num)

  hold1 = a%num/gcf1
  hold2 = b%num/gcf2
  c%num = hold1*hold2

  hold1 = a%den/gcf2
  hold2 = b%den/gcf1
  c%den = hold1*hold2

  call c%simplify()

end function rat_mul_rat

!***********************************************************************

pure elemental type(rational) function rat_mul_int(a,b) result(c)
!! Multiply rational and integer

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  integer(ir) :: gcf1

  gcf1 = gcf_ir(int(b,ir),a%den)

  c%num = a%num*b/gcf1
  c%den = a%den/gcf1

  call c%simplify()

end function rat_mul_int

!***********************************************************************

pure elemental type(rational) function int_mul_rat(a,b) result(c)
!! Multiple integer and rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  integer(ir) :: gcf1

  gcf1 = gcf_ir(int(a,ir),b%den)

  c%num = b%num*a/gcf1
  c%den = b%den/gcf1

  call c%simplify()

end function int_mul_rat

!***********************************************************************
!******************* D I V I S I O N ***********************************
!***********************************************************************

pure elemental type(rational) function rat_div_rat(a,b) result(c)
!! Divide two rationals safely

  type(rational), intent(in) :: a,b

  integer(ir) :: gcf1, gcf2

  gcf1 = gcf_ir(a%num,b%num)
  gcf2 = gcf_ir(a%den,b%den)

  c%num = (a%num/gcf1)*(b%den/gcf2)
  c%den = (a%den/gcf2)*(b%num/gcf1)

  ! c%num = (a%num)*(b%den)
  ! c%den = (a%den)*(b%num)

  call c%simplify()

end function rat_div_rat

!***********************************************************************

pure elemental type(rational) function rat_div_int(a,b) result(c)
!! Divide rational by integer

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  integer(ir) :: gcf1

  gcf1 = gcf_ir(int(b,ir),a%num)

  c%num = a%num/gcf1
  c%den = a%den*(b/gcf1)

  call c%simplify()

end function rat_div_int

!***********************************************************************

pure elemental type(rational) function int_div_rat(a,b) result(c)
!! Divide integer by rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  integer(ir) :: gcf1

  gcf1 = gcf_ir(int(a,ir),b%num)

  c%num = b%den*(a/gcf1)
  c%den = b%num/gcf1

  call c%simplify()

end function int_div_rat

!***********************************************************************
!******************* E X P O N E N T I A T I O N ***********************
!***********************************************************************

pure type(rational) function rat_power_int(a,b) result(c)
!! Raise rational to integer power

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  integer(ik) :: i

  c%num = 1
  c%den = 1
  do i = 1,b
    c%num = c%num*a%num
    c%den = c%den*a%den
  end do ! i

end function rat_power_int

!***********************************************************************
!******************* E Q U A L I T Y ***********************************
!***********************************************************************

pure elemental logical function rat_eq_rat(a,b) result(c)
!! Check rational == rational

  type(rational), intent(in) :: a,b

  c = (a%num == b%num).and.(a%den == b%den)

end function rat_eq_rat

!***********************************************************************

pure elemental logical function rat_eq_int(a,b) result(c)
!! Check rational == Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = (a%den == 1) .and. (a%num == b)

end function rat_eq_int

!***********************************************************************

pure elemental logical function int_eq_rat(a,b) result(c)
!! Check Int == rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = (b%num == a)

end function int_eq_rat

!***********************************************************************
!******************* I N E Q U A L I T Y *******************************
!***********************************************************************

pure elemental logical function rat_ne_rat(a,b) result(c)
!! Check rational != rational

  type(rational), intent(in) :: a,b

  c = (a%num /= b%num) .or. (a%den /= b%den)

end function rat_ne_rat

!***********************************************************************

pure elemental logical function rat_ne_int(a,b) result(c)
!! Check rational != Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = (a%num /= b) .or. (a%den /= 1)

end function rat_ne_int

!***********************************************************************

pure elemental logical function int_ne_rat(a,b) result(c)
!! Check Int != rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = (b%num /= a) .or. (b%den /= 1)

end function int_ne_rat

!***********************************************************************
!******************* S U P E R I O R I T Y *****************************
!***********************************************************************

pure elemental logical function rat_gt_rat(a,b) result(c)
!! Check rational > rational

  type(rational), intent(in) :: a,b

  c = (a%num*b%den) > (b%num*a%den)

end function rat_gt_rat

!***********************************************************************

pure elemental logical function rat_gt_int(a,b) result(c)
!! Check rational > Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = a%num > (a%den*b)

end function rat_gt_int

!***********************************************************************

pure elemental logical function int_gt_rat(a,b) result(c)
!! Check Int > rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = a*b%den > b%num

end function int_gt_rat

!***********************************************************************
!******************* I N F E R I O R I T Y *****************************
!***********************************************************************

pure elemental logical function rat_lt_rat(a,b) result(c)
!! Check rational < rational

  type(rational), intent(in) :: a,b

  c = (a%num*b%den) < (b%num*a%den)

end function rat_lt_rat

!***********************************************************************

pure elemental logical function rat_lt_int(a,b) result(c)
!! Check rational < Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = a%num < (a%den*b)

end function rat_lt_int

!***********************************************************************

pure elemental logical function int_lt_rat(a,b) result(c)
!! Check Int < rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = a*b%den < b%num

end function int_lt_rat

!***********************************************************************
!******************* S U P E R I O R I T Y *****************************
!***********************************************************************

pure elemental logical function rat_ge_rat(a,b) result(c)
!! Check rational >= rational

  type(rational), intent(in) :: a,b

  c = (a%num*b%den) >= (b%num*a%den)

end function rat_ge_rat

!***********************************************************************

pure elemental logical function rat_ge_int(a,b) result(c)
!! Check rational >= Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = a%num >= (a%den*b)

end function rat_ge_int

!***********************************************************************

pure elemental logical function int_ge_rat(a,b) result(c)
!! Check Int >= rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = a*b%den >= b%num

end function int_ge_rat

!***********************************************************************
!******************* I N F E R I O R I T Y *****************************
!***********************************************************************

pure elemental logical function rat_le_rat(a,b) result(c)
!! Check rational <= rational

  type(rational), intent(in) :: a,b

  c = (a%num*b%den) <= (b%num*a%den)

end function rat_le_rat

!***********************************************************************

pure elemental logical function rat_le_int(a,b) result(c)
!! Check rational <= Int

  type(rational), intent(in) :: a
  integer(ik), intent(in) :: b

  c = a%num <= (a%den*b)

end function rat_le_int

!***********************************************************************

pure elemental logical function int_le_rat(a,b) result(c)
!! Check Int <= rational

  integer(ik), intent(in) :: a
  type(rational), intent(in) :: b

  c = a*b%den <= b%num

end function int_le_rat

!***********************************************************************
!******************* U T I L I T Y *************************************
!***********************************************************************

pure elemental subroutine simplify(a)
!! Simplify the fraction of a rational

  class(rational), intent(inout) :: a

  integer(ir) :: fac

  if (a%den == 0) then
    error stop "rational%simplify: Division by zero"
  end if

  if (a%num == 0) then
    a%den = 1
    return
  end if

  if (a%den < 0) then
    a%num = -a%num
    a%den = -a%den
  end if

  fac = gcf_ir(a%num,a%den)
  a%num = a%num/fac
  a%den = a%den/fac

  if (a%num == a%den) then
    a%num = 1
    a%den = 1
  end if

end subroutine simplify

!***********************************************************************

pure elemental type(rational) function inverse(self) result(b)
!! Reciprocal of rational

  class(rational), intent(in) :: self

  b%num = self%den
  b%den = self%num

end function inverse

!***********************************************************************

subroutine set_order(ord)
!! Set order for random number generation

  integer(ik), intent(in) :: ord

  order = ord

end subroutine set_order

!***********************************************************************
!******************* I N T R I N S I C  F U N C T I O N S **************
!***********************************************************************

pure elemental type(rational) function rat_abs(b) result(a)
!! Absolute value

  type(rational), intent(in) :: b

  a%num = abs(b%num)
  a%den = b%den

end function rat_abs

!***********************************************************************

pure elemental type(rational) function rat_min(a, b) result(c)
!! Minimum rational

  type(rational), intent(in) :: a,b

  if (a <= b) then
    c = a
  else
    c = b
  end if

end function rat_min

!***********************************************************************

pure elemental type(rational) function rat_max(a, b) result(c)
!! Maximum rational

  type(rational), intent(in) :: a,b

  if (a >= b) then
    c = a
  else
    c = b
  end if

end function rat_max

!***********************************************************************

pure type(rational) function rat_dot(a,b) result(c)
!! Dot product of two rational vectors

  type(rational), intent(in) :: a(:), b(:)

  integer(ik) :: i

  if (size(a) /= size(b)) then
    error stop "rational, dot: Incompatible vector sizes"
  end if

  c = rational(0, 1)
  do i = 1,size(a)
    c = c + a(i)*b(i)
  end do ! i

end function rat_dot

!***********************************************************************

pure function rat_matmul(a,b) result(c)
!! Matrix multiply two rational arrays

  type(rational), intent(in) :: a(:,:), b(:,:)
  type(rational) :: c(size(a,1),size(b,2))

  integer :: i,j

  if (size(a,2) /= size(b,1)) then
    error stop "rational, matmul: Incompatible matrix sizes"
  end if

  do concurrent (j=1:size(b,2), i=1:size(a,1))
    c(i,j) = rat_dot(a(i,:),b(:,j))
  end do ! j, i

end function rat_matmul

!***********************************************************************

pure function rat_matvec(a,b) result(c)
!! Matrix multiply a rational matrix with a rational vector

  type(rational), intent(in) :: a(:,:), b(:)
  type(rational) :: c(size(b))

  integer :: i

  if (size(a,2) /= size(b)) then
    error stop "rational, matvec: Incompatible matrix sizes"
  end if

  do i = 1,size(b)
    c(i) = rat_dot(a(i,:),b)
  end do ! i

end function rat_matvec

!***********************************************************************

pure function rat_transpose(a) result(b)
!! Transpose rational matrix

  type(rational), intent(in) :: a(:,:)
  type(rational) :: b(size(a,2),size(a,1))

  integer :: i,j

  do i = 1,size(a,1)
    do j = 1,size(a,2)
      b(j,i) = a(i,j)
    end do ! j
  end do ! i

end function rat_transpose

!***********************************************************************
!******************* F U N C T I O N S *********************************
!***********************************************************************

pure elemental integer(ir) function lcm_ir(a,b) result(c)
!! Calculate the least common multiple of two integers

  integer(ir), intent(in) :: a, b

  integer(ir) :: i
  integer(ir) :: prime
  integer(ir) :: p

  if ((a == 0).or.(b == 0)) then
    c = 0
    return
  end if

  c = 1
  do i = 1,size(primes)
    prime = primes(i)
    if ((prime.gt.a).and.(prime.gt.b)) return
    p = prime
    do while ((modulo(a,p) == 0).or.(modulo(b,p) == 0))
      p = p*prime
    end do
    if (p /= prime) c = c*p/prime
  end do ! i

end function lcm_ir

!***********************************************************************

pure elemental integer(ik) function lcm_ik(a,b) result(c)
  integer(ik), intent(in) :: a, b
  integer(ir) :: hold
  hold = lcm_ir(int(a,ir),int(b,ir))
  if (hold > huge(1_ik)) then
    error stop "lcm_ik: overflow"
  else
    c = int(hold, ik)
  end if
end function lcm_ik

!***********************************************************************

pure elemental integer(ir) function gcf_ir(a,b) result(c)
!! Calculate the greatest common factor of two integers

  integer(ir), intent(in) :: a, b

  integer(ir) :: i
  integer(ir) :: prime
  integer(ir) :: p

  if ((a == 0) .or. (b == 0)) then
    c = 0
    return
  end if

  c = 1
  do i = 1,size(primes)
    prime = primes(i)
    if ((prime > abs(a)).and.(prime > abs(b))) return
    p = prime
    do while ((modulo(abs(a),p) == 0) .and. (modulo(abs(b),p) == 0))
      p = p*prime
    end do
    if (p /= prime) c = c*p/prime
  end do ! i

end function gcf_ir

!***********************************************************************

pure elemental integer(ik) function gcf_ik(a,b) result(c)
  integer(ik), intent(in) :: a, b
  integer(ir) :: hold
  hold = gcf_ir(int(a,ir),int(b,ir))
  if (hold > huge(1_ik)) then
    error stop "gcf_ik: overflow"
  else
    c = int(hold, ik)
  end if
end function gcf_ik

!***********************************************************************

pure elemental type(rational) function gcf_rat(a,b) result(c)
!! Calculate the greatest common factor of two rationals

  type(rational), intent(in) :: a, b

  if ((a%num == 0).or.(b%num == 0)) then
    c = rational(0,1)
    return
  end if

  c%num = gcf(a%num,b%num)
  c%den = lcm_ir(a%den,b%den)

  call c%simplify()

end function gcf_rat

!***********************************************************************

impure elemental subroutine rat_rand(a)
!! Retrieve random rational (0.0, order)

  type(rational), intent(out) :: a
  real(rk) :: ra

  call random_number(ra)

  a%num = nint(ra*order, ik)
  a%den = order

  call a%simplify()

end subroutine rat_rand

!***********************************************************************
!******************* E N D *********************************************
!***********************************************************************

end module rationals
