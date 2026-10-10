program hilbert_inverse
  !! Invert Hilbert matrices H(i,j) = 1/(i+j-1) exactly with rationals and in
  !! double precision with the same Gauss-Jordan elimination, and print how far
  !! the double-precision inverse is from the exact one.
  use, intrinsic :: iso_fortran_env, only: dp => real64
  use rationals, only: rational, real, abs, max, operator(/), operator(*), &
    operator(-), operator(==), assignment(=), matmul
  implicit none

  ! n = 15 needs integers beyond int64 and stops with an overflow error
  integer, parameter :: nmax = 14
  integer :: n

  write(*, '(a)') 'Inverse of the n x n Hilbert matrix: exact vs. real64'
  write(*, '(a3, 3a24)') 'n', 'largest |inv(H)(i,j)|', 'real64 relative error', 'H*inv(H) == I'
  do n = 2, nmax
    call compare(n)
  end do

contains

subroutine compare(n)
  !! Print one row of the table for matrix order n
  integer, intent(in) :: n
  type(rational) :: h(n, n), h0(n, n), x(n, n), eye(n, n), big
  real(dp) :: hd(n, n), xd(n, n)
  integer :: i, j

  do concurrent (i = 1:n, j = 1:n)
    h(i, j) = rational(1, i + j - 1)
    hd(i, j) = 1._dp/(i + j - 1)
    eye(i, j) = merge(1, 0, i == j)
  end do
  h0 = h
  x = eye
  xd = real(eye)
  call gauss_jordan(h, x)
  call gauss_jordan_dp(hd, xd)

  big = maxval_abs(x)
  write(*, '(i3, es24.6, es24.2, l24)') n, real(big), &
    maxval(abs(xd - real(x)))/real(big), all(matmul(h0, x) == eye)
end subroutine compare

pure function maxval_abs(x) result(m)
  !! Largest absolute value in x
  type(rational), intent(in) :: x(:, :)
  type(rational) :: m
  integer :: i, j
  m = 0
  do j = 1, size(x, 2)
    do i = 1, size(x, 1)
      m = max(m, abs(x(i, j)))
    end do
  end do
end function maxval_abs

pure subroutine gauss_jordan(a, x)
  !! Overwrite x with inv(a)*x; a is overwritten. No pivoting: H is positive definite.
  type(rational), intent(inout) :: a(:, :), x(:, :)
  integer :: i, k
  do k = 1, size(a, 1)
    x(k, :) = x(k, :)/a(k, k)
    a(k, :) = a(k, :)/a(k, k)
    do i = 1, size(a, 1)
      if (i == k) cycle
      x(i, :) = x(i, :) - a(i, k)*x(k, :)
      a(i, :) = a(i, :) - a(i, k)*a(k, :)
    end do
  end do
end subroutine gauss_jordan

pure subroutine gauss_jordan_dp(a, x)
  !! The same elimination in double precision
  real(dp), intent(inout) :: a(:, :), x(:, :)
  integer :: i, k
  do k = 1, size(a, 1)
    x(k, :) = x(k, :)/a(k, k)
    a(k, :) = a(k, :)/a(k, k)
    do i = 1, size(a, 1)
      if (i == k) cycle
      x(i, :) = x(i, :) - a(i, k)*x(k, :)
      a(i, :) = a(i, :) - a(i, k)*a(k, :)
    end do
  end do
end subroutine gauss_jordan_dp

end program hilbert_inverse
