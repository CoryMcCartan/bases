subroutine im2col(x, h, w, channels, size, stride, out, x__len_) bind(c)
  use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_ptrdiff_t), intent(in), value :: x__len_

  ! args
  real(c_double), intent(in) :: x(x__len_)
  integer(c_int), intent(in) :: h
  integer(c_int), intent(in) :: w
  integer(c_int), intent(in) :: channels
  integer(c_int), intent(in) :: size
  integer(c_int), intent(in) :: stride
real(c_double), intent(out) :: out((((int((h - size)) / int(stride) + 1) * (int((w - size)) / int(stride) + 1)) * channels), (size&
& * size))

  ! locals
  integer(c_int) :: h_out
  integer(c_int) :: w_out
  integer(c_int) :: k_j
  integer(c_int) :: k_i
  integer(c_int) :: out_col
  integer(c_int) :: ch
  integer(c_int) :: j
  integer(c_int) :: i
  integer(c_int) :: out_row
  integer(c_int) :: idx_in
  ! manifest end


  h_out = (int(floor(real(((h - size))) / real(stride))) + 1_c_int)
  w_out = (int(floor(real(((w - size))) / real(stride))) + 1_c_int)
  out = 0.0_c_double
  do k_j = 1, size
    do k_i = 1, size
      out_col = (k_i + (size * ((k_j - 1_c_int))))
      do ch = 1, channels
        do j = 1, w_out
          do i = 1, h_out
            out_row = (i + (h_out * ((((j - 1_c_int)) + (w_out * ((ch - 1_c_int)))))))
            idx_in = ((((i - 1_c_int)) * stride) + (h * (((((j - 1_c_int)) * stride) + (w * ((ch - 1_c_int)))))))
            out(out_row, out_col) = x(((idx_in + k_i) + (h * ((k_j - 1_c_int)))))
          end do
        end do
      end do
    end do
  end do
end subroutine

subroutine pool_max(x, out, x__dim_1_, x__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  real(c_double), intent(out) :: out(x__dim_2_)

  ! locals
  integer(c_int) :: j
  real(c_double) :: best
  integer(c_int) :: i
  ! manifest end


  out = 0
  do j = 1, size(x, 2)
    best = x(1_c_int, j)
    do i = 1, size(x, 1)
      if ((x(i, j) > best)) then
        best = x(i, j)
      end if
    end do
    out(j) = best
  end do
end subroutine
