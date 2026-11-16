subroutine forest_mat(x, depths, vars, thresh, p, out, depths__len_, thresh__len_, vars__len_, x__dim_1_, x__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_
  integer(c_ptrdiff_t), intent(in), value :: depths__len_
  integer(c_ptrdiff_t), intent(in), value :: vars__len_
  integer(c_ptrdiff_t), intent(in), value :: thresh__len_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  integer(c_int), intent(in) :: depths(depths__len_)
  integer(c_int), intent(in) :: vars(vars__len_)
  real(c_double), intent(in) :: thresh(thresh__len_)
  integer(c_int), intent(in) :: p
  integer(c_int), intent(out) :: out(x__dim_1_, p)

  ! locals
  integer(c_int), allocatable :: idx(:)
  integer(c_int) :: vt_off
  integer(c_int) :: out_off
  integer(c_int) :: t
  integer(c_int) :: i
  integer(c_int) :: k
  integer(c_int) :: j
  integer(c_int) :: bit
  integer(c_int) :: nc
  ! manifest end

  allocate(idx(x__dim_1_))


  out = 0_c_int
  idx = 0
  vt_off = 0_c_int
  out_off = 0_c_int
  do t = 1, size(depths)
    do i = 1, size(x, 1)
      idx(i) = 0_c_int
    end do
    do k = 1, depths(t)
      j = vars((vt_off + k))
      bit = (2_c_int ** ((k - 1_c_int)))
      do i = 1, size(x, 1)
        idx(i) = (idx(i) + (merge(1_c_int, 0_c_int, (x(i, j) <= thresh((vt_off + k)))) * bit))
      end do
    end do
    nc = (2_c_int ** depths(t))
    do j = 1, nc
      do i = 1, size(x, 1)
        out(i, (out_off + j)) = merge(1_c_int, 0_c_int, (idx(i) == (j - 1_c_int)))
      end do
    end do
    vt_off = (vt_off + depths(t))
    out_off = (out_off + nc)
  end do
end subroutine

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

subroutine row_prod(x, out, x__dim_1_, x__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  real(c_double), intent(out) :: out(x__dim_1_)

  ! locals
  integer(c_int) :: i
  integer(c_int) :: j
  ! manifest end


  out = 0
  do i = 1, size(x, 1)
    out(i) = x(i, 1_c_int)
  end do
  do j = 1, (size(x, 2) - 1_c_int)
    do i = 1, size(x, 1)
      out(i) = (out(i) * x(i, (j + 1_c_int)))
    end do
  end do
end subroutine

subroutine dist_l2_mat(x, y, out, x__dim_1_, x__dim_2_, y__dim_1_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_
  integer(c_int), intent(in), value :: y__dim_1_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  real(c_double), intent(in) :: y(y__dim_1_, x__dim_2_)
  real(c_double), intent(out) :: out(x__dim_1_, y__dim_1_)

  ! locals
  integer(c_int) :: j
  integer(c_int) :: i
  real(c_double) :: acc
  integer(c_int) :: k
  real(c_double) :: diff
  ! manifest end


  out = 0.0_c_double
  do j = 1, size(y, 1)
    do i = 1, size(x, 1)
      acc = 0.0_c_double
      do k = 1, size(x, 2)
        diff = (x(i, k) - y(j, k))
        acc = (acc + (diff * diff))
      end do
      out(i, j) = acc
    end do
  end do
end subroutine

subroutine dist_l1_mat(x, y, out, x__dim_1_, x__dim_2_, y__dim_1_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_
  integer(c_int), intent(in), value :: y__dim_1_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  real(c_double), intent(in) :: y(y__dim_1_, x__dim_2_)
  real(c_double), intent(out) :: out(x__dim_1_, y__dim_1_)

  ! locals
  integer(c_int) :: j
  integer(c_int) :: i
  real(c_double) :: acc
  integer(c_int) :: k
  ! manifest end


  out = 0.0_c_double
  do j = 1, size(y, 1)
    do i = 1, size(x, 1)
      acc = 0.0_c_double
      do k = 1, size(x, 2)
        acc = (acc + abs((x(i, k) - y(j, k))))
      end do
      out(i, j) = acc
    end do
  end do
end subroutine
