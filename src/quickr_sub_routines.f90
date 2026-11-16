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

subroutine im2col(x, h, w, channels, size_h, size_w, stride, out, x__len_) bind(c)
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
  integer(c_int), intent(in) :: size_h
  integer(c_int), intent(in) :: size_w
  integer(c_int), intent(in) :: stride
real(c_double), intent(out) :: out((((int((h - size_h)) / int(stride) + 1) * (int((w - size_w)) / int(stride) + 1)) * channels),&
& (size_h * size_w))

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


  h_out = (int(floor(real(((h - size_h))) / real(stride))) + 1_c_int)
  w_out = (int(floor(real(((w - size_w))) / real(stride))) + 1_c_int)
  out = 0.0_c_double
  do k_j = 1, size_w
    do k_i = 1, size_h
      out_col = (k_i + (size_h * ((k_j - 1_c_int))))
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

subroutine pool_max(x, h, w, channels, row_bounds, col_bounds, out, col_bounds__len_, row_bounds__len_, x__dim_1_, x__dim_2_)&
& bind(c)
  use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_
  integer(c_ptrdiff_t), intent(in), value :: row_bounds__len_
  integer(c_ptrdiff_t), intent(in), value :: col_bounds__len_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  integer(c_int), intent(in) :: h
  integer(c_int), intent(in) :: w
  integer(c_int), intent(in) :: channels
  integer(c_int), intent(in) :: row_bounds(row_bounds__len_)
  integer(c_int), intent(in) :: col_bounds(col_bounds__len_)
  real(c_double), intent(out) :: out(((row_bounds__len_ - 1) * (col_bounds__len_ - 1)), x__dim_2_)

  ! locals
  integer(c_int) :: grid_h
  integer(c_int) :: grid_w
  integer(c_int) :: j
  integer(c_int) :: gc
  integer(c_int) :: gr
  real(c_double) :: best
  integer(c_int) :: ch
  integer(c_int) :: col
  integer(c_int) :: base
  integer(c_int) :: row
  ! manifest end


  grid_h = (size(row_bounds) - 1_c_int)
  grid_w = (size(col_bounds) - 1_c_int)
  out = 0.0_c_double
  do j = 1, size(x, 2)
    do gc = 1, grid_w
      do gr = 1, grid_h
        best = x((row_bounds(gr) + (h * ((col_bounds(gc) - 1_c_int)))), j)
        do ch = 1, channels
do col = col_bounds(gc), ((col_bounds((gc + 1_c_int)) - 1_c_int)), sign(1, ((col_bounds((gc + 1_c_int)) - 1_c_int))-col_bounds(gc))
            base = (h * (((col - 1_c_int) + (w * ((ch - 1_c_int))))))
do row = row_bounds(gr), ((row_bounds((gr + 1_c_int)) - 1_c_int)), sign(1, ((row_bounds((gr + 1_c_int)) - 1_c_int))-row_bounds(gr))
              if ((x((base + row), j) > best)) then
                best = x((base + row), j)
              end if
            end do
          end do
        end do
        out((gr + (grid_h * ((gc - 1_c_int)))), j) = best
      end do
    end do
  end do
end subroutine

subroutine pool_mean(x, h, w, channels, row_bounds, col_bounds, out, col_bounds__len_, row_bounds__len_, x__dim_1_, x__dim_2_)&
& bind(c)
  use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: x__dim_1_
  integer(c_int), intent(in), value :: x__dim_2_
  integer(c_ptrdiff_t), intent(in), value :: row_bounds__len_
  integer(c_ptrdiff_t), intent(in), value :: col_bounds__len_

  ! args
  real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
  integer(c_int), intent(in) :: h
  integer(c_int), intent(in) :: w
  integer(c_int), intent(in) :: channels
  integer(c_int), intent(in) :: row_bounds(row_bounds__len_)
  integer(c_int), intent(in) :: col_bounds(col_bounds__len_)
  real(c_double), intent(out) :: out(((row_bounds__len_ - 1) * (col_bounds__len_ - 1)), x__dim_2_)

  ! locals
  integer(c_int) :: grid_h
  integer(c_int) :: grid_w
  integer(c_int) :: j
  integer(c_int) :: gc
  integer(c_int) :: gr
  real(c_double) :: total
  integer(c_int) :: count
  integer(c_int) :: ch
  integer(c_int) :: col
  integer(c_int) :: base
  integer(c_int) :: row
  ! manifest end


  grid_h = (size(row_bounds) - 1_c_int)
  grid_w = (size(col_bounds) - 1_c_int)
  out = 0.0_c_double
  do j = 1, size(x, 2)
    do gc = 1, grid_w
      do gr = 1, grid_h
        total = 0.0_c_double
        count = ((((row_bounds((gr + 1_c_int)) - row_bounds(gr))) * ((col_bounds((gc + 1_c_int)) - col_bounds(gc)))) * channels)
        do ch = 1, channels
do col = col_bounds(gc), ((col_bounds((gc + 1_c_int)) - 1_c_int)), sign(1, ((col_bounds((gc + 1_c_int)) - 1_c_int))-col_bounds(gc))
            base = (h * (((col - 1_c_int) + (w * ((ch - 1_c_int))))))
do row = row_bounds(gr), ((row_bounds((gr + 1_c_int)) - 1_c_int)), sign(1, ((row_bounds((gr + 1_c_int)) - 1_c_int))-row_bounds(gr))
              total = (total + x((base + row), j))
            end do
          end do
        end do
        out((gr + (grid_h * ((gc - 1_c_int)))), j) = (total / real(count, kind=c_double))
      end do
    end do
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
