! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! Contributed by Jason Blevins
! Adapted from Fortran versions by George R. Gonzalez and Simon Geard
!
! ifort -fast -openmp -o mandelbrot mandelbrot.f90
program mandelbrot
  implicit none

  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: int8 = selected_int_kind(2)
  integer, parameter :: iter = 50
  real(dp), parameter :: limit2 = 4.0_dp
  character(len=8) :: argv
  integer :: w, h, x, y, i, pos, bit_num
  integer(int8) :: byte
  real(dp) :: inv_w, inv_h, Zi, Zr, Ti, Tr, Cr, Ci
  logical :: inside
  integer(int8), dimension(:,:), allocatable :: buf

  ! read dimension from command line
  call get_command_argument(1, argv)
  read(argv, *) w
  h = w

  ! allocate output buffer
  allocate(buf(ceiling(w/8.0_dp),h))

  ! precalculate constants
  inv_w = 2.0_dp / w
  inv_h = 2.0_dp / h

  ! pbm header
  write(*,'("P4",/,i0," ",i0)') w, h

  !$OMP PARALLEL DO PRIVATE(y, x, bit_num, pos, byte, Zr, Cr, Ci, inside, i)
  do y = 0, h - 1
     bit_num = 8 ! when moving left to right, bits are numbered 7 to 0
     byte = 0_int8
     pos = 0
     do x = 0, w - 1
        bit_num = bit_num - 1

        Zr = 0.0_dp; Zi = 0.0_dp; Tr = 0.0_dp; Ti = 0.0_dp;
        Cr = inv_w * x - 1.5_dp
        Ci = inv_h * y - 1.0_dp
        inside = .true.
        do i = 1, iter
           Zi = 2.0 * Zr * Zi + Ci
           Zr = Tr - Ti + Cr
           Ti = Zi * Zi
           Tr = Zr * Zr
           if (Tr + Ti > limit2) then
              inside = .false.
              exit
           end if
        end do

        ! We're in the set, set this bit to 0
        if (inside) byte = ibset(byte, bit_num)

        if (bit_num == 0 .or. x == w - 1) then
           ! All bits set or end of row, so store full byte
           pos = pos + 1
           buf(pos,y+1) = byte
           byte = 0_int8
           bit_num = 8
        end if
     end do
  end do
  !$OMP END PARALLEL DO

  ! print output
  do y = 1, h
     write(*, '(10000000a1)', advance='no') buf(:,y)
  end do
  deallocate(buf)
end program mandelbrot