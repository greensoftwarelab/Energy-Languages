! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! Code by Andrei Jirnyi; modified from my earlier submission.
!  thanks to Gilbert Brietzke for the previous Fortran code,
!  and to other submitters for their work
! Compilation:
! - single-core: ifort -fast fannkuch3.f90
! - multi-core:  ifort -fast -openmp fannkuch3.f90

program fannkuch
  use iso_fortran_env
  use iso_c_binding
  implicit none

  integer,parameter :: ILONG = 8, ISHORT = 1, MAXL = 7
  integer :: NP, maxfk, cksum = 0, current = 0
  integer(ILONG) ::NQ
  integer(ISHORT), allocatable :: blk(:,:), cnt(:)
  logical ::saveblk = .true.
  character(len=2) :: arg
  character(len=20) :: out
  logical :: WR = .false., WR1 = .false.

  WR1 = WR1 .or. WR

  call get_command_argument(1,arg)
  read(arg,'(I2)') NP
  NQ = factorial(MAXL)/MAXL
  allocate(blk(NP,NQ))
  allocate(cnt(NP))
  cnt = 0

  call fkcompute(NP)

  write(out,'(i15)') cksum-2
  write(*,'(a)') trim(adjustl(out))
  write(*,'(3a,i3)') 'Pfannkuchen(',trim(adjustl(arg)),') =',maxfk

contains

  function factorial(n)
    integer n, factorial, i
    factorial = 1
    do i=1,n
       factorial = factorial*i
    end do
  end function factorial

  subroutine fkcompute(NP)
    integer :: NP
    integer(ILONG) :: bsize
    integer :: i,k,k1
    integer(ISHORT), dimension(NP) :: base,oldbase
    integer :: numblk, ii, nshift
    integer :: maxlevel
    integer(ISHORT),allocatable :: bases(:,:)
    integer ::icksum, imaxfk

    base = [(i,i=1,NP)]
    blk(:,1) = base;

    k = 2;
    bsize = 1;
    maxfk = 0;
    nshift = 1
    maxlevel = min(MAXL,NP);   ! max block level

    do i=2,NP       ! rot count
       current = i
       if(i>=maxlevel) saveblk = .false.

       if(i<=maxlevel) then
          numblk = i-1
          nshift = i
          bsize = bsize*(i-1)
       else
          numblk = (i-1)*factorial(i-1)/bsize
       end if
       oldbase = base
       allocate(bases(NP,numblk))
       do ii=1, numblk
          call baseshift(base, nshift)
          bases(:,ii) = base
       end do

       !$omp  parallel do default(shared) private(ii,k1,icksum,imaxfk) &
       !$omp& if(numblk>1000) schedule(guided) &
       !$omp& reduction(+:cksum) reduction(max: maxfk) 
       do ii = 1, numblk
          k1 = k+bsize*(ii-1)
          if(saveblk) then
             call writeblk(blk(1,k1),blk(1,1),bases(1,ii),bsize)
          end if
          call procblk(blk(1,1),bases(1,ii),bsize,icksum,imaxfk)
          cksum = cksum+icksum
          maxfk = max(maxfk, imaxfk)
       end do
       !$omp end parallel do

       k = k+bsize*numblk
       deallocate(bases)
       if(saveblk) then
          base = oldbase
          cnt(1:nshift-1)=0
          cnt(nshift) = cnt(nshift)+1
       end if
    end do

  end subroutine fkcompute

  recursive subroutine baseshift(base, n)
    integer(ISHORT) :: base(NP)
    integer :: n

    base(1:n) = cshift(base(1:n), 1)
    cnt(n) = cnt(n) +1
    if(cnt(n) >= n) then
       cnt(n) = 0
       if(n == NP) return
       call baseshift(base, n+1)
    end if

  end subroutine baseshift
  
  subroutine procblk(mult,base,bsize,icksum,imaxfk)
    integer(ILONG) :: bsize
    integer(ISHORT) :: base(NP),mult(NP,bsize)
    integer(ISHORT) :: line(NP), t, t1
    integer :: j, ii, iii, icksum, imaxfk

    icksum = 0
    imaxfk = 0

    do iii=1,bsize
       line = base(mult(:,iii));
       j = 0
       t = line(1)
       do while(t /= 1)        ! flip till line(1)==1 
          do ii=1,ishft(t,-1)  ! do the flip
             t1 = line(ii)
             line(ii) = line(t+1-ii)
             line(t+1-ii) = t1
          end do
          t = line(1)
          j = j+1
       end do
       imaxfk = max(imaxfk, j)
       icksum = icksum+j*(ishft(mod(iii,2),1)-1)
    end do
    
  end subroutine procblk

  subroutine writeblk(blk,mult,base,bsize);
    integer(ILONG) :: bsize
    integer(ISHORT) :: blk(NP*bsize),mult(NP*bsize),base(NP)

    blk = base(mult);

  end subroutine writeblk

end program fannkuch