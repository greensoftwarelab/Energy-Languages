! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! contributed by Andrei Jirnyi, modified from version by Steve Decker
!
! compilation:
!   ifort -ipo -O3 revcomp.f90

program revcomp
  use iso_fortran_env
  implicit none

  integer(1), parameter :: iEndStr = iachar(">"), EOL=10
  integer,   parameter :: LW = 60, BUFSIZE = 1000 

  integer(1), dimension(:), allocatable :: data, w
  integer(1) :: c, buf(BUFSIZE+1)
  integer :: i=0, j=0, k=0, dsize = 1000000, newdsize, stat
  logical :: intitle = .true.

  integer(1), dimension(LW) :: title
  character(len=LW) :: stitle
  equivalence(title, stitle)
  
  integer(1), dimension(65:121),parameter :: &
       Complement = iachar((/ "T", "V", "G", &
       "H", "E", "F", "C", "D", "I", "J", "M", "L", "K", "N", "O", "P",  &
       "Q", "Y", "S", "A", "A", "B", "W", "X", "R", (" ", stat = 90, 96),  &
       "T", "V", "G", "H", "E", "F", "C", "D", "I", "J", "M", "L", "K",  &
       "N", "O", "P", "Q", "Y", "S", "A", "A", "B", "W", "X", "R" /))

  ! Unformatted I/O is faster in ifort under linux.
  close(input_unit)
  open(unit=input_unit,access='stream',form='unformatted')
  close(output_unit)
  open(unit=output_unit, access='stream',form='unformatted')

  allocate(data(dsize))  
  k = dsize    ! position in the data array, decrementing

  intitle = .true.
  readFile: do
     buf = EOL
     read(input_unit, iostat=stat) buf(1:BUFSIZE)
     i = 0     ! position in buffer
     readBlock: do
        if(intitle) then
           readTitle: do           ! read title until EOL
              i = i+1
              if(i>BUFSIZE) cycle readFile
              c = buf(i)
              if(c /= EOL) then
                 j = j+1           ! position in title
                 title(j) = c
              else
                 intitle = .false.
                 exit readTitle
              end if
           end do readTitle
        else
           readData: do
              i = i+1
              if(i>BUFSIZE) cycle readFile
              c = buf(i)
              if(c /= EOL) then   
                 data(k) = Complement(c)
                 k = k-1
              else                 ! === EOL
                 if(k < 256) then  ! reallocate array
                    newdsize = dsize*3
                    allocate(w(newdsize))
                    w(newdsize-dsize+1:) = data
                    call move_alloc(w,data)
                    k = k+newdsize-dsize
                    dsize = newdsize
                 end if
                 
                 if(stat /=0) then
                    if(i<BUFSIZE .and. buf(i+1) == EOL) exit readFile
                 end if
                 if(buf(i+1) == iEndStr) then
                    intitle = .true.
                    call print_data
                    j = 0
                    k = dsize
                    cycle readBlock
                 end if
              end if
           end do readData
        end if
     end do readBlock
  end do readFile
  call print_data

contains

  subroutine print_data
    integer(1), dimension(:), allocatable:: data1
    integer :: lines, last, dlen, d1len, i1, j1

    dlen = dsize-k
    lines = dlen/LW
    last = dlen - lines*LW

    d1len = lines*(LW+1)
    if(last>0) d1len=d1len+last+1
    allocate(data1(d1len))

    i1=1; j1=1;
    ! copy to data1, inserting EOL's
    do i1=k+1, dsize-last, LW
       data1(j1:j1+LW-1) = data(i1:i1+LW-1)
       data1(j1+LW) = EOL
       j1 = j1+LW+1
    end do
    if(last>0) then
       data1(d1len-last:d1len-1) = data(dsize-last+1:dsize)
       data1(d1len) = EOL
    end if
    
    write(output_unit) trim(stitle(1:j)), EOL
    write(output_unit) data1
    flush(output_unit)
    deallocate(data1)
  end subroutine print_data

end program revcomp