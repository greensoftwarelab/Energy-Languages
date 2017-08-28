! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! contributed by Steve Decker
! using the hash function posted by Rich Townsend to comp.lang.fortran
! on 5 October 2005.
! compilation:
!    g95 -O1 knucleotide.f90
!    ifort -O3 -ip knucleotide.f90
!
! This implementation requires TR15581

module knuc_mod
  implicit none
  private
  public :: init_table, read_frame, keys_of_given_len, cnt

  integer, parameter :: MaxWordLen = 18

  type, public :: key
     integer                   :: count = 0
     character(len=MaxWordLen) :: word = ""
  end type key

  type, public :: table
     private
     integer :: hashBits, maxWords, nWords
     type(key), allocatable, dimension(:) :: words
  end type table

contains

  pure subroutine init_table(kNuc, nBits)
    type(table), intent(out) :: kNuc
    integer,     intent(in)  :: nBits

    kNuc = table(nBits, 2**nBits, 0, null())
    allocate(kNuc%words(kNuc%maxWords))
  end subroutine init_table

  subroutine read_frame(buf, n, length, kNuc)
    character, dimension(:), intent(in)    :: buf
    integer,                 intent(in)    :: n, length
    type(table),             intent(inout) :: kNuc

    integer               :: i, j
    character(len=length) :: word

    do i = 1, n
       do j = 1, length
          word(j:j) = buf(i+j-1)
       end do
       call add(kNuc, word)
    end do
  end subroutine read_frame

  subroutine add(kNuc, word)
    type(table),      intent(inout) :: kNuc
    character(len=*), intent(in)    :: word

    integer :: m

    m = hash_value(word, kNuc%maxWords)
    do
       if (kNuc%words(m)%count == 0) then
          kNuc%words(m) = key(1, word)
          kNuc%nWords = kNuc%nWords + 1
          if (kNuc%nWords > kNuc%maxWords/2) call resize_table(kNuc)
          exit
       else if (kNuc%words(m)%word == word) then
          kNuc%words(m)%count = kNuc%words(m)%count + 1
          exit
       end if
       m = merge(1, m+1, m == kNuc%maxWords)
    end do
  end subroutine add

  subroutine resize_table(kNuc)
    type(table), intent(inout) :: kNuc

    integer     :: i, m
    type(table) :: temp

    temp = table(kNuc%hashBits + 1, 2 * kNuc%maxWords, kNuc%nWords, null())
    allocate(temp%words(temp%maxWords))

    do i = 1, kNuc%maxWords
       if (kNuc%words(i)%count > 0) then
          m = hash_value(trim(kNuc%words(i)%word), temp%maxWords)
          do
             if (temp%words(m)%count == 0) then
                temp%words(m) = kNuc%words(i)
                exit
             end if
             m = merge(1, m+1, m == temp%maxWords)
          end do
       end if
    end do

    kNuc = temp
  end subroutine resize_table

  pure function keys_of_given_len(kNuc, length)
    type(table), intent(in) :: kNuc
    integer,     intent(in) :: length
    type(key), dimension(4**length) :: keys_of_given_len

    integer :: i, n

    n = 1
    do i = 1, kNuc%maxWords
       if (len_trim(kNuc%words(i)%word) == length) then
          keys_of_given_len(n) = kNuc%words(i)
          n = n + 1
          if (n > size(keys_of_given_len)) exit
       end if
    end do
  end function keys_of_given_len

  integer function cnt(kNuc, string)
    type(table), intent(in)      :: kNuc
    character(len=*), intent(in) :: string

    integer :: m

    m = hash_value(string, kNuc%maxWords)
    do
       if (kNuc%words(m)%word == string .or. kNuc%words(m)%count == 0) then
          cnt = kNuc%words(m)%count
          exit
       end if
       m = merge(1, m+1, m == kNuc%maxWords)
    end do
  end function cnt

  integer function hash_value(key, range)
    character(len=*), intent(in) :: key
    integer,          intent(in) :: range

    integer :: len_key, a, b, c, k

    ! Hash the key into a code, using the algorithm
    ! described by Bob Jenkins at:
    !  http://burtleburtle.net/bob/hash/doobs.html
    !
    ! Note that range should be a power of 2, and
    ! that the 32-bit algorithm is used

    len_key = len(key)

    a = -1640531527 ! 0x9E3779B9
    b = a
    c = 305419896   ! 0x12345678

    k = 1

    do
       if (len_key < 12) exit

       ! Pack the key into 32 bits
       a = a + ichar(key(k:k)) + ishft(ichar(key(k+1:k+1)), 8) +  &
            ishft(ichar(key(k+2:k+2)), 16) + ishft(ichar(key(k+3:k+3)), 24)
       b = b + ichar(key(k+4:k+4)) + ishft(ichar(key(k+5:k+5)), 8) +  &
            ishft(ichar(key(k+6:k+6)), 16) + ishft(ichar(key(k+7:k+7)), 24)
       c = c + ichar(key(k+8:k+8)) + ishft(ichar(key(k+9:k+9)), 8) +  &
            ishft(ichar(key(k+10:k+10)), 16) + ishft(ichar(key(k+11:k+11)), 24)

       ! Mix it up
       call hash_mix()
       k = k + 12
       len_key = len_key - 12
    end do

    c = c + len_key

    ! Process remaining bits
    select case(len_key)
    case(11)
       c = c + ishft(ichar(key(k+10:k+10)),24)  &
            + ishft(ichar(key(k+9:k+9)),16) + ishft(ichar(key(k+8:k+8)),8)
       b = b + ishft(ichar(key(k+7:k+7)),24) + ishft(ichar(key(k+6:k+6)),16)  &
            + ishft(ichar(key(k+5:k+5)),8) + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(10)
       c = c + ishft(ichar(key(k+9:k+9)),16) + ishft(ichar(key(k+8:k+8)),8)
       b = b + ishft(ichar(key(k+7:k+7)),24) + ishft(ichar(key(k+6:k+6)),16)  &
            + ishft(ichar(key(k+5:k+5)),8) + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(9)
       c = c + ishft(ichar(key(k+8:k+8)),8)
       b = b + ishft(ichar(key(k+7:k+7)),24) + ishft(ichar(key(k+6:k+6)),16)  &
            + ishft(ichar(key(k+5:k+5)),8) + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(8)
       b = b + ishft(ichar(key(k+7:k+7)),24) + ishft(ichar(key(k+6:k+6)),16)  &
            + ishft(ichar(key(k+5:k+5)),8) + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(7)
       b = b + ishft(ichar(key(k+6:k+6)),16) + ishft(ichar(key(k+5:k+5)),8)  &
            + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(6)
       b = b + ishft(ichar(key(k+5:k+5)),8) + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(5)
       b = b + ichar(key(k+4:k+4))
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(4)
       a = a + ishft(ichar(key(k+3:k+3)),24) + ishft(ichar(key(k+2:k+2)),16)  &
            + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(3)
       a = a + ishft(ichar(key(k+2:k+2)),16) + ishft(ichar(key(k+1:k+1)),8)  &
            + ichar(key(k:k))
    case(2)
       a = a + ishft(ichar(key(k+1:k+1)),8) + ichar(key(k:k))
    case(1)
       a = a + ichar(key(k:k))
    end select

    call hash_mix()

    hash_value = iand(c, range - 1) + 1

  contains

    subroutine hash_mix
      ! Mix a, b and c
      a = ieor(a - b - c, ishft(c, -13))
      b = ieor(b - c - a, ishft(a, 8))
      c = ieor(c - a - b, ishft(b, -13))

      a = ieor(a - b - c, ishft(c, -12))
      b = ieor(b - c - a, ishft(a, 16))
      c = ieor(c - a - b, ishft(b, -5))

      a = ieor(a - b - c, ishft(c, -3))
      b = ieor(b - c - a, ishft(a, 10))
      c = ieor(c - a - b, ishft(b, -15))
    end subroutine hash_mix
  end function hash_value
end module knuc_mod

program knucleotide
  use knuc_mod
  implicit none

  integer, parameter :: LineLen = 60, InitialTableSize = 1

  integer :: bufferSize = 16384, stat, n = 0, i
  logical :: atThirdPart = .false.
  type(table) :: kn
  character(len=LineLen) :: line
  character, dimension(:), allocatable :: buffer, tempBuffer

  character, dimension(65:116), parameter :: Codes = (/ "A", " ", "C",  &
       (" ", i = 68, 70), "G", (" ", i = 72, 83), "T", (" ", i = 85, 96),  &
       "A", " ", "C", (" ", i = 100, 102), "G", (" ", i = 104, 115), "T" /)

  allocate(buffer(bufferSize))

  ! Read FASTA file line-by-line, extracting sequence three, and converting to
  ! uppercase.
  do
     read(*, "(a)", iostat=stat) line
     if (stat /= 0) exit
     if (.not. atThirdPart) then
        atThirdPart = line(1:3) == ">TH"
     else
        if (n+LineLen > bufferSize) then
           allocate(tempBuffer(bufferSize))
           tempBuffer = buffer
           deallocate(buffer)
           allocate(buffer(2*bufferSize))
           buffer(1:bufferSize) = tempBuffer
           buffer(bufferSize+1:2*bufferSize) = " "
           deallocate(tempBuffer)
           bufferSize = 2*bufferSize
        end if
        do i = 1, LineLen
           buffer(n+i) = Codes(iachar(line(i:i)))
        end do
        n = n + LineLen
     end if
  end do

  n = minloc(iachar(buffer),1) - 1

  call init_table(kn, InitialTableSize)

  call write_frequencies(1)
  call write_frequencies(2)

  call write_count("GGT")
  call write_count("GGTA")
  call write_count("GGTATT")
  call write_count("GGTATTTTAATT")
  call write_count("GGTATTTTAATTTATAGT")

contains

  subroutine write_frequencies(length)
    integer, intent(in) :: length

    integer :: numNuc, j
    type(key), dimension(4**length) :: nucleotides
    type(key) :: temp

    numNuc = n - length + 1

    call read_frame(buffer, numNuc, length, kn)

    nucleotides = keys_of_given_len(kn, length)

    ! Insertion sort
    do i = 2, size(nucleotides)
       temp = nucleotides(i)
       do j = i, 2, -1
          if (nucleotides(j-1)%count > temp%count .or.  &
               nucleotides(j-1)%count == temp%count .and.  &
               nucleotides(j-1)%word < temp%word) exit
          nucleotides(j) = nucleotides(j-1)
       end do
       nucleotides(j) = temp
    end do

    do i = 1, size(nucleotides)
       write(*, "(a2,f6.3)") nucleotides(i)%word(1:2),  &
            100. * nucleotides(i)%count / real(numNuc)
    end do
    write(*, "(a)") ""
  end subroutine write_frequencies

  subroutine write_count(string)
    character(len=*), intent(in) :: string

    character, parameter :: tab = achar(9)
    integer :: length, numNuc

    length = len(string)
    numNuc = n - length + 1

    call read_frame(buffer, numNuc, length, kn)

    write(*, "(i0,a)") cnt(kn, string), tab//string
  end subroutine write_count
end program knucleotide