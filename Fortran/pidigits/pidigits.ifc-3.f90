! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! contributed by Andrei Jirnyi
!   Closely based on C codes by Bartlett/Bonzini/Mellor
! compilation: ifort -O2 -xHost -ipo pidigits2.f90 -lgmp

module gmp_mod
  ! declaring the GMP functions...
  use iso_c_binding
  type, bind(C) :: mpz_t 
     private
     integer :: mp_alloc
     integer :: mp_size
     type(c_ptr) :: mp_d  ! a pointer
  end type mpz_t
  
  interface
!!      int mpz_cmp (mpz_t op1, mpz_t op2)
     integer(c_int) function  mpz_cmp(op1, op2) bind(C,name='__gmpz_cmp')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
     end function mpz_cmp
!!   void mpz_init (mpz_t integer)
     subroutine mpz_init(op) bind(C,name='__gmpz_init')
       import
       type(mpz_t), intent(inout) :: op
     end subroutine mpz_init
!!   void mpz_init_set_ui (mpz_t rop, unsigned long int op)
     subroutine mpz_init_set_ui(op, N) bind(C,name='__gmpz_init_set_ui')
       import
       type(mpz_t), intent(inout) :: op
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_init_set_ui
!!   void mpz_set (mpz_t rop, mpz_t op1)
     subroutine mpz_set(op1, op2) bind(C,name='__gmpz_set')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
     end subroutine mpz_set
!!      unsigned long int mpz_get_ui (mpz_t op)
     integer function  mpz_get_ui(op1) bind(C,name='__gmpz_get_ui')
       import
       type(mpz_t), intent(inout) :: op1
     end function mpz_get_ui
!!   void mpz_add (mpz_t rop, mpz_t op1, mpz_t op2)
     subroutine mpz_add(op1, op2, op3) bind(C,name='__gmpz_add')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       type(mpz_t), intent(inout) :: op3
     end subroutine mpz_add
!!   void mpz_mul_2exp (mpz_t rop, mpz_t op1, unsigned long int op2)
     subroutine mpz_mul_2exp(op1, op2, N) bind(C,name='__gmpz_mul_2exp')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_mul_2exp
!!   void mpz_fdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d)
     subroutine mpz_fdiv_qr(op1, op2, op3, op4) bind(C,name='__gmpz_fdiv_qr')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       type(mpz_t), intent(inout) :: op3
       type(mpz_t), intent(inout) :: op4
     end subroutine mpz_fdiv_qr
!!   void mpz_mul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)
     subroutine mpz_mul_ui(op1, op2, N) bind(C,name='__gmpz_mul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_mul_ui
!!   void mpz_submul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)
     subroutine mpz_submul_ui(op1, op2, N) bind(C,name='__gmpz_submul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_submul_ui
!!   void mpz_addmul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)     
     subroutine mpz_addmul_ui(op1, op2, N) bind(C,name='__gmpz_addmul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_addmul_ui
  end interface
end module gmp_mod


program pidigits
  use iso_c_binding
  use gmp_mod
  implicit none

  integer(c_long) :: d, k=0, i=0, n=10000
  type(mpz_t) :: numer, accum, denom, tmp1, tmp2
  character(len=25) :: strout
  integer(8) :: intout=0

  character(len=10)  :: argv

  call getarg(1, argv)
  read(argv, *) n
  
  call mpz_init(tmp1);
  call mpz_init(tmp2);
  call mpz_init_set_ui(numer, 1);
  call mpz_init_set_ui(accum, 0);
  call mpz_init_set_ui(denom, 1);

  do
     do
        k = k+1
        call next_term(k)
        d = extract_digit()
        if(d /= -1) exit
     end do
     i = i+1
     intout = intout*10+d
     if(mod(i,10)==0) then
        write(strout,'(i10)') i
        write(*,'(i10.10,a,a)') intout,achar(9)//':',trim(adjustl(strout))
        intout=0
     end if
     if(i >= n) exit
     call eliminate_digit(d)
  end do

contains

!! Uses only one bigint division instead of two when checking a produced digit's validity.

!!  integer function extract_digit()
!!    if (mpz_cmp(numer, accum) > 0) then
!!       extract_digit = -1
!!       return
!!    end if

    call mpz_set(tmp1,accum)
    call mpz_addmul_ui(tmp1,numer,3)
    call mpz_fdiv_qr(tmp1, tmp2, tmp1, denom);
    call mpz_add(tmp2, tmp2, numer);

    if (mpz_cmp(tmp2, denom) >= 0) then
       extract_digit = -1
       return
    end if

    extract_digit = mpz_get_ui(tmp1);
    return
  end function extract_digit

  subroutine next_term(k)
    integer(c_long) k
    integer(c_long) y2
    y2 = k*2+1
    
    call mpz_addmul_ui(accum, numer, 2);
    call mpz_mul_ui(accum, accum, y2);
    call mpz_mul_ui(numer, numer, k);
    call mpz_mul_ui(denom, denom, y2);

  end subroutine next_term
  
  subroutine eliminate_digit(d)
    integer(c_long) d
    call mpz_submul_ui(accum, denom, d);
    call mpz_mul_ui(accum, accum, 10);
    call mpz_mul_ui(numer, numer, 10);
  end subroutine eliminate_digit

end program pidigits