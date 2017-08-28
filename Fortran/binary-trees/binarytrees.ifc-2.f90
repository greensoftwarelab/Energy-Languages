! The Computer Language Benchmarks Game
! http://benchmarksgame.alioth.debian.org/
!
! original C program by Francesco Abbate
! Fortran version by Vladimir Fuka
!
! *reset*


module apr
  use iso_c_binding
  implicit none

  interface

    integer(c_int) function apr_initialize() bind(C)
      import
    end function

    type(c_ptr) function apr_palloc(p,size) bind(C)
      import
      type(c_ptr), value :: p
      integer(c_size_t), value :: size
    end function

    integer(c_int) function apr_pool_create_unmanaged_ex(newpool, abort_fn, allocator) bind(C)
      import
      type(c_ptr), intent(out) :: newpool
      type(c_funptr), value :: abort_fn
      type(c_funptr), value :: allocator
    end function

    subroutine apr_pool_clear(p) bind(C)
      import
      type(c_ptr),value :: p
    end subroutine

    subroutine apr_pool_destroy(p) bind(C)
      import
      type(c_ptr),value :: p
    end subroutine

  end interface

  contains

    integer(c_int) function abrt(i) bind(C)
      integer(c_int), value ,intent(in) :: i
      abrt = i
      error stop
    end function

end module apr

module trees
  use iso_c_binding
  use apr

  implicit none

  type node
    type(node), pointer :: left
    type(node), pointer :: right
  end type

  contains

    recursive integer function node_check(n) result(res)
      type(node), intent(in) :: n
      integer lc,rc

      if (associated(n%left)) then
          lc = node_check(n%left)
          rc = node_check(n%right)
          res = lc + 1 + rc
      else
        res =  1
      endif
    end function


    recursive function make(depth, pool) result(res)
      type(node),pointer :: res
      type(c_ptr), intent(in) :: pool
      integer, intent(in) :: depth

      call c_f_pointer( apr_palloc(pool, sizeof(res)), res)

      if (depth > 0) then
          res%left  => make(depth - 1, pool)
          res%right => make(depth - 1, pool)
      else
          res%left  => null()
          res%right => null()
      end if
    end function

end module trees



program main
  use iso_c_binding
  use apr
  use trees

  implicit none

  integer, parameter :: line_size = 64

  type(c_ptr) :: long_lived_pool
  integer,parameter :: min_depth = 4
  integer :: req_depth, max_depth, stretch_depth
  integer(c_int) :: tmp
  character(32) :: str

  type(node),pointer :: long_lived_tree

  integer d,iterations,c,i
  type(c_ptr) :: store
  type(node),pointer :: a, b, curr
  character(line_size),dimension(:),allocatable :: outputstr
  character, parameter :: t = achar(9)
  type(c_funptr):: abrtptr

  abrtptr = c_funloc(abrt)

  if (command_argument_count()==1) then
    call get_command_argument(1,str)
    read(str,*) req_depth
  else
    req_depth = 10
  end if

  if (req_depth > min_depth+2) then
    max_depth = req_depth
  else
    max_depth = min_depth + 2
  end if

  allocate(outputstr(min_depth:max_depth))

  stretch_depth = max_depth+1

  tmp = apr_initialize()

  ! Alloc then dealloc stretchdepth tree

  tmp = apr_pool_create_unmanaged_ex(store, abrtptr, c_null_funptr)

  curr => make(stretch_depth, store)

  write(*,"(2(a,i0))") "stretch tree of depth ",stretch_depth, t//" check: ",  node_check(curr)

  call apr_pool_destroy(store)

  tmp = apr_pool_create_unmanaged_ex(long_lived_pool, abrtptr, c_null_funptr)

  long_lived_tree => make(max_depth, long_lived_pool)

  !$omp parallel do private(store, a, b, c, i, iterations, tmp) schedule(dynamic,1)
  do  d = min_depth, max_depth, 2
      iterations = ishft(1, max_depth - d + min_depth)
      c = 0

      tmp = apr_pool_create_unmanaged_ex(store, abrtptr, c_null_funptr)

      do i = 1,iterations
          a => make( d, store)
          c = c+ node_check(a)
          call apr_pool_clear(store)
      end do

      call apr_pool_destroy(store)

      write(outputstr(d),"(2(i0,a),i0)") iterations,t//" trees of depth ", d ,t//" check: ", c
  end do
  !$omp end parallel do

  do d = min_depth, max_depth, 2
    write(*,"(a)") trim(outputstr(d))
  end do

  write(*,"(2(a,i0))") "long lived tree of depth ", max_depth ,t//" check: ", node_check(long_lived_tree)
end program