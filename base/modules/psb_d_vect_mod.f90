module psb_d_vect_mod
  
  use psb_const_mod

  type psb_d_vect
    real(psb_dpk_), allocatable :: v(:)
  contains
    procedure, pass(x) :: get_nrows => d_base_get_nrows
    procedure, pass(x) :: dot_v    => d_base_dot_v
    procedure, pass(x) :: dot_a    => d_base_dot_a
    generic, public    :: dot      => dot_v, dot_a
    procedure, pass(y) :: axpby_v  => d_base_axpby_v
    procedure, pass(y) :: axpby_a  => d_base_axpby_a
    generic, public    :: axpby    => axpby_v, axpby_a
    procedure, pass(y) :: mlt_v    => d_base_mlt_v
    procedure, pass(y) :: mlt_a    => d_base_mlt_a
    procedure, pass(y) :: mlt_a_2  => d_base_mlt_a_2
    generic, public    :: mlt      => mlt_v, mlt_a, mlt_a_2
    procedure, pass(x) :: nrm2     => d_base_nrm2
    procedure, pass(x) :: amax     => d_base_amax
    procedure, pass(x) :: asum     => d_base_asum
    procedure, pass(x) :: all      => d_base_all
    procedure, pass(x) :: zero     => d_base_zero
    procedure, pass(x) :: asb      => d_base_asb
    procedure, pass(x) :: sync     => d_base_sync
    procedure, pass(x) :: free     => d_base_free
    procedure, pass(x) :: ins      => d_base_ins
    procedure, pass(x) :: bld_x    => d_base_bld_x
    procedure, pass(x) :: bld_n    => d_base_bld_n
    generic, public    :: bld => bld_x, bld_n
    procedure, pass(x) :: cpy_vect => d_base_cpy_vect
    generic, public    :: assignment(=) => cpy_vect
  end type psb_d_vect

  public  :: psb_d_vect_
  private :: constructor, size_const
  interface psb_d_vect_
    module procedure constructor, size_const
  end interface psb_d_vect_

contains
  
  subroutine d_base_bld_x(x,this)
    real(psb_dpk_), intent(in) :: this(:)
    class(psb_d_vect), intent(inout) :: x
    integer :: info

    x%v  = this 

  end subroutine d_base_bld_x
    
  
  subroutine d_base_bld_n(x,n)
    integer, intent(in) :: n
    class(psb_d_vect), intent(inout) :: x
    integer :: info

    call x%asb(n,info)

  end subroutine d_base_bld_n
    
  subroutine d_base_cpy_vect(this,x)
    real(psb_dpk_), allocatable, intent(out) :: this(:)
    class(psb_d_vect), intent(in) :: x
    integer :: info

    this = x%v 

  end subroutine d_base_cpy_vect
    
  
  function constructor(x) result(this)
    real(psb_dpk_)   :: x(:)
    type(psb_d_vect) :: this
    integer :: info

    this%v = x
    call this%asb(size(x),info)

  end function constructor
    
  
  function size_const(n) result(this)
    integer, intent(in) :: n
    type(psb_d_vect) :: this
    integer :: info

    call this%asb(n,info)
    if (info /= 0) then 
      write(0,*) 'Allocation failure ',n
    end if
  end function size_const
    

  function d_base_get_nrows(x) result(res)
    implicit none 
    class(psb_d_vect), intent(in) :: x
    integer :: res
    res = -1
    if (allocated(x%v)) res = size(x%v)
  end function d_base_get_nrows

  function d_base_dot_v(n,x,y) result(res)
    implicit none 
    class(psb_d_vect), intent(inout) :: x, y
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    real(psb_dpk_), external      :: ddot
    
    res = dzero
    !
    ! Note: this is the base implementation.
    !  When we get here, we are sure that X is of
    !  TYPE psb_d_vect
    !
    select type(yy => y)
    type is (psb_d_vect)
      res = ddot(n,x%v,1,y%v,1)
    class default
      res = y%dot(n,x%v)
    end select

  end function d_base_dot_v

  function d_base_dot_a(n,x,y) result(res)
    implicit none 
    class(psb_d_vect), intent(inout) :: x
    real(psb_dpk_), intent(in)    :: y(:)
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    real(psb_dpk_), external      :: ddot
    
    res = ddot(n,y,1,x%v,1)

  end function d_base_dot_a
    
  subroutine d_base_axpby_v(m,alpha, x, beta, y, info)
    use psi_serial_mod
    implicit none 
    integer, intent(in)               :: m
    class(psb_d_vect), intent(inout)  :: x
    class(psb_d_vect), intent(inout)  :: y
    real(psb_dpk_), intent (in)       :: alpha, beta
    integer, intent(out)              :: info
    
    select type(xx => x)
    type is (psb_d_vect)
      call psb_geaxpby(m,alpha,x%v,beta,y%v,info)
    class default
      call y%axpby(m,alpha,x%v,beta,info)
    end select

  end subroutine d_base_axpby_v

  subroutine d_base_axpby_a(m,alpha, x, beta, y, info)
    use psi_serial_mod
    implicit none 
    integer, intent(in)               :: m
    real(psb_dpk_), intent(in)        :: x(:)
    class(psb_d_vect), intent(inout)  :: y
    real(psb_dpk_), intent (in)       :: alpha, beta
    integer, intent(out)              :: info
    
    call psb_geaxpby(m,alpha,x,beta,y%v,info)
    
  end subroutine d_base_axpby_a

    
  subroutine d_base_mlt_v(x, y, info)
    use psi_serial_mod
    implicit none 
    class(psb_d_vect), intent(inout)  :: x
    class(psb_d_vect), intent(inout)  :: y
    integer, intent(out)              :: info    
    integer :: i, n

    info = 0
    select type(xx => x)
    type is (psb_d_vect)
      n = min(size(y%v), size(xx%v))
      do i=1, n 
        y%v(i) = y%v(i)*xx%v(i)
      end do
    class default
      call y%mlt(x%v,info)
    end select

  end subroutine d_base_mlt_v

  subroutine d_base_mlt_a(x, y, info)
    use psi_serial_mod
    implicit none 
    real(psb_dpk_), intent(in)        :: x(:)
    class(psb_d_vect), intent(inout)  :: y
    integer, intent(out)              :: info
    integer :: i, n

    info = 0
    n = min(size(y%v), size(x))
    do i=1, n 
      y%v(i) = y%v(i)*x(i)
    end do
    
  end subroutine d_base_mlt_a


  subroutine d_base_mlt_a_2(z,x, y, info)
    use psi_serial_mod
    implicit none 
    real(psb_dpk_), intent(out)       :: z(:)
    real(psb_dpk_), intent(in)        :: x(:)
    class(psb_d_vect), intent(inout)  :: y
    integer, intent(out)              :: info
    integer :: i, n

    info = 0    
    n = min(size(y%v), size(x), size(z))
!!$    write(0,*) 'Mlt_a_2: ',n
    do i=1, n 
      z(i) = y%v(i)*x(i)
!!$      write(0,*) 'Mlt_a_2: ',i,z(i),y%v(i),x(i)
    end do
    
  end subroutine d_base_mlt_a_2


  function d_base_nrm2(n,x) result(res)
    implicit none 
    class(psb_d_vect), intent(inout) :: x
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    real(psb_dpk_), external      :: dnrm2
    
    res =  dnrm2(n,x%v,1)

  end function d_base_nrm2
  
  function d_base_amax(n,x) result(res)
    implicit none 
    class(psb_d_vect), intent(inout) :: x
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    
    res =  maxval(abs(x%v(1:n)))

  end function d_base_amax

  function d_base_asum(n,x) result(res)
    implicit none 
    class(psb_d_vect), intent(inout) :: x
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    
    res =  sum(abs(x%v(1:n)))

  end function d_base_asum
  
  subroutine d_base_all(n, x, info)
    use psi_serial_mod
    use psb_realloc_mod
    implicit none 
    integer, intent(in)               :: n
    class(psb_d_vect), intent(out)    :: x
    integer, intent(out)              :: info
    
    call psb_realloc(n,x%v,info)
    
  end subroutine d_base_all

  subroutine d_base_zero(x)
    use psi_serial_mod
    implicit none 
    class(psb_d_vect), intent(inout)    :: x
    
    if (allocated(x%v)) x%v=dzero
  end subroutine d_base_zero

  subroutine d_base_asb(n, x, info)
    use psi_serial_mod
    use psb_realloc_mod
    implicit none 
    integer, intent(in)              :: n
    class(psb_d_vect), intent(inout) :: x
    integer, intent(out)             :: info
    
    if (x%get_nrows() < n) &
         & call psb_realloc(n,x%v,info)
    
  end subroutine d_base_asb

  subroutine d_base_sync(x)
    implicit none 
    class(psb_d_vect), intent(inout) :: x
    
    !
    ! The base version does nothing, it's just
    ! a placeholder.
    ! 
    
  end subroutine d_base_sync

  subroutine d_base_free(x, info)
    use psi_serial_mod
    use psb_realloc_mod
    implicit none 
    class(psb_d_vect), intent(inout)  :: x
    integer, intent(out)              :: info
    
    info = 0
    if (allocated(x%v)) deallocate(x%v, stat=info)
        
  end subroutine d_base_free

  subroutine d_base_ins(n,irl,val,dupl,x,info)
    use psi_serial_mod
    implicit none 
    class(psb_d_vect), intent(inout)  :: x
    integer, intent(in)               :: n, dupl
    integer, intent(in)               :: irl(:)
    real(psb_dpk_), intent(in)        :: val(:)
    integer, intent(out)              :: info

    integer :: i

    info = 0
    if (.not.allocated(x%v)) then 
      info = psb_err_invalid_vect_state_
      return
    end if
    
    
    if (n > min(size(irl),size(val))) then 
      info = psb_err_invalid_input_
      return 
    end if
    
    select case(dupl) 
    case(psb_dupl_ovwrt_) 
      do i = 1, n
        !loop over all val's rows
        
        ! row actual block row 
        if (irl(i) > 0) then
          ! this row belongs to me
          ! copy i-th row of block val in x
          x%v(irl(i)) = val(i)
        end if
      enddo
      
    case(psb_dupl_add_) 
      
      do i = 1, n
        !loop over all val's rows
        
        if (irl(i) > 0) then
          ! this row belongs to me
          ! copy i-th row of block val in x
          x%v(irl(i)) = x%v(irl(i)) +  val(i)
        end if
      enddo
      
    case default
      info = 321
!!$      call psb_errpush(info,name)
!!$      goto 9999
    end select
    
  end subroutine d_base_ins

end module psb_d_vect_mod
