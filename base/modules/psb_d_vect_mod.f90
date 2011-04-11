module psb_d_vect_mod
  
  use psb_const_mod

  type psb_d_vect
    real(psb_dpk_), allocatable :: v(:)
  contains
    procedure, pass(x) :: get_nrows => d_base_get_nrows
    procedure, pass(x) :: ddot_v    => d_base_ddot_v
    procedure, pass(x) :: ddot_a    => d_base_ddot_a
    generic, public    :: ddot      => ddot_v, ddot_a
    procedure, pass(y) :: daxpby_v  => d_base_daxpby_v
    procedure, pass(y) :: daxpby_a  => d_base_daxpby_a
    generic, public    :: daxpby    => daxpby_v, daxpby_a
  end type psb_d_vect

contains
  
  function d_base_get_nrows(x) result(res)
    implicit none 
    class(psb_d_vect), intent(in) :: x
    integer :: res
    
    res = size(x%v)
  end function d_base_get_nrows

  function d_base_ddot_v(n,x,y) result(res)
    implicit none 
    class(psb_d_vect), intent(in) :: x, y
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
      res = y%ddot(n,x%v)
    end select

  end function d_base_ddot_v

  function d_base_ddot_a(n,x,y) result(res)
    implicit none 
    class(psb_d_vect), intent(in) :: x
    real(psb_dpk_), intent(in)    :: y(:)
    integer, intent(in)           :: n
    real(psb_dpk_)                :: res
    real(psb_dpk_), external      :: ddot
    
    res = ddot(n,y,1,x%v,1)

  end function d_base_ddot_a
    
  subroutine d_base_daxpby_v(m,alpha, x, beta, y, info)
    use psi_serial_mod
    implicit none 
    integer, intent(in)               :: m
    class(psb_d_vect), intent(in)     :: x
    class(psb_d_vect), intent(inout)  :: y
    real(psb_dpk_), intent (in)       :: alpha, beta
    integer, intent(out)              :: info
    
    select type(xx => x)
    type is (psb_d_vect)
      call psb_geaxpby(m,alpha,x%v,beta,y%v,info)
    class default
      call y%daxpby(m,alpha,x%v,beta,info)
    end select

  end subroutine d_base_daxpby_v

  subroutine d_base_daxpby_a(m,alpha, x, beta, y, info)
    use psi_serial_mod
    implicit none 
    integer, intent(in)               :: m
    real(psb_dpk_), intent(in)        :: x(:)
    class(psb_d_vect), intent(inout)  :: y
    real(psb_dpk_), intent (in)       :: alpha, beta
    integer, intent(out)              :: info
    
    call psb_geaxpby(m,alpha,x,beta,y%v,info)
    
  end subroutine d_base_daxpby_a
  

end module psb_d_vect_mod
