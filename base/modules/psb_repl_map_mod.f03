module psb_repl_map_mod
  use psb_const_mod
  use psb_desc_const_mod
  use psb_indx_map_mod
  
  type, extends(psb_indx_map) :: psb_repl_map
    private 
    integer :: ictxt          = -1
    integer :: mpic           = -1
    integer :: state          = -1 
    integer :: global_rows    = -1
    integer :: global_cols    = -1
    integer :: local_rows     = -1
    integer :: local_cols     = -1

  contains

    procedure, pass(idxmap)  :: init      => repl_init

    procedure, pass(idxmap)  :: get_state => repl_get_state
    procedure, pass(idxmap)  :: set_state => repl_set_state
    procedure, pass(idxmap)  :: is_repl   => repl_is_repl
    procedure, pass(idxmap)  :: is_bld    => repl_is_bld
    procedure, pass(idxmap)  :: is_upd    => repl_is_upd
    procedure, pass(idxmap)  :: is_asb    => repl_is_asb
    procedure, pass(idxmap)  :: is_valid  => repl_is_valid
    procedure, pass(idxmap)  :: sizeof    => repl_sizeof
    procedure, pass(idxmap)  :: asb       => repl_asb
    procedure, pass(idxmap)  :: free      => repl_free

    procedure, pass(idxmap)  :: l2gs1 => repl_l2gs1
    procedure, pass(idxmap)  :: l2gs2 => repl_l2gs2
    procedure, pass(idxmap)  :: l2gv1 => repl_l2gv1
    procedure, pass(idxmap)  :: l2gv2 => repl_l2gv2

    procedure, pass(idxmap)  :: g2ls1 => repl_g2ls1
    procedure, pass(idxmap)  :: g2ls2 => repl_g2ls2
    procedure, pass(idxmap)  :: g2lv1 => repl_g2lv1
    procedure, pass(idxmap)  :: g2lv2 => repl_g2lv2

    procedure, pass(idxmap)  :: g2ls1_ins => repl_g2ls1_ins
    procedure, pass(idxmap)  :: g2ls2_ins => repl_g2ls2_ins
    procedure, pass(idxmap)  :: g2lv1_ins => repl_g2lv1_ins
    procedure, pass(idxmap)  :: g2lv2_ins => repl_g2lv2_ins

    procedure, pass(idxmap)  :: get_gr   => repl_get_gr
    procedure, pass(idxmap)  :: get_gc   => repl_get_gc
    procedure, pass(idxmap)  :: get_lr   => repl_get_lr
    procedure, pass(idxmap)  :: get_lc   => repl_get_lc
    procedure, pass(idxmap)  :: get_ctxt => repl_get_ctxt
    procedure, pass(idxmap)  :: get_mpic => repl_get_mpic

    procedure, pass(idxmap)  :: set_gr    => repl_set_gr
    procedure, pass(idxmap)  :: set_gc    => repl_set_gc
    procedure, pass(idxmap)  :: set_lr    => repl_set_lr
    procedure, pass(idxmap)  :: set_lc    => repl_set_lc
    procedure, pass(idxmap)  :: set_ctxt  => repl_set_ctxt
    procedure, pass(idxmap)  :: set_mpic  => repl_set_mpic

  end type psb_repl_map

contains

  function repl_get_state(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%state

  end function repl_get_state
  

  function repl_get_gr(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%global_rows

  end function repl_get_gr
  

  function repl_get_gc(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%global_cols

  end function repl_get_gc
  

  function repl_get_lr(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%local_rows

  end function repl_get_lr
  

  function repl_get_lc(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%local_cols

  end function repl_get_lc
  

  function repl_get_ctxt(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%ictxt

  end function repl_get_ctxt
  

  function repl_get_mpic(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer :: val
    
    val = idxmap%mpic

  end function repl_get_mpic
  

  subroutine repl_set_state(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%state = val
  end subroutine repl_set_state

  subroutine repl_set_ctxt(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%ictxt = val
  end subroutine repl_set_ctxt
  
  subroutine repl_set_gr(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%global_rows = val
  end subroutine repl_set_gr

  subroutine repl_set_gc(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%global_cols = val
  end subroutine repl_set_gc

  subroutine repl_set_lr(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%local_rows = val
  end subroutine repl_set_lr

  subroutine repl_set_lc(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%local_rows = val
  end subroutine repl_set_lc

  subroutine repl_set_mpic(idxmap,val)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: val
    
    idxmap%mpic = val
  end subroutine repl_set_mpic

  
  function repl_is_repl(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    logical :: val
    val = .true.
  end function repl_is_repl
    
  
  function repl_is_bld(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    logical :: val
    val = (idxmap%state == psb_desc_bld_)
  end function repl_is_bld
    
  function repl_is_upd(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    logical :: val
    val = (idxmap%state == psb_desc_upd_)
  end function repl_is_upd
    
  function repl_is_asb(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    logical :: val
    val = (idxmap%state == psb_desc_asb_)
  end function repl_is_asb
    
  function repl_is_valid(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    logical :: val
    val = idxmap%is_bld().or.idxmap%is_upd().or.idxmap%is_asb()
  end function repl_is_valid
    
  function repl_sizeof(idxmap) result(val)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer(psb_long_int_k_) :: val
    
    val = 8 * psb_sizeof_int

  end function repl_sizeof


  subroutine repl_free(idxmap)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    
  end subroutine repl_free


  subroutine repl_l2gs1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(inout) :: idx
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned
    integer  :: idxv(1)
    info = 0
    if (present(mask)) then 
      if (.not.mask) return
    end if

    idxv(1) = idx
    call idxmap%l2g(idxv,info,owned=owned)
    idx = idxv(1)

  end subroutine repl_l2gs1

  subroutine repl_l2gs2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned

    idxout = idxin
    call idxmap%l2g(idxout,info,mask,owned)
    
  end subroutine repl_l2gs2


  subroutine repl_l2gv1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(inout) :: idx(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: owned
    integer :: i
    logical :: owned_
    info = 0

    if (present(mask)) then 
      if (size(mask) < size(idx)) then 
        info = -1
        return
      end if
    end if
    if (present(owned)) then 
      owned_ = owned
    else
      owned_ = .false.
    end if

    if (present(mask)) then 

      do i=1, size(idx)
        if (mask(i)) then 
          if ((1<=idx(i)).and.(idx(i) <= idxmap%local_rows)) then
            ! do nothing
          else 
            idx(i) = -1
            info = -1
          end if
        end if
      end do

    else  if (.not.present(mask)) then 

      do i=1, size(idx)
        if ((1<=idx(i)).and.(idx(i) <= idxmap%local_rows)) then
          ! do nothing
        else 
          idx(i) = -1
          info = -1
        end if
      end do

    end if

  end subroutine repl_l2gv1

  subroutine repl_l2gv2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin(:)
    integer, intent(out)   :: idxout(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: owned
    integer :: is, im
    
    is = size(idxin)
    im = min(is,size(idxout))
    idxout(1:im) = idxin(1:im)
    call idxmap%l2g(idxout(1:im),info,mask,owned)
    if (is > im) info = -3 

  end subroutine repl_l2gv2


  subroutine repl_g2ls1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(inout) :: idx
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned
    integer :: idxv(1)
    info = 0

    if (present(mask)) then 
      if (.not.mask) return
    end if
    
    idxv(1) = idx 
    call idxmap%g2l(idxv,info,owned=owned)
    idx = idxv(1) 
      
  end subroutine repl_g2ls1

  subroutine repl_g2ls2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned

    idxout = idxin
    call idxmap%g2l(idxout,info,mask,owned)
    
  end subroutine repl_g2ls2


  subroutine repl_g2lv1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(inout) :: idx(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: owned
    integer :: i, nv, is
    logical :: owned_

    info = 0

    if (present(mask)) then 
      if (size(mask) < size(idx)) then 
        info = -1
        return
      end if
    end if
    if (present(owned)) then 
      owned_ = owned
    else
      owned_ = .false.
    end if

    is = size(idx)

    if (present(mask)) then 

      if (idxmap%is_asb()) then 
        do i=1, is
          if (mask(i)) then 
            if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
              ! do nothing
            else 
              idx(i) = -1
              info = -1
            end if
          end if
        end do
      else if (idxmap%is_valid()) then 
        do i=1,is
          if (mask(i)) then 
            if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
              ! do nothing

            else 
              idx(i) = -1
              info = -1
            end if
          end if
        end do
      else 
        idx(1:is) = -1
        info = -1
      end if

    else  if (.not.present(mask)) then 

      if (idxmap%is_asb()) then 
        do i=1, is
          if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
            ! do nothing
          else 
            idx(i) = -1
            info = -1
          end if
        end do
      else if (idxmap%is_valid()) then 
        do i=1,is
          if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
            ! do nothing 
          else 
            idx(i) = -1
            info = -1
          end if
        end do
      else 
        idx(1:is) = -1
        info = -1
      end if

    end if

  end subroutine repl_g2lv1

  subroutine repl_g2lv2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_repl_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin(:)
    integer, intent(out)   :: idxout(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: owned

    integer :: is, im
    
    is = size(idxin)
    im = min(is,size(idxout))
    idxout(1:im) = idxin(1:im)
    call idxmap%g2l(idxout(1:im),info,mask,owned)
    if (is > im) info = -3 

  end subroutine repl_g2lv2



  subroutine repl_g2ls1_ins(idx,idxmap,info,mask)
    use psb_realloc_mod
    use psb_sort_mod
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(inout) :: idx
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    
    integer :: idxv(1)

    info = 0
    if (present(mask)) then 
      if (.not.mask) return
    end if
    idxv(1) = idx
    call idxmap%g2l_ins(idxv,info)
    idx = idxv(1) 

  end subroutine repl_g2ls1_ins

  subroutine repl_g2ls2_ins(idxin,idxout,idxmap,info,mask)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    
    idxout = idxin
    call idxmap%g2l_ins(idxout,info)
    
  end subroutine repl_g2ls2_ins


  subroutine repl_g2lv1_ins(idx,idxmap,info,mask)
    use psb_realloc_mod
    use psb_sort_mod
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(inout) :: idx(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    integer :: i, nv, is, ix

    info = 0
    is = size(idx)

    if (present(mask)) then 
      if (size(mask) < size(idx)) then 
        info = -1
        return
      end if
    end if


    if (idxmap%is_asb()) then 
      ! State is wrong for this one ! 
      idx = -1
      info = -1

    else if (idxmap%is_valid()) then 

      if (present(mask)) then 
        do i=1, is
          if (mask(i)) then 
            if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
              ! do nothing
            else 
              idx(i) = -1
              info = -1
            end if
          end if
        end do

      else if (.not.present(mask)) then 

        do i=1, is
          if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
            ! do nothing
          else 
            idx(i) = -1
            info = -1
          end if
        end do
      end if

    else 
      idx = -1
      info = -1
    end if

  end subroutine repl_g2lv1_ins

  subroutine repl_g2lv2_ins(idxin,idxout,idxmap,info,mask)
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)    :: idxin(:)
    integer, intent(out)   :: idxout(:)
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask(:)
    integer :: is, im
    
    is = size(idxin)
    im = min(is,size(idxout))
    idxout(1:im) = idxin(1:im)
    call idxmap%g2l_ins(idxout(1:im),info,mask)
    if (is > im) info = -3 

  end subroutine repl_g2lv2_ins





  subroutine repl_init(idxmap,ictxt,nl,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, nl
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot
    integer, allocatable :: vnl(:)

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if
    
    
    idxmap%global_rows  = nl
    idxmap%global_cols  = nl
    idxmap%local_rows   = nl
    idxmap%local_cols   = nl
    idxmap%ictxt        = ictxt
    idxmap%state        = psb_desc_bld_
    call psb_get_mpicomm(ictxt,idxmap%mpic)

  end subroutine repl_init


  subroutine repl_asb(idxmap,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_repl_map), intent(inout) :: idxmap
    integer, intent(out) :: info
    
    integer :: ictxt, iam, np 
    
    info = 0 
    ictxt = idxmap%get_ctxt()
    call psb_info(ictxt,iam,np)

    idxmap%state = psb_desc_asb_
    
  end subroutine repl_asb

end module psb_repl_map_mod
