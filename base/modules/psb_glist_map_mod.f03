module psb_glist_map_mod
  use psb_const_mod
  use psb_desc_const_mod
  use psb_list_map_mod
  
  type, extends(psb_list_map) :: psb_glist_map
    integer, allocatable :: vgp(:)
  contains
    procedure, pass(idxmap)  :: initvg  => glist_initvg
    generic, public          :: init    => initvg
    procedure, pass(idxmap)  :: sizeof  => glist_sizeof
    procedure, pass(idxmap)  :: free    => glist_free
    procedure, pass(idxmap)  :: get_fmt => glist_get_fmt

  end type psb_glist_map

  private :: glist_initvg, glist_sizeof, glist_free, glist_get_fmt


contains

    
  function glist_sizeof(idxmap) result(val)
    implicit none 
    class(psb_glist_map), intent(in) :: idxmap
    integer(psb_long_int_k_) :: val
    
    val = idxmap%psb_list_map%sizeof()

    if (allocated(idxmap%vgp)) &
         & val = val + size(idxmap%vgp)*psb_sizeof_int

  end function glist_sizeof


  subroutine glist_free(idxmap)
    implicit none 
    class(psb_glist_map), intent(inout) :: idxmap
    
    if (allocated(idxmap%vgp)) &
         & deallocate(idxmap%vgp)
    
    call idxmap%psb_list_map%free()
    
  end subroutine glist_free




  subroutine glist_initvg(idxmap,vg,ictxt,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_glist_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, vg(:)
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, n, nl
    

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if
    n = size(vg) 
    
    idxmap%global_rows  = n
    idxmap%global_cols  = n

    allocate(idxmap%loc_to_glob(n),idxmap%glob_to_loc(n),&
         & idxmap%vgp(n),stat=info) 
    if (info /= 0)  then
      info = -2
      return
    end if

    idxmap%ictxt        = ictxt
    idxmap%state        = psb_desc_bld_
    call psb_get_mpicomm(ictxt,idxmap%mpic)

    nl = 0 
    do i=1, n 
      if ((vg(i)  > np-1).or.(vg(i) < 0)) then
        info=psb_err_partfunc_wrong_pid_
        exit
      end if
      idxmap%vgp(i) = vg(i)
      if (vg(i) == iam) then
        ! this point belongs to me
        nl = nl + 1
        idxmap%glob_to_loc(i)  = nl
        idxmap%loc_to_glob(nl) = i
      else
        idxmap%glob_to_loc(i) = -(np+vg(i)+1)
      end if
    end do
    
    call idxmap%set_lr(nl)
    call idxmap%set_lc(nl)
   
  end subroutine glist_initvg

  function glist_get_fmt(idxmap) result(res)
    implicit none 
    class(psb_glist_map), intent(in) :: idxmap
    character(len=5) :: res
    res = 'GLIST'
  end function glist_get_fmt


end module psb_glist_map_mod
