module psb_hash_map_mod
  use psb_const_mod
  use psb_desc_const_mod
  use psb_indx_map_mod
  use psb_hash_mod 
  
  type, extends(psb_indx_map) :: psb_hash_map

    integer              :: hashvsize, hashvmask
    integer, allocatable :: hashv(:), glb_lc(:,:)
    type(psb_hash_type), allocatable  :: hash
  contains
    procedure, pass(idxmap)  :: initvl    => hash_init_vl
    procedure, pass(idxmap)  :: initvg    => hash_init_vg
    generic, public          :: init      => initvl, initvg

    procedure, pass(idxmap)  :: sizeof    => hash_sizeof
    procedure, pass(idxmap)  :: asb       => hash_asb
    procedure, pass(idxmap)  :: free      => hash_free
    procedure, pass(idxmap)  :: get_fmt   => hash_get_fmt

    procedure, pass(idxmap)  :: l2gs1     => hash_l2gs1
    procedure, pass(idxmap)  :: l2gs2     => hash_l2gs2
    procedure, pass(idxmap)  :: l2gv1     => hash_l2gv1
    procedure, pass(idxmap)  :: l2gv2     => hash_l2gv2

    procedure, pass(idxmap)  :: g2ls1     => hash_g2ls1
    procedure, pass(idxmap)  :: g2ls2     => hash_g2ls2
    procedure, pass(idxmap)  :: g2lv1     => hash_g2lv1
    procedure, pass(idxmap)  :: g2lv2     => hash_g2lv2

    procedure, pass(idxmap)  :: g2ls1_ins => hash_g2ls1_ins
    procedure, pass(idxmap)  :: g2ls2_ins => hash_g2ls2_ins
    procedure, pass(idxmap)  :: g2lv1_ins => hash_g2lv1_ins
    procedure, pass(idxmap)  :: g2lv2_ins => hash_g2lv2_ins

  end type psb_hash_map

  private :: hash_initvl, hash_initvg, hash_sizeof, hash_asb,&
       & hash_free, hash_get_fmt, hash_l2gs1, hash_l2gs2,&
       & hash_l2gv1, hash_l2gv2, hash_g2ls1, hash_g2ls2,&
       & hash_g2lv1, hash_g2lv2, hash_g2ls1_ins, hash_g2ls2_ins,&
       & hash_g2lv1_ins, hash_g2lv2_ins


contains

  
  function hash_sizeof(idxmap) result(val)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
    integer(psb_long_int_k_) :: val
    
    val = idxmap%psb_indx_map%sizeof() 
    val = val + 2 * psb_sizeof_int
    if (allocated(idxmap%hashv)) &
         & val = val + size(idxmap%hashv)*psb_sizeof_int
    if (allocated(idxmap%glb_lc)) &
         & val = val + size(idxmap%glb_lc)*psb_sizeof_int
    if (allocated(idxmap%hash)) &
         & val = val + psb_sizeof(idxmap%hash)

  end function hash_sizeof


  subroutine hash_free(idxmap)
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer :: info
    
    if (allocated(idxmap%hashv)) &
         & deallocate(idxmap%hashv)
    if (allocated(idxmap%glb_lc)) &
         & deallocate(idxmap%glb_lc)

    if (allocated(idxmap%hash)) then 
      call psb_free(idxmap%hash,info) 
      deallocate(idxmap%hash)
    end if

    call idxmap%psb_indx_map%free()

  end subroutine hash_free


  subroutine hash_l2gs1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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

  end subroutine hash_l2gs1

  subroutine hash_l2gs2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned

    idxout = idxin
    call idxmap%l2g(idxout,info,mask,owned)
    
  end subroutine hash_l2gs2


  subroutine hash_l2gv1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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

!!$    if (present(mask)) then 
!!$
!!$      do i=1, size(idx)
!!$        if (mask(i)) then 
!!$          if ((1<=idx(i)).and.(idx(i) <= idxmap%local_rows)) then
!!$            idx(i) = idxmap%min_glob_row + idx(i) - 1
!!$          else if ((idxmap%local_rows < idx(i)).and.(idx(i) <= idxmap%local_cols)&
!!$               & .and.(.not.owned_)) then
!!$            idx(i) = idxmap%loc_to_glob(idx(i)-idxmap%local_rows)
!!$          else 
!!$            idx(i) = -1
!!$            info = -1
!!$          end if
!!$        end if
!!$      end do
!!$
!!$    else  if (.not.present(mask)) then 
!!$
!!$      do i=1, size(idx)
!!$        if ((1<=idx(i)).and.(idx(i) <= idxmap%local_rows)) then
!!$          idx(i) = idxmap%min_glob_row + idx(i) - 1
!!$        else if ((idxmap%local_rows < idx(i)).and.(idx(i) <= idxmap%local_cols)&
!!$             & .and.(.not.owned_)) then
!!$          idx(i) = idxmap%loc_to_glob(idx(i)-idxmap%local_rows)
!!$        else 
!!$          idx(i) = -1
!!$          info = -1
!!$        end if
!!$      end do
!!$
!!$    end if

  end subroutine hash_l2gv1

  subroutine hash_l2gv2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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

  end subroutine hash_l2gv2


  subroutine hash_g2ls1(idx,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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
      
  end subroutine hash_g2ls1

  subroutine hash_g2ls2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    logical, intent(in), optional :: owned

    idxout = idxin
    call idxmap%g2l(idxout,info,mask,owned)
    
  end subroutine hash_g2ls2


  subroutine hash_g2lv1(idx,idxmap,info,mask,owned)
    use psb_sort_mod
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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

!!$    if (present(mask)) then 
!!$
!!$      if (idxmap%is_asb()) then 
!!$        do i=1, is
!!$          if (mask(i)) then 
!!$            if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$              idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$            else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)&
!!$                 &.and.(.not.owned_)) then
!!$              nv  = size(idxmap%srt_l2g,1)
!!$              idx(i) = psb_ibsrch(idx(i),nv,idxmap%srt_l2g(:,1))
!!$              if (idx(i) > 0) idx(i) = idxmap%srt_l2g(idx(i),2)+idxmap%local_rows
!!$            else 
!!$              idx(i) = -1
!!$              info = -1
!!$            end if
!!$          end if
!!$        end do
!!$      else if (idxmap%is_valid()) then 
!!$        do i=1,is
!!$          if (mask(i)) then 
!!$            if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$              idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$            else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)&
!!$                 &.and.(.not.owned_)) then
!!$              nv  = idxmap%local_cols-idxmap%local_rows+1
!!$              idx(i) = psb_issrch(idx(i),nv,idxmap%loc_to_glob)
!!$              if (idx(i) > 0) idx(i) = idx(i) + idxmap%local_rows
!!$            else 
!!$              idx(i) = -1
!!$              info = -1
!!$            end if
!!$          end if
!!$        end do
!!$      else 
!!$        idx(1:is) = -1
!!$        info = -1
!!$      end if
!!$
!!$    else  if (.not.present(mask)) then 
!!$
!!$      if (idxmap%is_asb()) then 
!!$        do i=1, is
!!$          if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$            idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$          else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)&
!!$               &.and.(.not.owned_)) then
!!$            nv  = size(idxmap%srt_l2g,1)
!!$            idx(i) = psb_ibsrch(idx(i),nv,idxmap%srt_l2g(:,1))
!!$            if (idx(i) > 0) idx(i) = idxmap%srt_l2g(idx(i),2)+idxmap%local_rows
!!$          else 
!!$            idx(i) = -1
!!$            info = -1
!!$          end if
!!$        end do
!!$      else if (idxmap%is_valid()) then 
!!$        do i=1,is
!!$          if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$            idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$          else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)&
!!$               &.and.(.not.owned_)) then
!!$            nv  = idxmap%local_cols-idxmap%local_rows+1
!!$            idx(i) = psb_issrch(idx(i),nv,idxmap%loc_to_glob)
!!$            if (idx(i) > 0) idx(i) = idx(i) + idxmap%local_rows
!!$          else 
!!$            idx(i) = -1
!!$            info = -1
!!$          end if
!!$        end do
!!$      else 
!!$        idx(1:is) = -1
!!$        info = -1
!!$      end if
!!$
!!$    end if

  end subroutine hash_g2lv1

  subroutine hash_g2lv2(idxin,idxout,idxmap,info,mask,owned)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
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

  end subroutine hash_g2lv2



  subroutine hash_g2ls1_ins(idx,idxmap,info,mask)
    use psb_realloc_mod
    use psb_sort_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
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

  end subroutine hash_g2ls1_ins

  subroutine hash_g2ls2_ins(idxin,idxout,idxmap,info,mask)
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)    :: idxin
    integer, intent(out)   :: idxout
    integer, intent(out)   :: info 
    logical, intent(in), optional :: mask
    
    idxout = idxin
    call idxmap%g2l_ins(idxout,info)
    
  end subroutine hash_g2ls2_ins


  subroutine hash_g2lv1_ins(idx,idxmap,info,mask)
    use psb_realloc_mod
    use psb_sort_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
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

!!$      if (present(mask)) then 
!!$        do i=1, is
!!$          if (mask(i)) then 
!!$            if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$              idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$            else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
!!$              nv  = idxmap%local_cols-idxmap%local_rows+1
!!$              ix  = psb_issrch(idx(i),nv,idxmap%loc_to_glob)
!!$              if (ix < 0) then 
!!$                ix = idxmap%local_cols + 1
!!$                call psb_ensure_size(ix,idxmap%loc_to_glob,info,addsz=500)
!!$                if (info /= 0) then 
!!$                  info = -4
!!$                  return
!!$                end if
!!$                idxmap%local_cols      = ix
!!$                ix                     = ix - idxmap%local_rows
!!$                idxmap%loc_to_glob(ix) = idx(i)
!!$              end if
!!$              idx(i)                   = ix
!!$            else 
!!$              idx(i) = -1
!!$              info = -1
!!$            end if
!!$          end if
!!$        end do
!!$
!!$      else if (.not.present(mask)) then 
!!$
!!$        do i=1, is
!!$          if ((idxmap%min_glob_row <= idx(i)).and.(idx(i) <= idxmap%max_glob_row)) then
!!$            idx(i) = idx(i) - idxmap%min_glob_row + 1
!!$          else if ((1<= idx(i)).and.(idx(i) <= idxmap%global_rows)) then
!!$            nv  = idxmap%local_cols-idxmap%local_rows+1
!!$            ix  = psb_issrch(idx(i),nv,idxmap%loc_to_glob)
!!$            if (ix < 0) then 
!!$              ix = idxmap%local_cols + 1
!!$              call psb_ensure_size(ix,idxmap%loc_to_glob,info,addsz=500)
!!$              if (info /= 0) then 
!!$                info = -4
!!$                return
!!$              end if
!!$              idxmap%loc_to_glob(ix) = idx(i)
!!$              idxmap%local_cols      = ix
!!$            end if
!!$            idx(i)                   = ix
!!$          else 
!!$            idx(i) = -1
!!$            info = -1
!!$          end if
!!$        end do
!!$      end if

    else 
      idx = -1
      info = -1
    end if

  end subroutine hash_g2lv1_ins

  subroutine hash_g2lv2_ins(idxin,idxout,idxmap,info,mask)
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
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

  end subroutine hash_g2lv2_ins

  subroutine hash_init_vl(idxmap,ictxt,vl,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, vl(:)
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot, nl
    integer, allocatable :: vnl(:)

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if
    allocate(vnl(0:np),stat=info)
    if (info /= 0)  then
      info = -2
      return
    end if

    nl = size(vl)
    vnl(:)   = 0
    vnl(iam) = nl
    call psb_sum(ictxt,vnl)
    ntot = sum(vnl)
    vnl(1:np) = vnl(0:np-1)
    vnl(0) = 0
    do i=1,np
      vnl(i) = vnl(i) + vnl(i-1)
    end do
    if (ntot /= vnl(np)) then 
      write(0,*) ' Mismatch in hash_init ',ntot,vnl(np)
    end if

    
    idxmap%global_rows  = ntot
    idxmap%global_cols  = ntot
    idxmap%local_rows   = nl
    idxmap%local_cols   = nl
    idxmap%ictxt        = ictxt
    idxmap%state        = psb_desc_bld_
    call psb_get_mpicomm(ictxt,idxmap%mpic)
!!$    idxmap%min_glob_row = vnl(iam)+1
!!$    idxmap%max_glob_row = vnl(iam+1) 
!!$    call move_alloc(vnl,idxmap%vnl)
!!$    allocate(idxmap%loc_to_glob(nl),stat=info) 
    if (info /= 0)  then
      info = -2
      return
    end if
    
    
  end subroutine hash_init_vl

  subroutine hash_init_vg(idxmap,ictxt,n,vg,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, n,vg(:)
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot, nl
    integer, allocatable :: vnl(:)

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if
    allocate(vnl(0:np),stat=info)
    if (info /= 0)  then
      info = -2
      return
    end if
    
    ntot = n
    nl = 0
    
    idxmap%global_rows  = ntot
    idxmap%global_cols  = ntot
    idxmap%local_rows   = nl
    idxmap%local_cols   = nl
    idxmap%ictxt        = ictxt
    idxmap%state        = psb_desc_bld_
    call psb_get_mpicomm(ictxt,idxmap%mpic)
!!$    idxmap%min_glob_row = vnl(iam)+1
!!$    idxmap%max_glob_row = vnl(iam+1) 
!!$    call move_alloc(vnl,idxmap%vnl)
!!$    allocate(idxmap%loc_to_glob(nl),stat=info) 
    if (info /= 0)  then
      info = -2
      return
    end if
    
    
  end subroutine hash_init_vg


  subroutine hash_asb(idxmap,info)
    use psb_penv_mod
    use psb_error_mod
    use psb_realloc_mod
    use psb_sort_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(out) :: info
    
    integer :: nhal, ictxt, iam, np 
    
    info = 0 
    ictxt = idxmap%get_ctxt()
    call psb_info(ictxt,iam,np)

    nhal = idxmap%local_cols-idxmap%local_rows

!!$    call psb_realloc(nhal,idxmap%loc_to_glob,info)
!!$    call psb_realloc(nhal,2,idxmap%srt_l2g,info)
!!$    idxmap%srt_l2g(1:nhal,1) = idxmap%loc_to_glob(1:nhal)
!!$    call psb_qsort(idxmap%srt_l2g(:,1),&
!!$         & ix=idxmap%srt_l2g(:,2),dir=psb_sort_up_)
!!$    idxmap%state = psb_desc_asb_
    
  end subroutine hash_asb

  function hash_get_fmt(idxmap) result(res)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
    character(len=5) :: res
    res = 'HASH'
  end function hash_get_fmt

end module psb_hash_map_mod
