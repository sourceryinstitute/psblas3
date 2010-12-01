module psb_hash_map_mod
  use psb_const_mod
  use psb_desc_const_mod
  use psb_indx_map_mod
  use psb_hash_mod 
  
  type, extends(psb_indx_map) :: psb_hash_map

    integer              :: hashvsize, hashvmask
    integer, allocatable :: hashv(:), glb_lc(:,:), loc_to_glob(:)
    type(psb_hash_type), allocatable  :: hash

  contains

    procedure, pass(idxmap)  :: initvl    => hash_init_vl
    procedure, pass(idxmap)  :: initvg    => hash_init_vg
    generic,   public        :: init      => initvl, initvg

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

    procedure, pass(idxmap)  :: bld_g2l_map => hash_bld_g2l_map

  end type psb_hash_map

  private :: hash_initvl, hash_initvg, hash_sizeof, hash_asb, &
       & hash_free, hash_get_fmt, hash_l2gs1, hash_l2gs2, &
       & hash_l2gv1, hash_l2gv2, hash_g2ls1, hash_g2ls2, &
       & hash_g2lv1, hash_g2lv2, hash_g2ls1_ins, hash_g2ls2_ins, &
       & hash_g2lv1_ins, hash_g2lv2_ins, hash_init_vlu, &
       & hash_bld_g2l_map


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

  subroutine hash_init_vl(idxmap,ictxt,nl,vl,info)
    use psb_penv_mod
    use psb_error_mod
    use psb_sort_mod
    use psb_realloc_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, vl(:), nl
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot, nlu, m, nrt,int_err(5)
    integer, allocatable :: vlu(:)
    character(len=20), parameter :: name='hash_map_init_vl'

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if

    if (nl > size(vl)) then 
      write(psb_err_unit,*) 'Invalid nl:',&
           & nl, size(vl)
      info = -1
      return
    end if
      
    m   = maxval(vl(1:nl))
    nrt = nl
    call psb_sum(ictxt,nrt)
    call psb_max(ictxt,m)

    allocate(vlu(nl), stat=info) 
    if (info /= 0) then 
      info = -1
      return
    end if

    do i=1,nl
      if ((vl(i)<1).or.(vl(i)>m)) then 
        info = psb_err_entry_out_of_bounds_
        int_err(1) = i
        int_err(2) = vl(i)
        int_err(3) = nl
        int_err(4) = m
        exit
      endif
      vlu(i) = vl(i) 
    end do

    if ((m /= nrt).and.(iam == psb_root_))  then 
      write(psb_err_unit,*) trim(name),&
           & ' Warning: globalcheck=.false., but there is a mismatch'
      write(psb_err_unit,*) trim(name),&
           & '        : in the global sizes!',m,nrt
    end if
    !
    ! Now sort the input items, and check for  duplicates
    ! (unlikely, but possible)
    !
    call psb_msort_unique(vlu,nlu)
    if (nlu /= nl) then 
      write(0,*) 'Warning: duplicates in input'
    end if
   
    call hash_init_vlu(idxmap,ictxt,ntot,nlu,vlu,info)    

  end subroutine hash_init_vl

  subroutine hash_init_vg(idxmap,ictxt,vg,info)
    use psb_penv_mod
    use psb_error_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, vg(:)
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot, lc2, nl, nlu, n, nrt,int_err(5)
    integer :: key, ih, ik, nh, idx, nbits, hsize, hmask
    integer, allocatable :: vlu(:)

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if

    n    = size(vg)
    ntot = n
    nl   = 0
    do i=1, n
      if ((vg(i)<0).or.(vg(i)>=np)) then 
        info = psb_err_partfunc_wrong_pid_
        int_err(1) = 3
        int_err(2) = vg(i)
        int_err(3) = i
        exit
      endif
      if (vg(i) == iam) nl = nl + 1 
    end do
    
    allocate(vlu(nl), stat=info) 
    if (info /= 0) then 
      info = -1
      return
    end if

    j = 0
    do i=1, n
      if (vg(i) == iam) then 
        j      = j + 1 
        vlu(j) = i
      end if
    end do
    
    
    call hash_init_vlu(idxmap,ictxt,ntot,nl,vlu,info)    

    
  end subroutine hash_init_vg


  subroutine hash_init_vlu(idxmap,ictxt,ntot,nl,vlu,info)
    use psb_penv_mod
    use psb_error_mod
    use psb_sort_mod
    use psb_realloc_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, vlu(:), nl, ntot
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, lc2, nlu, m, nrt,int_err(5)
    integer :: key, ih, ik, nh, idx, nbits, hsize, hmask
    character(len=20), parameter :: name='hash_map_init_vlu'

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if

    idxmap%global_rows  = ntot
    idxmap%global_cols  = ntot
    idxmap%local_rows   = nl
    idxmap%local_cols   = nl
    idxmap%ictxt        = ictxt
    idxmap%state        = psb_desc_bld_
    call psb_get_mpicomm(ictxt,idxmap%mpic)

    lc2 = int(1.5*nl) 
    allocate(idxmap%hash,idxmap%loc_to_glob(lc2),stat=info) 
    if (info /= 0)  then
      info = -2
      return
    end if

    call psb_hash_init(nl,idxmap%hash,info)
    if (info /= 0) then 
      info = -3
      return 
    endif

    do i=1, nl
      idxmap%loc_to_glob(i) = vlu(i) 
    end do

    call hash_bld_g2l_map(idxmap,ictxt,info)

  end subroutine hash_init_vlu



  subroutine hash_bld_g2l_map(idxmap,info)
    use psb_penv_mod
    use psb_error_mod
    use psb_sort_mod
    use psb_realloc_mod
    implicit none 
    class(psb_hash_map), intent(inout) :: idxmap
    integer, intent(out) :: info
    !  To be implemented
    integer :: ictxt, iam, np, i, j, lc2, nlu, m, nrt,int_err(5), nl
    integer :: key, ih, ik, nh, idx, nbits, hsize, hmask
    character(len=20), parameter :: name='hash_map_init_vlu'

    info = 0
    ictxt = idxmap%get_ctxt()

    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      write(psb_err_unit,*) 'Invalid ictxt:',ictxt
      info = -1
      return
    end if

    nl = idxmap%get_lc()

    call psb_realloc(nl,2,idxmap%glb_lc,info) 

    nbits = psb_hash_bits
    hsize = 2**nbits
    do 
      if (hsize < 0) then 
        ! This should never happen for sane values
        ! of psb_max_hash_bits.
        write(psb_err_unit,*) &
             & 'Error: hash size overflow ',hsize,nbits
        info = -2 
        return
      end if
      if (hsize > nl) exit
      if (nbits >= psb_max_hash_bits) exit
      nbits = nbits + 1
      hsize = hsize * 2 
    end do

    hmask = hsize - 1 
    idxmap%hashvsize = hsize
    idxmap%hashvmask = hmask

    if (info == psb_success_) &
         & call psb_realloc(hsize+1,idxmap%hashv,info,lb=0)
    if (info /= psb_success_) then 
! !$      ch_err='psb_realloc'
! !$      call psb_errpush(info,name,a_err=ch_err)
! !$      goto 9999
      info = -4 
      return
    end if

    idxmap%hashv(:) = 0

    do i=1, nl
      key = idxmap%loc_to_glob(i) 
      ih  = iand(key,hmask) 
      idxmap%hashv(ih) = idxmap%hashv(ih) + 1
    end do

    nh = idxmap%hashv(0) 
    idx = 1

    do i=1, hsize
      idxmap%hashv(i-1) = idx
      idx = idx + nh
      nh = idxmap%hashv(i)
    end do

    do i=1, nl
      key                  = idxmap%loc_to_glob(i)
      ih                   = iand(key,hmask)
      idx                  = idxmap%hashv(ih) 
      idxmap%glb_lc(idx,1) = key
      idxmap%glb_lc(idx,2) = i
      idxmap%hashv(ih)     = idxmap%hashv(ih) + 1
    end do

    do i = hsize, 1, -1 
      idxmap%hashv(i) = idxmap%hashv(i-1)
    end do

    idxmap%hashv(0) = 1
    do i=0, hsize-1 
      idx = idxmap%hashv(i)
      nh  = idxmap%hashv(i+1) - idxmap%hashv(i) 
      if (nh > 1) then 
        call psb_msort(idxmap%glb_lc(idx:idx+nh-1,1),&
             & ix=idxmap%glb_lc(idx:idx+nh-1,2),&
             & flag=psb_sort_keep_idx_)
      end if
    end do
    
  end subroutine hash_bld_g2l_map


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

    call hash_bld_g2l_map(idxmap,info)

    call idxmap%set_state(psb_desc_asb_)
    
  end subroutine hash_asb

  function hash_get_fmt(idxmap) result(res)
    implicit none 
    class(psb_hash_map), intent(in) :: idxmap
    character(len=5) :: res
    res = 'HASH'
  end function hash_get_fmt

end module psb_hash_map_mod
