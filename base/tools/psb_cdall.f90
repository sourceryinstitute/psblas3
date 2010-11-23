
subroutine psb_cdall(ictxt, desc, info,mg,ng,parts,vg,vl,flag,nl,repl, globalcheck)
  use psb_descriptor_type
  use psb_serial_mod
  use psb_const_mod
  use psb_error_mod
  use psb_penv_mod
  use psb_gen_block_map_mod
  use psb_repl_map_mod
  implicit None
  include 'parts.fh'
  Integer, intent(in)               :: mg,ng,ictxt, vg(:), vl(:),nl
  integer, intent(in)               :: flag
  logical, intent(in)               :: repl, globalcheck
  integer, intent(out)              :: info
  type(psb_desc_type), intent(out)  :: desc

  optional :: mg,ng,parts,vg,vl,flag,nl,repl, globalcheck

  interface 
    subroutine psb_cdals(m, n, parts, ictxt, desc, info)
      use psb_descriptor_type
      include 'parts.fh'
      Integer, intent(in)                 :: m,n,ictxt
      Type(psb_desc_type), intent(out)    :: desc
      integer, intent(out)                :: info
    end subroutine psb_cdals
    subroutine psb_cdalv(v, ictxt, desc, info, flag)
      use psb_descriptor_type
      Integer, intent(in)               :: ictxt, v(:)
      integer, intent(in), optional     :: flag
      integer, intent(out)              :: info
      Type(psb_desc_type), intent(out)  :: desc
    end subroutine psb_cdalv
    subroutine psb_cd_inloc(v, ictxt, desc, info, globalcheck)
      use psb_descriptor_type
      implicit None
      Integer, intent(in)               :: ictxt, v(:)
      integer, intent(out)              :: info
      type(psb_desc_type), intent(out)  :: desc
      logical, intent(in), optional     :: globalcheck
    end subroutine psb_cd_inloc
    subroutine psb_cdrep(m, ictxt, desc,info)
      use psb_descriptor_type
      Integer, intent(in)               :: m,ictxt
      Type(psb_desc_type), intent(out)  :: desc
      integer, intent(out)              :: info
    end subroutine psb_cdrep
  end interface
  character(len=20)   :: name
  integer :: err_act, n_, flag_, i, me, np, nlp, nnv
  integer, allocatable :: itmpsz(:) 



  if (psb_get_errstatus() /= 0) return 
  info=psb_success_
  name = 'psb_cdall'
  call psb_erractionsave(err_act)

  call psb_info(ictxt, me, np)

  if (count((/ present(vg),present(vl),&
       &  present(parts),present(nl), present(repl) /)) /= 1) then 
    info=psb_err_no_optional_arg_
    call psb_errpush(info,name,a_err=" vg, vl, parts, nl, repl")
    goto 999 
  endif

  desc%base_desc => null() 


  if (present(parts)) then 
    if (.not.present(mg)) then 
      info=psb_err_no_optional_arg_
      call psb_errpush(info,name)
      goto 999 
    end if
    if (present(ng)) then 
      n_ = ng
    else
      n_ = mg 
    endif
    call  psb_cdals(mg, n_, parts, ictxt, desc, info)

    if (info == psb_success_) then 
      if (np == 1) then 
        allocate(psb_repl_map      :: desc%indxmap, stat=info)
        if (info == psb_success_) then 
          select type(aa => desc%indxmap) 
          type is (psb_repl_map) 
            call aa%init(ictxt,mg,info)
          class default 
              ! This cannot happen 
            info = psb_err_internal_error_
          end select
        end if
      end if
    end if

  else if (present(repl)) then 
    if (.not.present(mg)) then 
      info=psb_err_no_optional_arg_
      call psb_errpush(info,name)
      goto 999 
    end if
    if (.not.repl) then 
      info=psb_err_no_optional_arg_
      call psb_errpush(info,name)
      goto 999 
    end if

    call  psb_cdrep(mg, ictxt, desc, info)
    if (info == psb_success_) then 
      allocate(psb_repl_map      :: desc%indxmap, stat=info)
      if (info == psb_success_) then 
        select type(aa => desc%indxmap) 
        type is (psb_repl_map) 
          call aa%init(ictxt,mg,info)
        class default 
            ! This cannot happen 
          info = psb_err_internal_error_
        end select
      end if
    end if



  else if (present(vg)) then 

    if (present(flag)) then 
      flag_=flag
    else
      flag_=0
    endif
    if (present(mg)) then 
      nnv = min(mg,size(vg))
    else
      nnv = size(vg)
    end if
    call psb_cdalv(vg(1:nnv), ictxt, desc, info, flag=flag_)

    if (info == psb_success_) then 
      if (np == 1) then 
        allocate(psb_repl_map      :: desc%indxmap, stat=info)
        if (info == psb_success_) then 
          select type(aa => desc%indxmap) 
          type is (psb_repl_map) 
            call aa%init(ictxt,nnv,info)
          class default 
            ! This cannot happen 
            info = psb_err_internal_error_
          end select
        end if
      end if
    end if

  else if (present(vl)) then 

    if (present(nl)) then 
      nnv = min(nl,size(vl))
    else
      nnv = size(vl)
    end if
    call psb_cd_inloc(vl(1:nnv),ictxt,desc,info, globalcheck=globalcheck)
    if (info == psb_success_) then 
      if (np == 1) then 
        allocate(psb_repl_map      :: desc%indxmap, stat=info)
        if (info == psb_success_) then 
          select type(aa => desc%indxmap) 
          type is (psb_repl_map) 
            ! This is potentially wrong if there are multiple entries.
            ! 
            call aa%init(ictxt,nnv,info)
          class default 
              ! This cannot happen 
            info = psb_err_internal_error_
          end select
        end if
      end if
    end if

  else if (present(nl)) then 

    allocate(itmpsz(0:np-1),stat=info)
    if (info /= psb_success_) then 
      info = psb_err_alloc_dealloc_ 
      call psb_errpush(info,name)
      goto 999
    endif

    itmpsz = 0
    itmpsz(me) = nl
    call psb_sum(ictxt,itmpsz)
    nlp=0 
    do i=0, me-1
      nlp = nlp + itmpsz(i)
    end do
    call psb_cd_inloc((/(i,i=nlp+1,nlp+nl)/),ictxt,desc,info,globalcheck=.false.)

    if (info == psb_success_) then 
      if (np == 1) then 
        allocate(psb_repl_map      :: desc%indxmap, stat=info)
      else
        allocate(psb_gen_block_map :: desc%indxmap, stat=info)
      end if
      if (info == psb_success_) then 
        select type(aa => desc%indxmap) 
        type is (psb_repl_map) 
          call aa%init(ictxt,nl,info)
        type is (psb_gen_block_map) 
          call aa%init(ictxt,nl,info)
        class default 
            ! This cannot happen 
          info = psb_err_internal_error_
        end select
      end if
    end if
  endif

  if (info /= psb_success_) goto 999

  call psb_erractionrestore(err_act)
  return

999 continue
  call psb_erractionrestore(err_act)
  if (err_act == psb_act_abort_) then
    call psb_error(ictxt)
    return
  end if
  return


end subroutine psb_cdall
