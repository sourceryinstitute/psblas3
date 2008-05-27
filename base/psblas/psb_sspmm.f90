!!$ 
!!$              Parallel Sparse BLAS  version 2.2
!!$    (C) Copyright 2006/2007/2008
!!$                       Salvatore Filippone    University of Rome Tor Vergata
!!$                       Alfredo Buttari        University of Rome Tor Vergata
!!$ 
!!$  Redistribution and use in source and binary forms, with or without
!!$  modification, are permitted provided that the following conditions
!!$  are met:
!!$    1. Redistributions of source code must retain the above copyright
!!$       notice, this list of conditions and the following disclaimer.
!!$    2. Redistributions in binary form must reproduce the above copyright
!!$       notice, this list of conditions, and the following disclaimer in the
!!$       documentation and/or other materials provided with the distribution.
!!$    3. The name of the PSBLAS group or the names of its contributors may
!!$       not be used to endorse or promote products derived from this
!!$       software without specific written permission.
!!$ 
!!$  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!!$  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
!!$  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!!$  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE PSBLAS GROUP OR ITS CONTRIBUTORS
!!$  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!!$  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!!$  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!!$  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!!$  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!!$  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!!$  POSSIBILITY OF SUCH DAMAGE.
!!$ 
!!$  
! File: psb_sspmm.f90
!
! Subroutine: psb_sspmm
!     Performs one of the distributed matrix-vector operations
!
!     sub( Y ) := alpha * Pr * A * Pc * sub( X )  + beta * sub( Y ),  or
!
!     sub( Y ) := alpha * Pr * A' * Pr * sub( X )  + beta * sub( Y ),
!
!     where:
!
!        sub( X ) denotes:  X(1:N,JX:JX+K-1),
!
!        sub( Y ) denotes:  Y(1:M,JY:JY+K-1),
!
!  alpha and beta are scalars, and sub( X ) and sub( Y ) are distributed
!  vectors and A is a M-by-N distributed matrix.
!
! Arguments:   
!    alpha   -  real                   The scalar alpha.
!    a       -  type(psb_zspmat_type). The sparse matrix containing A.
!    x(:,:)  -  real                   The input vector containing the entries of ( X ).
!    beta    -  real                   The scalar beta.
!    y(:,:)  -  real                   The input vector containing the entries of ( Y ).
!    desc_a  -  type(psb_desc_type).   The communication descriptor.
!    info    -  integer.               Return code
!    trans   -  character(optional).   Whether A or A'. Default: 'N' 
!    k       -  integer(optional).     The number of right-hand sides.
!    jx      -  integer(optional).     The column offset for ( X ). Default:  1
!    jy      -  integer(optional).     The column offset for ( Y ). Default:  1
!    work(:) -  real,(optional).       Working area.
!    doswap  -  logical(optional).     Whether to performe halo updates.
! 
subroutine  psb_sspmm(alpha,a,x,beta,y,desc_a,info,&
     & trans, k, jx, jy, work, doswap)   

  use psb_spmat_type
  use psb_serial_mod
  use psb_descriptor_type
  use psb_comm_mod
  use psi_mod
  use psb_check_mod
  use psb_error_mod
  use psb_string_mod
  use psb_penv_mod
  implicit none

  real(psb_spk_), intent(in)             :: alpha, beta
  real(psb_spk_), intent(inout), target  :: x(:,:)
  real(psb_spk_), intent(inout), target  :: y(:,:)
  type(psb_sspmat_type), intent(in)        :: a
  type(psb_desc_type), intent(in)          :: desc_a
  integer, intent(out)                     :: info
  real(psb_spk_), optional, target       :: work(:)
  character, intent(in), optional          :: trans
  integer, intent(in), optional            :: k, jx, jy
  logical, intent(in), optional            :: doswap

  ! locals
  integer                  :: ictxt, np, me,&
       & err_act, n, iix, jjx, ia, ja, iia, jja, ix, iy, ik, ijx, ijy,&
       & m, nrow, ncol, lldx, lldy, liwork, iiy, jjy,&
       & i, ib, ib1, ip, idx
  integer, parameter       :: nb=4
  real(psb_spk_), pointer     :: xp(:,:), yp(:,:), iwork(:)
  real(psb_spk_), allocatable :: xvsave(:,:)
  character                :: trans_
  character(len=20)        :: name, ch_err
  logical                  :: aliw, doswap_
  integer                  :: debug_level, debug_unit

  name='psb_sspmm'
  if(psb_get_errstatus() /= 0) return 
  info=0
  call psb_erractionsave(err_act)
  debug_unit  = psb_get_debug_unit()
  debug_level = psb_get_debug_level()

  ictxt=psb_cd_get_context(desc_a)

  call psb_info(ictxt, me, np)
  if (np == -1) then
    info = 2010
    call psb_errpush(info,name)
    goto 9999
  endif

  ia = 1
  ja = 1

  ix = 1
  if (present(jx)) then
    ijx = jx
  else
    ijx = 1
  endif

  iy = 1
  if (present(jy)) then
    ijy = jy
  else
    ijy = 1
  endif

  if (present(doswap)) then
    doswap_ = doswap
  else
    doswap_ = .true.
  endif

  if (present(k)) then     
    ik = min(k,size(x,2)-ijx+1)
    ik = min(ik,size(y,2)-ijy+1)
  else
    ik = min(size(x,2)-ijx+1,size(y,2)-ijy+1)
  endif

  if (present(trans)) then     
    trans_ = psb_toupper(trans)
  else
    trans_ = 'N'
  endif
  if ( (trans_ == 'N').or.(trans_ == 'T')&
       & .or.(trans_ == 'C')) then
  else
    info = 70
    call psb_errpush(info,name)
    goto 9999
  end if

  m    = psb_cd_get_global_rows(desc_a)
  n    = psb_cd_get_global_cols(desc_a)
  nrow = psb_cd_get_local_rows(desc_a)
  ncol = psb_cd_get_local_cols(desc_a)
  lldx = size(x,1)
  lldy = size(y,1)

  ! check for presence/size of a work area
  liwork= 2*ncol

  if (present(work)) then
    if (size(work) >= liwork) then
      aliw =.false.
    else
      aliw=.true.
    endif
  else
    aliw=.true.
  end if

  if (aliw) then
    allocate(iwork(liwork),stat=info)
    if(info /= 0) then
      info=4010
      ch_err='psb_realloc'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
  else
    iwork => work
  endif

  iwork(1)=szero

  ! checking for matrix correctness
  call psb_chkmat(m,n,ia,ja,desc_a,info,iia,jja)
  if(info /= 0) then
    info=4010
    ch_err='psb_chkmat'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if


  if (trans_ == 'N') then
    !  Matrix is not transposed
    if((ja /= ix).or.(ia /= iy)) then
      ! this case is not yet implemented
      info = 3030
      call psb_errpush(info,name)
      goto 9999
    end if

    ! checking for vectors correctness
    call psb_chkvect(n,ik,size(x,1),ix,ijx,desc_a,info,iix,jjx)
    if (info == 0) &
         & call psb_chkvect(m,ik,size(y,1),iy,ijy,desc_a,info,iiy,jjy)
    if(info /= 0) then
      info=4010
      ch_err='psb_chkvect'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    if((iix /= 1).or.(iiy /= 1)) then
      ! this case is not yet implemented
      info = 3040
      call psb_errpush(info,name)
      goto 9999
    end if


    if (doswap_.and.(np>1)) then 
      ib1=min(nb,ik)
      xp => x(iix:lldx,jjx:jjx+ib1-1)
      if (doswap_)&
           & call psi_swapdata(ior(psb_swap_send_,psb_swap_recv_),&
           & ib1,szero,xp,desc_a,iwork,info)


      blk: do i=1, ik, nb
        ib=ib1
        ib1 = max(0,min(nb,(ik)-(i-1+ib)))
        xp => x(iix:lldx,jjx+i+ib-1:jjx+i+ib+ib1-2)
        if ((ib1 > 0).and.(doswap_)) &
             & call psi_swapdata(psb_swap_send_,ib1,&
             & szero,xp,desc_a,iwork,info)

        if(info /= 0) exit blk

        !  local Matrix-vector product
        call psb_csmm(alpha,a,x(:,jjx+i-1:jjx+i+ib-1),&
             & beta,y(:,jjy+i-1:jjy+i+ib-1),info,trans=trans_)

        if(info /= 0) exit blk

        if((ib1 > 0).and.(doswap_))&
             & call psi_swapdata(psb_swap_recv_,ib1,&
             & szero,xp,desc_a,iwork,info)

        if(info /= 0) exit blk
      end do blk
    else
      if (doswap_)&
           & call psi_swapdata(ior(psb_swap_send_,psb_swap_recv_),&
           & ib1,szero,x(:,1:ik),desc_a,iwork,info)
      if (info == 0) call psb_csmm(alpha,a,x(:,1:ik),beta,y(:,1:ik),info)
    end if
    if(info /= 0) then
      info = 4011
      call psb_errpush(info,name)
      goto 9999
    end if

  else

    !  Matrix is transposed
    if((ja /= iy).or.(ia /= ix)) then
      ! this case is not yet implemented
      info = 3030
      call psb_errpush(info,name)
      goto 9999
    end if


    ! checking for vectors correctness
    call psb_chkvect(m,ik,size(x,1),ix,ijx,desc_a,info,iix,jjx)
    if (info == 0) &
         & call psb_chkvect(n,ik,size(y,1),iy,ijy,desc_a,info,iiy,jjy)
    if(info /= 0) then
      info=4010
      ch_err='psb_chkvect'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    if((iix /= 1).or.(iiy /= 1)) then
      ! this case is not yet implemented
      info = 3040
      call psb_errpush(info,name)
      goto 9999
    end if

    !
    ! Non-empty overlap, need a buffer to hold
    ! the entries updated with average operator.
    ! 
    call psi_ovrl_save(x(:,1:ik),xvsave,desc_a,info)
    if (info == 0) call psi_ovrl_upd(x,desc_a,psb_avg_,info)
    y(nrow+1:ncol,1:ik)    = szero

    if (info == 0) call psb_csmm(alpha,a,x(:,1:ik),beta,y(:,1:ik),info,trans=trans_)
    if (debug_level >= psb_debug_comp_) &
         & write(debug_unit,*) me,' ',trim(name),' csmm ', info
    if (info /= 0) then
      info = 4010
      ch_err='psb_csmm'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
    if (info == 0) call psi_ovrl_restore(x,xvsave,desc_a,info)

    if (doswap_)then 
      call psi_swaptran(ior(psb_swap_send_,psb_swap_recv_),&
           & ik,sone,y(:,1:ik),desc_a,iwork,info)
      if (info == 0) call psi_swapdata(ior(psb_swap_send_,psb_swap_recv_),&
           & ik,sone,y(:,1:ik),desc_a,iwork,info,data=psb_comm_ovr_)

      if (debug_level >= psb_debug_comp_) &
           & write(debug_unit,*) me,' ',trim(name),' swaptran ', info
      if(info /= 0) then
        info = 4010
        ch_err='PSI_dSwapTran'
        call psb_errpush(info,name,a_err=ch_err)
        goto 9999
      end if
    end if

  end if

  if (aliw) deallocate(iwork,stat=info)
  if (debug_level >= psb_debug_comp_) &
       & write(debug_unit,*) me,' ',trim(name),' deallocat ',aliw, info
  if(info /= 0) then
    info = 4010
    ch_err='Deallocate iwork'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if
  nullify(iwork)

  call psb_erractionrestore(err_act)
  return  

9999 continue
  call psb_erractionrestore(err_act)

  if (err_act == psb_act_abort_) then
    call psb_error(ictxt)
    return
  end if
  return
end subroutine psb_sspmm




!!$ 
!!$              Parallel Sparse BLAS  version 2.2
!!$    (C) Copyright 2006/2007/2008
!!$                       Salvatore Filippone    University of Rome Tor Vergata
!!$                       Alfredo Buttari        University of Rome Tor Vergata
!!$ 
!!$  Redistribution and use in source and binary forms, with or without
!!$  modification, are permitted provided that the following conditions
!!$  are met:
!!$    1. Redistributions of source code must retain the above copyright
!!$       notice, this list of conditions and the following disclaimer.
!!$    2. Redistributions in binary form must reproduce the above copyright
!!$       notice, this list of conditions, and the following disclaimer in the
!!$       documentation and/or other materials provided with the distribution.
!!$    3. The name of the PSBLAS group or the names of its contributors may
!!$       not be used to endorse or promote products derived from this
!!$       software without specific written permission.
!!$ 
!!$  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!!$  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
!!$  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!!$  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE PSBLAS GROUP OR ITS CONTRIBUTORS
!!$  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!!$  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!!$  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!!$  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!!$  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!!$  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!!$  POSSIBILITY OF SUCH DAMAGE.
!!$ 
!!$
!
! Subroutine: psb_sspmv
!     Performs one of the distributed matrix-vector operations
!
!     Y := alpha * Pr * A * Pc * X  + beta * Y,  or
!
!     Y := alpha * Pr * A' * Pr * X  + beta * Y,
!
!  alpha and beta are scalars, and X and Y are distributed
!  vectors and A is a M-by-N distributed matrix.
!
! Arguments:   
!    alpha   -  real                   The scalar alpha.
!    a       -  type(psb_zspmat_type). The sparse matrix containing A.
!    x(:)    -  real                   The input vector containing the entries of ( X ).
!    beta    -  real                   The scalar beta.
!    y(:)    -  real                   The input vector containing the entries of ( Y ).
!    desc_a  -  type(psb_desc_type).   The communication descriptor.
!    info    -  integer.               Return code
!    trans   -  character(optional).   Whether A or A'. Default: 'N' 
!    work(:) -  real,(optional).       Working area.
!    doswap  -  logical(optional).     Whether to performe halo updates.
! 
subroutine  psb_sspmv(alpha,a,x,beta,y,desc_a,info,&
     & trans, work, doswap)   

  use psb_spmat_type
  use psb_serial_mod
  use psb_descriptor_type
  use psb_comm_mod
  use psb_const_mod
  use psi_mod
  use psb_check_mod
  use psb_error_mod
  use psb_string_mod
  use psb_penv_mod
  implicit none

  real(psb_spk_), intent(in)             :: alpha, beta
  real(psb_spk_), intent(inout), target  :: x(:)
  real(psb_spk_), intent(inout), target  :: y(:)
  type(psb_sspmat_type), intent(in)        :: a
  type(psb_desc_type), intent(in)          :: desc_a
  integer, intent(out)                     :: info
  real(psb_spk_), optional, target       :: work(:)
  character, intent(in), optional          :: trans
  logical, intent(in), optional            :: doswap

  ! locals
  integer                  :: ictxt, np, me,&
       & err_act, n, iix, jjx, ia, ja, iia, jja, ix, iy, ik, &
       & m, nrow, ncol, lldx, lldy, liwork, jx, jy, iiy, jjy,&
       & ib, ip, idx
  integer, parameter       :: nb=4
  real(psb_spk_), pointer :: iwork(:), xp(:), yp(:)
  real(psb_spk_), allocatable :: xvsave(:)
  character                :: trans_
  character(len=20)        :: name, ch_err
  logical                  :: aliw, doswap_
  integer                  :: debug_level, debug_unit

  name='psb_sspmv'
  if(psb_get_errstatus() /= 0) return 
  info=0
  call psb_erractionsave(err_act)
  debug_unit  = psb_get_debug_unit()
  debug_level = psb_get_debug_level()

  ictxt=psb_cd_get_context(desc_a)
  call psb_info(ictxt, me, np)
  if (np == -1) then
    info = 2010
    call psb_errpush(info,name)
    goto 9999
  endif

  ia = 1
  ja = 1
  ix = 1
  jx = 1
  iy = 1
  jy = 1
  ik = 1
  ib = 1

  if (present(doswap)) then
    doswap_ = doswap
  else
    doswap_ = .true.
  endif

  if (present(trans)) then     
    trans_ = psb_toupper(trans)
  else
    trans_ = 'N'
  endif
  if ( (trans_ == 'N').or.(trans_ == 'T')&
       & .or.(trans_ == 'C')) then
  else
    info = 70
    call psb_errpush(info,name)
    goto 9999
  end if

  m    = psb_cd_get_global_rows(desc_a)
  n    = psb_cd_get_global_cols(desc_a)
  nrow = psb_cd_get_local_rows(desc_a)
  ncol = psb_cd_get_local_cols(desc_a)
  lldx = size(x)
  lldy = size(y)

  iwork => null()
  ! check for presence/size of a work area
  liwork= 2*ncol

  if (present(work)) then
    if (size(work) >= liwork) then
      aliw =.false.
    else
      aliw=.true.
    endif
  else
    aliw=.true.
  end if

  if (aliw) then
    allocate(iwork(liwork),stat=info)
    if(info /= 0) then
      info=4010
      ch_err='Allocate'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
  else
    iwork => work
  endif

  if (debug_level >= psb_debug_comp_) &
       & write(debug_unit,*) me,' ',trim(name),' Allocated work ', info
  ! checking for matrix correctness
  call psb_chkmat(m,n,ia,ja,desc_a,info,iia,jja)
  if(info /= 0) then
    info=4010
    ch_err='psb_chkmat'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if

  if (debug_level >= psb_debug_comp_) &
       & write(debug_unit,*) me,' ',trim(name),' Checkmat ', info
  if (trans_ == 'N') then
    !  Matrix is not transposed
    if((ja /= ix).or.(ia /= iy)) then
      ! this case is not yet implemented
      info = 3030
      call psb_errpush(info,name)
      goto 9999
    end if

    ! checking for vectors correctness
    call psb_chkvect(n,ik,size(x),ix,jx,desc_a,info,iix,jjx)
    if (info == 0) &
         & call psb_chkvect(m,ik,size(y),iy,jy,desc_a,info,iiy,jjy)
    if(info /= 0) then
      info=4010
      ch_err='psb_chkvect'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    if((iix /= 1).or.(iiy /= 1)) then
      ! this case is not yet implemented
      info = 3040
      call psb_errpush(info,name)
      goto 9999
    end if

    if (doswap_) then
      call psi_swapdata(ior(psb_swap_send_,psb_swap_recv_),&
           & szero,x,desc_a,iwork,info,data=psb_comm_halo_)
    end if

    !  local Matrix-vector product
    call psb_csmm(alpha,a,x(iix:lldx),beta,y(iiy:lldy),info)

    if(info /= 0) then
      info = 4011
      call psb_errpush(info,name)
      goto 9999
    end if

  else
    !  Matrix is transposed
    if((ja /= iy).or.(ia /= ix)) then
      ! this case is not yet implemented
      info = 3030
      call psb_errpush(info,name)
      goto 9999
    end if

    ! checking for vectors correctness
    call psb_chkvect(m,ik,size(x),ix,jx,desc_a,info,iix,jjx)
    if (info == 0)&
         & call psb_chkvect(n,ik,size(y),iy,jy,desc_a,info,iiy,jjy)
    if(info /= 0) then
      info=4010
      ch_err='psb_chkvect'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    if((iix /= 1).or.(iiy /= 1)) then
      ! this case is not yet implemented
      info = 3040
      call psb_errpush(info,name)
      goto 9999
    end if

    xp => x(1:lldx)
    yp => y(1:lldy)

    !
    ! Non-empty overlap, need a buffer to hold
    ! the entries updated with average operator.
    ! 
    call psi_ovrl_save(x,xvsave,desc_a,info)
    if (info == 0) call psi_ovrl_upd(x,desc_a,psb_avg_,info)
    yp(nrow+1:ncol) = szero
    
    !  local Matrix-vector product
    if (info == 0) call psb_csmm(alpha,a,x,beta,yp,info,trans=trans_)

    if (debug_level >= psb_debug_comp_) &
         & write(debug_unit,*) me,' ',trim(name),' csmm ', info

    if (info == 0) call psi_ovrl_restore(x,xvsave,desc_a,info)
    if (info /= 0) then
      info = 4010
      ch_err='psb_csmm'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
    
    if (doswap_) then
      call psi_swaptran(ior(psb_swap_send_,psb_swap_recv_),&
           & sone,yp,desc_a,iwork,info)
      if (info == 0) call psi_swapdata(ior(psb_swap_send_,psb_swap_recv_),&
           & sone,yp,desc_a,iwork,info,data=psb_comm_ovr_)
      
      if (debug_level >= psb_debug_comp_) &
           & write(debug_unit,*) me,' ',trim(name),' swaptran ', info
      if(info /= 0) then
        info = 4010
        ch_err='PSI_dSwapTran'
        call psb_errpush(info,name,a_err=ch_err)
        goto 9999
      end if
    end if

  end if

  if (aliw) deallocate(iwork,stat=info)
  if (debug_level >= psb_debug_comp_) &
       & write(debug_unit,*) me,' ',trim(name),' deallocat ',aliw, info
  if(info /= 0) then
    info = 4010
    ch_err='Deallocate iwork'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if

  nullify(iwork)

  call psb_erractionrestore(err_act)
  if (debug_level >= psb_debug_comp_) then 
    call psb_barrier(ictxt)
    write(debug_unit,*) me,' ',trim(name),' Returning '
  endif
  return  

9999 continue
  call psb_erractionrestore(err_act)

  if (err_act == psb_act_abort_) then
    call psb_error(ictxt)
    return
  end if
  return
end subroutine psb_sspmv
