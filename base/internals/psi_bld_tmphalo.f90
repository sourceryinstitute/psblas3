!!$ 
!!$              Parallel Sparse BLAS  v2.0
!!$    (C) Copyright 2006 Salvatore Filippone    University of Rome Tor Vergata
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
! File: psi_bld_tmphalo.f90
!
! Subroutine: psi_bld_tmphalo
!   Build initial versions of data exchange lists.
!   When the descriptor is for a large index space, we cannot build 
!   the data exchange lists "on-the-fly", but we also want to keep using the 
!   same format conversion routines we use in the small index space case, 
!   hence this adapter routine.
!   
! 
! Arguments: 
!    desc     - type(psb_desc_type).    The communication descriptor.        
!    info     - integer.                  return code.
!
subroutine psi_bld_tmphalo(desc,info)
  use psb_descriptor_type
  use psb_serial_mod
  use psb_const_mod
  use psb_error_mod
  use psb_penv_mod
  use psb_realloc_mod
  use psi_mod, psb_protect_name => psi_bld_tmphalo
  implicit none
  type(psb_desc_type), intent(inout) :: desc
  integer, intent(out) :: info

  integer,allocatable :: helem(:),hproc(:)
  integer,allocatable :: tmphl(:)

  integer          ::  i,j,np,me,lhalo,nhalo,&
       & n_col, err_act,  key, ih, nh, idx, nk,icomm
  integer             :: ictxt,n_row
  character(len=20)   :: name,ch_err

  info = 0
  name = 'psi_bld_tmphalo'
  call psb_erractionsave(err_act)

  ictxt = psb_cd_get_context(desc)
  icomm = psb_cd_get_mpic(desc)
  n_row = psb_cd_get_local_rows(desc)
  n_col = psb_cd_get_local_cols(desc)

  ! check on blacs grid 
  call psb_info(ictxt, me, np)
  if (np == -1) then
    info = 2010
    call psb_errpush(info,name)
    goto 9999
  endif


  if (.not.(psb_is_bld_desc(desc).and.psb_is_large_desc(desc))) then 
    info = 1122
    call psb_errpush(info,name)
    goto 9999
  end if

  ! Here we do not know yet who owns what, so we have 
  ! to call fnd_owner.
  nh = (n_col-n_row)
  if (nh > 0) then 
    Allocate(helem(nh),stat=info)
    if (info /= 0) then 
      call psb_errpush(4010,name,a_err='Allocate')
      goto 9999      
    end if

    do i=1, nh
      helem(i) = desc%loc_to_glob(n_row+i)
    end do

    call psi_fnd_owner(nh,helem,hproc,desc,info)
    allocate(tmphl((3*((n_col-n_row)+1)+1)),stat=info)
    if (info /= 0) then 
      call psb_errpush(4010,name,a_err='Allocate')
      goto 9999      
    end if
    j  = 1
    do i=1,nh
      tmphl(j+0) = hproc(i)
      if (tmphl(j+0)<0) then 
        write(0,*) 'Unrecoverable error: missing proc from asb'
      end if
      tmphl(j+1) = 1
      tmphl(j+2) = n_row+i
      j          = j + 3
    end do
    tmphl(j) = -1
    lhalo = j
    nhalo = (lhalo-1)/3
  else 
    allocate(tmphl(1),stat=info) 
    if (info /= 0) then 
      call psb_errpush(4010,name,a_err='Allocate')
      goto 9999      
    end if
    tmphl=-1
  endif

  call psb_transfer(tmphl,desc%halo_index,info)

  call psb_erractionrestore(err_act)
  return

9999 continue
  call psb_erractionrestore(err_act)

  if (err_act.eq.psb_act_ret_) then
    return
  else
    call psb_error(ictxt)
  end if
  return


end subroutine psi_bld_tmphalo