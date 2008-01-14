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
! File:  psb_dcsmm.f90 
! Subroutine: 
! Arguments:
subroutine psb_dcsmm(alpha,a,b,beta,c,info,trans)
  use psb_spmat_type
  use psb_error_mod
  implicit none 

  type(psb_dspmat_type) :: a
  real(kind(1.d0))      :: alpha, beta, b(:,:), c(:,:)
  integer               :: info
  character, optional   :: trans
  
  real(kind(1.d0)), allocatable :: work(:)
  character                     :: trans_
  integer                       :: iwsz,m,n,k,lb,lc,err_act
  character(len=20)             :: name, ch_err

  name='psb_dcsmm'
  info  = 0
  call psb_erractionsave(err_act)
  
  if (present(trans)) then 
    trans_ = trans
  else
    trans_ = 'N'
  endif

  if (trans_=='N') then 
    m = a%m
    k = a%k
  else
    k = a%m
    m = a%k
  end if
  n = min(size(b,2),size(c,2))
  lb = size(b,1)
  lc = size(c,1)
  iwsz = 2*m*n
  allocate(work(iwsz),stat=info)
  if (info /= 0) then
    info = 4010
    ch_err='allocate'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if
      
  call dcsmm(trans_,m,n,k,alpha,&
       & a%pl,a%fida,a%descra,a%aspk,a%ia1,a%ia2,a%infoa,a%pr,&
       & b,lb,beta,c,lc,work,iwsz,info)
  

  if (info /= 0) then
    info = 4010
    ch_err='Serial csmm'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999

    goto 9999
  end if
  deallocate(work,stat=info)

  if (info /= 0) then
    info = 4010
    ch_err='Deallocate'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999

    goto 9999
  end if
  call psb_erractionrestore(err_act)
  return

9999 continue
  call psb_erractionrestore(err_act)

  if (err_act == psb_act_abort_) then
    call psb_error()
    return
  end if
  return
end subroutine psb_dcsmm
