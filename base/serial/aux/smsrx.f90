!!$ 
!!$              Parallel Sparse BLAS  version 3.4
!!$    (C) Copyright 2006, 2010, 2015
!!$                       Salvatore Filippone    University of Rome Tor Vergata
!!$                       Alfredo Buttari        CNRS-IRIT, Toulouse
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
! File:  imsrx.f90 
 ! Subroutine: 
 ! Parameters:
subroutine smsrx(n,x,indx,idir,flag)
  use psb_serial_mod
  use psb_ip_reord_mod
  implicit none
  integer(psb_ipk_) :: n,idir,flag
  real(psb_spk_) :: x(n)
  integer(psb_ipk_) :: indx(n)

  integer(psb_ipk_), allocatable :: iaux(:)

  integer(psb_ipk_) :: iret, info, lp, k,lswap, ixswap
  real(psb_spk_) :: swap

  if (n<0) then 
    return
  endif

  if (n == 0) return

  if (flag == psb_sort_ovw_idx_) then 
    do k=1,n
      indx(k) = k
    enddo
  end if

  if (n == 1) return

  allocate(iaux(0:n+1),stat=info)
  if (info /= psb_success_) then 
    call psb_errpush(psb_err_alloc_dealloc_,r_name='smsrx')
    call psb_error()
  endif
  
  select case(idir)
  case (psb_sort_up_)
    call smsort_up(n,x,iaux,iret)
  case (psb_asort_up_)
    call samsort_up(n,x,iaux,iret)
  case (psb_asort_down_)
    call samsort_dw(n,x,iaux,iret)
  case (psb_sort_down_)
    call smsort_dw(n,x,iaux,iret)
  end select

  if (iret == 0) call psb_ip_reord(n,x,indx,iaux)

  deallocate(iaux,stat=info)
  if (info /= psb_success_) then 
    call psb_errpush(psb_err_alloc_dealloc_,r_name='smsrx')
    call psb_error()
  endif
  return
end subroutine smsrx
