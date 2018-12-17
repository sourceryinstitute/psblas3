!   
!                Parallel Sparse BLAS  version 3.5
!      (C) Copyright 2006, 2010, 2015, 2017
!        Salvatore Filippone    Cranfield University
!        Alfredo Buttari        CNRS-IRIT, Toulouse
!   
!    Redistribution and use in source and binary forms, with or without
!    modification, are permitted provided that the following conditions
!    are met:
!      1. Redistributions of source code must retain the above copyright
!         notice, this list of conditions and the following disclaimer.
!      2. Redistributions in binary form must reproduce the above copyright
!         notice, this list of conditions, and the following disclaimer in the
!         documentation and/or other materials provided with the distribution.
!      3. The name of the PSBLAS group or the names of its contributors may
!         not be used to endorse or promote products derived from this
!         software without specific written permission.
!   
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!    ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
!    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE PSBLAS GROUP OR ITS CONTRIBUTORS
!    BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!    POSSIBILITY OF SUCH DAMAGE.
!   
!    
module psb_string_mod
  use psb_const_mod, only : psb_ipk_
  public psb_tolower, psb_toupper, psb_touppers
  interface psb_tolower
    module procedure psb_tolowerc
  end interface

  interface psb_toupper
    module procedure psb_toupperc
  end interface

  interface psb_touppers
    module procedure psb_sub_toupperc
  end interface

  private 
  character(len=*), parameter   :: lcase='abcdefghijklmnopqrstuvwxyz'
  character(len=*), parameter   :: ucase='ABCDEFGHIJKLMNOPQRSTUVWXYZ'

contains 

  function idx_bsrch(key,v) result(ipos)

    implicit none
    integer(psb_ipk_) :: ipos
    character key
    character(len=*)  v

    integer(psb_ipk_) :: lb, ub, m


    lb = 1 
    ub = len(v)
    ipos = 0 

    do 
      if (lb > ub) exit
      m = (lb+ub)/2
      if (key.eq.v(m:m))  then
        ipos = m 
        exit
      else if (key.lt.v(m:m))  then
        ub = m-1
      else 
        lb = m + 1
      end if
    enddo
    return
  end function idx_bsrch


  function  psb_tolowerc(string)
    character(len=*), intent(in)  :: string
    character(len=len(string))    :: psb_tolowerc
    integer(psb_ipk_) :: i,k

    do i=1,len(string)
      k = idx_bsrch(string(i:i),ucase)
      if (k /= 0) then 
        psb_tolowerc(i:i) = lcase(k:k)
      else          
        psb_tolowerc(i:i) = string(i:i)
      end if
    enddo
    
  end function psb_tolowerc

  function  psb_toupperc(string)
    character(len=*), intent(in)  :: string
    character(len=len(string))    :: psb_toupperc
    integer(psb_ipk_) :: i,k

    do i=1,len(string)
      k = idx_bsrch(string(i:i),lcase)
      if (k /= 0) then 
        psb_toupperc(i:i) = ucase(k:k)
      else          
        psb_toupperc(i:i) = string(i:i)
      end if
    enddo
  end function psb_toupperc

  subroutine   psb_sub_toupperc(string,strout)
    character(len=*), intent(in)  :: string
    character(len=*), intent(out)  :: strout
    integer(psb_ipk_) :: i,k

    do i=1,len(string)
      k = idx_bsrch(string(i:i),lcase)
      if (k /= 0) then 
        strout(i:i) = ucase(k:k)
      else          
        strout(i:i) = string(i:i)
      end if
    enddo

  end subroutine psb_sub_toupperc



end module psb_string_mod
