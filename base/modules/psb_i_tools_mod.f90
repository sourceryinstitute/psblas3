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
module psb_i_tools_mod
  use psb_desc_mod, only : psb_desc_type, psb_ipk_, psb_success_
  use psb_i_vect_mod, only : psb_i_base_vect_type, psb_i_vect_type
  

  interface  psb_geall
    subroutine psb_ialloc(x, desc_a, info,n, lb)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_), allocatable, intent(out) :: x(:,:)
      type(psb_desc_type), intent(in)   :: desc_a
      integer(psb_ipk_), intent(out)              :: info
      integer(psb_ipk_), optional, intent(in)     :: n, lb
    end subroutine psb_ialloc
    subroutine psb_iallocv(x, desc_a,info,n)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_), allocatable, intent(out) :: x(:)
      type(psb_desc_type), intent(in)   :: desc_a
      integer(psb_ipk_), intent(out)              :: info
      integer(psb_ipk_), optional, intent(in)     :: n
    end subroutine psb_iallocv
    subroutine psb_ialloc_vect(x, desc_a,info,n)
      import :: psb_desc_type,  psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_i_vect_type), intent(out)  :: x
      type(psb_desc_type), intent(in) :: desc_a
      integer(psb_ipk_),intent(out)             :: info
      integer(psb_ipk_), optional, intent(in)   :: n
    end subroutine psb_ialloc_vect
    subroutine psb_ialloc_vect_r2(x, desc_a,info,n,lb)
      import :: psb_desc_type, psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_i_vect_type), allocatable, intent(out)  :: x(:)
      type(psb_desc_type), intent(in) :: desc_a
      integer(psb_ipk_),intent(out)             :: info
      integer(psb_ipk_), optional, intent(in)   :: n, lb
    end subroutine psb_ialloc_vect_r2
  end interface


  interface psb_geasb
    subroutine psb_iasb(x, desc_a, info)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in) ::  desc_a
      integer(psb_ipk_), allocatable, intent(inout)  ::  x(:,:)
      integer(psb_ipk_), intent(out)            ::  info
    end subroutine psb_iasb
    subroutine psb_iasbv(x, desc_a, info)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in) ::  desc_a
      integer(psb_ipk_), allocatable, intent(inout)   ::  x(:)
      integer(psb_ipk_), intent(out)        ::  info
    end subroutine psb_iasbv
    subroutine psb_iasb_vect(x, desc_a, info,mold, scratch)
      import :: psb_desc_type,  psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_desc_type), intent(in)      ::  desc_a
      type(psb_i_vect_type), intent(inout) :: x
      integer(psb_ipk_), intent(out)                 ::  info
      class(psb_i_base_vect_type), intent(in), optional :: mold
      logical, intent(in), optional        :: scratch
    end subroutine psb_iasb_vect
    subroutine psb_iasb_vect_r2(x, desc_a, info,mold, scratch)
      import :: psb_desc_type, psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_desc_type), intent(in)      ::  desc_a
      type(psb_i_vect_type), intent(inout) :: x(:)
      integer(psb_ipk_), intent(out)                 ::  info
      class(psb_i_base_vect_type), intent(in), optional :: mold
      logical, intent(in), optional        :: scratch
    end subroutine psb_iasb_vect_r2
  end interface


  interface psb_gefree
    subroutine psb_ifree(x, desc_a, info)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_),allocatable, intent(inout) :: x(:,:)
      type(psb_desc_type), intent(in)    :: desc_a
      integer(psb_ipk_), intent(out)               :: info
    end subroutine psb_ifree
    subroutine psb_ifreev(x, desc_a, info)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_), allocatable, intent(inout) :: x(:)
      type(psb_desc_type), intent(in)     :: desc_a
      integer(psb_ipk_), intent(out)                :: info
    end subroutine psb_ifreev
    subroutine psb_ifree_vect(x, desc_a, info)
      import :: psb_desc_type,  psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_desc_type), intent(in)  ::  desc_a
      type(psb_i_vect_type), intent(inout) :: x
      integer(psb_ipk_), intent(out)             ::  info
    end subroutine psb_ifree_vect
    subroutine psb_ifree_vect_r2(x, desc_a, info)
      import :: psb_desc_type, psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      type(psb_desc_type), intent(in)  ::  desc_a
      type(psb_i_vect_type), allocatable, intent(inout) :: x(:)
      integer(psb_ipk_), intent(out)             ::  info
    end subroutine psb_ifree_vect_r2
  end interface

  interface psb_geins
    subroutine psb_iinsi(m,irw,val, x,desc_a,info,dupl,local)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_), intent(in)              ::  m
      type(psb_desc_type), intent(in)  ::  desc_a
      integer(psb_ipk_),intent(inout)                  ::  x(:,:)
      integer(psb_ipk_), intent(in)              ::  irw(:)
      integer(psb_ipk_), intent(in)              ::  val(:,:)
      integer(psb_ipk_), intent(out)             ::  info
      integer(psb_ipk_), optional, intent(in)    ::  dupl
      logical, intent(in), optional        :: local
    end subroutine psb_iinsi
    subroutine psb_iinsvi(m, irw,val, x,desc_a,info,dupl,local)
      import :: psb_ipk_, psb_desc_type
      integer(psb_ipk_), intent(in)             ::  m
      type(psb_desc_type), intent(in) ::  desc_a
      integer(psb_ipk_),intent(inout)                 ::  x(:)
      integer(psb_ipk_), intent(in)             ::  irw(:)
      integer(psb_ipk_), intent(in)             ::  val(:)
      integer(psb_ipk_), intent(out)            ::  info
      integer(psb_ipk_), optional, intent(in)   ::  dupl
      logical, intent(in), optional        :: local
    end subroutine psb_iinsvi
    subroutine psb_iins_vect(m,irw,val,x,desc_a,info,dupl,local)
      import :: psb_desc_type,  psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      integer(psb_ipk_), intent(in)              :: m
      type(psb_desc_type), intent(in)  :: desc_a
      type(psb_i_vect_type), intent(inout) :: x
      integer(psb_ipk_), intent(in)              :: irw(:)
      integer(psb_ipk_), intent(in)    :: val(:)
      integer(psb_ipk_), intent(out)             :: info
      integer(psb_ipk_), optional, intent(in)    :: dupl
      logical, intent(in), optional        :: local
    end subroutine psb_iins_vect
    subroutine psb_iins_vect_v(m,irw,val,x,desc_a,info,dupl,local)
      import :: psb_desc_type, psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      integer(psb_ipk_), intent(in)              :: m
      type(psb_desc_type), intent(in)  :: desc_a
      type(psb_i_vect_type), intent(inout) :: x
      type(psb_i_vect_type), intent(inout)       :: irw
      type(psb_i_vect_type), intent(inout)    :: val
      integer(psb_ipk_), intent(out)             :: info
      integer(psb_ipk_), optional, intent(in)    :: dupl
      logical, intent(in), optional        :: local
    end subroutine psb_iins_vect_v
    subroutine psb_iins_vect_r2(m,irw,val,x,desc_a,info,dupl,local)
      import :: psb_desc_type, psb_ipk_, &
           & psb_i_base_vect_type, psb_i_vect_type
      integer(psb_ipk_), intent(in)              :: m
      type(psb_desc_type), intent(in)  :: desc_a
      type(psb_i_vect_type), intent(inout) :: x(:)
      integer(psb_ipk_), intent(in)              :: irw(:)
      integer(psb_ipk_), intent(in)    :: val(:,:)
      integer(psb_ipk_), intent(out)             :: info
      integer(psb_ipk_), optional, intent(in)    :: dupl
      logical, intent(in), optional        :: local
    end subroutine psb_iins_vect_r2
  end interface


  interface psb_glob_to_loc
    subroutine psb_glob_to_loc2v(x,y,desc_a,info,iact,owned)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(in)                 ::  x(:)  
      integer(psb_ipk_),intent(out)                ::  y(:)  
      integer(psb_ipk_), intent(out)               ::  info
      logical, intent(in),  optional     :: owned
      character, intent(in), optional    ::  iact
    end subroutine psb_glob_to_loc2v
    subroutine psb_glob_to_loc1v(x,desc_a,info,iact,owned)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(inout)              ::  x(:)  
      integer(psb_ipk_), intent(out)               ::  info
      logical, intent(in),  optional     :: owned
      character, intent(in), optional    ::  iact
    end subroutine psb_glob_to_loc1v
    subroutine psb_glob_to_loc2s(x,y,desc_a,info,iact,owned)
      import :: psb_ipk_, psb_desc_type
      implicit none 
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(in)                 ::  x
      integer(psb_ipk_),intent(out)                ::  y  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
      logical, intent(in),  optional     :: owned
    end subroutine psb_glob_to_loc2s
    subroutine psb_glob_to_loc1s(x,desc_a,info,iact,owned)
      import :: psb_ipk_, psb_desc_type
      implicit none 
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(inout)              ::  x  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
      logical, intent(in),  optional     :: owned
    end subroutine psb_glob_to_loc1s
  end interface

  interface psb_loc_to_glob
    subroutine psb_loc_to_glob2v(x,y,desc_a,info,iact)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(in)                 ::  x(:)  
      integer(psb_ipk_),intent(out)                ::  y(:)  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
    end subroutine psb_loc_to_glob2v
    subroutine psb_loc_to_glob1v(x,desc_a,info,iact)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(inout)              ::  x(:)  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
    end subroutine psb_loc_to_glob1v
    subroutine psb_loc_to_glob2s(x,y,desc_a,info,iact)
      import :: psb_ipk_, psb_desc_type
      implicit none 
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(in)                 ::  x
      integer(psb_ipk_),intent(out)                ::  y  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
    end subroutine psb_loc_to_glob2s
    subroutine psb_loc_to_glob1s(x,desc_a,info,iact)
      import :: psb_ipk_, psb_desc_type
      type(psb_desc_type), intent(in)    ::  desc_a
      integer(psb_ipk_),intent(inout)              ::  x  
      integer(psb_ipk_), intent(out)               ::  info
      character, intent(in), optional    ::  iact
    end subroutine psb_loc_to_glob1s

  end interface


  interface psb_is_owned
    module procedure psb_is_owned
  end interface

  interface psb_is_local
    module procedure psb_is_local
  end interface

  interface psb_owned_index
    module procedure psb_owned_index, psb_owned_index_v
  end interface

  interface psb_local_index
    module procedure psb_local_index, psb_local_index_v
  end interface

contains

  function psb_is_owned(idx,desc)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx
    type(psb_desc_type), intent(in) :: desc
    logical :: psb_is_owned
    logical :: res
    integer(psb_ipk_) :: info

    call psb_owned_index(res,idx,desc,info)
    if (info /= psb_success_) res=.false.
    psb_is_owned = res
  end function psb_is_owned

  function psb_is_local(idx,desc)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx
    type(psb_desc_type), intent(in) :: desc
    logical :: psb_is_local
    logical :: res
    integer(psb_ipk_) :: info

    call psb_local_index(res,idx,desc,info)
    if (info /= psb_success_) res=.false.
    psb_is_local = res
  end function psb_is_local

  subroutine psb_owned_index(res,idx,desc,info)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx
    type(psb_desc_type), intent(in) :: desc
    logical, intent(out) :: res
    integer(psb_ipk_), intent(out) :: info

    integer(psb_ipk_) :: lx

    call psb_glob_to_loc(idx,lx,desc,info,iact='I',owned=.true.)

    res = (lx>0)
  end subroutine psb_owned_index

  subroutine psb_owned_index_v(res,idx,desc,info)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx(:)
    type(psb_desc_type), intent(in) :: desc
    logical, intent(out) :: res(:)
    integer(psb_ipk_), intent(out) :: info
    integer(psb_ipk_), allocatable  :: lx(:)

    allocate(lx(size(idx)),stat=info)
    res=.false.
    if (info /= psb_success_) return
    call psb_glob_to_loc(idx,lx,desc,info,iact='I',owned=.true.)

    res = (lx>0)
  end subroutine psb_owned_index_v

  subroutine psb_local_index(res,idx,desc,info)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx
    type(psb_desc_type), intent(in) :: desc
    logical, intent(out) :: res
    integer(psb_ipk_), intent(out) :: info

    integer(psb_ipk_) :: lx

    call psb_glob_to_loc(idx,lx,desc,info,iact='I',owned=.false.)

    res = (lx>0)
  end subroutine psb_local_index

  subroutine psb_local_index_v(res,idx,desc,info)
    implicit none 
    integer(psb_ipk_), intent(in) :: idx(:)
    type(psb_desc_type), intent(in) :: desc
    logical, intent(out) :: res(:)
    integer(psb_ipk_), intent(out) :: info    
    integer(psb_ipk_), allocatable  :: lx(:)

    allocate(lx(size(idx)),stat=info)
    res=.false.
    if (info /= psb_success_) return
    call psb_glob_to_loc(idx,lx,desc,info,iact='I',owned=.false.)

    res = (lx>0)
  end subroutine psb_local_index_v

end module psb_i_tools_mod

