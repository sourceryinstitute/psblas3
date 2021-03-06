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
! File: psb_d_pde2d.f90
!
! Program: psb_d_pde2d
! This sample program solves a linear system obtained by discretizing a
! PDE with Dirichlet BCs. 
! 
!
! The PDE is a general second order equation in 2d
!
!   a1 dd(u)  a2 dd(u)   b1 d(u)   b2 d(u) 
! -   ------ -  ------   -----  +  ------  + c u = f
!      dxdx     dydy        dx       dy    
!
! with Dirichlet boundary conditions
!   u = g 
!
!  on the unit square  0<=x,y<=1.
!
!
! Note that if b1=b2=c=0., the PDE is the  Laplace equation.
!
! In this sample program the index space of the discretized
! computational domain is first numbered sequentially in a standard way, 
! then the corresponding vector is distributed according to a BLOCK
! data distribution.
!
module psb_d_pde2d_mod

  use psb_base_mod, only : psb_dpk_, psb_ipk_, psb_desc_type,&
       &  psb_dspmat_type, psb_d_vect_type, dzero,&
       &  psb_d_base_sparse_mat, psb_d_base_vect_type, psb_i_base_vect_type

  interface 
    function d_func_2d(x,y) result(val)
      import :: psb_dpk_
      real(psb_dpk_), intent(in) :: x,y
      real(psb_dpk_) :: val
    end function d_func_2d
  end interface 

  interface psb_gen_pde2d
    module procedure  psb_d_gen_pde2d
  end interface psb_gen_pde2d
  
contains
  
  function d_null_func_2d(x,y) result(val)

    real(psb_dpk_), intent(in) :: x,y
    real(psb_dpk_) :: val
    
    val = dzero

  end function d_null_func_2d



  !
  !  subroutine to allocate and fill in the coefficient matrix and
  !  the rhs. 
  !
  subroutine psb_d_gen_pde2d(ictxt,idim,a,bv,xv,desc_a,afmt,&
       & a1,a2,b1,b2,c,g,info,f,amold,vmold,imold,nrl,iv)
    use psb_base_mod
    !
    !   Discretizes the partial differential equation
    ! 
    !   a1 dd(u)  a2 dd(u)    b1 d(u)  b2 d(u) 
    ! -   ------ -  ------ +  -----  +  ------  + c u = f
    !      dxdx     dydy         dx       dy     
    !
    ! with Dirichlet boundary conditions
    !   u = g 
    !
    !  on the unit square  0<=x,y<=1.
    !
    !
    ! Note that if b1=b2=c=0., the PDE is the  Laplace equation.
    !
    implicit none
    procedure(d_func_2d)  :: b1,b2,c,a1,a2,g
    integer(psb_ipk_)     :: idim
    type(psb_dspmat_type) :: a
    type(psb_d_vect_type) :: xv,bv
    type(psb_desc_type)   :: desc_a
    integer(psb_ipk_)     :: ictxt, info
    character(len=*)      :: afmt
    procedure(d_func_2d), optional :: f
    class(psb_d_base_sparse_mat), optional :: amold
    class(psb_d_base_vect_type), optional :: vmold
    class(psb_i_base_vect_type), optional :: imold
    integer(psb_ipk_), optional :: nrl, iv(:)

    ! Local variables.

    integer(psb_ipk_), parameter :: nb=20
    type(psb_d_csc_sparse_mat)  :: acsc
    type(psb_d_coo_sparse_mat)  :: acoo
    type(psb_d_csr_sparse_mat)  :: acsr
    real(psb_dpk_)           :: zt(nb),x,y,z
    integer(psb_ipk_) :: m,n,nnz,glob_row,nlr,i,ii,ib,k
    integer(psb_ipk_) :: ix,iy,iz,ia,indx_owner
    integer(psb_ipk_) :: np, iam, nr, nt
    integer(psb_ipk_) :: icoeff
    integer(psb_ipk_), allocatable     :: irow(:),icol(:),myidx(:)
    real(psb_dpk_), allocatable :: val(:)
    ! deltah dimension of each grid cell
    ! deltat discretization time
    real(psb_dpk_)            :: deltah, sqdeltah, deltah2
    real(psb_dpk_), parameter :: rhs=0.d0,one=1.d0,zero=0.d0
    real(psb_dpk_)    :: t0, t1, t2, t3, tasb, talc, ttot, tgen, tcdasb
    integer(psb_ipk_) :: err_act
    procedure(d_func_2d), pointer :: f_
    character(len=20)  :: name, ch_err,tmpfmt

    info = psb_success_
    name = 'create_matrix'
    call psb_erractionsave(err_act)

    call psb_info(ictxt, iam, np)


    if (present(f)) then 
      f_ => f
    else
      f_ => d_null_func_2d
    end if

    deltah   = 1.d0/(idim+2)
    sqdeltah = deltah*deltah
    deltah2  = 2.d0* deltah

    ! initialize array descriptor and sparse matrix storage. provide an
    ! estimate of the number of non zeroes 

    m   = idim*idim
    n   = m
    nnz = ((n*7)/(np))
    if(iam == psb_root_) write(psb_out_unit,'("Generating Matrix (size=",i0,")...")')n

    if (.not.present(iv)) then 
      if (present(nrl)) then 
        nr = nrl
      else
        !
        ! Using a simple BLOCK distribution.
        !
        nt = (m+np-1)/np
        nr = max(0,min(nt,m-(iam*nt)))
      end if

      nt = nr
      call psb_sum(ictxt,nt) 
      if (nt /= m) then 
        write(psb_err_unit,*) iam, 'Initialization error ',nr,nt,m
        info = -1
        call psb_barrier(ictxt)
        call psb_abort(ictxt)
        return    
      end if
    else
      if (size(iv) /= m) then
        write(psb_err_unit,*) iam, 'Initialization error IV',size(iv),m
        info = -1
        call psb_barrier(ictxt)
        call psb_abort(ictxt)
        return    
      end if

    end if
    call psb_barrier(ictxt)
    t0 = psb_wtime()
    if (present(iv)) then 
      call psb_cdall(ictxt,desc_a,info,vg=iv)
    else
      call psb_cdall(ictxt,desc_a,info,nl=nr)
    end if
    if (info == psb_success_) call psb_spall(a,desc_a,info,nnz=nnz)
    ! define  rhs from boundary conditions; also build initial guess 
    if (info == psb_success_) call psb_geall(xv,desc_a,info)
    if (info == psb_success_) call psb_geall(bv,desc_a,info)
    call psb_barrier(ictxt)
    talc = psb_wtime()-t0

    if (info /= psb_success_) then
      info=psb_err_from_subroutine_
      ch_err='allocation rout.'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    ! we build an auxiliary matrix consisting of one row at a
    ! time; just a small matrix. might be extended to generate 
    ! a bunch of rows per call. 
    ! 
    allocate(val(20*nb),irow(20*nb),&
         &icol(20*nb),stat=info)
    if (info /= psb_success_ ) then 
      info=psb_err_alloc_dealloc_
      call psb_errpush(info,name)
      goto 9999
    endif


    myidx = desc_a%get_global_indices()
    nlr = size(myidx)

    ! loop over rows belonging to current process in a block
    ! distribution.

    call psb_barrier(ictxt)
    t1 = psb_wtime()
    do ii=1, nlr,nb
      ib = min(nb,nlr-ii+1) 
      icoeff = 1
      do k=1,ib
        i=ii+k-1
        ! local matrix pointer 
        glob_row=myidx(i)
        ! compute gridpoint coordinates
        if (mod(glob_row,(idim)) == 0) then
          ix = glob_row/(idim)
        else
          ix = glob_row/(idim)+1
        endif
        iy = (glob_row-(ix-1)*idim)
        ! x, y
        x = (ix-1)*deltah
        y = (iy-1)*deltah

        zt(k) = f_(x,y)
        ! internal point: build discretization
        !   
        !  term depending on   (x-1,y)
        !
        val(icoeff) = -a1(x,y)/sqdeltah-b1(x,y)/deltah2
        if (ix == 1) then 
          zt(k) = g(dzero,y)*(-val(icoeff)) + zt(k)
        else
          icol(icoeff) = (ix-2)*idim+iy
          irow(icoeff) = glob_row
          icoeff       = icoeff+1
        endif
        !  term depending on     (x,y-1)
        val(icoeff)  = -a2(x,y)/sqdeltah-b2(x,y)/deltah2
        if (iy == 1) then 
          zt(k) = g(x,dzero)*(-val(icoeff))   + zt(k)
        else
          icol(icoeff) = (ix-1)*idim+(iy-1)
          irow(icoeff) = glob_row
          icoeff       = icoeff+1
        endif

        !  term depending on     (x,y)
        val(icoeff)=2.d0*(a1(x,y) + a2(x,y))/sqdeltah + c(x,y)
        icol(icoeff) = (ix-1)*idim+iy
        irow(icoeff) = glob_row
        icoeff       = icoeff+1                  
        !  term depending on     (x,y+1)
        val(icoeff)=-a2(x,y)/sqdeltah+b2(x,y)/deltah2
        if (iy == idim) then 
          zt(k) = g(x,done)*(-val(icoeff))   + zt(k)
        else
          icol(icoeff) = (ix-1)*idim+(iy+1)
          irow(icoeff) = glob_row
          icoeff       = icoeff+1
        endif
        !  term depending on     (x+1,y)
        val(icoeff)=-a1(x,y)/sqdeltah+b1(x,y)/deltah2
        if (ix==idim) then 
          zt(k) = g(done,y)*(-val(icoeff))   + zt(k)
        else
          icol(icoeff) = (ix)*idim+(iy)
          irow(icoeff) = glob_row
          icoeff       = icoeff+1
        endif

      end do
      call psb_spins(icoeff-1,irow,icol,val,a,desc_a,info)
      if(info /= psb_success_) exit
      call psb_geins(ib,myidx(ii:ii+ib-1),zt(1:ib),bv,desc_a,info)
      if(info /= psb_success_) exit
      zt(:)=0.d0
      call psb_geins(ib,myidx(ii:ii+ib-1),zt(1:ib),xv,desc_a,info)
      if(info /= psb_success_) exit
    end do

    tgen = psb_wtime()-t1
    if(info /= psb_success_) then
      info=psb_err_from_subroutine_
      ch_err='insert rout.'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if

    deallocate(val,irow,icol)

    call psb_barrier(ictxt)
    t1 = psb_wtime()
    call psb_cdasb(desc_a,info,mold=imold)
    tcdasb = psb_wtime()-t1
    call psb_barrier(ictxt)
    t1 = psb_wtime()
    if (info == psb_success_) then 
      if (present(amold)) then 
        call psb_spasb(a,desc_a,info,dupl=psb_dupl_err_,mold=amold)
      else
        call psb_spasb(a,desc_a,info,dupl=psb_dupl_err_,afmt=afmt)
      end if
    end if
    call psb_barrier(ictxt)
    if(info /= psb_success_) then
      info=psb_err_from_subroutine_
      ch_err='asb rout.'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
    if (info == psb_success_) call psb_geasb(xv,desc_a,info,mold=vmold)
    if (info == psb_success_) call psb_geasb(bv,desc_a,info,mold=vmold)
    if(info /= psb_success_) then
      info=psb_err_from_subroutine_
      ch_err='asb rout.'
      call psb_errpush(info,name,a_err=ch_err)
      goto 9999
    end if
    tasb = psb_wtime()-t1
    call psb_barrier(ictxt)
    ttot = psb_wtime() - t0 

    call psb_amx(ictxt,talc)
    call psb_amx(ictxt,tgen)
    call psb_amx(ictxt,tasb)
    call psb_amx(ictxt,ttot)
    if(iam == psb_root_) then
      tmpfmt = a%get_fmt()
      write(psb_out_unit,'("The matrix has been generated and assembled in ",a3," format.")')&
           &   tmpfmt
      write(psb_out_unit,'("-allocation  time : ",es12.5)') talc
      write(psb_out_unit,'("-coeff. gen. time : ",es12.5)') tgen
      write(psb_out_unit,'("-desc asbly  time : ",es12.5)') tcdasb
      write(psb_out_unit,'("- mat asbly  time : ",es12.5)') tasb
      write(psb_out_unit,'("-total       time : ",es12.5)') ttot

    end if
    call psb_erractionrestore(err_act)
    return

9999 call psb_error_handler(ictxt,err_act)

    return
  end subroutine psb_d_gen_pde2d

end module psb_d_pde2d_mod

program psb_d_pde2d
  use psb_base_mod
  use psb_prec_mod
  use psb_krylov_mod
  use psb_util_mod
  use psb_d_pde2d_mod
  implicit none

  ! input parameters
  character(len=20) :: kmethd, ptype
  character(len=5)  :: afmt
  integer(psb_ipk_) :: idim

  ! miscellaneous 
  real(psb_dpk_), parameter :: one = 1.d0
  real(psb_dpk_) :: t1, t2, tprec 

  ! sparse matrix and preconditioner
  type(psb_dspmat_type) :: a
  type(psb_dprec_type)  :: prec
  ! descriptor
  type(psb_desc_type)   :: desc_a
  ! dense vectors
  type(psb_d_vect_type) :: xxv,bv
  ! parallel environment
  integer(psb_ipk_) :: ictxt, iam, np

  ! solver parameters
  integer(psb_ipk_) :: iter, itmax,itrace, istopc, irst
  integer(psb_long_int_k_) :: amatsize, precsize, descsize, d2size
  real(psb_dpk_)   :: err, eps

  ! other variables
  integer(psb_ipk_) :: info, i
  character(len=20) :: name,ch_err
  character(len=40) :: fname

  info=psb_success_


  call psb_init(ictxt)
  call psb_info(ictxt,iam,np)

  if (iam < 0) then 
    ! This should not happen, but just in case
    call psb_exit(ictxt)
    stop
  endif
  if(psb_get_errstatus() /= 0) goto 9999
  name='pde2d90'
  call psb_set_errverbosity(itwo)
  !
  ! Hello world
  !
  if (iam == psb_root_) then 
    write(*,*) 'Welcome to PSBLAS version: ',psb_version_string_
    write(*,*) 'This is the ',trim(name),' sample program'
  end if
  !
  !  get parameters
  !
  call get_parms(ictxt,kmethd,ptype,afmt,idim,istopc,itmax,itrace,irst)

  !
  !  allocate and fill in the coefficient matrix, rhs and initial guess 
  !
  call psb_barrier(ictxt)
  t1 = psb_wtime()
  call psb_gen_pde2d(ictxt,idim,a,bv,xxv,desc_a,afmt,a1,a2,b1,b2,c,g,info)  
  call psb_barrier(ictxt)
  t2 = psb_wtime() - t1
  if(info /= psb_success_) then
    info=psb_err_from_subroutine_
    ch_err='psb_gen_pde2d'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if
  if (iam == psb_root_) write(psb_out_unit,'("Overall matrix creation time : ",es12.5)')t2
  if (iam == psb_root_) write(psb_out_unit,'(" ")')
  !
  !  prepare the preconditioner.
  !  
  if(iam == psb_root_) write(psb_out_unit,'("Setting preconditioner to : ",a)')ptype
  call prec%init(ptype,info)

  call psb_barrier(ictxt)
  t1 = psb_wtime()
  call prec%build(a,desc_a,info)
  if(info /= psb_success_) then
    info=psb_err_from_subroutine_
    ch_err='psb_precbld'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if

  tprec = psb_wtime()-t1

  call psb_amx(ictxt,tprec)

  if (iam == psb_root_) write(psb_out_unit,'("Preconditioner time : ",es12.5)')tprec
  if (iam == psb_root_) write(psb_out_unit,'(" ")')
  call prec%descr()
  !
  ! iterative method parameters 
  !
  if(iam == psb_root_) write(psb_out_unit,'("Calling iterative method ",a)')kmethd
  call psb_barrier(ictxt)
  t1 = psb_wtime()  
  eps   = 1.d-9
  call psb_krylov(kmethd,a,prec,bv,xxv,eps,desc_a,info,& 
       & itmax=itmax,iter=iter,err=err,itrace=itrace,istop=istopc,irst=irst)     

  if(info /= psb_success_) then
    info=psb_err_from_subroutine_
    ch_err='solver routine'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if

  call psb_barrier(ictxt)
  t2 = psb_wtime() - t1
  call psb_amx(ictxt,t2)
  amatsize = a%sizeof()
  descsize = desc_a%sizeof()
  precsize = prec%sizeof()
  call psb_sum(ictxt,amatsize)
  call psb_sum(ictxt,descsize)
  call psb_sum(ictxt,precsize)

  if (iam == psb_root_) then
    write(psb_out_unit,'(" ")')
    write(psb_out_unit,'("Number of processes           : ",i0)')np
    write(psb_out_unit,'("Time to solve system          : ",es12.5)')t2
    write(psb_out_unit,'("Time per iteration            : ",es12.5)')t2/iter
    write(psb_out_unit,'("Number of iterations          : ",i0)')iter
    write(psb_out_unit,'("Convergence indicator on exit : ",es12.5)')err
    write(psb_out_unit,'("Info  on exit                 : ",i0)')info
    write(psb_out_unit,'("Total memory occupation for A:      ",i12)')amatsize
    write(psb_out_unit,'("Total memory occupation for PREC:   ",i12)')precsize    
    write(psb_out_unit,'("Total memory occupation for DESC_A: ",i12)')descsize
    write(psb_out_unit,'("Storage type for DESC_A: ",a)') desc_a%get_fmt()
  end if


  !  
  !  cleanup storage and exit
  !
  call psb_gefree(bv,desc_a,info)
  call psb_gefree(xxv,desc_a,info)
  call psb_spfree(a,desc_a,info)
  call prec%free(info)
  call psb_cdfree(desc_a,info)
  if(info /= psb_success_) then
    info=psb_err_from_subroutine_
    ch_err='free routine'
    call psb_errpush(info,name,a_err=ch_err)
    goto 9999
  end if

  call psb_exit(ictxt)
  stop

9999 call psb_error(ictxt)

  stop

contains
  !
  ! get iteration parameters from standard input
  !
  subroutine  get_parms(ictxt,kmethd,ptype,afmt,idim,istopc,itmax,itrace,irst)
    integer(psb_ipk_) :: ictxt
    character(len=*) :: kmethd, ptype, afmt
    integer(psb_ipk_) :: idim, istopc,itmax,itrace,irst
    integer(psb_ipk_) :: np, iam
    integer(psb_ipk_) :: ip

    call psb_info(ictxt, iam, np)

    if (iam == 0) then
      read(psb_inp_unit,*) ip
      if (ip >= 3) then
        read(psb_inp_unit,*) kmethd
        read(psb_inp_unit,*) ptype
        read(psb_inp_unit,*) afmt

        read(psb_inp_unit,*) idim
        if (ip >= 4) then
          read(psb_inp_unit,*) istopc
        else
          istopc=1        
        endif
        if (ip >= 5) then
          read(psb_inp_unit,*) itmax
        else
          itmax=500
        endif
        if (ip >= 6) then
          read(psb_inp_unit,*) itrace
        else
          itrace=-1
        endif
        if (ip >= 7) then
          read(psb_inp_unit,*) irst
        else
          irst=1
        endif

        write(psb_out_unit,'("Solving matrix       : ell1")')      
        write(psb_out_unit,'("Grid dimensions      : ",i5," x ",i5)')idim,idim
        write(psb_out_unit,'("Number of processors : ",i0)')np
        write(psb_out_unit,'("Data distribution    : BLOCK")')
        write(psb_out_unit,'("Preconditioner       : ",a)') ptype
        write(psb_out_unit,'("Iterative method     : ",a)') kmethd
        write(psb_out_unit,'(" ")')
      else
        ! wrong number of parameter, print an error message and exit
        call pr_usage(izero)      
        call psb_abort(ictxt)
        stop 1
      endif
    end if

    ! broadcast parameters to all processors
    call psb_bcast(ictxt,kmethd)
    call psb_bcast(ictxt,afmt)
    call psb_bcast(ictxt,ptype)
    call psb_bcast(ictxt,idim)
    call psb_bcast(ictxt,istopc)
    call psb_bcast(ictxt,itmax)
    call psb_bcast(ictxt,itrace)
    call psb_bcast(ictxt,irst)


    return

  end subroutine get_parms
  !
  !  print an error message 
  !  
  subroutine pr_usage(iout)
    integer(psb_ipk_) :: iout
    write(iout,*)'incorrect parameter(s) found'
    write(iout,*)' usage:  pde2d90 methd prec dim &
         &[istop itmax itrace]'  
    write(iout,*)' where:'
    write(iout,*)'     methd:    cgstab cgs rgmres bicgstabl' 
    write(iout,*)'     prec :    bjac diag none'
    write(iout,*)'     dim       number of points along each axis'
    write(iout,*)'               the size of the resulting linear '
    write(iout,*)'               system is dim**3'
    write(iout,*)'     istop     stopping criterion  1, 2  '
    write(iout,*)'     itmax     maximum number of iterations [500] '
    write(iout,*)'     itrace    <=0  (no tracing, default) or '  
    write(iout,*)'               >= 1 do tracing every itrace'
    write(iout,*)'               iterations ' 
  end subroutine pr_usage

  !
  ! functions parametrizing the differential equation 
  !  
  function b1(x,y)
    use psb_base_mod, only : psb_dpk_
    real(psb_dpk_) :: b1
    real(psb_dpk_), intent(in) :: x,y
    b1=1.d0/sqrt(2.d0)
  end function b1
  function b2(x,y)
    use psb_base_mod, only : psb_dpk_
    real(psb_dpk_) ::  b2
    real(psb_dpk_), intent(in) :: x,y
    b2=1.d0/sqrt(2.d0)
  end function b2
  function c(x,y)
    use psb_base_mod, only : psb_dpk_
    real(psb_dpk_) ::  c
    real(psb_dpk_), intent(in) :: x,y
    c=0.d0
  end function c
  function a1(x,y)
    use psb_base_mod, only : psb_dpk_
    real(psb_dpk_) ::  a1   
    real(psb_dpk_), intent(in) :: x,y
    a1=1.d0/80
  end function a1
  function a2(x,y)
    use psb_base_mod, only : psb_dpk_
    real(psb_dpk_) ::  a2
    real(psb_dpk_), intent(in) :: x,y
    a2=1.d0/80
  end function a2
  function g(x,y)
    use psb_base_mod, only : psb_dpk_, done, dzero
    real(psb_dpk_) ::  g
    real(psb_dpk_), intent(in) :: x,y
    g = dzero
    if (x == done) then
      g = done
    else if (x == dzero) then 
      g = exp(-y**2)
    end if
  end function g
end program psb_d_pde2d


