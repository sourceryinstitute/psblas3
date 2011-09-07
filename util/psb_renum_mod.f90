module psb_renum_mod
  use psb_base_mod

  integer, parameter :: psb_mat_renum_gps_ = 456

  
  interface psb_mat_renum
    subroutine psb_d_mat_renum(alg,mat,info,perm)
      import  psb_dspmat_type
      integer, intent(in) :: alg
      type(psb_dspmat_type), intent(inout) :: mat
      integer, intent(out) :: info
      integer, allocatable, optional, intent(out) :: perm(:)
    end subroutine psb_d_mat_renum
  end interface psb_mat_renum

end module psb_renum_mod
