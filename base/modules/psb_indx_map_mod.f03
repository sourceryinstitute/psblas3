module psb_indx_map_mod
  use psb_const_mod
  use psb_desc_const_mod
  
  type, abstract :: psb_indx_map
  contains
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_state
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_state
    procedure(idx_get_logical), deferred, pass(idxmap)  :: is_repl
    procedure(idx_get_logical), deferred, pass(idxmap)  :: is_bld
    procedure(idx_get_logical), deferred, pass(idxmap)  :: is_upd
    procedure(idx_get_logical), deferred, pass(idxmap)  :: is_asb
    procedure(idx_get_logical), deferred, pass(idxmap)  :: is_valid
    procedure(idx_get_long_int), deferred, pass(idxmap) :: sizeof
    procedure(idx_fix), deferred, pass(idxmap)          :: asb
    procedure(idx_free), deferred, pass(idxmap)         :: free

    procedure(idx_i2is1), deferred, pass(idxmap)        :: l2gs1
    procedure(idx_i2is2), deferred, pass(idxmap)        :: l2gs2
    procedure(idx_i2iv1), deferred, pass(idxmap)        :: l2gv1
    procedure(idx_i2iv2), deferred, pass(idxmap)        :: l2gv2
    generic, public         :: l2g => l2gs1, l2gs2, l2gv1, l2gv2

    procedure(idx_i2is1), deferred, pass(idxmap)        :: g2ls1
    procedure(idx_i2is2), deferred, pass(idxmap)        :: g2ls2
    procedure(idx_i2iv1), deferred, pass(idxmap)        :: g2lv1
    procedure(idx_i2iv2), deferred, pass(idxmap)        :: g2lv2
    generic, public         :: g2l => g2ls1, g2ls2, g2lv1, g2lv2

    procedure(idx_i2is1_ins), deferred, pass(idxmap)    :: g2ls1_ins
    procedure(idx_i2is2_ins), deferred, pass(idxmap)    :: g2ls2_ins
    procedure(idx_i2iv1_ins), deferred, pass(idxmap)    :: g2lv1_ins
    procedure(idx_i2iv2_ins), deferred, pass(idxmap)    :: g2lv2_ins
    generic, public            :: g2l_ins => g2ls1_ins, g2ls2_ins,&
         &                                   g2lv1_ins, g2lv2_ins

    procedure(idx_get_int), deferred, pass(idxmap)      :: get_gr
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_gc
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_lr
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_lc
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_ctxt
    procedure(idx_get_int), deferred, pass(idxmap)      :: get_mpic

    procedure(idx_set_int), deferred, pass(idxmap)      :: set_gr
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_gc
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_lr
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_lc
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_ctxt
    procedure(idx_set_int), deferred, pass(idxmap)      :: set_mpic

  end type psb_indx_map

  abstract interface 
    function idx_get_int(idxmap) result(val)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      integer :: val
    end function idx_get_int
  end interface

  abstract interface 
    subroutine idx_set_int(idxmap,val)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(in)  :: val
    end subroutine idx_set_int
  end interface


  abstract interface 
    function idx_get_logical(idxmap) result(val)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      logical :: val
    end function idx_get_logical
  end interface

  abstract interface 
    function idx_get_long_int(idxmap) result(val)
      import :: psb_indx_map, psb_long_int_k_
      class(psb_indx_map), intent(in) :: idxmap
      integer(psb_long_int_k_) :: val
    end function idx_get_long_int
  end interface

  abstract interface 
    subroutine idx_fix(idxmap,info)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(out) :: info
    end subroutine idx_fix
  end interface

  abstract interface 
    subroutine idx_free(idxmap)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
    end subroutine idx_free
  end interface

  abstract interface 
    subroutine idx_i2is1(idx,idxmap,info,mask,owned)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      integer, intent(inout) :: idx
      integer, intent(out)   :: info 
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: owned
    end subroutine idx_i2is1
  end interface

  abstract interface   
    subroutine idx_i2is2(idxin,idxout,idxmap,info,mask,owned)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      integer, intent(in)   :: idxin
      integer, intent(out)  :: idxout, info 
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: owned
    end subroutine idx_i2is2
  end interface

  abstract interface 
    subroutine idx_i2iv1(idx,idxmap,info,mask,owned)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      integer, intent(inout) :: idx(:)
      integer, intent(out)   :: info 
      logical, intent(in), optional :: mask(:)
      logical, intent(in), optional :: owned
    end subroutine idx_i2iv1
  end interface

  abstract interface   
    subroutine idx_i2iv2(idxin,idxout,idxmap,info,mask,owned)
      import :: psb_indx_map
      class(psb_indx_map), intent(in) :: idxmap
      integer, intent(in)   :: idxin(:)
      integer, intent(out)  :: idxout(:), info 
      logical, intent(in), optional :: mask(:)
      logical, intent(in), optional :: owned
    end subroutine idx_i2iv2
  end interface

  abstract interface 
    subroutine idx_i2is1_ins(idx,idxmap,info,mask)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(inout) :: idx
      integer, intent(out)   :: info 
      logical, intent(in), optional :: mask
    end subroutine idx_i2is1_ins
  end interface

  abstract interface   
    subroutine idx_i2is2_ins(idxin,idxout,idxmap,info,mask)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(in)   :: idxin
      integer, intent(out)  :: idxout, info 
      logical, intent(in), optional :: mask
    end subroutine idx_i2is2_ins
  end interface

  abstract interface 
    subroutine idx_i2iv1_ins(idx,idxmap,info,mask)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(inout) :: idx(:)
      integer, intent(out)   :: info 
      logical, intent(in), optional :: mask(:)
    end subroutine idx_i2iv1_ins
  end interface

  abstract interface   
    subroutine idx_i2iv2_ins(idxin,idxout,idxmap,info,mask)
      import :: psb_indx_map
      class(psb_indx_map), intent(inout) :: idxmap
      integer, intent(in)   :: idxin(:)
      integer, intent(out)  :: idxout(:), info 
      logical, intent(in), optional :: mask(:)
    end subroutine idx_i2iv2_ins
  end interface

end module psb_indx_map_mod
