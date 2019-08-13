module global_type_mod
 
  use hash_table_mod

  implicit none

  private

  public :: global_type

  type :: global_type
    type(hash_table_type), private :: global_attribute
  contains
    procedure                      :: attribution => set_global_attribute
    procedure                      :: get_attribute => get_global_attribute
  end type global_type

contains

  subroutine netcdf_set_global_attribute(this, key, value)

    type(global_type), intent(inout) :: this
    character        , intent(in)    :: key
    class(*)         , intent(in)    :: value

    call this%global_attribute%insert(key, value)

  end subroutine netcdf_set_global_attribute


  function netcdf_get_global_attribute(this) result(res)

    type(global_type)    , intent(in) :: this
    type(hash_table_type)             :: res

    res = this%global_attribute
 
  end function netcdf_get_global_attribute

end module global_type_mod