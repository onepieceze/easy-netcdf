module netcdf_param_mod

  implicit none

  private

  public :: array_2d_type

  type :: array_2d_type
    class(*), pointer :: array(:)
  end type

end module netcdf_param_mod