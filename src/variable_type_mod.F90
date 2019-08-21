module variable_type_mod

  use base_type_mod
  use linked_list_mod
  use netcdf_param_mod
  use dimension_type_mod
  
  
  implicit none

  private

  public :: variable_type

  type, extends(base_type) :: variable_type
    character(:)          , allocatable          :: name
    integer                                      :: xtype = -99999
    integer                            , private :: varid
    character(60)         , allocatable, private :: dimensions(:)
    type(linked_list_type)             , private :: dimids
    class(*)              , pointer              :: value_2d(:, :)       => null()
    class(*)              , pointer              :: value_3d(:, :, :)    => null()
    class(*)              , pointer              :: value_4d(:, :, :, :) => null()
  contains
    procedure                                    :: set_varid
    procedure                                    :: get_varid
    procedure                                    :: dimension => set_dimensions
    procedure                                    :: get_dimensions
    procedure                          , private :: set_value_2d
    procedure                          , private :: set_value_3d
    procedure                          , private :: set_value_4d
    generic                                      :: assignment(=) => &
                                                    set_value_2d,    &
                                                    set_value_3d,    &
                                                    set_value_4d                    
  end type

contains

  subroutine set_varid(this, varid)

    class(variable_type), intent(inout) :: this
    integer             , intent(in)    :: varid

    this%varid = varid

  end subroutine set_varid


  function get_varid(this) result(res)

    class(variable_type), intent(in) :: this
    integer                          :: res

    res = this%varid

  end function get_varid


  subroutine set_dimensions(this, dimensions)

    class(variable_type), intent(inout) :: this
    type(dimension_type), intent(in)    :: dimensions(:)

    integer                             :: i

    allocate(this%dimensions(size(dimensions)))

    do i=1, size(dimensions)
      this%dimensions(i) = dimensions(i)%name
    end do

  end subroutine set_dimensions


  subroutine get_dimensions(this, dimensions_name)

    class(variable_type)             , intent(in)    :: this
    character(60)       , allocatable, intent(inout) :: dimensions_name(:)

    allocate(dimensions_name(size(this%dimensions)))

    dimensions_name = this%dimensions

  end subroutine get_dimensions


  subroutine set_value_2d(this, value)

    class(variable_type)        , intent(inout) :: this
    class(*)            , target, intent(in)    :: value(:, :)

    this%value_2d => value

  end subroutine set_value_2d


  subroutine set_value_3d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , target , intent(in)    :: value(:, :, :)

    this%value_3d => value

  end subroutine set_value_3d


  subroutine set_value_4d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , target , intent(in)    :: value(:, :, :, :)

    this%value_4d => value

  end subroutine set_value_4d

end module variable_type_mod
