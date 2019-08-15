module variable_type_mod

  use netcdf_param_mod
  use linked_list_mod
  
  implicit none

  private

  public :: variable_type

  type :: variable_type
    character(:)          , allocatable          :: name
    integer                                      :: xtype = -99999
    integer                            , private :: varid
    type(linked_list_type), pointer    , private :: attributes           => null()
    class(*)              , pointer              :: value_2d(:, :)       => null()
    class(*)              , pointer              :: value_3d(:, :, :)    => null()
    class(*)              , pointer              :: value_4d(:, :, :, :) => null()
  contains
    procedure                                    :: set_varid
    procedure                                    :: get_varid
    procedure                          , private :: set_attribute
    procedure                          , private :: set_attribute_2d
    generic                                      :: attribute =>   &
                                                    set_attribute, &
                                                    set_attribute_2d
    procedure                                    :: get_attributes
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


  subroutine set_attribute(this, key, value)

    class(variable_type), intent(inout) :: this
    character(*)        , intent(in)    :: key
    class(*)            , intent(in)    :: value

    if (.not. associated(this%attributes)) allocate(this%attributes)

    call this%attributes%append_ptr(key, value)

  end subroutine set_attribute


  subroutine set_attribute_2d(this, key, value)

    class(variable_type), intent(inout) :: this
    character(*)        , intent(in)    :: key
    class(*) , target   , intent(in)    :: value(:)

    type(array_2d_type)                 :: array_2d

    if (.not. associated(this%attributes)) allocate(this%attributes)

    array_2d%array => value

    call this%attributes%append_ptr(key, array_2d)

  end subroutine


  function get_attributes(this) result(res)

    class(variable_type)  , intent(in) :: this
    type(linked_list_type), pointer    :: res

    res => this%attributes

  end function get_attributes


  subroutine set_value_2d(this, value)

    class(variable_type)        , intent(inout) :: this
    class(*)            , target, intent(in)    :: value(:, :)

    this%value_2d => value

  end subroutine set_value_2d


  subroutine set_value_3d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , pointer, intent(in)    :: value(:, :, :)

    this%value_3d => value

  end subroutine set_value_3d


  subroutine set_value_4d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , pointer, intent(in)    :: value(:, :, :, :)

    this%value_4d => value

  end subroutine set_value_4d

end module variable_type_mod
