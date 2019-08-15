module base_type_mod

  use linked_list_mod
  use netcdf_param_mod

  implicit none

  private

  public :: base_type

  type, abstract :: base_type
    type(linked_list_type)    , pointer :: attributes => null()
  contains
    procedure, non_overridable, private :: set_attribute
    procedure, non_overridable, private :: set_attribute_2d
    procedure, non_overridable          :: get_attributes
    generic                             :: attribute =>   &
                                           set_attribute, &
                                           set_attribute_2d
  end type

contains

subroutine set_attribute(this, key, value)

    class(base_type), intent(inout) :: this
    character(*)      , intent(in)  :: key
    class(*)          , intent(in)  :: value

    if (.not. associated(this%attributes)) allocate(this%attributes)

    call this%attributes%append_ptr(key, value)

  end subroutine set_attribute


  subroutine set_attribute_2d(this, key, value)

    class(base_type), intent(inout) :: this
    character(*)      , intent(in)  :: key
    class(*) , target , intent(in)  :: value(:)

    type(array_2d_type)             :: array_2d

    if (.not. associated(this%attributes)) allocate(this%attributes)

    array_2d%array => value

    call this%attributes%append_ptr(key, array_2d)

  end subroutine


  function get_attributes(this) result(res)

    class(base_type)    , intent(in) :: this
    type(linked_list_type), pointer  :: res

    res => this%attributes
 
  end function get_attributes


end module base_type_mod