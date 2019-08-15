module dimension_type_mod

  use netcdf_param_mod
  use linked_list_mod

  implicit none

  private

  public :: dimension_type

  type :: dimension_type
    character(:)          , allocatable :: name
    integer                             :: xtype = -99999
    type(linked_list_type), pointer     :: attributes => null()
    class(*)              , pointer     :: value(:)   => null()
    integer               , private     :: dims_varid
    integer               , private     :: dims_length
  contains
    procedure             , private     :: set_attribute
    procedure             , private     :: set_attribute_2d
    generic                             :: attribute =>   &
                                           set_attribute, &
                                           set_attribute_2d
    procedure                           :: get_attributes
    procedure             , private     :: set_value
    generic                             :: assignment(=) => set_value
    procedure                           :: get_value
    procedure                           :: set_varid
    procedure                           :: get_varid 
    procedure                           :: length => get_length                                  
  end type dimension_type

contains

  subroutine set_attribute(this, key, value)

    class(dimension_type), intent(inout) :: this
    character(*)         , intent(in)    :: key
    class(*)             , intent(in)    :: value

    if (.not. associated(this%attributes)) allocate(this%attributes)

    call this%attributes%append_ptr(key, value)

  end subroutine set_attribute


  subroutine set_attribute_2d(this, key, value)

    class(dimension_type), intent(inout) :: this
    character(*)         , intent(in)    :: key
    class(*)    , target , intent(in)    :: value(:)

    type(array_2d_type)                  :: array_2d

    if (.not. associated(this%attributes)) allocate(this%attributes)

    array_2d%array => value

    call this%attributes%append_ptr(key, array_2d)

  end subroutine


  function get_attributes(this) result(res)

    class(dimension_type) , intent(inout) :: this
    type(linked_list_type), pointer       :: res

    res => this%attributes

  end function get_attributes


  subroutine set_value(this, value)

    class(dimension_type)        , intent(inout) :: this
    class(*)             , target, intent(in)    :: value(:)

    this%dims_length = size(value)
    this%value => value

  end subroutine set_value

  function get_value(this) result(res)

    class(dimension_type), intent(in) :: this
    class(*), pointer                 :: res(:)
    
    res => this%value

  end function get_value


  subroutine set_varid(this, varid)

    class(dimension_type), intent(inout) :: this
    integer              , intent(in)    :: varid

    this%dims_varid = varid

  end subroutine set_varid


  function get_varid(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    res = this%dims_varid

  end function get_varid


  function get_length(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    if (.not. associated(this%value)) stop "Error: dimension value not allocated, length unknown."

    res = this%dims_length

  end function get_length


end module dimension_type_mod
