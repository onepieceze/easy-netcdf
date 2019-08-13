module dimension_type_mod

  use linked_list_mod

  implicit none

  private

  public :: dimension_type

  type :: dimension_type
    character(:)          , allocatable :: name
    integer                             :: xtype = -99999
    type(linked_list_type), pointer     :: dimension_attribute => null()
    class(*)              , pointer     :: value(:)            => null()
    integer               , private     :: dimension_varid
    integer               , private     :: dimension_length
  contains
    procedure                           :: attribute => set_dimension_attribute
    procedure                           :: get_attribute => get_dimension_attribute
    procedure             , private     :: set_dimension_value
    generic                             :: assignment(=) => set_dimension_value
    procedure                           :: get_value     => get_dimension_value
    procedure                           :: set_varid     => set_dimension_varid
    procedure                           :: get_varid     => get_dimension_varid
    procedure                           :: length        => get_dimension_length                                  
  end type dimension_type

contains

  subroutine set_dimension_attribute(this, key, value)

    class(dimension_type), intent(inout) :: this
    character(*)         , intent(in)    :: key
    class(*)             , intent(in)    :: value

    if (.not. associated(this%dimension_attribute)) allocate(this%dimension_attribute)

    call this%dimension_attribute%append_ptr(key, value)

  end subroutine set_dimension_attribute


  function get_dimension_attribute(this) result(res)

    class(dimension_type) , intent(inout) :: this
    type(linked_list_type), pointer       :: res

    res => this%dimension_attribute

  end function get_dimension_attribute


  subroutine set_dimension_value(this, value)

    class(dimension_type)        , intent(inout) :: this
    class(*)             , target, intent(in)    :: value(:)

    this%dimension_length = size(value)
    this%value => value

  end subroutine set_dimension_value


!  subroutine set_dimension_value_short(this, value)
!
!    class(dimension_type), intent(inout) :: this
!    integer(2), target   , intent(in)    :: value(:)
!
!    this%dimension_value => value
!
!  end subroutine set_dimension_value_short
!
!
!  subroutine set_dimension_value_int(this, value)
!
!    class(dimension_type), intent(inout) :: this
!    integer(4), target   , intent(in)    :: value(:)
!
!    this%dimension_value => value
!
!  end subroutine set_dimension_value_int
!
!
!  subroutine set_dimension_value_long(this, value)
!
!    class(dimension_type), intent(inout) :: this
!    integer(8), target   , intent(in)    :: value(:)
!
!    this%dimension_value => value
!
!  end subroutine set_dimension_value_long
!
!
!  subroutine set_dimension_value_float(this, value)
!
!    class(dimension_type), intent(inout) :: this
!    real(4), target      , intent(in)    :: value(:)
!
!    this%dimension_value => value
!
!  end subroutine set_dimension_value_float
!
!
!  subroutine set_dimension_value_double(this, value)
!
!    class(dimension_type), intent(inout) :: this
!    real(8), target      , intent(in)    :: value(:)
!
!    this%dimension_value => value
!
!  end subroutine set_dimension_value_double


  function get_dimension_value(this) result(res)

    class(dimension_type), intent(in) :: this
    class(*), pointer                 :: res(:)
    
    res => this%value

  end function get_dimension_value


  subroutine set_dimension_varid(this, varid)

    class(dimension_type), intent(inout) :: this
    integer              , intent(in)    :: varid

    this%dimension_varid = varid

  end subroutine set_dimension_varid


  function get_dimension_varid(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    res = this%dimension_varid

  end function get_dimension_varid


  function get_dimension_length(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    if (.not. associated(this%value)) stop "Error: dimension value not allocated, length unknown."

    res = this%dimension_length

  end function get_dimension_length


end module dimension_type_mod
