module dimension_type_mod

  use base_type_mod
  use netcdf_param_mod
  use linked_list_mod

  implicit none

  private

  public :: dimension_type

  type, extends(base_type) :: dimension_type
    character(:)          , allocatable :: name
    integer                             :: xtype = -99999
    logical                             :: unlimited = .false.
    class(*)              , pointer     :: value(:)   => null()
    integer               , private     :: dimid
    integer               , private     :: varid
    integer               , private     :: length
  contains
    procedure             , private     :: set_value
    generic                             :: assignment(=) => set_value
    procedure                           :: get_value
    procedure                           :: set_varid
    procedure                           :: get_varid
    procedure                           :: set_dimid
    procedure                           :: get_dimid
    procedure                           :: get_length                                  
  end type dimension_type

contains

  subroutine set_value(this, value)

    class(dimension_type)        , intent(inout) :: this
    class(*)             , target, intent(in)    :: value(:)

    this%length = size(value)
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

    this%varid = varid

  end subroutine set_varid


  function get_varid(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    res = this%varid

  end function get_varid


  subroutine set_dimid(this, dimid)

    class(dimension_type), intent(inout) :: this
    integer              , intent(in)    :: dimid

    this%dimid = dimid

  end subroutine set_dimid
  
  
  function get_dimid(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    res = this%dimid

  end function get_dimid


  function get_length(this) result(res)

    class(dimension_type), intent(in) :: this
    integer                           :: res

    if (.not. associated(this%value)) stop "Error: dimension value not allocated, length unknown."

    res = this%length

  end function get_length

end module dimension_type_mod
