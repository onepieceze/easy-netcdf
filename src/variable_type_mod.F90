module variable_type_mod

  use linked_list_mod
  
  implicit none

  private

  public :: variable_type

  type :: variable_type
    character(:)          , allocatable          :: name
    integer                                      :: xtype = -99999
    integer                            , private :: variable_id
    type(linked_list_type), pointer    , private :: variable_attribute   => null()
    class(*)              , pointer              :: value_2d(:, :)       => null()
    class(*)              , pointer              :: value_3d(:, :, :)    => null()
    class(*)              , pointer              :: value_4d(:, :, :, :) => null()
  contains
    procedure                                    :: set_varid => set_variable_id
    procedure                                    :: get_varid => get_variable_id
    procedure                                    :: attribute => set_variable_attribute
    procedure                                    :: get_attribute => get_variable_attribute
    procedure                          , private :: set_variable_value_2d
    procedure                          , private :: set_variable_value_3d
    procedure                          , private :: set_variable_value_4d
    generic                                      :: assignment(=) =>       &
                                                    set_variable_value_2d, &
                                                    set_variable_value_3d, &
                                                    set_variable_value_4d                    
  end type

contains

  subroutine set_variable_id(this, varid)

    class(variable_type), intent(inout) :: this
    integer             , intent(in)    :: varid

    this%variable_id = varid

  end subroutine set_variable_id


  function get_variable_id(this) result(res)

    class(variable_type), intent(in) :: this
    integer                          :: res

    res = this%variable_id

  end function get_variable_id


  subroutine set_variable_attribute(this, key, value)

    class(variable_type), intent(inout) :: this
    character(*)        , intent(in)    :: key
    class(*)            , intent(in)    :: value

    if (.not. associated(this%variable_attribute)) allocate(this%variable_attribute)

    call this%variable_attribute%append(key, value)

  end subroutine set_variable_attribute


  function get_variable_attribute(this) result(res)

    class(variable_type)  , intent(in) :: this
    type(linked_list_type), pointer    :: res

    res => this%variable_attribute

  end function get_variable_attribute


  subroutine set_variable_value_2d(this, value)

    class(variable_type)        , intent(inout) :: this
    class(*)            , target, intent(in)    :: value(:, :)

    this%value_2d => value

  end subroutine set_variable_value_2d


  subroutine set_variable_value_3d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , pointer, intent(in)    :: value(:, :, :)

    this%value_3d => value

  end subroutine set_variable_value_3d


  subroutine set_variable_value_4d(this, value)

    class(variable_type)         , intent(inout) :: this
    class(*)            , pointer, intent(in)    :: value(:, :, :, :)

    this%value_4d => value

  end subroutine set_variable_value_4d

end module variable_type_mod
