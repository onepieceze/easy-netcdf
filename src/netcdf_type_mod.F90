module netcdf_type_mod

  !! The io module to open or create netcdf file. 

  use netcdf
  use netcdf_i_mod
  use netcdf_o_mod
  use base_type_mod
  use linked_list_mod
  use netcdf_tool_mod
  use netcdf_param_mod
  use variable_type_mod
  use dimension_type_mod

  implicit none

  private

  public :: netcdf_type

  type, extends(base_type) :: netcdf_type
    integer               , private              :: ncid
    character(:)          , allocatable, private :: iotype
    type(linked_list_type), pointer    , private :: dimensions => null()
    type(linked_list_type), pointer    , private :: variables  => null()
  contains
    procedure                                    :: add_file
    procedure                          , private :: add_variable1
    procedure                          , private :: add_variable2
    generic                                      :: add_variable => &
                                                    add_variable1,  &
                                                    add_variable2
    procedure                                    :: read         => read_netcdf
    procedure                                    :: write        => write_netcdf
  end type netcdf_type

contains

  subroutine add_file(this, file, model)

    class(netcdf_type), intent(inout) :: this
    character(*)      , intent(in)    :: file
    character(*)      , intent(in)    :: model
    
    if (allocated(this%iotype)) stop "Error: netcdf file have been open or create."

    select case (model)
    case ('r', 'read')
      call check(nf90_open(file, nf90_nowrite, this%ncid))
      this%iotype = "read"
    case ('w', 'write')
      call check(nf90_create(file, NF90_CLOBBER, this%ncid))
      this%iotype = "write"
    case default
      stop "Error: unknown model to add netcdf file."
    end select

  end subroutine add_file


  subroutine add_variable1(this, variable)

    class(netcdf_type) , intent(inout) :: this
    type(variable_type), intent(in)    :: variable

    if (.not. associated(this%variables)) allocate(this%variables)

    call this%variables%append_ptr(variable)

  end subroutine add_variable1


  subroutine add_variable2(this, variable)

    class(netcdf_type)  , intent(inout) :: this
    type(dimension_type), intent(in)    :: variable

    if (.not. associated(this%dimensions)) allocate(this%dimensions)

    call this%dimensions%append_ptr(variable)

  end subroutine add_variable2


  subroutine read_netcdf(this)

    class(netcdf_type)    , intent(inout) :: this

    if (this%iotype /= "r" .and. this%iotype /= "read") stop "Error: should be read model."

    call netcdf_read_coordinate(this%ncid, this%dimensions)

    call netcdf_read_variable(this%ncid, this%variables)

    call netcdf_read_attribute(this%ncid, nf90_global, this%get_attributes())

    call check(nf90_close(this%ncid))

  end subroutine read_netcdf


  subroutine write_netcdf(this)

    class(netcdf_type)    , intent(inout) :: this

    if (this%iotype /= "w" .and. this%iotype /= "write") stop "Error: should be write model."

    call netcdf_define_coordinate(this%ncid, this%dimensions)

    call netcdf_define_variable(this%ncid, this%variables, this%dimensions)

    call netcdf_define_attribute(this%ncid, nf90_global, this%get_attributes())

    call check(nf90_enddef(this%ncid))

    call netcdf_write_coordinate(this%ncid, this%dimensions)

    call netcdf_write_variable(this%ncid, this%variables)

    call check(nf90_close(this%ncid))

  end subroutine write_netcdf
  

end module netcdf_type_mod

