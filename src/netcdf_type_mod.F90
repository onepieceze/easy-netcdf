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
    integer               , allocatable, private :: dimids(:)
    type(dimension_type)                         :: x
    type(dimension_type)                         :: y
    type(dimension_type)                         :: z
    type(dimension_type)                         :: t
    type(linked_list_type), pointer    , private :: variables  => null()
  contains
    procedure                                    :: add_file
    procedure                                    :: add_variable
    procedure                                    :: read   => read_netcdf
    procedure                                    :: write  => write_netcdf
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


  subroutine add_variable(this, variable)

    class(netcdf_type) , intent(inout) :: this
    type(variable_type), intent(in)    :: variable

    if (.not. associated(this%variables)) allocate(this%variables)

    call this%variables%append_ptr(variable)

  end subroutine add_variable


  subroutine read_netcdf(this)

    class(netcdf_type), intent(inout) :: this

    integer                           :: dims_number
    integer           , allocatable   :: dimids(:)
    integer                           :: include_parents
    integer                           :: i
    character(30)                     :: dims_name
    integer                           :: length
    integer                           :: varid

    if (this%iotype /= "r" .and. this%iotype /= "read") stop "Error: should be read model."

    call check(nf90_inquire(ncid=this%ncid, ndimensions=dims_number))

    allocate(dimids(dims_number))

    call check(nf90_inq_dimids(this%ncid, dims_number, dimids, include_parents))

    do i=1, dims_number
      call check(nf90_inquire_dimension(this%ncid, dimids(i), dims_name, length))
      call check(nf90_inq_varid(this%ncid, dims_name, varid))
      if (dims_name == this%x%name) then
        print*, "Read x dimension:"
        call netcdf_read_coordinate(this%ncid, varid, this%x%get_value())
        call netcdf_read_attribute(this%ncid, varid, this%x%get_attributes())
      end if
      if (dims_name == this%y%name) then
        print*, "Read y dimension:"
        call netcdf_read_coordinate(this%ncid, varid, this%y%get_value())
        call netcdf_read_attribute(this%ncid, varid, this%y%get_attributes())
      end if
      if (dims_name == this%z%name) then
        print*, "Read z dimension:"
        call netcdf_read_coordinate(this%ncid, varid, this%z%get_value())
        call netcdf_read_attribute(this%ncid, varid, this%z%get_attributes())
      end if
      if (dims_name == this%t%name) then
        print*, "Read t dimension:"
        call netcdf_read_coordinate(this%ncid, varid, this%t%get_value())
        call netcdf_read_attribute(this%ncid, varid, this%t%get_attributes())
      end if
    end do

    print*, "Read variable:"
    call netcdf_read_variable(this%ncid, this%variables)

    print*, "Read global attribute:"
    call netcdf_read_attribute(this%ncid, nf90_global, this%get_attributes())

    call check(nf90_close(this%ncid))

    print*, "   --- finished reading ---   "

  end subroutine read_netcdf


  subroutine write_netcdf(this)

    class(netcdf_type), intent(inout) :: this
  
    integer                           :: dims_number
    integer                           :: varid
    integer                           :: x_dimid
    integer                           :: y_dimid
    integer                           :: z_dimid
    integer                           :: t_dimid
    integer, allocatable              :: dimids(:)
    integer                           :: i

    if (this%iotype /= "w" .and. this%iotype /= "write") stop "Error: should be write model."
    
    dims_number = 0

    print*, "Define x dimension:"
    if (allocated(this%x%name)) then
      call netcdf_define_coordinate(this%ncid, this%x%name, this%x%length(), this%x%xtype, x_dimid, varid)
      call netcdf_define_attribute(this%ncid, varid, this%x%get_attributes())
      call this%x%set_varid(varid)
      dims_number = dims_number + 1
    end if

    print*, "Define y dimension:"
    if (allocated(this%y%name)) then
      call netcdf_define_coordinate(this%ncid, this%y%name, this%y%length(), this%y%xtype, y_dimid, varid)
      call netcdf_define_attribute(this%ncid, varid, this%y%get_attributes())
      call this%y%set_varid(varid)
      dims_number = dims_number + 1
    end if

    print*, "Define z dimension:"
    if (allocated(this%z%name)) then
      call netcdf_define_coordinate(this%ncid, this%z%name, this%z%length(), this%z%xtype, z_dimid, varid)
      call netcdf_define_attribute(this%ncid, varid, this%z%get_attributes())
      call this%z%set_varid(varid)
      dims_number = dims_number + 1
    end if

    print*, "Define t dimension:"
    if (allocated(this%t%name)) then
      call netcdf_define_coordinate(this%ncid, this%t%name, nf90_unlimited, this%t%xtype, t_dimid, varid)
      call netcdf_define_attribute(this%ncid, varid, this%t%get_attributes())
      call this%t%set_varid(varid)
      dims_number = dims_number + 1
    end if

    allocate(dimids(dims_number))

    select case (dims_number)
    case(4)
      dimids = [x_dimid, y_dimid, z_dimid, t_dimid]
    case(3)
      if (allocated(this%z%name)) dimids = [x_dimid, y_dimid, z_dimid]
      if (allocated(this%t%name)) dimids = [x_dimid, y_dimid, t_dimid]
    case(2)
      dimids = [x_dimid, y_dimid]
    case(1, 0)
      stop "Error: dimension number of netcdf file can not less than 2."
    end select

    print*, "Define variable:"
    call netcdf_define_variable(this%ncid, this%variables, dimids)

    deallocate(dimids)

    print*, "Define global attribute"
    call netcdf_define_attribute(this%ncid, nf90_global, this%get_attributes())

    call check(nf90_enddef(this%ncid))

    print*, "Write x dimension:"
    if (allocated(this%x%name)) then
      call netcdf_write_coordinate(this%ncid, this%x)
    end if

    print*, "Write y dimension:"
    if (allocated(this%y%name)) then
      call netcdf_write_coordinate(this%ncid, this%y)
    end if

    print*, "Write z dimension:"
    if (allocated(this%z%name)) then
      call netcdf_write_coordinate(this%ncid, this%z)
    end if

    print*, "Write t dimension:"
    if (allocated(this%t%name)) then
      call netcdf_write_coordinate(this%ncid, this%t)
    end if

    print*, "Write variable:"
    call netcdf_write_variable(this%ncid, this%variables)

    call check(nf90_close(this%ncid))

    print*, "   --- finished writing ---   "

  end subroutine write_netcdf
  

end module netcdf_type_mod

