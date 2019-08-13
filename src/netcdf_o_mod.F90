module netcdf_o_mod

  !!  ------   create netcdf file method   ------  !!  

  use netcdf
  use datetime
  use netcdf_tool_mod
  use linked_list_mod
  
  implicit none

  private

  public :: netcdf_define_coordinate
  public :: netcdf_define_variable
  public :: netcdf_define_global
  public :: netcdf_write_coordinate
  public :: netcdf_write_variable
  
contains
  
  subroutine netcdf_define_coordinate(ncid, dim, varid, dimid)
  
    integer                    , intent(in)    :: ncid
    class(dimension_type)      , intent(in)    :: dim
    integer                    , intent(out)   :: varid
    integer                    , intent(out)   :: dimid    
    
    type(linked_list_type)     , pointer       :: attribute
    type(linked_list_item_type), pointer       :: item
    integer                                    :: dimension_size
    integer                                    :: i
  
    if (.not. allocated(dim%name)) stop "Error: name attribute of dimension not found."
    dimension_size = size(dime%get_value())
    call check(nf90_def_dim(ncid, dim%name, dimension_size, dimid))
    call check(nf90_def_var(ncid, dim%name, dim%xtype, dimid, varid))
  
    if (.not. associated(dim%get_value())) stop "  Error: value of dimension not allocated."

    attribute => dim%get_attribute()

    do i=1, attribute%size
      item => attribute%item_at(i)
      select type(value => item%value)
      type is (character(*))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(2))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(4))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(8))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (real(4))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (real(8))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (logical)
        call check(nf90_put_att(ncid, varid, item%key, value))
      end select
    end do
  
  end subroutine netcdf_define_coordinate
  
  
  subroutine netcdf_define_variable(ncid, variable, dimids)

    integer                    , intent(in)    :: ncid
    class(variable_type)       , intent(inout) :: variable
    integer                    , intent(in)    :: dimids(:)

    type(linked_list_type)     , pointer       :: attribute
    type(linked_list_item_type), pointer       :: item
    integer                                    :: varid
    logical                                    :: logical
    integer                                    :: i

    if (variable%xtype == -99999)   stop "Error: data type of variable not define."
    if (.not. allocated(variable%name)) stop "Error: name attribute of variable not found."
    print*, " --- "/variable%name

    call check(nf90_def_var(ncid, value, variable%xtype, dimids, varid))

    attribute => variable%get_attribute()

    do i=1, attribute%size
      item => attribute%item_at(i)
      select type (value => item%value)
      type is (character(*))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(2))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(4))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (integer(8))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (real(4))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (real(8))
        call check(nf90_put_att(ncid, varid, item%key, value))
      type is (logical(4))
        call check(nf90_put_att(ncid, varid, item%key, value))
      end select
    end do

    call variable%set_varid(varid)
      
  end subroutine netcdf_define_variable

  
  subroutine netcdf_define_global(ncid, attribute)

    integer                    , intent(in) :: ncid 
    type(linked_list_type)     , intent(in) :: attribute
    
    type(linked_list_item_type), pointer    :: item 
    type(datetime_type)                     :: datetime
    integer                                 :: i

    call datetime%init()

    call check(nf90_put_att(ncid, nf90_global, "creation_date", trim(datetime%isoformat())))

      do i=1, attribute%size
        item => attribute%item_at(i)
        select type (value => item%value)
        type is (character(*))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (integer(2))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (integer(4))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (integer(8))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (real(4))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (real(8))
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        type is (logical)
          call check(nf90_put_att(ncid, nf90_global, item%key, value))
        end select
      end do

  end subroutine netcdf_define_global


  subroutine netcdf_write_coordinate(ncid, dim)

    integer              , intent(in) :: ncid
    class(dimension_type), intent(in) :: dime

    integer                           :: varid                        

    if (allocated(dimen%get_value())) then
      varid = dimension%get_varid()
      select type (value => dime%get_value())
      type is (integer(2))
        call check(nf90_put_var(this%ncid, varid, value))
      type is (integer(4))
        call check(nf90_put_var(this%ncid, varid, value))
      type is (integer(8))
        call check(nf90_put_var(this%ncid, varid, value))
      type is (real(4))
        call check(nf90_put_var(this%ncid, varid, value))
      type is (real(8))
        call check(nf90_put_var(this%ncid, varid, value))
      end select
    else
      stop "  Error: dimension value not set."
    end if

  end subroutine netcdf_write_coordinate


  subroutine netcdf_write_variable(ncid, variable_list)

    integer               , intent(in) :: ncid
    type(linked_list_type), intent(in) :: variable_list

    integer                            :: i
    integer                            :: varid

    do i=1, this%variable_list%size
      select type (value => this%variable_list%value_at(i))
      type is (variable_type)
        print*, " --- "/value%name
        varid = value%get_varid()
        if (associated(value%value_2d)) then
          select type (value_2d => value%value_2d)
          type is (integer(2))
            call check(nf90_put_var(ncid, varid, value_2d))
          type is (integer(4))
            call check(nf90_put_var(ncid, varid, value_2d))
          type is (integer(8))
            call check(nf90_put_var(ncid, varid, value_2d))
          type is (real(4))
            call check(nf90_put_var(ncid, varid, value_2d))
          type is (real(8))
            call check(nf90_put_var(ncid, varid, value_2d))
          end select
        end if
        if (associated(value%value_3d)) then
          select type (value_3d => value%value_3d)
          type is (integer(2))
            call check(nf90_put_var(ncid, varid, value_3d))
          type is (integer(4))
            call check(nf90_put_var(ncid, varid, value_3d))
          type is (integer(8))
            call check(nf90_put_var(ncid, varid, value_3d))
          type is (real(4))
            call check(nf90_put_var(ncid, varid, value_3d))
          type is (real(8))
            call check(nf90_put_var(ncid, varid, value_3d))
          end select
        end if
        if (associated(value%value_4d)) then
          select type (value_4d => value%value_4d)
          type is (integer(2))
            call check(nf90_put_var(ncid, varid, value_4d))
          type is (integer(4))
            call check(nf90_put_var(ncid, varid, value_4d))
          type is (integer(8))
            call check(nf90_put_var(ncid, varid, value_4d))
          type is (real(4))
            call check(nf90_put_var(ncid, varid, value_4d))
          type is (real(8))
            call check(nf90_put_var(ncid, varid, value_4d))
          end select
        end if    
      type default
        stop "Error: variable type not match."
      end select
    end do
    
  end subroutine netcdf_write_variable

end module netcdf_o_mod