module netcdf_i_mod

  !!  ------   create netcdf file method   ------  !!  

  use netcdf
  use netcdf_param_mod
  use netcdf_tool_mod
  use linked_list_mod
  use dimension_type_mod
  use variable_type_mod
  
  implicit none

  private

  public :: netcdf_define_coordinate
  public :: netcdf_define_attribute
  public :: netcdf_define_variable
  public :: netcdf_write_coordinate
  public :: netcdf_write_variable
  
contains
  
  subroutine netcdf_define_coordinate(ncid, dims_name, dims_size, xtype, dimid, varid)
  
    integer     , intent(in)  :: ncid
    character(*), intent(in)  :: dims_name
    integer     , intent(in)  :: dims_size
    integer     , intent(in)  :: xtype
    integer     , intent(out) :: dimid    
    integer     , intent(out) :: varid
  
    call check(nf90_def_dim(ncid, dims_name, dims_size, dimid))
    call check(nf90_def_var(ncid, dims_name, xtype, dimid, varid))

  end subroutine netcdf_define_coordinate


  subroutine netcdf_define_attribute(ncid, varid, attributes)

    integer                             , intent(in) :: ncid
    integer                             , intent(in) :: varid
    type(linked_list_type)     , pointer, intent(in) :: attributes 

    type(linked_list_item_type), pointer             :: item
    integer                                          :: i

    do i=1, attributes%size
      item => attributes%item_at(i)
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
      type is (array_2d_type)
        select type (array => value%array)
        type is (integer(2))
          call check(nf90_put_att(ncid, varid, item%key, array))
        type is (integer(4))
          call check(nf90_put_att(ncid, varid, item%key, array))
        type is (integer(8))
          call check(nf90_put_att(ncid, varid, item%key, array))
        type is (real(4))
          call check(nf90_put_att(ncid, varid, item%key, array))
        type is (real(8))
          call check(nf90_put_att(ncid, varid, item%key, array))
        end select
      end select
    end do
  
  end subroutine netcdf_define_attribute
  
  
  subroutine netcdf_define_variable(ncid, variables, dimids)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables
    integer                        , intent(in) :: dimids(:)

    integer                                     :: varid
    integer                                     :: i
    type(variable_type)   , pointer             :: variable
    type(linked_list_type), pointer             :: attributes

    do i=1, variables%size
      variable => variables%value_at(i)
      if (variable%xtype == -99999)   stop "Error: data type of variable not define."
      if (.not. allocated(variable%name)) stop "Error: name attribute of variable not found."
      print*, " --- "//variable%name
      call check(nf90_def_var(ncid, variable%name, variable%xtype, dimids, varid))
      call variable%set_varid(varid)
      attributes => variable%get_attributes()
      call netcdf_define_attribute(ncid, varid, variable%get_attributes())
    end do
      
  end subroutine netcdf_define_variable


  subroutine netcdf_write_coordinate(ncid, dims)

    integer              , intent(in) :: ncid
    class(dimension_type), intent(in) :: dims

    integer                           :: varid 
    integer :: k                       

    if (associated(dims%value)) then
      varid = dims%get_varid()
      select type (value => dims%value)
      type is (integer(2))
        call check(nf90_put_var(ncid, varid, value))
      type is (integer(4))
        call check(nf90_put_var(ncid, varid, value))
      type is (integer(8))
        call check(nf90_put_var(ncid, varid, value))
      type is (real(4))
        call check(nf90_put_var(ncid, varid, value))
      type is (real(8))
        call check(nf90_put_var(ncid, varid, value))
      end select
    else
      stop "  Error: dimension value not set."
    end if

  end subroutine netcdf_write_coordinate


  subroutine netcdf_write_variable(ncid, variables)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables

    integer                                     :: i
    type(variable_type)   , pointer             :: variable
    integer                                     :: varid

    do i=1, variables%size
      variable => variables%value_at(i)
      print*, " --- "//variable%name
      varid = variable%get_varid()
      if (associated(variable%value_2d)) then
        select type (value => variable%value_2d)
        type is (integer(2))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_put_var(ncid, varid, value))
        end select
      end if
      if (associated(variable%value_3d)) then
        select type (value => variable%value_3d)
        type is (integer(2))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_put_var(ncid, varid, value))
        end select
      end if
      if (associated(variable%value_4d)) then
        select type (value => variable%value_4d)
        type is (integer(2))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_put_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_put_var(ncid, varid, value))
        end select
      end if    
    end do
    
  end subroutine netcdf_write_variable

end module netcdf_i_mod
