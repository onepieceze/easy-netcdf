module netcdf_o_mod

  use netcdf
  use netcdf_param_mod
  use linked_list_mod
  use netcdf_tool_mod
  use variable_type_mod

  implicit none

  private

  public :: netcdf_read_coordinate
  public :: netcdf_read_attribute
  public :: netcdf_read_variable

contains
  
  subroutine netcdf_read_coordinate(ncid, varid, data)
  
    integer ,          intent(in) :: ncid
    integer ,          intent(in) :: varid
    class(*), pointer, intent(in) :: data(:)

    select type (data)
    type is (integer(2))
      call check(nf90_get_var(ncid, varid, data))
    type is (integer(4))
      call check(nf90_get_var(ncid, varid, data))
    type is (integer(8))
      call check(nf90_get_var(ncid, varid, data))
    type is (real(4))
      call check(nf90_get_var(ncid, varid, data))
    type is (real(8))
      call check(nf90_get_var(ncid, varid, data))
    end select
  
  end subroutine netcdf_read_coordinate


  subroutine netcdf_read_attribute(ncid, varid, attributes)

    integer                             , intent(in) :: ncid
    integer                             , intent(in) :: varid
    type(linked_list_type)     , pointer, intent(in) :: attributes

    type(linked_list_item_type), pointer             :: item
    integer                                          :: i
    class(*)                   , pointer             :: value

    if (.not. associated(attributes)) return

    do i=1, attributes%size
      item => attributes%item_at(i)
      value => item%value
      select type (value)
      type is (character(*))
        call check(nf90_get_att(ncid, varid, item%key, value))
      type is (integer(2))
        call check(nf90_get_att(ncid, varid, item%key, value))
      type is (integer(4))
        call check(nf90_get_att(ncid, varid, item%key, value))
      type is (integer(8))
        call check(nf90_get_att(ncid, varid, item%key, value))
      type is (real(4))
        call check(nf90_get_att(ncid, varid, item%key, value))
      type is (real(8))
        call check(nf90_get_att(ncid, varid, item%key, value))
        type is (array_2d_type)
        select type (array => value%array)
        type is (integer(2))
          call check(nf90_get_att(ncid, varid, item%key, array))
        type is (integer(4))
          call check(nf90_get_att(ncid, varid, item%key, array))
        type is (integer(8))
          call check(nf90_get_att(ncid, varid, item%key, array))
        type is (real(4))
          call check(nf90_get_att(ncid, varid, item%key, array))
        type is (real(8))
          call check(nf90_get_att(ncid, varid, item%key, array))
        end select
      end select
    end do

  end subroutine netcdf_read_attribute


  subroutine netcdf_read_variable(ncid, variables)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables

    integer                                     :: i
    type(variable_type)   , pointer             :: variable
    integer                                     :: varid
    class(*)              , pointer             :: value_2d(:, :)
    class(*)              , pointer             :: value_3d(:, :, :)
    class(*)              , pointer             :: value_4d(:, :, :, :)

    do i=1, variables%size
      variable => variables%value_at(i)
      call check(nf90_inq_varid(ncid, variable%name, varid))
      if (associated(variable%value_2d)) then
        value_2d => variable%value_2d
        select type (value_2d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value_2d))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value_2d))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value_2d))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value_2d))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value_2d))
        end select
      end if
      if (associated(variable%value_3d)) then
        value_3d => variable%value_3d
        select type (value_3d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value_3d))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value_3d))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value_3d))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value_3d))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value_3d))
        end select
      end if
      if (associated(variable%value_3d)) then
        value_4d => variable%value_4d
        select type (value_4d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value_4d))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value_4d))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value_4d))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value_4d))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value_4d))
        end select
      end if
      call netcdf_read_attribute(ncid, varid, variable%get_attributes())
    end do

  end subroutine netcdf_read_variable
  

end module netcdf_o_mod
