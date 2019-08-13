module netcdf_i_mod

  use netcdf
  use linked_list_mod
  use netcdf_tool_mod

  implicit none

  private

  public :: netcdf_read_coordinate
  public :: netcdf_read_attribute
  public :: netcdf_read_variable

contains
  
  subroutine netcdf_read_coordinate(ncid, varid, data)
  
    integer ,          intent(in)    :: ncid
    integer ,          intent(in)    :: varid
    class(*), pointer, intent(inout) :: data(:)

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


  subroutine netcdf_read_attribute(ncid, varid, attribute)

    integer                             , intent(in)    :: ncid
    integer                             , intent(in)    :: varid
    type(linked_list_type)     , pointer, intent(inout) :: attribute

    type(linked_list_item_type), pointer                :: item
    integer                                             :: i

    do i=1, attribute%size
      item => attribute%value_at(i)
      select type (value => item%value)
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
      end select
    end do

  end subroutine netcdf_read_attribute


  subroutine netcdf_read_variable(ncid, variable_list)

    integer                             , intent(in) :: ncid
    type(linked_list_type)     , pointer, intent(in) :: variable_list

    integer                                          :: i
    integer                                          :: varid

    do i=1, variable_list%size
      select type (value => variable_list%value_at(i))
      type is (variable_type)
        call check(nf90_inq_varid(ncid, value%name, varid))
        if (associated(value%value_2d)) then
          select type (value_2d => value%value_2d)
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
        if (associated(value%value_3d)) then
          select type (value_3d => value%value_3d)
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
        if (associated(value%value_3d)) then
          select type (value_3d => value%value_3d)
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
        call netcdf_read_attribute(ncid, varid, value%get_attribute())
      type default
        stop "Error: variable type not match."
      end select
    end do

  end subroutine netcdf_read_variable
  

end module netcdf_i_mod