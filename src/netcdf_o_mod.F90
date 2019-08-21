module netcdf_o_mod

  use netcdf
  use netcdf_param_mod
  use linked_list_mod
  use netcdf_tool_mod
  use dimension_type_mod
  use variable_type_mod

  implicit none

  private

  public :: netcdf_read_coordinate
  public :: netcdf_read_attribute
  public :: netcdf_read_variable

contains

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

  
  subroutine netcdf_read_coordinate(ncid, dimensions)
  
    integer               , intent(in)  :: ncid
    type(linked_list_type), intent(in)  :: dimensions

    integer                             :: dimension_number
    integer               , allocatable :: dimids(:)
    integer                             :: include_parents
    integer                             :: i, j
    type(dimension_type)  , pointer     :: dimension
    character(60)                       :: dimension_name
    integer                             :: length
    integer                             :: varid

    call check(nf90_inquire(ncid, ndimensions=dimension_number))

    allocate(dimids(dimension_number))
    
    call check(nf90_inq_dimids(ncid, dimension_number, dimids, include_parents))

    do i=1, dimensions%size
      dimension => dimensions%value_at(i)
      do j=1, dimension_number
        call check(nf90_inquire_dimension(ncid, dimids(j), dimension_name, length))
        if (dimension_name /= dimension%name) cycle
        call check(nf90_inq_varid(ncid, dimension_name, varid))
        select type (value => dimension%value)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value))
        end select
        call netcdf_read_attribute(ncid, varid, dimension%get_attributes())
      end do
    end do
  
  end subroutine netcdf_read_coordinate


  subroutine netcdf_read_variable(ncid, variables)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables

    integer                                     :: i
    type(variable_type)   , pointer             :: variable
    integer                                     :: varid

    do i=1, variables%size
      variable => variables%value_at(i)
      call check(nf90_inq_varid(ncid, variable%name, varid))
      if (associated(variable%value_2d)) then
        select type (value => variable%value_2d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value))
        end select
      end if
      if (associated(variable%value_3d)) then
        select type (value => variable%value_3d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value))
        end select
      end if
      if (associated(variable%value_3d)) then
        select type (value => variable%value_4d)
        type is (integer(2))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (integer(8))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(4))
          call check(nf90_get_var(ncid, varid, value))
        type is (real(8))
          call check(nf90_get_var(ncid, varid, value))
        end select
      end if
      call netcdf_read_attribute(ncid, varid, variable%get_attributes())
    end do

  end subroutine netcdf_read_variable
  

end module netcdf_o_mod
