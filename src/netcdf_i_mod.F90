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

  subroutine netcdf_define_attribute(ncid, varid, attributes)

    integer                             , intent(in) :: ncid
    integer                             , intent(in) :: varid
    type(linked_list_type)     , pointer, intent(in) :: attributes 
  
    type(linked_list_item_type), pointer             :: item
    integer                                          :: i
  
    if (.not. associated(attributes)) return
  
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

  
  subroutine netcdf_define_coordinate(ncid, dimensions)
  
    integer               , intent(in) :: ncid
    type(linked_list_type), intent(in) :: dimensions
    
    type(dimension_type)  , pointer    :: dimension
    integer                            :: i
    integer                            :: dimid
    integer                            :: varid

    do i=1, dimensions%size
      dimension => dimensions%value_at(i)
      if (dimension%unlimited) then
        call check(nf90_def_dim(ncid, dimension%name, nf90_unlimited, dimid))
      else
        call check(nf90_def_dim(ncid, dimension%name, dimension%get_length(), dimid))
      end if
      call check(nf90_def_var(ncid, dimension%name, dimension%xtype, dimid, varid))
      call dimension%set_dimid(dimid)
      call dimension%set_varid(varid)
      call netcdf_define_attribute(ncid, varid, dimension%get_attributes())
    end do

  end subroutine netcdf_define_coordinate
  
  
  subroutine netcdf_define_variable(ncid, variables, dimensions)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables
    type(linked_list_type), pointer, intent(in) :: dimensions

    integer                                     :: varid
    integer                                     :: i, j, k
    type(variable_type)   , pointer             :: variable
    type(dimension_type)  , pointer             :: dimension
    character(60)         , allocatable         :: dimensions_name(:)
    integer               , allocatable         :: dimids(:)

    do i=1, variables%size
      variable => variables%value_at(i)
      if (variable%xtype == -99999)   stop "Error: data type of variable not define."
      if (.not. allocated(variable%name)) stop "Error: name attribute of variable not found."
      call variable%get_dimensions(dimensions_name)
      allocate(dimids(size(dimensions_name)))
      do j=1, dimensions%size
        dimension => dimensions%value_at(j)
        do k=1, size(dimensions_name)
          if (dimensions_name(k) == dimension%name) dimids(k) = dimension%get_dimid()
        end do
      end do
      call check(nf90_def_var(ncid, variable%name, variable%xtype, dimids, varid))
      call variable%set_varid(varid)
      call netcdf_define_attribute(ncid, varid, variable%get_attributes())
      deallocate(dimensions_name)
      deallocate(dimids)
    end do
      
  end subroutine netcdf_define_variable


  subroutine netcdf_write_coordinate(ncid, dimensions)

    integer                , intent(in) :: ncid
    class(linked_list_type), intent(in) :: dimensions

    integer                             :: i
    integer                             :: varid
    type(dimension_type)   , pointer    :: dimension                   

    do i=1, dimensions%size
      dimension => dimensions%value_at(i)
      if (associated(dimension%value)) then
        varid = dimension%get_varid()
        select type (value => dimension%value)
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
    end do
      
  end subroutine netcdf_write_coordinate


  subroutine netcdf_write_variable(ncid, variables)

    integer                        , intent(in) :: ncid
    type(linked_list_type), pointer, intent(in) :: variables

    integer                                     :: i
    type(variable_type)   , pointer             :: variable
    integer                                     :: varid

    do i=1, variables%size
      variable => variables%value_at(i)
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
