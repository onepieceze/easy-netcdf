module netcdf_tool_mod

  use netcdf

  implicit none

  private

  public :: check

contains

  subroutine check(netcdf_status)

    integer, intent(in) :: netcdf_status

    if(netcdf_status /= nf90_noerr) then
        print*, 'Error: ', trim(nf90_strerror(netcdf_status))
        stop
    end if

  end subroutine check


end module netcdf_tool_mod