module netcdf_io_mod

  !! The io module to create netcdf file. 

  use netcdf
  use datetime

  private

  public netcdf_create
  public netcdf_define_att
  public netcdf_write_data
  public netcdf_close

  interface netcdf_write_data
    module procedure netcdf_write_data_short
    module procedure netcdf_write_data_int
    module procedure netcdf_write_data_float
    module procedure netcdf_write_data_double 
  end interface netcdf_write_data

end interface

contains

  !!
  integer function netcdf_create(filename) result(ncid)

    character(*), intent(in) :: filename

    call check(nf90_create(filename, NF90_CLOBBER, ncid))

  end function netcdf_create

  !!  
  !! @parameter
  !!    ncid:
  !!    x
  !!
  !!
  !!
  !!
  subroutine netcdf_define_coordinate(ncid, nx, ny, nz, nt, dimids, x_varid, y_varid, z_varid, t_varid)

    integer, intent(in)            :: ncid
    logical, intent(in), optional  :: nx
    logical, intent(in), optional  :: ny
    logical, intent(in), optional  :: nz
    logical, intent(in), optional  :: nt

    integer, intent(inout)         :: dimids(:)
    integer, intent(out), optional :: x_varid
    integer, intent(out), optional :: y_varid
    integer, intent(out), optional :: z_varid
    integer, intent(out), optional :: t_varid

    integer :: x_dimid
    integer :: y_dimid
    integer :: z_dimid
    integer :: t_dimid

    if(present(nx)) then
      call check(nf90_def_dim(ncid, "lon", nx, x_dimid))
      call check(nf90_def_var(ncid, "lon", nf90_real, x_dimid, x_varid))
      call check(nf90_put_att(ncid, x_varid. "long_name", "longitude"))
      call check(nf90_put_att(ncid, x_varid, "units", "degrees_east"))
    end if

    if(present(ny)) then
      call check(nf90_def_dim(ncid, "lat", ny, y_dimid))
      call check(nf90_def_var(ncid, "lat", nf90_real, y_dimid, y_varid))
      call check(nf90_put_att(ncid, y_varid, "long_name", "latitude"))
      call check(nf90_put_att(ncid, y_varid, "units", "degrees_north"))
    end if

    if(present(nz)) then
      call check(nf90_def_dim(ncid, "level", nz, z_dimid))
      call check(nf90_def_var(ncid, "level", nf90_int, z_dimid, z_varid))
      call check(nf90_put_att(ncid, z_varid, "long_name", "Isobaric surface"))
      call check(nf90_put_att(ncid, z_varid, "units", "Pa"))
    end if

    if(present(nt)) then
      call check(nf90_def_dim(ncid, "time", nf90_unlimited, tdimid))
      call check(nf90_def_var(ncid, "time", nf90_double, t_dimid, t_varid))
      call check(nf90_put_att(ncid, t_varid, "long_name", "Time"))
      call check(nf90_put_att(ncid, t_varid, "units", "seconds since 1970-01-01 00:00"))
    end if

    select case(size(dimids))
    case(1)
      write(6, '(g0)') 'Error: dimension of netcdf can not equal 1.'
      stop
    case(2)
      dimids = [x_dimid, y_dimid]
    case(3)
      if(present(nz)) dimids = [x_dimid, y_dimid, z_dimid]
      if(present(nt)) dimids = [x_dimid, y_dimid, t_dimid]
    case(4)
      dimids = [x_dimid, y_dimid, z_dimid, t_dimid]
    end select

  end subroutine netcdf_define_coordinate

  subroutine netcdf_define_variable(ncid, var_name, dimids, init_time, varid)

    integer, intent(in)                       :: ncid
    character(*), intent(in)                  :: var_name
    integer, intent(in)                       :: dimids(:)
    type(datetime_type), intent(in), optional :: init_time

    integer, intent(out)                      :: varid

    if(var_name(1:4) == 'APCP') then
      if(.not. present(init_time)) stop "Error: wirting APCP data should input initial time"
      call check(nf90_def_var(ncid, "APCP", nf90_float, dimids, varid))
      call check(nf90_put_att(ncid, varid, "long_name", "Total_precipitation"))
      select case(var_name(6:7))
      case('A1')
        call check(nf90_put_att(ncid, varid, "level", "A1"))
        call check(nf90_put_att(ncid, varid, "accum_time", "010000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 3600))
      case('A3')
        call check(nf90_put_att(ncid, varid, "level", "A3"))
        call check(nf90_put_att(ncid, varid, "accum_time", "030000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 10800))
      case('A6')
        call check(nf90_put_att(ncid, varid, "level", "A6"))
        call check(nf90_put_att(ncid, varid, "accum_time", "060000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 21600))
      case('A12')
        call check(nf90_put_att(ncid, varid, "level", "A12"))
        call check(nf90_put_att(ncid, varid, "accum_time", "120000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 43200))
      case('A24')
        call check(nf90_put_att(ncid, varid, "level", "A24"))
        call check(nf90_put_att(ncid, varid, "accum_time", "240000"))
        call check(nf90_put_att(ncid, varid, "accum_time_sec", 86400))
      end select
      call check(nf90_put_att(ncid, varid, "units", "kg/m^2"))
      call check(nf90_put_att(ncid, varid, "grib_code", 61))
      call check(nf90_put_att(ncid, varid, "_FillValue", -999.0))
      call check(nf90_put_att(ncid, varid, "init_time", "00000000_000000"))
      call check(nf90_put_att(ncid, varid, "init_time_ut", 0))
      call check(nf90_put_att(ncid, varid, "valid_time", trim(init_time%format("%Y%m%d_%H%M%S"))))
      call check(nf90_put_att(ncid, varid, "valid_time_ut", init_time%timestamp()))
    else
      select case
      case()
    end if
    
  end subroutine netcdf_define_variable

  !!
  subroutine netcdf_define_global(ncid, data_type, met_version)

    integer, intent(in)                :: ncid
    character(*), intent(in)           :: data_type
    character(*), intent(in), optional :: met_version
    
    type(datetime_type)                :: datetime

    call datetime%init()

    select case(data_type)
    case('CMORPH_0P01')
      if(.not. present(met_version)) stop "Error: transport CMORPH to netcdf is for run met, met_version is not input"
      call check(nf90_put_att(ncid, nf90_global, "Profection", "LatLon"))
      call check(nf90_put_att(ncid, nf90_global, "lat_ll", "15.05 degrees_north"))
      call check(nf90_put_att(ncid, nf90_global, "lon_ll", "70.05 degrees_east"))
      call check(nf90_put_att(ncid, nf90_global, "delta_lat", "0.1 degrees"))
      call check(nf90_put_att(ncid, nf90_global, "delta_lon", "0.1 degrees"))
      call check(nf90_put_att(ncid, nf90_global, "Nlat", "440 grid_points"))
      call check(nf90_put_att(ncid, nf90_global, "Nlon", "700 grid_points"))
      call check(nf90_put_att(ncid, nf90_global, "MET_version", met_version))
      call check(nf90_put_att(ncid, nf90_global, "creation_date", trim(datetime%isoformat())))
    end select

    call check(nf90_enddef(ncid))

  end subroutine netcdf_define_global

  !!

  !!
  subroutine netcdf_write_coordinate

    

  
  end subroutine netcdf_write_coordinate

  !!
  subroutine netcdf_write_data()

  end subroutine netcdf_write_data

  !!
  subroutine netcdf_close(ncid)

    integer, intent(in) :: ncid

    call check(nf90_close(ncid))

  end subroutine netcdf_close


  !! 
  subroutine check(netcdf_status)

    integer, intent(in) :: netcdf_status

    if(netcdf_status /= nf90_noerr) then
        print*, 'Error:', trim(nf90_strerror(netcdf_status))
        stop
    end if

  end subroutine check







    

end module netcdf_io_mod

