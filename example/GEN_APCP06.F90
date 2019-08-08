subroutine netcdf_define_variable(this, var_name, data_type, var_att_list, init_time)

  type(netcdf_type)  , intent(inout)        :: ncid
  character(*)       , intent(in)           :: var_name
  integer            , intent(in)           :: data_type
  type(datetime_type), intent(in), optional :: init_time

  integer                                   :: varid

  if (.not. allocated(this%iotype)) stop "Error: netcdf file have not open or create."
  if (this%iotype /= "write")       stop "Error: netcdf open model is not write model."
  if (.not. allocated(this%dimdis)) stop "Error: netcdf file dimension was not define."

  if(var_name(1:4) == 'APCP') then
    if(.not. present(init_time)) stop "Error: wirting APCP data should input initial time"
    call check(nf90_def_var(ncid, "APCP", data_type, dimids, varid))
    call check(nf90_put_att(ncid, varid, "long_name", "Total_precipitation"))
    select case(var_name(6:len_trim(var_name)))
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
    end select
  end if

  variable_list%append(var_name, varid)
  
end subroutine netcdf_define_variable


subroutine netcdf_define_global(ncid, data_type, met_version)

  integer, intent(in)                :: ncid
  character(*), intent(in)           :: data_type
  character(*), intent(in), optional :: met_version
  
  type(datetime_type)                :: datetime

  call datetime%init()

  select case(data_type)
  case('CMORPH_0P10')
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
  case default
    call check(nf90_put_att(ncid, nf90_global, "creation_date", trim(datetime%isoformat())))
  end select

  call check(nf90_enddef(ncid))

end subroutine netcdf_define_global 