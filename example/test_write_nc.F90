program write_demo

  use easy_netcdf

  type(netcdf_type)    :: F
  type(variable_type)  :: TMP
  type(dimension_type) :: LON
  type(dimension_type) :: LAT

  call F%add_file("./test.nc", "w")

  LON%name = "lon"
  LON%xtype = int4
  call LON%attribute("long_name", "longitude")
  LON = [1, 2]

  LAT%name = "lat"
  LAT%xtype = int4
  call LAT%attribute("long_name", "latitude")
  LAT = [1, 2]

  TMP%name = "TMP"
  TMP%xtype = int4
  call TMP%dimension([LON, LAT])
  call TMP%attribute("units", "K")
  call TMP%attribute("range", [-100, 100])
  TMP = reshape([26, 27, 28, 27], [2, 2])

  call F%add_variable(variable=TMP)
  call F%add_variable(variable=LON)
  call F%add_variable(variable=LAT)
  call F%attribute("author", "onepieceze")

  call F%write()

end program write_demo
