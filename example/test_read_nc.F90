program read_demo

  use easy_netcdf

  type(netcdf_type)    :: F
  type(variable_type)  :: TMP
  type(dimension_type) :: LON
  type(dimension_type) :: LAT
  integer              :: LON_value(2)
  integer              :: LAT_value(2)
  integer              :: TMP_value(2, 2)
  integer              :: range(2)
  character(30)        :: lon_long_name
  character(30)        :: lat_long_name
  character(30)        :: units
  character(30)        :: author


  call f%add_file("./test.nc", "r")

  LON%name = "lon"
  call LON%attribute("long_name", lon_long_name)
  LON = LON_value

  LAT%name = "lat"
  call LAT%attribute("long_name", lat_long_name)
  LAT = LAT_value

  call F%attribute("author", author)

  TMP%name = "TMP"
  call TMP%dimension([LON, LAT])
  call TMP%attribute("units", units)
  call TMP%attribute("range", range)
  TMP = TMP_value

  call F%add_variable(variable=TMP)
  call F%add_variable(variable=LON)
  call F%add_variable(variable=LAT)
  
  call F%read()

  print*, "lon_long_name:  ", lon_long_name
  print*, "lat_long_name:  ", lat_long_name
  print*, "author       :  ", author
  print*, "units        :  ", units
  print*, "lon          :  ", LON_value
  print*, "lat          :  ", LAT_value
  print*, "data         :  ", TMP_value
  print*, "range        :  ", range

end program read_demo
