program read_demo

  use easy_netcdf

  type(netcdf_type)    :: f
  type(variable_type)  :: TMP
  integer              :: lon(2)
  integer              :: lat(2)
  integer              :: data(2, 2)
  character(30)        :: x_long_name
  character(30)        :: x_long_name
  character(30)        :: unit
  character            :: auther


  call f%add_file("./test.nc", "r")

  f%x%name = "lon"
  call f%x%attributie("long_name", x_long_name)
  f%x = lon

  f%y%name = "lat"
  call f%y%attributie("long_name", y_long_name)
  f%y = lat

  call f%global("auther", auther)

  TMP%name = "TMP"
  call TMP%attribute("unit", unit)
  TMP = data

  call f%add_variable(TMP)
  
  call f%read()

  print*, lon
  print*, x_long_name
  print*, lat
  print*, y_long_name
  print*, auther
  print*, TMP
  print*, unit

end program read_demo
